#include "gtest/gtest.h"
#include "Models/StateSpace/StateSpaceModel.hpp"
#include "Models/StateSpace/StateModels/LocalLinearTrend.hpp"
#include "Models/HMM/GeneralHmmStateSpaceWrapper.hpp"
#include "Models/HMM/PosteriorSamplers/LiuWestParticleFilter.hpp"
#include "distributions.hpp"

#include "test_utils/test_utils.hpp"
#include <fstream>

namespace {
  using namespace BOOM;
  using std::endl;
  using std::cout;
  
  class LiuWestParticleFilterTest : public ::testing::Test {
   protected:
    LiuWestParticleFilterTest() {
      GlobalRng::rng.seed(8675309);
    }
  };

  TEST_F(LiuWestParticleFilterTest, Basics) {
  }

  TEST_F(LiuWestParticleFilterTest, Filtering) {
    Vector true_state(200);
    Vector observations(true_state.size());
    std::vector<Ptr<DoubleData>> data;
    for (int i = 0; i < true_state.size(); ++i) {
      true_state[i] = (i == 0) ? rnorm(0, 3) : rnorm(true_state[i-1], .1);
      observations[i] = true_state[i] + rnorm(0, .5);
      data.push_back(new DoubleData(observations[i]));
    }

    NEW(StateSpaceModel, base_model)();
    EXPECT_TRUE(base_model->observation_model() != nullptr);
    EXPECT_DOUBLE_EQ(base_model->observation_model()->sigsq(), 1.0);
    
    NEW(LocalLinearTrendStateModel, trend)();
    base_model->add_state(trend);
    NEW(GeneralHmmStateSpaceWrapper, model)(base_model);
    int number_of_particles = 1000;
    LiuWestParticleFilter filter(model, number_of_particles);
    Matrix initial_parameters(number_of_particles, 4);
    Matrix initial_state(number_of_particles, 2);

    // The parameters of the model are
    // 1) a scalar variance.
    // 2) The lower triangle of a variance matrix.
    
    for (int i = 0; i < number_of_particles; ++i) {
      initial_parameters.row(i)[0] = 1.0 / rgamma(1, 1);
      SpdMatrix Sigma(2);
      Sigma.randomize();
      initial_parameters.row(i)[1] = Sigma(0, 0);
      initial_parameters.row(i)[2] = Sigma(0, 1);
      initial_parameters.row(i)[3] = Sigma(1, 1);

      initial_state.row(i)[0] = rnorm(0, 10);
      initial_state.row(i)[1] = rnorm(0, 10);
    }
    filter.set_particles(initial_state, initial_parameters);
    
    for (int i = 0; i < data.size(); ++i) {
      filter.update(GlobalRng::rng, *data[i], i);
    }

    std::ofstream param_file("params.out");
    param_file << filter.parameter_distribution(&GlobalRng::rng);

    std::ofstream weight_file("weights.out");
    weight_file << filter.particle_weights();

    std::ofstream state_file("final_state.out");
    state_file << filter.state_distribution(&GlobalRng::rng);
  }
  
}  // namespace