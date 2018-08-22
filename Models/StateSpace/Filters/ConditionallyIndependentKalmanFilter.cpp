/*
  Copyright (C) 2005-2018 Steven L. Scott

  This library is free software; you can redistribute it and/or modify it under
  the terms of the GNU Lesser General Public License as published by the Free
  Software Foundation; either version 2.1 of the License, or (at your option)
  any later version.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
  details.

  You should have received a copy of the GNU Lesser General Public License along
  with this library; if not, write to the Free Software Foundation, Inc., 51
  Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
*/

#include "Models/StateSpace/Filters/ConditionallyIndependentKalmanFilter.hpp"
#include "Models/StateSpace/MultivariateStateSpaceModelBase.hpp"
#include "LinAlg/DiagonalMatrix.hpp"
#include "LinAlg/QR.hpp"
#include "cpputil/math_utils.hpp"

namespace BOOM {
  namespace Kalman {
    namespace {
      using Marginal = ConditionallyIndependentMarginalDistribution;
    } // namespace 

    double Marginal::high_dimensional_threshold_factor_(1.0);
    
    Marginal::ConditionallyIndependentMarginalDistribution(
        ModelType *model, MarginalType *previous, int time_index)
        : MultivariateMarginalDistributionBase(model->state_dimension(),
                                               time_index),
          model_(model),
          previous_(previous) {}

    const MultivariateStateSpaceModelBase * Marginal::model() const {
      return model_;
    }

    SpdMatrix Marginal::direct_forecast_precision() const {
      SpdMatrix variance;
      if (previous()) {
        variance = previous()->state_variance();
      } else {
        variance = model_->initial_state_variance();
      }
      const Selector &observed(model_->observed_status(time_index()));
      SpdMatrix ans = model_->observation_coefficients(
          time_index(), observed)->sandwich(variance);
      ans.diag() += model_->observation_variance(time_index()).diag();
      return ans.inv();
    }
    
    SpdMatrix Marginal::forecast_precision() const {
      DiagonalMatrix observation_precision =
          model_->observation_variance(time_index()).inv();
      const Selector &observed(model_->observed_status(time_index()));
      const SparseKalmanMatrix *observation_coefficients(
          model_->observation_coefficients(time_index(), observed));

      SpdMatrix variance;
      if (previous()) {
        variance = previous()->state_variance();
      } else {
        variance = model_->initial_state_variance();
      }
      // 'inner' is  I + P * Z' Hinv Z
      Matrix inner = variance * observation_coefficients->inner(
          observation_precision.diag());
      inner.diag() += 1.0;
      SpdMatrix outer = inner.solve(variance);
      SpdMatrix ans = observation_precision.sandwich(
          observation_coefficients->sandwich(outer));
      ans *= -1;
      ans.diag() += observation_precision.diag();
      return ans;      
    }

    void Marginal::high_dimensional_update(
        const Vector &observation,
        const Selector &observed,
        const SparseKalmanMatrix &transition,
        const SparseKalmanMatrix &observation_coefficients) {
      Vector observation_mean = observed.select(
          observation_coefficients * state_mean());
      Vector observed_data = observed.select(observation);
      set_prediction_error(observed_data - observation_mean);

      // At this point the Kalman recursions compute the forecast precision Finv
      // and its log determinant.  However, we can get rid of the forecast
      // precision matrix, and replace it with the scaled error = Finv *
      // prediction_error.
      //
      // To evaluate the normal likelihood, we need the quadratic form:
      //   error * Finv * error == error.dot(scaled_error).
      // We also need the log determinant of Finv.
      //
      // The forecast_precision can be computed using the binomial inverse
      // theorem:
      //  (A + UBV).inv =
      //    A.inv - A.inv * U * (I + B * V * Ainv * U).inv * B * V * Ainv.
      //
      // When applied to F = H + Z P Z' the theorem gives
      //
      //   Finv = Hinv - Hinv * Z * (I + P Z' Hinv Z).inv * P * Z' * Hinv
      //
      // We don't compute Finv directly, we compute Finv * prediction_error.
      DiagonalMatrix observation_precision =
          model_->observation_variance(time_index()).inv();

      // Z_inner = Z' Hinv Z
      Matrix Z_inner = observation_coefficients.inner(
          observation_precision.diag());
      Matrix inner_matrix = state_variance() * Z_inner;
      inner_matrix.diag() += 1.0;
      QR inner_qr(inner_matrix);
      
      Vector scaled_error =
          observation_precision * 
          (observation_coefficients * inner_qr.solve(
              state_variance() * (observation_coefficients.Tmult(
                  observation_precision * prediction_error()))));
      set_scaled_prediction_error(
          observation_precision * prediction_error() - scaled_error);

      
      // The log determinant of F.inverse is the negative log of det(H + ZPZ').
      // That determinant can be computed using the "matrix determinant lemma,"
      // which says det(A + UV') = det(I + V' * A.inv * U) * det(A)
      //
      // Let A = H, U = Z, V' = PZ'.  Then det(F) = det(I + PZ' * Hinv * Z) * det(H)

      set_forecast_precision_log_determinant(
          -1 * (inner_qr.logdet() + observation_precision.logdet()));
    }
    
    void Marginal::low_dimensional_update(
        const Vector &observation,
        const Selector &observed,
        const SparseKalmanMatrix &transition,
        const SparseKalmanMatrix &observation_coefficients) {
      set_prediction_error(
          observation - observation_coefficients * state_mean());
      SpdMatrix forecast_variance = model_->observation_variance(time_index()) +
          observation_coefficients.sandwich(state_variance());
      SpdMatrix forecast_precision = forecast_variance.inv();
      set_forecast_precision_log_determinant(forecast_precision.logdet());
      set_scaled_prediction_error(forecast_precision * prediction_error());
      set_kalman_gain(transition * state_variance() *
                      observation_coefficients.Tmult(forecast_precision));
    }

    
  }  // namespace Kalman
}  // namespace BOOM