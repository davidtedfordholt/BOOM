// Copyright 2018 Google LLC. All Rights Reserved.
/*
  Copyright (C) 2007 Steven L. Scott

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
*/
#include "Models/Glm/PosteriorSamplers/MultivariateRegressionSampler.hpp"
#include "distributions.hpp"

namespace BOOM {

  typedef MultivariateRegressionSampler MRS;

  MRS::MultivariateRegressionSampler(
      MultivariateRegressionModel *model,
      const Matrix &Beta_guess,
      double prior_beta_nobs,
      double prior_df,
      const SpdMatrix &Sigma_guess,
      RNG &seeding_rng)
      : PosteriorSampler(seeding_rng),
        model_(model),
        SS_(Sigma_guess * prior_df),
        prior_df_(prior_df),
        Ominv_(model_->xdim()),
        beta_prior_mean_(Beta_guess) {
    double kappa = prior_beta_nobs;
    Ominv_.set_diag(kappa);
    ldoi_ = model_->ydim() * log(kappa);
  }

  double MRS::logpri() const {
    const SpdMatrix &Siginv(model_->Siginv());
    double ldsi = model_->ldsi();
    const Matrix &Beta(model_->Beta());
    double ans = dWish(Siginv, SS_, prior_df_, true);
    ans += dmatrix_normal_ivar(Beta, beta_prior_mean_, Ominv_, ldoi_,
                               Siginv, ldsi, true);
    return ans;
  }

  void MRS::draw() {
    draw_Sigma();
    draw_Beta();
  }

  void MRS::draw_Beta() {
    Ptr<NeMvRegSuf> s(model_->suf().dcast<NeMvRegSuf>());
    SpdMatrix ivar = Ominv_ + s->xtx();
    Matrix Mu = s->xty() + Ominv_ * beta_prior_mean_;
    Mu = ivar.solve(Mu);
    Matrix ans = rmatrix_normal_ivar(Mu, ivar, model_->Siginv());
    model_->set_Beta(ans);
  }

  void MRS::draw_Sigma() {
    Ptr<MvRegSuf> s(model_->suf());
    SpdMatrix sumsq = SS_ + s->SSE(model_->Beta());
    double df = prior_df_ + s->n();
    SpdMatrix ans = rWish(df, sumsq.inv());
    model_->set_Siginv(ans);
  }
}  // namespace BOOM