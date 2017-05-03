###### Econ 722, PS6

#### Question 3

library("ABCV")

## DGP
nsim = 100;
T = 100;
k_max = 6; # max number of lags

aic_criterion_values_y1 <- matrix(NaN,nrow = nsim,ncol = k_max )
bic_criterion_values_y1 <- matrix(NaN,nrow = nsim,ncol = k_max )
crv_criterion_values_y1 <- matrix(NaN,nrow = nsim,ncol = k_max )

aic_criterion_values_y2 <- matrix(NaN,nrow = nsim,ncol = k_max )
bic_criterion_values_y2 <- matrix(NaN,nrow = nsim,ncol = k_max )
crv_criterion_values_y2 <- matrix(NaN,nrow = nsim,ncol = k_max )

for (ii in 1:nsim) {
  for (jj in 1:k_max) {
    y1 = dgp1(T,0.7,0)
    y2 = dgp2(T,0.6,0)
    y1_effective_AIC = y1[(k_max+1):T,1]
    y1_effective_BIC = y1[(k_max+1):T,1]
    y2_effective_AIC = y2[(jj+1):T,1]
    y2_effective_BIC = y2[(jj+1):T,1]
    resi1_aic <- correct_fit(y1,jj,k_max)
    resi2_aic <- correct_fit(y2,jj,k_max)
    resi1_bic <- correct_fit(y1,jj,k_max)
    resi2_bic <- correct_fit(y2,jj,k_max)

    aic_criterion_values_y1[ii,jj] = log(t(resi1_aic)%*%resi1_aic/(T-k_max-jj)) + 2*jj/(T-k_max-jj)
    bic_criterion_values_y1[ii,jj] = log(t(resi1_bic)%*%resi1_bic/(T-k_max-jj)) + jj*log(T-k_max-jj)/(T-k_max-jj)
    crv_criterion_values_y1[ii,jj] = cv_hv(y1,jj,10,20)

    aic_criterion_values_y2[ii,jj] = log(t(resi2_aic)%*%resi2_aic/(T)) + 2*jj/(T)
    bic_criterion_values_y2[ii,jj] = log(t(resi2_bic)%*%resi2_bic/(T-2*jj)) + jj*log(T-jj)/(T-jj)
    crv_criterion_values_y2[ii,jj] = cv_hv(y2,jj,10,20)
  }
}

aic_choice_y1 <- matrix(1,nrow = nsim, ncol = 1)
bic_choice_y1 <- matrix(1,nrow = nsim, ncol = 1)
crv_choice_y1 <- matrix(1,nrow = nsim, ncol = 1)

aic_choice_y2 <- matrix(1,nrow = nsim, ncol = 1)
bic_choice_y2 <- matrix(1,nrow = nsim, ncol = 1)
crv_choice_y2 <- matrix(1,nrow = nsim, ncol = 1)

for (ii in 1:nsim) {
  for (jj in 2:k_max) {
    if (aic_criterion_values_y1[ii,jj] < aic_criterion_values_y1[ii,(jj-1)]) {
      aic_choice_y1[ii,1] = jj
    }
    if (bic_criterion_values_y1[ii,jj] < bic_criterion_values_y1[ii,(jj-1)]) {
      bic_choice_y1[ii,1] = jj
    }
    if (crv_criterion_values_y1[ii,jj] < crv_criterion_values_y1[ii,(jj-1)]) {
      crv_choice_y1[ii,1] = jj
    }

    if (aic_criterion_values_y2[ii,jj] < aic_criterion_values_y2[ii,(jj-1)]) {
      aic_choice_y2[ii,1] = jj
    }
    if (bic_criterion_values_y2[ii,jj] < bic_criterion_values_y2[ii,(jj-1)]) {
      bic_choice_y2[ii,1] = jj
    }
    if (crv_criterion_values_y2[ii,jj] < crv_criterion_values_y2[ii,(jj-1)]) {
      crv_choice_y2[ii,1] = jj
    }
  }
}


aic_choice_y1 <- data.frame(aic_choice_y1)
bic_choice_y1 <- data.frame(bic_choice_y1)
crv_choice_y1 <- data.frame(crv_choice_y1)

aic_choice_y2 <- data.frame(aic_choice_y2)
bic_choice_y2 <- data.frame(bic_choice_y2)
crv_choice_y2 <- data.frame(crv_choice_y2)


aic_summary_y1 <- matrix(NaN,nrow = k_max, ncol = 1)
bic_summary_y1 <- matrix(NaN,nrow = k_max, ncol = 1)
crv_summary_y1 <- matrix(NaN,nrow = k_max, ncol = 1)

aic_summary_y2 <- matrix(NaN,nrow = k_max, ncol = 1)
bic_summary_y2 <- matrix(NaN,nrow = k_max, ncol = 1)
crv_summary_y2 <- matrix(NaN,nrow = k_max, ncol = 1)

for (ii in 1:k_max) {
  aic_summary_y1[ii,1] = sum(with(aic_choice_y1,aic_choice_y1==ii))
  bic_summary_y1[ii,1] = sum(with(bic_choice_y1,bic_choice_y1==ii))
  crv_summary_y1[ii,1] = sum(with(crv_choice_y1,crv_choice_y1==ii))

  aic_summary_y2[ii,1] = sum(with(aic_choice_y2,aic_choice_y2==ii))
  bic_summary_y2[ii,1] = sum(with(bic_choice_y2,bic_choice_y2==ii))
  crv_summary_y2[ii,1] = sum(with(crv_choice_y2,crv_choice_y2==ii))
}

