# mllrnrs

<!-- badges: start -->
[![R build status](https://github.com/kapsner/mllrnrs/workflows/R%20CMD%20Check%20via%20{tic}/badge.svg)](https://github.com/kapsner/mllrnrs/actions)
[![R build status](https://github.com/kapsner/mllrnrs/workflows/lint/badge.svg)](https://github.com/kapsner/mllrnrs/actions)
[![R build status](https://github.com/kapsner/mllrnrs/workflows/test-coverage/badge.svg)](https://github.com/kapsner/mllrnrs/actions)
[![codecov](https://codecov.io/gh/kapsner/mllrnrs/branch/main/graph/badge.svg)](https://app.codecov.io/gh/kapsner/mllrnrs)
<!-- badges: end -->

The goal of mllrnrs is to provide a set of R6-based learners that can be used with the [`mlexperiments`](https://github.com/kapsner/mlexperiments) R package.

Currently implemented are:

| Name | Based on | Description / Tasks |
| ---- | -------- | ------------------- |
| LearnerSurvCoxPHCox | `survival::coxph` | Cox Proportional Hazards Regression |
| LearnerSurvGlmnetCox | `glmnet::glmnet` | Regularized Cox Regression |
| LearnerSurvXgboostCox | `xgboost::xgb.train` | Cox Regression for right-censored data |
| LearnerSurvRangerCox | `ranger::ranger` | Random Survival Forest for right-censored data |
| LearnerXgboost | `xgboost::xgb.train` | General interface to `xgboost` (unit-tests available for objectives [`binary:logistic`](tests/testthat/test-xgboost_binary.R) , [`multi:softprob`](tests/testthat/test-xgboost_multiclass.R), and [`reg:squaredlogerror`](tests/testthat/test-xgboost_regression.R) |
| LearnerLightgbm | `lightgbm::lgb.train` | General interface to `lightgbm` (unit-tests available for objectives [`binary`](tests/testthat/test-lightgbm_binary.R) , [`multiclass`](tests/testthat/test-lightgbm_multiclass.R), and [`regression`](tests/testthat/test-lightgbm_regression.R) |

TODO:
- add lightgbm (multiclass, binary, regression)
- add xgboost (multiclass (done), binary (done), regression)
