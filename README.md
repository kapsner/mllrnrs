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
| LearnerSurvCoxPHCox | survival::coxph | Cox Proportional Hazards Regression |
| LearnerSurvGlmnetCox | glmnet::glmnet | Regularized Cox Regression |
| LearnerSurvXgboostCox | xgboost::xgb.train | Cox Regression for right-censored data |
| LearnerSurvRangerCox | ranger::ranger | Random Survival Forest for right-censored data |

TODO:
- add lightgbm
- add xgboost
