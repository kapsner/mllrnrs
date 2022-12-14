# mllrnrs

<!-- badges: start -->
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R build status](https://github.com/kapsner/mllrnrs/workflows/R%20CMD%20Check%20via%20{tic}/badge.svg?branch=main)](https://github.com/kapsner/mllrnrs/actions)
[![R build status](https://github.com/kapsner/mllrnrs/workflows/lint/badge.svg?branch=main)](https://github.com/kapsner/mllrnrs/actions)
[![R build status](https://github.com/kapsner/mllrnrs/workflows/test-coverage/badge.svg?branch=main)](https://github.com/kapsner/mllrnrs/actions)
[![codecov](https://codecov.io/gh/kapsner/mllrnrs/branch/main/graph/badge.svg?branch=main)](https://app.codecov.io/gh/kapsner/mllrnrs)
<!-- badges: end -->

The goal of `mllrnrs` is to enhance the [`mlexperiments`](https://github.com/kapsner/mlexperiments) R package with learners. 

Currently implemented learners are:

| Name | Based on | Description / Tasks |
| ---- | -------- | ------------------- |
| LearnerGlmnet | `glmnet::glmnet` | General interface to `glmnet` (unit-tests available for families [`binomial`](tests/testthat/test-glmnet_binary.R) , [`multinomial`](tests/testthat/test-glmnet_multiclass.R), and [`regression`](tests/testthat/test-glmnet_regression.R) |
| LearnerLightgbm | `lightgbm::lgb.train` | General interface to `lightgbm` (unit-tests available for objectives [`binary`](tests/testthat/test-lightgbm_binary.R) , [`multiclass`](tests/testthat/test-lightgbm_multiclass.R), and [`regression`](tests/testthat/test-lightgbm_regression.R) |
| LearnerRanger | `ranger::ranger` | General interface to `ranger` (unit-tests available for tasks [`binary`](tests/testthat/test-ranger_binary.R) , [`multiclass`](tests/testthat/test-ranger_multiclass.R), and [`regression`](tests/testthat/test-ranger_regression.R) |
| LearnerXgboost | `xgboost::xgb.train` | General interface to `xgboost` (unit-tests available for objectives [`binary:logistic`](tests/testthat/test-xgboost_binary.R) , [`multi:softprob`](tests/testthat/test-xgboost_multiclass.R), and [`reg:squarederror`](tests/testthat/test-xgboost_regression.R) |

For a short introduction on how to use the learners together with the `mlexperiments` R package, please visit the [wiki page](https://github.com/kapsner/mllrnrs/wiki).

Some learner for survival tasks are implemented in the [`mlsurvlrnrs`](https://github.com/kapsner/mlsurvlrnrs) R package.

## Installation

To install the development version, run

```r
install.packages("remotes")
remotes::install_github("kapsner/mllrnrs")
```
