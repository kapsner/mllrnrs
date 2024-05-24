

# mllrnrs

<!-- badges: start -->

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://www.r-pkg.org/badges/version/mllrnrs)](https://cran.r-project.org/package=mllrnrs)
[![CRAN
checks](https://badges.cranchecks.info/worst/mllrnrs.svg)](https://cran.r-project.org/web/checks/check_results_mllrnrs.html)
[![](http://cranlogs.r-pkg.org/badges/grand-total/mllrnrs?color=blue)](https://cran.r-project.org/package=mllrnrs)
[![](http://cranlogs.r-pkg.org/badges/last-month/mllrnrs?color=blue)](https://cran.r-project.org/package=mllrnrs)
[![Dependencies](https://tinyverse.netlify.app/badge/mllrnrs)](https://cran.r-project.org/package=mllrnrs)
[![R build
status](https://github.com/kapsner/mllrnrs/workflows/R%20CMD%20Check%20via%20%7Btic%7D/badge.svg)](https://github.com/kapsner/mllrnrs/actions)
[![R build
status](https://github.com/kapsner/mllrnrs/workflows/lint/badge.svg)](https://github.com/kapsner/mllrnrs/actions)
[![R build
status](https://github.com/kapsner/mllrnrs/workflows/test-coverage/badge.svg)](https://github.com/kapsner/mllrnrs/actions)
[![](https://codecov.io/gh/https://github.com/kapsner/mllrnrs/branch/main/graph/badge.svg)](https://codecov.io/gh/https://github.com/kapsner/mllrnrs)

The `mllrnrs` R package ships with additional ML learners for
[`mlexperiments`](https://github.com/kapsner/mlexperiments).

Currently implemented learners are:

| Name            | Based on              | Description / Tasks                                                                                                                                                                                                                                            |
|-----------------|-----------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| LearnerGlmnet   | `glmnet::glmnet`      | General interface to `glmnet` (examples available for families [`binomial`](tests/testthat/test-glmnet_binary.R) , [`multinomial`](tests/testthat/test-glmnet_multiclass.R), and [`regression`](tests/testthat/test-glmnet_regression.R)                       |
| LearnerLightgbm | `lightgbm::lgb.train` | General interface to `lightgbm` (examples available for objectives [`binary`](tests/testthat/test-lightgbm_binary.R) , [`multiclass`](tests/testthat/test-lightgbm_multiclass.R), and [`regression`](tests/testthat/test-lightgbm_regression.R)                |
| LearnerRanger   | `ranger::ranger`      | General interface to `ranger` (examples available for tasks [`binary`](tests/testthat/test-ranger_binary.R) , [`multiclass`](tests/testthat/test-ranger_multiclass.R), and [`regression`](tests/testthat/test-ranger_regression.R)                             |
| LearnerXgboost  | `xgboost::xgb.train`  | General interface to `xgboost` (examples available for objectives [`binary:logistic`](tests/testthat/test-xgboost_binary.R) , [`multi:softprob`](tests/testthat/test-xgboost_multiclass.R), and [`reg:squarederror`](tests/testthat/test-xgboost_regression.R) |

For a short introduction on how to use the learners together with the
`mlexperiments` R package, please visit the [wiki
page](https://github.com/kapsner/mllrnrs/wiki).

Some learner for survival tasks are implemented in the
[`mlsurvlrnrs`](https://github.com/kapsner/mlsurvlrnrs) R package.

## Installation

`mllrnrs` can be installed directly from CRAN:

``` r
install.packages("mllrnrs")
```

To install the development version, run

``` r
install.packages("remotes")
remotes::install_github("kapsner/mllrnrs")
```
