# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files
# https://github.com/Rdatatable/data.table/issues/5658
Sys.setenv("OMP_THREAD_LIMIT" = 2)
Sys.setenv("Ncpu" = 2)

library(testthat)
library(mllrnrs)

test_check("mllrnrs")
