# The function partial_eta_sq_ci() is licensed unter MIT
# Original Author: David Stanley
# Code copied from https://github.com/dstanley4/apaTables
# (https://github.com/dstanley4/apaTables/blob/master/R/etaSquaredCI.R)
#
#   Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
#   The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#' @keywords internal
.ci_partial_eta_squared <- function(F.value, df1, df2, conf.level = .90) {
  F_value <- F.value

  conf_level <- conf.level

  F_limits <- .confint_ncg(
    F.value = F_value,
    df.1 = df1,
    df.2 = df2,
    conf.level = conf_level
  )

  LL_lambda <- F_limits$Lower.Limit
  UL_lambda <- F_limits$Upper.Limit


  LL_partial_eta2 <- get_partial_eta2_from_lambda(lambda = LL_lambda, df1 = df1, df2 = df2)
  UL_partial_eta2 <- get_partial_eta2_from_lambda(lambda = UL_lambda, df1 = df1, df2 = df2)


  if (is.na(LL_partial_eta2)) {
    LL_partial_eta2 <- 0
  }

  if (is.na(UL_partial_eta2)) {
    UL_partial_eta2 <- 1
  }

  list(LL = LL_partial_eta2, UL = UL_partial_eta2)
}


get_partial_eta2_from_lambda <- function(lambda, df1, df2) {
  lambda / (lambda + df1 + df2 + 1)
}
