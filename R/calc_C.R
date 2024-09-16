#' @encoding UTF-8
#' @title Calculate Student Challenge as the Percent Likelihood of Non-Success
#' @param x A data.frame containing student-level data.
#' @param S A vector of \eqn{n_{student}} 0/1 student success indicators.
#' @param year Character scalar name of the variable in `x` containing years.
#' @param subject Character scalar name of the variable in `x` containing subjects.
#' @param grade Character scalar name of the variable in `x` containing grades.
#' @param prev.ss A character vector of previous-year scaled score values variable names.
#' @param demos An optional character vector of demographic variable names.
#' @param clear A logical scalar indicating whether to clear the console for user updates.
#' @return A numeric vector of \eqn{n_{student}} values in the interval \eqn{\left[0, 100\right]}.
#' @export
calc_C_p <- function(x, S, prev.ss, demos = NULL, clear = T) {
  uj::say("Calculate probability-based student challenge with [S = ", S, "]", lev = 1, clear = clear)                    # initial user update
  nx <- uj::nr(x)                                                                                                        # number of rows of input data
  p  <- base::rep(NA_real_, nx)                                                                                          # initialize a probability output vector
  uj::say("Identify unique data sets for computation of student challenge values", lev = 2)                              # user update before time-consuming step
  gs <- base::unique(x[ , base::c("year", "subject", "grade")])                                                          # unique observed combinations of grouping variables
  ng <- uj::nr(gs)                                                                                                       # number of group data sets
  for (i in 1:ng) {                                                                                                      # FOR each unique combination/data set
    uj::say("Calculate probability of success for grouping ", i, " of ", ng, lev = 2)                                    # : user update
    k    <- base::rep(T, nx) & x$year == gs$year[i] & x$subject == gs$subject[i] & x$grade == gs$grade[i]                # : initialize an indexing vector
    pss  <- x[k , prev.ss]                                                                                               # : extract prior scaled scores and store and extra for squared prior scaled scores
    dms  <- x[k , demos]                                                                                                 # : extract demographic variables
    Y    <- uj::av(x[k, S])                                                                                              # : extract the 0/1 success values
    I    <- tibble::tibble(`(Intercept)` = base::rep(1, uj::N(Y)))                                                       # : build intercept vector
    X    <- base::cbind(I, pss, dms)                                                                                     # : bind the prior scaled scores, their centered squares, and the demographic variables into a single predictor data.frame
    X    <- base::as.matrix(X)                                                                                           # : convert to matrix for use by [glm.fit]
    y    <- stats::glm.fit(X, Y, family = stats::binomial(link = "logit"))                                               # : estimate a logistic regression of the probability of student success
    y    <- stats::fitted.values(y)                                                                                      # : extract fitted values
    p[k] <- y                                                                                                            # : store in the indexed locations of the output value
  }                                                                                                                      # END FOR
  (1 - p) * 100                                                                                                          # convert to challenge from easiness and rescale to [0, 100]
}

#' @encoding UTF-8
#' @title Calculate Student Challenge as z-Score Distance of Predicted Score to Cut Score
#' @param x A data.frame containing student-level data.
#' @param cut The value of the cut score
#' @param year Character scalar name of the variable in `x` containing years.
#' @param subject Character scalar name of the variable in `x` containing subjects.
#' @param grade Character scalar name of the variable in `x` containing grades.
#' @param prev.ss A character vector of previous-year scaled score values variable names.
#' @param demos An optional character vector of demographic variable names.
#' @param clear A logical scalar indicating whether to clear the console for user updates.
#' @return A numeric vector of \eqn{n_{student}} values in the interval \eqn{\left[0, 100\right]}.
#' @export
calc_C_z <- function(x, cut, ss, prev.ss, demos = NULL, square = T, clear = T) {
  uj::say("Calculate z-score-based student challenge with [cut = ", cut, "]", lev = 1, clear = clear)                    # initial user update
  nx <- uj::nr(x)                                                                                                        # number of rows of input data
  z  <- base::rep(NA_real_, nx)                                                                                          # initialize a z-score distance output vector
  uj::say("Identity unique data sets for computation of student challenge values", lev = 2)                              # user update before time-consuming step
  gs <- base::unique(x[ , base::c("year", "subject", "grade")])                                                          # unique observed combinations of grouping variables
  ng <- uj::nr(gs)                                                                                                       # number of group data sets
  for (i in 1:ng) {                                                                                                      # FOR each unique combination/data set
    uj::say("Calculate predicted z-score distance to cut for grouping ", i, " of ", ng, lev = 2)                         # : user update
    k    <- base::rep(T, nx) & x$year == gs$year[i] & x$subject == gs$subject[i] & x$grade == gs$grade[i]                # : initialize an indexing vector
    pss  <- ps2 <- x[k , prev.ss]                                                                                        # : extract prior scaled scores and store and extra for squared prior scaled scores
    dms  <- x[k , demos]                                                                                                 # : extract demographic variables
    for (m in 1:uj::N(prev.ss)) {ps2[ , m] <- (uj::av(pss[ , m]) - base::mean(uj::av(pss[ , m]), na.rm = T)) ^ 2}        # : FOR each prior score variable, calculate its centered square
    Y    <- uj::av(x[k, ss])                                                                                             # : get the value to be predicted (current year scaled score)
    mnY  <- uj::mean0(Y)                                                                                                 # : get the mean of current year scaled scores
    sdY  <- uj::sd0(Y)                                                                                                   # : get the SD of current-year scaled scores
    cutZ <- (cut - mnY) / sdY                                                                                            # : z-score location of cut score
    I    <- tibble::tibble(`(Intercept)` = base::rep(1, uj::N(Y)))                                                       # : build intercept vector
    X    <- uj::f0(square, base::cbind(I, pss, ps2, dms), base::cbind(I, pss, dms))                                      # : bind the prior scaled scores, their centered squares, and the demographic variables into a single predictor data.frame
    X    <- base::as.matrix(X)                                                                                           # : convert to matrix for use with [glm.fit]
    y    <- stats::glm.fit(X, Y, family = stats::gaussian(link = "identity"))                                            # : estimate a regression of current scaled score
    y    <- stats::fitted.values(y)                                                                                      # : extract fitted values
    z[k] <- cutZ - (y - mnY) / sdY                                                                                       # : store in the indexed locations of the output value
  }                                                                                                                      # END FOR
  z
}
