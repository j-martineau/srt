#' @encoding UTF-8
#' @title Calculate Probability-Based Challenge Measures from Current-Year Success Indicators, Previous-Year Scaled Scores, and Demographics
#' @param x A data frame containing at least one current-year success indicator variable and at least one previous-year scaled score variable.
#' @param S A character vector of the names of variables in `x` containing current-year success indicators.
#' @param prev.ss A character vector of the names of variables in `x` containing previous-year scaled scores.
#' @param demos A character vector of the names of variables in `x` containing demographic variables.
#' @param square A logical scalar indicating whether to square the multiple variables in `prev.ss` in the logistic regression predicting each success indicator variable.
#' @param clear A logical scalar indicating whether to clear the console for major user updates.
#' @return A data frame with as many new student challenge variables as there are elements in the argument `Ss`.
#' @export
calc_Cs_P2 <- function(x, S, prev.ss, demos, square = T, clear = T) {
  NAC <- NA_character_; NAI <- NA_integer_; NAR <- NA_real_                                                              # for convenience
  uj::say("Calculate challenge measures from current-year success and prev-year scaled scores", lev = 1, clear = clear)  # major user update, lev 1
  uj::say("Validate function arguments", lev = 2)                                                                        # major user update, lev 2
  if (!uj::.atmrctdtf(x)) {uj::stopperr("[x] must be a rectangular atomic data frame.")}                                 # IF [x] is not an atomic rectangular data frame THEN throw an error
  uj::check_funs("cmp_str_vec", S = S, prev.ss = prev.ss, demos = demos)                                                 # check [S], [prev.ss] for complete string vec-ness
  uj::check_tf(square = square, clear = clear)                                                                           # check [square] and [clear] for logical scalar-ness
  uj::checkerr()                                                                                                         # check for banked errors
  vars <- base::colnames(x)                                                                                              # names of variables in [x]
  if (!base::all(S       %in% vars)) {uj::bankerr("Each value of [S] must be the name of a variable in [x]")}            # IF [S]       contains values not in [vars] THEN bank an error
  if (!base::all(prev.ss %in% vars)) {uj::bankerr("Each value of [prev.ss] must be the name of a variable in [x]")}      # IF [prev.ss] contains values not in [vars] THEN bank an error
  if (!base::all(demos   %in% vars)) {uj::bankerr("Each value of [demos] must be the name of a variable in [x]")}        # IF [prev.ss] contains values not in [vars] THEN bank an error
  uj::checkerr()                                                                                                         # check for banked errors
  uj::say("Calculate challenge measures", lev = 2)                                                                       # major user update, lev 2
  AIC <- tibble::tibble(C = NAC, subject = NAC, year = NAI, grade = NAI, square = NA, AIC = NAR, .rows = 0)              # initialize AIC vector
  for (SS in S) {                                                                                                        # FOR each success indicator
    lab <- uj::p0(uj::fsub(SS, "S", "C"), ".P2")                                                                         # : build label for challenge indicator
    uj::say("Calculate challenge measure [", lab, "] for success indicator [", SS, "]", lev = 3)                         # : major user update, lev 3
    CC <- uj::r(uj::nr(x), NA_integer_)                                                                                  # : init challenge measure
    for (s in uj::suv(x$subject)) {                                                                                      # : FOR each subject
      is <- x$subject == s                                                                                               # : : logically index matching cases of [x]
      for (y in uj::suv(x$year)) {                                                                                       # : : FOR each year
        iy <- x$year == y                                                                                                # : : : logically index matching cases of [x]
        for (g in uj::suv(x$grade)) {                                                                                    # : : : FOR each grade
          uj::say()                                                                                                      # : : : : minor user update
          ig <- x$grade == g                                                                                             # : : : : index matching cases of [x]
          ix <- is & iy & ig                                                                                             # : : : : index cases of [x] matching all criteria
          ix[base::is.na(ix)] <- F                                                                                       # : : : : replace any [NA] values with [FALSE]
          ix <- base::which(ix)                                                                                          # : : : : numerically index matching cases
          Y  <- x[ix, SS]                                                                                                # : : : : logistic regression outcome is the current-year success indicator
          X  <- x[ix, prev.ss, drop = F]                                                                                 # : : : : logistic regression predictor set 1 are previous-year scaled scores
          if (square) {                                                                                                  # : : : : IF prev-year scaled scores should also be squared
            X2 <- X                                                                                                      # : : : : : duplicate scaled scores
            for (j in 1:uj::nc(X)) {X2[ , j] <- X2[ , j] ^ 2}                                                            # : : : : : FOR each scaled score: square it
            X <- base::cbind(X, X2)                                                                                      # : : : : : bind the squared scaled scores to the predictors
          }                                                                                                              # : : : : END IF
          X2  <- x[ix, demos  , drop = F]                                                                                # : : : : logistic regression predictor set 2 are demographics
          X   <- base::cbind(X, X2)                                                                                      # : : : : bind the demographics to the predictors
          I   <- tibble::tibble(`(Intercept)` = base::rep(1, uj::nr(X)))                                                 # : : : : build a 1-col tibble with an intercept term
          X   <- base::cbind(I, X)                                                                                       # : : : : bind the intercept term to the predictors
          X   <- base::as.matrix(X)                                                                                      # : : : : coerce predictors to matrix
          Y   <- stats::glm.fit(X, Y, family = stats::binomial(link = "logit"))                                          # : : : : fit the logistic regression
          aic <- tibble::tibble(C = lab, subject = s, year = y, grade = g, square = square, AIC = Y$aic)                 # : : : : build the AIC row for the regression
          AIC <- base::rbind(AIC, aic)                                                                                   # : : : : append the AIC row
          Y   <- stats::fitted.values(Y)                                                                                 # : : : : get the fitted values
          CC[ix] <- 100 * (1 - Y)                                                                                        # : : : : convert to percent probability of failing to meet the cut sc
        }                                                                                                                # : : : END FOR
      }                                                                                                                  # : : END FOR
    }                                                                                                                    # : END FOR
    CC <- uj::name_cs(tibble::tibble(C = CC), lab)                                                                       # : put the new challenge measure in a named 1-column tibble
    x  <- base::cbind(x, CC)                                                                                             # : bind the new challenge measure to the data
  }                                                                                                                      # END FOR
  AIC1 <- attr(x, "AIC")
  if (!base::is.null(AIC1)) {AIC <- base::rbind(AIC1, AIC)}
  base::attr(x, "AIC") <- AIC
  x
}
