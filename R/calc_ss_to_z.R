#' @encoding UTF-8
#' @title Recalculate Scaled Scores as \eqn{z}-Scores for Use in Value-Added Modeling
#' @param x A data frame containing variables named `'year'`, `'subject'`, and `'grade'` as well as variables named in `curr.ss` and `priors`.
#' @param curr.ss Character scalar name of the variable containing current-year scaled scores.
#' @param priors Character vector of names of the variables containing prior scaled scores.
#' @param clear Logical scalar indicating whether to clear the console for user updates.
#' @return The argument `x` with \eqn{z}-scores attached for the variables named in `ss` and `priors`.
#' @export
calc_ss_to_z <- function(x, curr.ss, priors, clear = T) {
  uj::say("Calculate z-scores for scaled-score containing variables", lev = 1, clear = clear)                            # user update
  ssLabs <- base::c(curr.ss, priors)                                                                                     # names of variables containing scaled scores
  zLabs <- uj::p0("z.", ssLabs)                                                                                          # names of variables to contain z-scores
  nSS <- uj::N(ssLabs)                                                                                                   # number of variables containing scaled scores
  zs <- base::matrix(NA, nrow = uj::nr(x), ncol = nSS)                                                                   # create a matrix for z-scores
  zs <- uj::name_cols(zs, zLabs)                                                                                         # name its columns
  zs <- tibble::as_tibble(zs)                                                                                            # convert to tibble
  for (y in uj::suv(x$year)) {                                                                                           # FOR each (sorted) unique value of year
    for (g in uj::suv(x$grade)) {                                                                                        # : FOR each (sorted) unique value of grade
      for (s in uj::suv(x$subject)) {                                                                                    # : : FOR each (sorted) unique value of subject
        uj::say("For [year = ", y, "], [grade = ", g, "], and [subject = ", s, "]", lev = 2)                             # : : : user update
        i <- x$year == y & x$grade == g & x$subject == s                                                                 # : : : index matching rows of [x]
        i[base::is.na(i)] <- F                                                                                           # : : : set any [NA] index values to [FALSE]
        for (j in 1:nSS) {                                                                                               # : : : FOR each variable containing scaled scores
          sLab <- ssLabs[j]                                                                                              # : : : : get its name
          zs[i, zLabs[j]] <- (x[i, sLab] - uj::mean0(x[i , sLab])) / uj::sd0(x[i , sLab])                                # : : : : calculate and store its z-score
        }                                                                                                                # : : : END FOR
      }                                                                                                                  # : : END FOR
    }                                                                                                                    # : END FOR
  }                                                                                                                      # END FOR
  base::cbind(x, zs)                                                                                                     # bind the z-scores to the original data
}
