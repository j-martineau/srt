#' @encoding UTF-8
#' @title Calculate Proportion Successful by Subject, Year, and Cut Score
#' @description Assumes three cut scores generating three percent successful measures.
#' @param x A data frame containing variables `entity.id`, `year`, `subject`, `S1`, `S2`, and `S3`.
#' @param min.n The minimum number of students necessary for conducting an SRT analysis for an entity group.
#' @param lo,hi The success rates at or below which or at or above which, respectively, SRT analysis is not useful.
#' @return A data frame with the following variables:
#' ```
#'   entity.id  Entity ID number
#'   year       School year
#'   subject    Subject matter
#'   pS2        Proportion meeting cut score 2
#'   pS3        Proportion meeting cut score 3
#'   pS4        Proportion meeting cut score 4
#'   cond2      Condition code for cut score 2
#'   cond3      Condition code for cut score 3
#'   cond4      Condition code for cut score 4
#' ```
#' @export
calc_pS <- function(x, min.n = 30, lo = 0.05, hi = 0.95) {
  uj::say("Calculate percent success statistics by entity group", lev = 1)
  NR  <- NA_real_
  NI  <- NA_integer_
  NC  <- NA_character_
  unq <- base::unique(x[ , base::c("entity.id", "subject", "year")])
  NU  <- uj::nr(unq)
  NN  <- "too few cases"
  LO  <- "too few successful"
  HI  <- "too few unsuccessful"
  OK  <- "ok"
  pS  <- tibble::tibble(n = NI, pS2 = NR, pS3 = NR, pS4 = NR, cond2 = NC, cond3 = NC, cond4 = NC, .rows = NU)
  for (r in 1:NU) {
    if (r == 1 | r %% 100 == 0) {uj::say("Entity-group [", r, "] of [", NU, "]", lev = 2)}
    i <- x$entity.id == unq$entity.id[r] & x$subject == unq$subject[r] & x$year == unq$year[r] & !base::is.na(x$S2)
    j <- base::which(i)
    n <- uj::N(j)
    pS2 <- uj::mean0(x$S2[i])
    pS3 <- uj::mean0(x$S3[i])
    pS4 <- uj::mean0(x$S4[i])
    cond2 <- uj::f0(n < min.n, NN, uj::f0(pS2 <= lo, LO, uj::f0(pS2 >= hi, HI, OK)))
    cond3 <- uj::f0(n < min.n, NN, uj::f0(pS3 <= lo, LO, uj::f0(pS3 >= hi, HI, OK)))
    cond4 <- uj::f0(n < min.n, NN, uj::f0(pS4 <= lo, LO, uj::f0(pS4 >= hi, HI, OK)))
    pS[r, ] <- tibble::tibble(n = n, pS2 = pS2, pS3 = pS3, pS4 = pS4, cond2 = cond2, cond3 = cond3, cond4 = cond4)
  }
  base::cbind(unq, pS)
}
