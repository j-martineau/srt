#' @encoding UTF-8
#' @title Conduct a Student Response Theory Analysis
#' @description Conducts an SRT analysis from a student-level input data frame containing entity ID, subject, and year plus student success variables for one or more cut score and student challenge values for one or more cut scores. A separate model is estimated for every combination of entity ID, subject, year, and cut score in the form:
#' \deqn{P \left( S_{ckl} = 1 \mid C_{ckl}, E_{ck}, D_{ck} \right) = 1 / \left( 1 + e^{ -D_{ck} \left( E_{ck} - C_{ckl} \right) } \right) }
#' where \tabular{lrll}{
#'      \tab \eqn{c}       \tab    \tab is an indicator for cut score,                                                                          \cr
#'      \tab \eqn{k}       \tab    \tab is an indicator for entity,                                                                             \cr
#'      \tab \eqn{l}       \tab    \tab is an indicator for student,                                                                            \cr
#'      \tab \eqn{S_{ckl}} \tab    \tab is a 0/1 indicator of whether student \eqn{l} in entity \eqn{k} succeeded in meeting cut score \eqn{c}, \cr
#'      \tab \eqn{C_{ckl}} \tab    \tab is the level of challenge posed by student \eqn{l} in entity \eqn{k} in meeting cut score \eqn{c},      \cr
#'      \tab \eqn{E_{ck}}  \tab    \tab is entity \eqn{k}'s capacity to support students in meeting cut score \eqn{c},                          \cr
#'      \tab \eqn{D_{ck}}  \tab    \tab is entity \eqn{k}'s discrimination/consistency of success with less vs. more challenging students, and  \cr
#'      \tab \eqn{P(...)}  \tab    \tab is the probability of student \eqn{l} in entity \eqn{k} meeting cut score \eqn{c}.
#' }
#' Note that student challenge (\eqn{C_{ckl}}) is estimated in advance of conducting an SRT analysis. For example, \eqn{C_{ckl}} could be the one-year adequate growth percentile for student \eqn{l} in entity \eqn{k} to meet cut score \eqn{c} or the probability of student \eqn{l} meeting cut score \eqn{c} in the average school (as estimated via \code{\link{calc_C}}).
#' @param x A data frame containing group identifying variables, student success variables, and student challenge variables.
#' @param Cs Character vector of the names of variables that contain student challenge variables.
#' @param Ss Character vector of the names of variables that contain 0/1 student success indicator variables corresponding to the variables in `C`.
#' @param LBs Numeric vector of lower bounds on \eqn{E} associated with each value of `Cs`.
#' @param UBs Numeric vector of upper bounds on \eqn{E} associated with each value of `Cs`.
#' @param max.jack The maximum number of jackknifed estimations to conduct per entity in calculating point estimates and standard errors of \eqn{E_k} and \eqn{D_k}.
#' @param min.n Minimum number of students required for an entity to get an estimate.
#' @param dir Character scalar directory in which to save interim results.
#' @param clear Logical scalar indicating whether to clear the console with each new set of student success and challenge variables.
#' @param save.n Integer scalar number of entity-groups after which to save interim results.
#' @param say.n Integer scalar number of entity-groups after which to provide a user update.
#' @param suff Optional character scalar suffix for user update messages.
#' @return A list of the following structure
#' ```
#'   $x           A copy of the data frame in argument [x]
#'   $y           A data frame with the following variables
#'     $ S          Character name of the student success variable
#'     $ C          Character name of the student challenge variable
#'     $ entity.id  Character entity-group ID
#'     $ year       Integer year
#'     $ subject    Character subject/content area
#'     $ ns         The number of students with complete data in the entity
#'     $ nj         The number of jackknifed samples drawn for the entity
#'     $ E          Jackknifed estimate of entity capacity
#'     $ D          Jackknifed estimate of entity discrimination
#'     $ se.E       Jackknifed standard error of [E]
#'     $ se.D       Jackknifed standard error of [D]
#'     $ e          Single-sample estimate of entity capacity.
#'     $ d          Single-sample estimate of entity discrimination
#'     $ u          Unbounded, jackknifed estimate of entity capacity
#'     $ se.u       Unbounded, jackknifed standard error of [ue]
#'     $ neg.D      TRUE/FALSE indicator that D < 0
#'     $ neg.d      TRUE/FALSE indicator that d < 0
#'   $ args       A list containing preferences/settings of the following structure:
#'     $ Cs         Character vector argument [Cs]
#'     $ Ss         Character vector argument [Ss]
#'     $ max.jack   Integer scalar argument [max.jack]
#'     $ min.n      Integer scalar argument [min.n]
#' ```
#' @export
srt <- function(x, Cs, Ss, LBs, UBs, max.jack = 5000, min.n = 10, dir = getwd(), clear = T, save.n = 500, say.n = 50, no.pickup = F, n.core = 6, suff = NULL, lo = NULL, hi = NULL, E.known = NULL) {
  uj::say("CONDUCT AN SRT STUDY", lev = 1, clear = clear)                                                                                                                                                                           # user update header
  file <- uj::p0(dir, base::.Platform$file.sep, "srt_results.tsv")                                                                                                                                                                  # path to results file
  NC <- uj::N(Cs)                                                                                                                                                                                                                   # the number of student challenge variables
  y <- uj::f0(!base::file.exists(file) | no.pickup, NULL, uj::f0(uj::NO("Pick up overall interim results?"), NULL, readr::read_tsv(file)))                                                                                          # initialize results or pick up interim results
  for (i in 1:NC) {                                                                                                                                                                                                                 # FOR each student challenge variable
    C <- Cs[i]                                                                                                                                                                                                                      # : get the name of the current challenge variable
    S <- Ss[i]                                                                                                                                                                                                                      # : get the name of the current success variable
    lb <- LBs[i]
    ub <- UBs[i]
    skip <- uj::f0(base::is.null(y), F, C %in% y$C)                                                                                                                                                                                 # : is the analysis for this challenge variable already done?
    if (!skip) {                                                                                                                                                                                                                    # : IF not already done
      if (base::is.null(suff)) {head <- uj::p0("CONDUCT AN SRT STUDY for [S = ", S, "] and [C = ", C, "]\n")}                                                                                                                       # : : user update header
      else                     {head <- uj::p0("CONDUCT AN SRT STUDY for [S = ", S, "], [C = ", C, "] and [", suff, "]\n")}                                                                                                         # : :   ...
      yy <- srt::est_srt(x, C, S, lb, ub, max.jack = max.jack, min.n = min.n, clear = clear, head = head, dir = dir, save.n = save.n, say.n = say.n, no.pickup = no.pickup, n.core = n.core, lo = lo, hi = hi, E.known = E.known)$y # : : estimate the models
      if (base::is.null(y)) {y <- yy} else {y <- base::rbind(y, yy)}                                                                                                                                                                # : : store the results
      readr::write_tsv(y, file)                                                                                                                                                                                                     # : : save interim results
    }                                                                                                                                                                                                                               # : END IF
  }                                                                                                                                                                                                                                 # END FOR
  base::list(x = x, y = y, args = base::list(Cs = Cs, Ss = Ss, max.jack = max.jack, min.n = min.n))                                                                                                                                 # return the result
}
