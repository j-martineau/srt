#' @encoding UTF-8
#' @title Estimate a Student Response Theory Model for a Single Educational Entity-Group
#' @description An entity-group is the collection of students with each unique combination of identifying variable values. For example, if estimates are desired for each subject, grade, and year for each school building, then there are four variables whose unique combinations identify an entity-group: subject, grade, year, and school building ID.
#' \cr\cr
#' This function estimates, for each entity-group, the following SRT model:
#'   \deqn{P\left(S_{jk} = 1 \mid C_{jk}, E_k, D_k \right) = 1 / \big(1 + e^{-D_k\left(E_k - C_{jk} \right)}\big)}
#' where
#' \tabular{lrll}{
#'      \tab \eqn{S_{jk}} \tab    \tab is a 0/1 indicator of student success for student \eqn{j} in entity \eqn{k},                                           \cr
#'      \tab \eqn{C_{jk}} \tab    \tab is the numeric degree of challenge posed in eliciting student success for student \eqn{j} in entity \eqn{k},           \cr
#'      \tab \eqn{E_k}    \tab    \tab is entity \eqn{k}'s capacity to elicit student success on the same scale as \eqn{C_i},                                 \cr
#'      \tab \eqn{D_k}    \tab    \tab is entity \eqn{k}'s discrimination in not eliciting vs. eliciting student success below and above \eqn{E_j}, respectively.
#' }
#' To ensure that a model can be estimated for each entity, for each model estimated, two student records are imputed:
#' \itemize{
#'   \item A student \eqn{l} for whom \eqn{S_{lk} = 1} and \eqn{C_{lk} = LB_C + R \sim U(0, min_{C_k} - LB_C)}.
#'   \item A student \eqn{m} for whom \eqn{S_{mk} = 0} and \eqn{C_{mk} = UB_C - R \sim U(0, UB_C - max_{C_k})}.
#' }
#' where \eqn{R \sim U(A, B)} is a single uniform random value sampled from the interval \eqn{\left[A, B\right]}.
#' @param x A data frame for educational entity \eqn{k}.
#' @param entity.id Character scalar name of the variable in `x` containing entity IDs.
#' @param year Character scalar name of the variable in `x` containing year identifiers.
#' @param subject Character scalar name of the variable in `x` containing subject identifiers.
#' @param C Character scalar name of the variable in `x` containing values of \eqn{C_{jk}}, or the challenge posed by students in eliciting student success.
#' @param S Character scalar name of the variable in `x` containing values of \eqn{S_{jk}}, or the 0/1 student success indicator.
#' @param clear Logical scalar indicating whether to clear the console for each round of estimation.
#' @param head Optional character scalar user update header.
#' @inheritParams srt
#' @return A list of the following structure:
#' ```
#'   $ x            An extract of the data frame in argument [x] containing complete
#'                    cases of only variables entity.id, year, subject, [S], and [C].
#'   $ y            A data frame with the following variables:
#'     $ entity.id    Entity-group ID variable 1
#'     $ year         Entity-group ID variable 2
#'     $ subject      Entity-group ID variable N
#'     $ ns           The number of students with complete data in the entity
#'     $ nj           The number of jackknifed samples drawn for the entity
#'     $ E            Bounded, jackknifed estimate of entity capacity
#'     $ D            Bounded, jackknifed estimate of entity discrimination
#'     $ se.E         Bounded, jackknifed standard error of [E]
#'     $ se.D         Bounded, jackknifed standard error of [D]
#'     $ e            Single-sample estimate of entity capacity.
#'     $ d            Single-sample estimate of entity discrimination
#'     $ u            Unbounded, jackknifed estimate of entity capacity
#'     $ se.u         Unbounded, jackknifed standard error of [ue]
#'     $ neg.D        TRUE/FALSE indicator that D < 0
#'     $ neg.d        TRUE/FALSE indicator that d < 0
#'   $ args         A list containing preferences/settings of the following structure:
#'     $ S            Character scalar argument [S]
#'     $ C            Character scalar argument [C]
#'     $ max.jack     Integer scalar argument [max.jack]
#'     $ min.n        Integer scalar argument [min.n]
#' ```
#' @export
est_srt <- function(x, C, S, lb, ub, max.jack = 100000, min.n = 30, clear = T, head = NULL, dir = getwd(), save.n = 500, say.n = 50, no.pickup = F, n.core = 6, lo = NULL, hi = NULL, E.known = NULL) {
  NR <- NA_real_ ; NC <- NA_character_ ; NI <- NA_integer_                                                                 # for convenience
  maxSD <- (ub - lb) / 2                                                                                                   # asymptotic sd with half of obs at lb and half at ub
  cl <- parallel::makeCluster(getOption("cl.cores", n.core))                                                               # set up parallel processing cluster

  # ESTIMATE A LOGISTIC REGRESSION MODEL RETURNING INTERCEPT AND SLOPE
  # @param z A data frame for a single entity containing success [S] and challenge [C] variables
  est <- function(z) {                                                                                                     # SUB function to estimate B0_j, B1_j, E_j and A_j the boundary students
    zC  <- z[[C]]                                                                                                          # : extract student challenge variable
    zS  <- z[[S]]                                                                                                          # : extract student success variable
    mod <- Rfast::glm_logistic(zC, zS)                                                                                     # : estimate a simple logistic regression
    b0  <- uj::av(mod$be[1])                                                                                               # : extract the intercept parameter
    b1  <- uj::av(mod$be[2])                                                                                               # : extract the slope parameter
    tibble::tibble(b0 = b0, b1 = b1)                                                                                       # : return a single-row data.frame with all four estimates
  }                                                                                                                        # END SUB

  # GENERATE UP TO [max.jack] PAIRS OF STUDENTS TO BE COLLAPSED
  # @param n Integer scalar number of students in an entity
  pairs <- function(n) {                                                                                                   # SUB function to generate student pairs for jackknife sampling
    z <- base::expand.grid(one = 1:n, two = 1:n)                                                                           # : generate a data frame of all possible pairs
    z <- z[z$one < z$two, ]                                                                                                # : get only the first time each non-same-student pair appears
    n <- base::nrow(z)                                                                                                     # : number of pairs generated
    if (n > max.jack) {z <- z[base::sample(1:n, max.jack), ]}                                                              # : IF the number of pairs is greater than the maximum jackknife sample, sample the max
    z                                                                                                                      # : return the pairs data frame
  }                                                                                                                        # END SUB

  # IMPUTE TWO STUDENTS: ONE SUCCESSFUL STUDENT AT THE MIN CHALLENGE AND ONE UNSUCCESSFUL AT THE MAX CHALLENGE
  # @param z A data frame for a single entity containing success [S] and challenge [C] variables
  imp <- function(z) {                                                                                                     # SUB function to impute a pair of students at the extremes
    new      <- z[1:2, ]                                                                                                   # : extract the first four rows as template for imputed pairs
    new[[C]] <- base::c(lb, ub)                                                                                            # : store the lower bound and upper bound on challenge as the imputed pair's challenge values
    new[[S]] <- base::c(1 , 0 )                                                                                            # : store success and failure as the imputed pair's success values
    base::rbind(z, new)                                                                                                    # : append the imputed pair to the data frame
  }                                                                                                                        # END SUB

  # COLLAPSE A PAIR OF STUDENTS INTO A SINGLE STUDENT
  # @param z A data frame for a single entity containing success [S] and challenge [C] variables
  # @param one The student number for the first of the pair to be collapsed
  # @param two The student number for the second of the pair to be collapsed
  collapse <- function(z, one, two) {                                                                                      # SUB function to collapse a student pair's data for jackknifing
    keep     <- 1:base::nrow(z)                                                                                            # : index all rows
    keep     <- keep[keep != one & keep != two]                                                                            # : remove rows [one] and [two]
    s1       <- z[[S]][one]; c1 <- z[[C]][one]                                                                             # : extract success and challenge for student [one]
    s2       <- z[[S]][two]; c2 <- z[[C]][two]                                                                             # : extract success and challenge for student [two]
    row      <- z[1, ]                                                                                                     # : extract a row for the combined students
    z        <- z[keep, ]                                                                                                  # : keep all rows except for students [one] and [two]
    uS       <- base::unique(z[[S]])                                                                                       # : unique values of the success (0/1) variable
    row[[C]] <- (c1 + c2) / 2                                                                                              # : average the challenge level for the two students
    row[[S]] <- base::sample(0:1, 1)                                                                                       # : randomly sample the success variable
    z        <- base::rbind(z, row)                                                                                        # : row bind the collapsed pair's row to the remaining data
    imp(z)                                                                                                                 # : add imputed student rows and return
  }                                                                                                                        # END SUB

  # CONDUCT UP TO [max.jack] JACKKNIFE DRAWS FOR A SINGLE ENTITY RETURNING A TIBBLE OF INTERCEPTS AND SLOPES
  # @param z A data frame for a single entity containing success [S] and challenge [C] variables
  jack <- function(z) {                                                                                                    # SUB function to perform jackknife resampling
    est_collapse <- function(pair) {                                                                                       # : SUB-SUB function to estimate a model with one collapsed pair
      pair <- uj::av(pair)                                                                                                 # : : collapse pair to a vector of two indices
      est(collapse(z, pair[1], pair[2]))                                                                                   # : : estimate model with collapsed pair and return
    }                                                                                                                      # : END SUB-SUB
    nn <- base::nrow(z)                                                                                                    # : number of students for this entity
    pp <- pairs(nn)                                                                                                        # : generate pairs of students for jackknifing
    np <- base::nrow(pp)                                                                                                   # : number of pairs of jackknife draws
    yy <- parallel::parApply(cl = cl, pp, 1, est_collapse)                                                                 # : parallel apply estimation with collapsed pairs
    yy <- base::matrix(uj::av(yy), nrow = np, byrow = T, dimnames = base::list(NULL, base::c("b0", "b1")))                 # : convert to matrix with correct col names
    tibble::as_tibble(yy)                                                                                                  # : convert to tibble and return
  }                                                                                                                        # END SUB

  # CONDUCT JACKKNIFE ESTIMATION FOR A SINGLE ENTITY-GROUP
  # @param z A data frame for a single entity-group containing success [S] and challenge [C] variables
  # @param id A single-row data.frame containing ID variables for the entity-group
  single <- function(z, id, yr, subj, E.known) {                                                                           # SUB function to conduct estimation for a single entity
    pS <- uj::mean0(z[[S]])                                                                                                # : calculate proportion successful
    zz <- est(imp(z))                                                                                                      # : single-sample estimation
    d  <- -zz$b1                                                                                                           # : single-sample discrimination
    e  <- zz$b0 / d                                                                                                        # : single-sample capacity
    yy <- jack(z)                                                                                                          # : jackknifed estimation of E and D
    ns <- base::nrow(z)                                                                                                    # : number of students in entity
    nj <- base::nrow(yy)                                                                                                   # : number of jackknifed estimates
    b0 <- yy$b0                                                                                                            # : jackknifed intercepts
    b1 <- yy$b1                                                                                                            # : jackknifed slopes
    y  <-  b1; vy <- stats::var(y); my <- base::mean(y); my2 <- my ^ 2; my3 <- my ^ 3; my4 <- my ^ 4                       # : slope quantities for estimator of E
    x  <- -b0; vx <- stats::var(x); mx <- base::mean(x); mx2 <- mx ^ 2; cxy <- stats::cov(x, y)                            # : intercept quantities for estimator of E
    D  <- base::mean(-b1)                                                                                                  # : D is just the mean of -b1
    SD <- stats::sd(-b1)                                                                                                   # : SE(D) is the standard deviation of -b1
    E  <- (mx / my) - (1 / nj) * ((mx * vy / my3) - (cxy / my2))                                                           # : estimator for a ratio (single E = -b0 / b1)
    VE <- (vx / my2) + (mx2 * vy / my4) - (2 * mx * cxy / my3)                                                             # : variance of estimator for a ratio (var(E))
    SE <- base::sqrt(VE)                                                                                                   # : std.err(E)
    u  <- E                                                                                                                # : unbounded value of E
    E  <- base::min(ub, base::max(lb, E))                                                                                  # : set bounds on E
    su <- SE                                                                                                               # : unbounded std.err(E)
    SE <- base::min(maxSD, SE)                                                                                             # : set bounds on std.err(E)
    tibble::tibble(entity.id = id, year = yr, subject = subj, C = C, S = S, ns = ns, nj = nj, E = E, D = D, se.E = SE,     # : return the result
                   se.D = SD, d = d, e = e, u = u, se.u = su, neg.D = D < 0, neg.d = d < 0, p.S = pS, condition = "ok",    # :   ...
                   known.E = E.known)                                                                                      # :   ...
  }                                                                                                                        # END SUB

  # FUNCTION BODY                                                                                                          # FUNCTION BODY
  cols   <- base::c("entity.id", "year", "subject", S, C)                                                                  # columns to be kept
  rows   <- uj::ok(x[ , S]) & uj::ok(x[ , C]) & uj::ok(x$entity.id) & uj::ok(x$year) & uj::ok(x$subject)                   # rows to be kept based on C and S variables
  x      <- x[rows, cols]                                                                                                  # subset the data
  IDrows <- base::unique(x[ , base::c("entity.id", "year", "subject")])                                                    # ID variables for each entity-group (part 1 of results template)
  ng     <- base::nrow(IDrows)                                                                                             # number of entity-groups
  rTemp  <- base::rep(T, base::nrow(x))                                                                                    # row indexing template
  file   <- uj::p0(dir, base::.Platform$file.sep, C, "_results.tsv")                                                       # path to interim results
  if (base::file.exists(file) & !no.pickup) {skip <- uj::YES("Pick up interim results for [", C, "]?")} else {skip <- F}   # whether to skip some estimations
  if (!skip) {                                                                                                             # IF no interim results exist
    y  <- tibble::tibble(C = NC, S = NC, ns = NI, nj = NI, E = NR, D = NR, se.E = NR, se.D = NR, d = NR, e = NR,           # : build part of results template
                         u = NR, se.u = NR, neg.D = NA, neg.d = NA, p.S = NR, condition = NC, E.known = NR, .rows = ng)    # :   ...
    y  <- base::cbind(IDrows, y)                                                                                           # : bind the ID variables to the results template
    g0 <- 1                                                                                                                # : start from the first entity-group
  } else {                                                                                                                 # ELSE
    y  <- readr::read_tsv(file)                                                                                            # : read in the interim results
    g0 <- base::length(base::which(!base::is.na(y$C))) + 1                                                                 # : start at the first missing row
  }                                                                                                                        # END IF
  base::gc()                                                                                                               # garbage collection
  for (g in g0:ng) {                                                                                                       # FOR each entity-group
    SAY <- g == g0 | g %% say.n == 0                                                                                       # : whether a user update is needed
    if (SAY) {uj::say(head, "| Estimation for entity-group [", g, "] of [", ng, "]", lev = 1, clear = clear)}              # : IF needed: major user update
    else     {uj::say()}                                                                                                   # : ELSE minor update
    IDrow <- IDrows[g, ]                                                                                                   # : get a row-data frame containing the entity-group's ID variables
    id    <- IDrow$entity.id                                                                                               # : current entity ID
    yr    <- IDrow$year                                                                                                    # : current year
    subj  <- IDrow$subject                                                                                                 # : current subject
    rows  <- rTemp & x$entity.id == id & x$year == yr & x$subject == subj                                                  # : index the rows to keep
    z     <- x[rows, ]                                                                                                     # : get the rows of data for just this entity-group
    ns    <- base::nrow(z)                                                                                                 # : get the number of students
    yy    <- tibble::tibble(C = C, S = S, ns = ns, nj = NI, E = NR, D = NR, se.E = NR, se.D = NR, d = NR, e = NR, u = NR,  # : build part 2 of the row for this entity-group with missing values
                            se.u = NR, neg.D = NA, neg.d = NA, p.S = NA, condition = NC, known.E = NR)                     # :   ...
    if (ns >= min.n) {                                                                                                     # : IF there are enough students
      pS <- uj::mean0(z[[S]])                                                                                              # : : calculate mean of the success indicator
      tooLo <- uj::f0(base::is.null(lo), F, pS <= lo)                                                                      # : : determine whether percent successful is too low
      tooHi <- uj::f0(base::is.null(hi), F, pS >= hi)                                                                      # : : determine whether percent successful is too high
      if      (tooLo) {yy <- base::cbind(IDrow, yy); yy$condition <- "too few successful"           }                      # : : IF too few are successful  : bind part 1 of the row result to part 2, set the condition
      else if (tooHi) {yy <- base::cbind(IDrow, yy); yy$condition <- "too few unsuccessful"         }                      # : : IF too few are unsuccessful: bind part 1 of the row result to part 2, set the condition
      else            {yy <- single(z, id, yr, subj, uj::f0(base::is.null(E.known), NR, E.known[g]))}                      # : : ELSE do the estimation
    } else            {yy <- base::cbind(IDrow, yy); yy$condition <- "too few students"             }                      # : ELSE: bind part 1 of the row result to part 2, set the condition
    y[g, ] <- yy                                                                                                           # : store the entity's results
    if (g %% save.n == 0) {readr::write_tsv(y, file = file)}                                                               # : IF at the save interval: save the results
  }                                                                                                                        # END FOR
  parallel::stopCluster(cl = cl)                                                                                           # release parallel processing cluster
  base::list(x = x, y = y, args = base::list(C = C, S = S, max.jack = max.jack, min.n = min.n))                            # store and return the results
}
