#' @encoding UTF-8
#' @title Ingest a Data Frame of Threshold/Cut Scores
#' @details Used by the function \code{\link{}}
#' @inheritParams ingest_data
#' @details The data frame ingested either as the argument `x` or as read from disk must have variables associated with year, subject, grade, achievement level, and threshold score.
#' @return A data frame.
#' @export
ingest_cuts <- function(x = NULL, d = NULL, file = NULL) {
  if (uj::NLL(x)) {x <- uj::rd_xsv(d = d, type = "data file", file = file)}                                              # IF [x] is NULL THEN read a data file
  else if (!uj::.atmrctdtf(x)) {uj::stopperr("[x] must be [NULL] or an atomic data.frame (?uj::atm_dtf).")}              # ELSE IF [x] is not an atomic data frame THEN throw an error
  x     <- uj::xatt(x, "spec", "problems")                                                                               # delete [rd_xsv] attributes ['spec'] and ['problems']
  x     <- uj::clean_data(x)                                                                                             # clean the data frame (select, rename, recode, and/or remode variables)
  roles <- base::c("year", "grade", "subject", "level"            , "cut"            )                                   # the required roles for variables in [x]
  defs  <- base::c("year", "grade", "subject", "achievement level", "threshold score")                                   # associated definitions of the roles
  x     <- uj::map_data(x, roles, defs, trim = T, rename = T)                                                            # map the variable roles and rename variables to match roles
  uj::xatt(x, "map")                                                                                                     # delete [map_data] attribute ['map']
  x
}
