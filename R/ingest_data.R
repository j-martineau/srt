#' @encoding UTF-8
#' @title Ingest a Student-Level Data Frame to Be Used in SRT Analysis
#' @param x Optional \code{\link[uj:atm_dtf]{atomic data frame}} to be ingested. When `x` is `NULL`, a delimited data file is read from disk.
#' @param d Optional single-character scalar text delimiter. When both `x` and `d` are `NULL`, the user is asked to select from among a set of delimiters.
#' @param file Optional character scalar path to a file. When both `x` and `file` are `NULL`, the user is asked to select a data file using the operating system's file selection dialog box.
#' @details The data frame ingested either as the argument `x` or as read from disk must have variables associated with entity ID, year, subject, and grade.
#' @return A data frame.
#' @export
ingest_student_data <- function(x = NULL, d = NULL, file = NULL) {
  if (uj::NLL(x)) {x <- uj::rd_xsv(d = d, type = "data file", file = file)}                                              # IF [x] is NULL THEN read a data file
  else if (!uj::.atmrctdtf(x)) {uj::stopperr("[x] must be [NULL] or an atomic data.frame (?uj::atm_dtf).")}              # ELSE IF [x] is not an atomic data frame THEN throw an error
  x     <- uj::xatt(x, "spec", "problems")                                                                               # delete [rd_xsv] attributes ['spec'] and ['problems']
  x     <- uj::clean_data(x)                                                                                             # clean the data frame (select, rename, recode, and/or remode variables)
  roles <- base::c("student.id", "entity.id", "year", "grade", "subject")                                                # the required roles for variables in [x]
  defs  <- base::c("student ID", "entity ID", "year", "grade", "subject")                                                # associated definitions of the roles
  x     <- uj::map_data(x, roles, defs, trim = F, rename = T)                                                            # map the variable roles and rename variables to match roles
  uj::xatt(x, "map")                                                                                                     # delete [map_data] attribute ['map']
}

