#' @encoding UTF-8
#' @title Calculate Success Indicators from Achievement Levels
#' @param x A data frame containing a current-year achievement level variable
#' @param ach.lev Character scalar name of the variable in `x` containing achievement levels.
#' @return A data frame with as many new columns as there are achievement levels above `1`.
#' @export
calc_Ss_from_ALs <- function(x, ach.lev) {
  if (!uj::.atmrctdtf(x)           ) {uj::stopperr("[x] must be an atomic data frame.")}                                 # IF [x] is not an atomic rectangular data frame            THEN throw an error
  if (!uj::.cmp_str_scl(ach.lev)   ) {uj::stopperr("[ach.lev] must be a non-blank character scalar.")}                   # IF [ach.lev] is not a complete string scalar              THEN throw an error
  if (!(ach.lev %in% uj::cnames(x))) {uj::stopperr("[ach.lev] must be the name of a variable in [x].")}                  # IF [ach.lev] is not the name of a variable in [x]         THEN throw an error
  if (!uj::psw_vec(x[[ach.lev]])   ) {uj::stopperr("x[[ach.lev]] must be a positive whole-number variable.")}            # IF [x[[ach.lev]]] is not a positive whole-number variable THEN throw an error
  levs <- uj::suv(x[[ach.lev]])                                                                                          # sorted unique values of achievement levels
  levs <- levs[uj::ok(levs)]                                                                                             # remove missing values
  levs <- levs[levs > 1]                                                                                                 # remove values of [1]
  if (uj::n0(levs)) {uj::stopperr("x[[ach.lev]] must contain at least one non-missing value greater than 1.")}           # IF no values are left THEN throw an error
  for (lev in levs) {                                                                                                    # FOR each achievement level greater than one
    S <- (x[[ach.lev]] >= lev) / 1                                                                                       # : calculate success as 0/1 indicator
    S[base::is.na(S)] <- F                                                                                               # : replace any [NA] values with 0s
    S <- uj::name_cs(tibble::tibble(S = S), uj::p0("S", lev))                                                            # : place in a 1-col tibble and name [S{lev}]
    x <- base::cbind(x, S)                                                                                               # : bind that 1-col tibble to [x] as a new success indicator
  }                                                                                                                      # END FOR
  x
}
