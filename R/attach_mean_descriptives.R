#' @encoding UTF-8
#' @title Attach Input Descriptive Statistics to Output
#' @param x A data frame of input data.
#' @param Cs Character vector of variable names containing student challenge measures.
#' @param z.curr Character scalar variable name containing current-grade/year z-score scaled scores.
#' @param z.priors Character vector of variable names containing prior-grade/year z-score scaled scores.
#' @param demos Character vector of variable names containing demographic dummy variables or numeric demographic variables.
#' @param clear Logical scalar indicating whether to clear the console for user updates.
#' @return The argument `x` with descriptive statistics calculated from `x$x` added to `x$y`.
#' @export
calc_descriptives <- function(x, Cs, Ss, z.curr, z.priors, demos, clear = T) {
  uj::say("Calculate descriptives", lev = 1, clear = clear)
  vars1 <- base::c(Cs, z.curr, z.priors, demos, Ss)
  vars2 <- base::c(Cs, z.curr, z.priors)
  vars <- base::c(uj::p0("mn.", vars1, " = mean(", vars1, ", na.rm = T)"),                                               # "mn.var1 = mean(var1, na.rm = T), mn.var2 = mean(var2, na.rm = T), ..., mn.varN = mean(varN, na.rm = T)"
                  uj::p0("sd.", vars2, " = sd("  , vars2, ", na.rm = T)"))                                               # "sd.var1 = sd(var1, na.rm = T), sd.var2 = sd(var2, na.rm = T), ..., sd.varN = sd(varN, na.rm = T)"
  x <- dplyr::group_by(x, entity.id, subject, year)                                                                      # group by
  y <- base::unique(x[ , base::c("entity.id", "subject", "year")])                                                       # init the output data.frame
  n <- dplyr::summarize(x, n = dplyr::n())
  y <- dplyr::left_join(y, n, by = base::c("entity.id", "subject", "year"))
  for (i in 1:uj::N(vars)) {                                                                                             # FOR each aggregation expression
    var <- vars[i]                                                                                                       # : get the aggregation expression
    uj::say("Variable [", var, "]", lev = 2)                                                                             # : user update
    z <- uj::run(uj::p0("dplyr::summarize(x, ", var, ")"))                                                               # : do the aggregation
    y <- dplyr::left_join(y, z, by = base::c("entity.id", "subject", "year"))                                            # : join the results to the output data.frame
  }                                                                                                                      # END FOR
  uj::say("DONE!", lev = 1)                                                                                              # user update
  y                                                                                                                      # return the result
}
