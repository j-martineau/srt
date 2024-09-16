#' @encoding UTF-8
#' @title Calculate Mean Student Growth Percentile (MGP)
#' @param x A data frame containing entity.id, year, subject, and SGPs.
#' @return A data frame.
#' @export
calc_mgp <- function(x) {
  y <- x$y[ , c("entity.id", "year", "subject", "sgp")]
  y <- dplyr::group_by(y, entity.id, year, subject)
  dplyr::summarize(y, mgp = mean(sgp))
}
