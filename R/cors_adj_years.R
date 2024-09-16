#' @encoding UTF-8
#' @title Calculate Adjacent-Year Correlations of \eqn{E}
#' @param x The output of a call to \code{\link{srt}}.
#' @return A data frame
#' @export
cors_adj_years <- function(x, lo = 0.1, hi = 0.9, min.n = 20) {
  NAC   <- NA_character_; NAI <- NA_integer_; NAR <- NA_real_
  subjs <- uj::suv(x$subject)
  years <- uj::suv(x$year)
  Cs    <- uj::suv(x$C)
  ny    <- uj::N(years)
  y     <- tibble::tibble(C = NAC, subject = NAC, year1 = NAI, year2 = NAI, r = NAR, n = NAI, .rows = 0)
  for (C in Cs) {
    iC <- x$C == C
    for (S in subjs) {
      iS <- x$subject == S
      for (Y1 in years[1:(ny - 1)]) {
        Y2  <- Y1 + 1
        iE1 <- iC & iS & x$year == Y1 & x$p.S > lo & x$p.S < hi & x$ns >= min.n
        iE2 <- iC & iS & x$year == Y2 & x$p.S > lo & x$p.S < hi & x$ns >= min.n
        iE1[base::is.na(iE1)] <- F
        iE2[base::is.na(iE2)] <- F
        E1 <- x[iE1, base::c("entity.id", "E")]; base::colnames(E1) <- base::c("entity.id", "E1")
        E2 <- x[iE2, base::c("entity.id", "E")]; base::colnames(E2) <- base::c("entity.id", "E2")
        E  <- dplyr::full_join(E1, E2, by = "entity.id")
        R  <- uj::cor0(E$E1, E$E2)
        N  <- uj::nw(stats::complete.cases(E))
        y  <- base::rbind(y, tibble::tibble(C = C, subject = S, year1 = Y1, year2 = Y2, r = R, n = N))
      }
    }
  }
  y
}

#' @encoding UTF-8
#' @title Calculate Correlations of \eqn{E} with VAM
#' @param x The output of a call to \code{\link{srt}}.
#' @param vam The output of a call to \code{\link{hlm_vam}}.
#' @param mgp The output of a call to \code{\link{calc_mgp}}.
#' @return A data frame.
#' @export
cors_vam <- function(x, lo = 0.1, hi = 0.9, min.n = 20) {
  NAC   <- NA_character_; NAI <- NA_integer_; NAR <- NA_real_
  subjs <- uj::suv(x$subject)
  years <- uj::suv(x$year)
  Cs    <- uj::suv(x$C)
  y     <- tibble::tibble(C = NAC, subject = NAC, year = NAI, rE1 = NAR, rE2 = NAR, rEM = NAR, .rows = 0)
  for (C in Cs) {
    iC <- x$C == C
    for (S in subjs) {
      iS <- x$subject == S
      for (Y in years) {
        iY <- iC & iS & x$year == Y & x$p.S > lo & x$p.S < hi & !base::is.na(x$E) & x$ns >= min.n
        iY[base::is.na(iY)] <- F
        cat("\n N = ", uj::nw(iY), ", C = ", C, ", sub = ", S, ", year = ", Y)
        E  <- x$E[   iY]
        V1 <- x$vam1[iY]
        V2 <- x$vam2[iY]
        M  <- x$mgp[ iY]
        nE1 <- uj::nw(!base::is.na(E) & !base::is.na(V1))
        nE2 <- uj::nw(!base::is.na(E) & !base::is.na(V2))
        nEM <- uj::nw(!base::is.na(E) & !base::is.na(M ))
        rE1 <- uj::f0(nE1 < 30, NA, uj::cor0(E, V1))
        rE2 <- uj::f0(nE2 < 30, NA, uj::cor0(E, V2))
        rEM <- uj::f0(nEM < 30, NA, uj::cor0(E, M ))
        y <- base::rbind(y, tibble::tibble(C = C, subject = S, year = Y, rE1 = rE1, rE2 = rE2, rEM = rEM))
      }
    }
  }
  y
}

#' @encoding UTF-8
#' @title Calculate Correlations of \eqn{E} with VAM
#' @param x The output of a call to \code{\link{srt}}.
#' @param vam The output of a call to \code{\link{hlm_vam}}.
#' @param mgp The output of a call to \code{\link{calc_mgp}}.
#' @return A data frame.
#' @export
cors_subjects <- function(x, lo = 0.1, hi = 0.9, min.n = 20) {
  NAC   <- NA_character_; NAI <- NA_integer_; NAR <- NA_real_
  years <- uj::suv(x$year)
  Cs    <- uj::suv(x$C)
  y     <- tibble::tibble(C = NAC, year = NAI, rEM = NAR, .rows = 0)
  for (C in Cs) {
    iC <- x$C == C
    for (Y in years) {
      iY <- x$year == Y
      iE <- iC & iY & x$subject == "ela"  & x$p.S > lo & x$p.S < hi & x$ns >= min.n
      iM <- iC & iY & x$subject == "math" & x$p.S > lo & x$p.S < hi & x$ns >= min.n
      iE[base::is.na(iE)] <- F
      iM[base::is.na(iM)] <- F
      E <- x[iE, base::c("entity.id", "E")]; base::colnames(E) <- base::c("entity.id", "E.ela" )
      M <- x[iM, base::c("entity.id", "E")]; base::colnames(M) <- base::c("entity.id", "E.math")
      EM <- dplyr::full_join(E, M, by = "entity.id")
      rEM <- uj::cor0(EM$E.ela, EM$E.math)
      y <- base::rbind(y, tibble::tibble(C = C, year = Y, rEM = rEM))
    }
  }
  y
}

#' @encoding UTF-8
#' @title Calculate Correlations of \eqn{E} with VAM
#' @param x The output of a call to \code{\link{srt}}.
#' @param vam The output of a call to \code{\link{hlm_vam}}.
#' @param mgp The output of a call to \code{\link{calc_mgp}}.
#' @return A data frame.
#' @export
cors_Cs <- function(x, lo = 0.1, hi = 0.9, min.n = 20) {
  NAC   <- NA_character_; NAI <- NA_integer_; NAR <- NA_real_
  subjs <- uj::suv(x$subject)
  years <- uj::suv(x$year)
  y     <- tibble::tibble(subject = NAC, year = NAI,
                          ra23  = NAR, ra24  = NAR, ra34  = NAR,
                          rp23  = NAR, rp24  = NAR, rp34  = NAR,
                          rpp23 = NAR, rpp24 = NAR, rpp34 = NAR,
                          rap2  = NAR, rap3  = NAR, rap4  = NAR,
                          rapp2 = NAR, rapp3 = NAR, rapp4 = NAR,
                          rppp2 = NAR, rppp3 = NAR, rppp4 = NAR, .rows = 0)
  for (S in subjs) {
    iS <- x$subject == S
    for (Y in years) {
      iY     <- iS & x$year == Y & x$p.S > lo & x$p.S < hi & x$ns >= min.n
      E2.a   <- x[iY & x$C == "C2.agp", base::c("entity.id", "E")]; colnames(E2.a ) <- base::c("entity.id", "E2.a" )
      E3.a   <- x[iY & x$C == "C3.agp", base::c("entity.id", "E")]; colnames(E3.a ) <- base::c("entity.id", "E3.a" )
      E4.a   <- x[iY & x$C == "C4.agp", base::c("entity.id", "E")]; colnames(E4.a ) <- base::c("entity.id", "E4.a" )
      E2.p   <- x[iY & x$C == "C2.p"  , base::c("entity.id", "E")]; colnames(E2.p ) <- base::c("entity.id", "E2.p" )
      E3.p   <- x[iY & x$C == "C3.p"  , base::c("entity.id", "E")]; colnames(E3.p ) <- base::c("entity.id", "E3.p" )
      E4.p   <- x[iY & x$C == "C4.p"  , base::c("entity.id", "E")]; colnames(E4.p ) <- base::c("entity.id", "E4.p" )
      E2.pp  <- x[iY & x$C == "C2.pp" , base::c("entity.id", "E")]; colnames(E2.pp) <- base::c("entity.id", "E2.pp")
      E3.pp  <- x[iY & x$C == "C3.pp" , base::c("entity.id", "E")]; colnames(E3.pp) <- base::c("entity.id", "E3.pp")
      E4.pp  <- x[iY & x$C == "C4.pp" , base::c("entity.id", "E")]; colnames(E4.pp) <- base::c("entity.id", "E4.pp")
      xx     <- dplyr::full_join(E2.a, E3.a , by = "entity.id")
      xx     <- dplyr::full_join(xx  , E4.a , by = "entity.id")
      xx     <- dplyr::full_join(xx  , E2.p , by = "entity.id")
      xx     <- dplyr::full_join(xx  , E3.p , by = "entity.id")
      xx     <- dplyr::full_join(xx  , E4.p , by = "entity.id")
      xx     <- dplyr::full_join(xx  , E2.pp, by = "entity.id")
      xx     <- dplyr::full_join(xx  , E3.pp, by = "entity.id")
      xx     <- dplyr::full_join(xx  , E4.pp, by = "entity.id")
      ra23   <- tryCatch(uj::cor0(xx$E2.a , xx$E3.a ), error = function(e) NA_real_)
      ra24   <- tryCatch(uj::cor0(xx$E2.a , xx$E4.a ), error = function(e) NA_real_)
      ra34   <- tryCatch(uj::cor0(xx$E3.a , xx$E4.a ), error = function(e) NA_real_)
      rp23   <- tryCatch(uj::cor0(xx$E2.p , xx$E3.p ), error = function(e) NA_real_)
      rp24   <- tryCatch(uj::cor0(xx$E2.p , xx$E4.p ), error = function(e) NA_real_)
      rp34   <- tryCatch(uj::cor0(xx$E3.p , xx$E4.p ), error = function(e) NA_real_)
      rpp23  <- tryCatch(uj::cor0(xx$E2.pp, xx$E3.pp), error = function(e) NA_real_)
      rpp24  <- tryCatch(uj::cor0(xx$E2.pp, xx$E4.pp), error = function(e) NA_real_)
      rpp34  <- tryCatch(uj::cor0(xx$E3.pp, xx$E4.pp), error = function(e) NA_real_)
      rap2   <- tryCatch(uj::cor0(xx$E2.a , xx$E2.p ), error = function(e) NA_real_)
      rap3   <- tryCatch(uj::cor0(xx$E3.a , xx$E3.p ), error = function(e) NA_real_)
      rap4   <- tryCatch(uj::cor0(xx$E4.a , xx$E3.p ), error = function(e) NA_real_)
      rapp2  <- tryCatch(uj::cor0(xx$E2.a , xx$E2.pp), error = function(e) NA_real_)
      rapp3  <- tryCatch(uj::cor0(xx$E3.a , xx$E3.pp), error = function(e) NA_real_)
      rapp4  <- tryCatch(uj::cor0(xx$E4.p , xx$E3.pp), error = function(e) NA_real_)
      rppp2  <- tryCatch(uj::cor0(xx$E2.p , xx$E2.pp), error = function(e) NA_real_)
      rppp3  <- tryCatch(uj::cor0(xx$E3.p , xx$E3.pp), error = function(e) NA_real_)
      rppp4  <- tryCatch(uj::cor0(xx$E4.p , xx$E3.pp), error = function(e) NA_real_)
      y <- base::rbind(y, tibble::tibble(subject = S, year = Y,
                                         ra23  = ra23 , ra24  = ra24 , ra34  = ra34 ,
                                         rp23  = rp23 , rp24  = rp24 , rp34  = rp34 ,
                                         rpp23 = rpp23, rpp24 = rpp24, rpp34 = rpp34,
                                         rap2  = rap2 , rap3  = rap3 , rap4  = rap4 ,
                                         rapp2 = rapp2, rapp3 = rapp3, rapp4 = rapp4,
                                         rppp2 = rppp2, rppp3 = rppp3, rppp4 = rppp4))
    }
  }
y
}
