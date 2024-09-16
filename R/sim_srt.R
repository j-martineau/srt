#' @encoding UTF-8
#' @title Simulate an SRT Analysis
#' @param prob Logical scalar indicating whether to calculate student challenge (\eqn{C}) as probability of meeting the cut score (prob = TRUE) vs. as the \eqn{z}-score distance of previous scaled score to cut score.
#' @param cut Numeric scalar \eqn{z}-score cut score
#' @inheritParams srt
#' @return A data.frame with the following variables:
#' ```
#'   $ year         Year with value [1]
#'   $ subject      Subject area with value ['x'].
#'   $ entity.id    Entity identifier
#'   $ C            Character scalar type of challenge from ["C"]
#'   $ S            Character scalar type of success from ["S"]
#'   $ ns           Number of students in the entity
#'   $ nj           Number of jackknife draws
#'   $ E            Jackknifed estimate of entity capacity (bounded)
#'   $ D            Jackknifed estimate of entity discrimination
#'   $ se.E         Jackknifed standard error of entity capacity (bounded)
#'   $ se.D         Jackknifed standard error of entity discrimination
#'   $ d            Single-sample estimate of entity discrimination
#'   $ e            Single-sample estimate of entity capacity (bounded)
#'   $ u            Jackknifed estimate of entity capacity (unbounded)
#'   $ se.u         Jackknifed estimated standard error of entity capacity (unbounded)
#'   $ neg.D        Logical indicator of whether [D < 0]
#'   $ neg.d        Logical indicator of whether [d < 0]
#'   $ vam          HLM value-added model estimate
#'   $ pS           Proportion successful (S = 1) in the entity
#'   $ mnC          Mean challenge (C) value in this entity
#' ```
#' @export
#' @encoding UTF-8
#' @title Simulate SRT Analyses for Each of Sequence of Cut Scores
#' @param ne Integer scalar number of entities for which to simulate scaled scores
#' @inheritParams sim_srt
#' @inheritParams srt
#' @param cuts Numeric vector of \eqn{z}-score cut scores.
#' @return A data frame with one row per entity and the following variables
#' ```
#'   $ entity.id   Simulated entity ID
#'   $ E           Known entity capacity value
#'   $ z.cut       Z-scored cut score
#'   $ E.hat.p     Probability-based entity capacity estimate (with C based on probability of missing the cut score)
#'   $ year        Variable indicating year (all values equal to [1])
#'   $ subject     Variable indicating subject (all values equal to ['x'])
#'   $ pS          Proportion of students meeting the cut score
#' ```
#' @export
sim_srts_z <- function(ne = 1000, mn.mn.z = 0, sd.mn.z = 0.6, mn.sd.z = 0.85, sd.sd.z = 0.15, mn.E = 0, sd.E = 1.5, lb = -6, ub = 6, r.prev.curr = 0.8, z.cuts = round(seq(-2, 2, by = 0.1), 1), clear = T, save.n = 100, say.n = 10, n.core = 6) {
  uj::say("Simulate scaled scores", lev = 1, clear = clear)                                                                            # top level user update
  NC  <- NA_character_; NI <- NA_integer_; NR <- NA_real_                                                                              # for convenience
  mnZ <- stats::rnorm(ne, mean = mn.mn.z, sd = sd.mn.z)                                                                                # within-entity mean of previous z-scores
  sdZ <- base::pmax(0.2, stats::rnorm(ne, mean = mn.sd.z, sd = sd.sd.z))                                                               # within-entity standard deviations of previous z-scores
  ns  <- 30:500                                                                                                                        # all possible sample sizes
  ps  <- (1 / ns) / base::sum(1 / ns)                                                                                                  # probabilities for generating sample sizes
  ns  <- base::sample(ns, ne, replace = T, prob = ps)                                                                                  # generate sample sizes with lower numbers more likely
  x   <- tibble::tibble(entity.id = NC, subject = NC, year = NI, grade = NI, z.curr.ss = NR, z.prev.ss = NR, .rows = 0)                # init the student-level data frame
  E   <- base::pmin(ub, base::pmax(lb, stats::rnorm(ne, mn.E, sd.E)))                                                                  # generate known Es
  clipr::write_clip(tibble::tibble(known.E = E))                                                                                       # write known E to clipboard
  ids <- uj::p0("entity.", 1:ne)                                                                                                       # generate entity IDs
  for (e in 1:ne) {                                                                                                                    # FOR each entity
    if (e == 1 | e %% 25 == 0) {uj::say()}                                                                                             # : minor user update periodically
    n   <- ns[e]                                                                                                                       # : number of students to simulate
    id  <- ids[e]                                                                                                                      # : get the entity's ID number
    mu  <- base::c(mnZ[e], mnZ[e])                                                                                                     # : mean for generating z-scores
    var <- sdZ[e] ^ 2                                                                                                                  # : variance for generating z-scores
    cov <- base::matrix(base::c(var, r.prev.curr * var, r.prev.curr * var, var), nrow = 2)                                             # : covariance matrix for generating z-scores
    zs  <- MASS::mvrnorm(n, mu, cov)                                                                                                   # : calculate within-entity current z-scores
    z1  <- uj::av(zs[ , 1])                                                                                                            # : get curr-year z-scores
    z2  <- uj::av(zs[ , 2])                                                                                                            # : get prev-year z-scores
    x   <- base::rbind(x, tibble::tibble(entity.id = id, subject = "x", year = 1, grade = 4, z.curr.ss = z1, z.prev.ss = z2))          # : append current-entity data to student-level data frame
  }                                                                                                                                    # END FOR
  x$z.curr.ss <- (x$z.curr.ss - base::mean(x$z.curr.ss)) / stats::sd(x$z.curr.ss)                                                      # re-z-score previous scaled scores
  x$z.prev.ss <- (x$z.prev.ss - base::mean(x$z.prev.ss)) / stats::sd(x$z.prev.ss)                                                      # re-z-score previous scaled scores
  uj::say("Calculate predicted current scaled scores", lev = 1)                                                                        # user update
  fits  <- stats::lm(z.curr.ss ~ z.prev.ss, data = x)$fitted.values                                                                    # calculate predicted current-year scores
  ncut  <- uj::N(z.cuts)                                                                                                               # the number of cut scores
  Cs    <- base::matrix(NR, nrow = uj::nr(x), ncol = ncut)                                                                             # matrix of challenge values for analysis
  Ss    <- base::matrix(NI, nrow = uj::nr(x), ncol = ncut)                                                                             # matrix of success values for analysis
  pS    <- base::matrix(NR, nrow = ne       , ncol = ncut)                                                                             # matrix of proportions successful for output
  suffs <- NULL                                                                                                                        # init vector of suffixes for variable names
  uj::say("Calculate measures of challenge and success for each cut score", lev = 1)                                                   # top level user update
  for (i in 1:ncut) {                                                                                                                  # FOR each cut score
    uj::say()                                                                                                                          # : minor user update
    zCut  <- z.cuts[i]                                                                                                                 # : get the current z-score cut score
    suffs <- base::c(suffs, uj::p0(".", uj::f0(zCut < 0, "neg", uj::f0(zCut > 0, "pos", "")), base::abs(zCut)))                        # : build a suffix for the cut score
    Cs[ , i] <- zCut - fits                                                                                                            # : calculate challenge as distance from predicted current score to cut score
    for (j in 1:ne) {                                                                                                                  # : FOR each entity
      id <- ids[j]                                                                                                                     # : : get its ID
      Ej <- E[j]                                                                                                                       # : : get its known capacity
      k  <- x$entity.id == id                                                                                                          # : : index matching rows of data
      nj <- uj::nw(k)                                                                                                                  # : : get the number of students
      Cj <- Cs[k, i]                                                                                                                   # : : get its students' challenge values
      Pj <- 1 / (1 + base::exp(-(Ej - Cj)))                                                                                            # : : calculate its students' probabilities of success
      Rj <- stats::runif(nj)                                                                                                           # : : generate uniform random numbers
      Sj <- base::as.integer(Rj <= Pj)                                                                                                 # : : calculate success indicators
      Ss[k, i] <- Sj                                                                                                                   # : : store success indicators
      pS[j, i] <- base::mean(Sj)                                                                                                       # : : store mean of success indicators
    }                                                                                                                                  # : : END FOR
  }                                                                                                                                    # END FOR
  labsC <- uj::p0("C" , suffs)                                                                                                         # column labels for C value
  labsS <- uj::p0("S" , suffs)                                                                                                         # column labels for S values
  labsP <- uj::p0("pS", suffs)                                                                                                         # column labels for pS values
  base::colnames(Cs) <- labsC; Cs <- tibble::as_tibble(Cs)                                                                             # name them and convert to tibbles
  base::colnames(Ss) <- labsS; Ss <- tibble::as_tibble(Ss)                                                                             #   ...
  base::colnames(pS) <- labsP; pS <- tibble::as_tibble(pS)                                                                             #   ...
  x <- base::cbind(x, Cs, Ss)                                                                                                          # bind the student-level data
  y <- srt::srt(x, labsC, labsS, lb, ub, clear = clear, save.n = save.n, say.n = say.n, no.pickup = T, n.core = n.core, E.known = E)$y # run the SRT analyses
  y <- base::cbind(y, tibble::tibble(pS = uj::av(pS), gen.E = base::rep(E, ncut)))                                                     # store the results
  y                                                                                                                                    # return the result
}

#' @encoding UTF-8
#' @title Plot Simulated SRT Recovery Results
#' @param x The output of a call to \code{\link{sim_srts_prob}}.
#' @inheritParams plot_C_densities
#' @return `NULL`. Called for the side effect of building and saving plots.
#' @export
plot_sim_srts_z <- function(x, lo = 0.05, hi = 0.95, dir = getwd(), sub.dir = "plots", sub.sub = "simulation", w = 6.5, h = 8.2, dpi = 500, dev = "png", clear = T) {
  uj::say("Build simulation recovery plots", lev = 1, clear = clear)                                                                 # top level update
  sep   <- base::.Platform$file.sep                                                                                                  # file separator for file paths
  dir   <- uj::p0(base::c(dir, sub.dir, sub.sub), collapse = sep)                                                                    # create directory for saving plots
  if (!base::dir.exists(dir)) {base::dir.create(dir, recursive = T)}                                                                 # IF directory does not exist: create it recursively
  x$C   <- uj::fsub(x$C, "C." , "" )                                                                                                 # remove "C." prefix
  x$C   <- uj::fsub(x$C, "neg", "-")                                                                                                 # replace "neg" prefix with negative sign
  x$C   <- uj::fsub(x$C, "pos", "" )                                                                                                 # remove "pos" prefix
  x$C   <- base::round(base::as.numeric(x$C), 1)                                                                                     # convert to numeric and round to one decimal
  cuts  <- uj::suv(x$C)                                                                                                              # cuts are sorted unique values
  ncut  <- uj::N(cuts)                                                                                                               # number of cuts
  xlab  <- "Known Entity Capacity\n"                                                                                                 # x-axis title
  ylab  <- "Estimated Entity Capacity"                                                                                               # y-axis title
  title <- "Recovery of Entity Capacity via Simulation (with R-squared by cut score)"                                                # plot title
  leg   <- "Percent of students meeting the cut score"                                                                               # legend title
  NC    <- NA_character_; NR <- NA_real_                                                                                             # for convenience
  lb    <- -6                                                                                                                        # lower bound on E
  ub    <-  6                                                                                                                        # upper bound on E
  nTL   <- 1 + base::floor(ncut / 2)                                                                                                 # number of labels to place in top left of panels
  nBR   <- ncut - nTL                                                                                                                # number of labels to place in bottom right
  labLO <- lb + 0.05 * (ub - lb)                                                                                                     # label vert location when in bottom right
  labHI <- ub - 0.05 * (ub - lb)                                                                                                     # label vert location when in upper left
  labX  <- base::c(base::rep(labLO, nTL), base::rep(labHI, nBR))                                                                     # label horz location vector
  labY  <- base::c(base::rep(labHI, nTL), base::rep(labLO, nBR))                                                                     # label vert location vector
  labH  <- base::c(base::rep(0    , nTL), base::rep(1    , nBR))                                                                     # label horz justification vector
  labV  <- base::c(base::rep(1    , nTL), base::rep(0    , nBR))                                                                     # label vert justification vector
  genE  <- x$gen.E; E <- x$E; C <- x$C; pS <- x$pS                                                                                   # get vectors of generating E, estimated E, estimated C, and proportion successful
  x     <- tibble::tibble(x = genE, y = E   , `z(cut)` = C , pS = pS, lab = NC)                                                      # init point plotting data frame
  t1    <- tibble::tibble(x = labX, y = labY, `z(cut)` = NC, pS = NR, lab = NC, .rows = ncut)                                        # init labeling data frame for plot 1 (unfiltered)
  t2    <- tibble::tibble(x = labX, y = labY, `z(cut)` = NC, pS = NR, lab = NC, .rows = ncut)                                        # init labeling data frame for plot 2 (filtered)
  for (i in 1:ncut) {                                                                                                                # FOR each cut score
    cut  <- cuts[i]                                                                                                                  # : get the current cut score
    xx   <- x[x$`z(cut)` == cut, ]                                                                                                   # : subset the data for this cut score
    tmp1 <- tibble::tibble(x = xx$x, y = xx$y, pS = xx$pS)                                                                           # : store data for plot 1 regression
    tmp2 <- tmp1[tmp1$pS > lo & tmp1$pS < hi, ]                                                                                      # : filter out lo and high proportion successful for plot 2
    lrg1 <- stats::lm(y ~ x, data = tmp1)                                                                                            # : estimate linear regression of estimated E on known E for plot 1
    lrg2 <- stats::lm(y ~ x, data = tmp2)                                                                                            # : estimate linear regression of estimated E on known E for plot 2
    Rsq1 <- uj::spf("%0.2f", 1 - (stats::var(lrg1$residuals)) / stats::var(tmp1$y))                                                  # : calculate and store R2 for this cut's panel for plot 1
    Rsq2 <- uj::spf("%0.2f", 1 - (stats::var(lrg2$residuals)) / stats::var(tmp2$y))                                                  # : calculate and store R2 for this cut's panel for plot 2
    t1$lab[i] <- base::paste0("~italic(R)^2 == ", Rsq1)                                                                              # : store the label for this cut's panel for plot 1
    t2$lab[i] <- base::paste0("~italic(R)^2 == ", Rsq2)                                                                              # : store the label for this cut's panel for plot 2
    t1$`z(cut)`[i] <- cut                                                                                                            # : store the cut score in the plot-1 labeling data frame
    t2$`z(cut)`[i] <- cut                                                                                                            # : store the cut score in the plot-2 labeling data frame
  }                                                                                                                                  # END FOR
  x$`z(cut)`  <- base::factor( x$`z(cut)`, levels = cuts, ordered = T)                                                               # convert cut score to ordered factor for data
  t1$`z(cut)` <- base::factor(t1$`z(cut)`, levels = cuts, ordered = T)                                                               # convert cut score to ordered factor for plot-1 labeling data frame
  t2$`z(cut)` <- base::factor(t2$`z(cut)`, levels = cuts, ordered = T)                                                               # convert cut score to ordered factor for plot-2 labeling data frame
  cap   <- uj::p0("data removed for entities with ", uj::f0(lo == 0, "0%" , uj::p0("≤ ", base::round(100 * lo), "%")),               # build plot caption
                  " or "                           , uj::f0(lo == 1, "100", uj::p0("≥ ", base::round(100 * hi), "%")),               #   ...
                  " S = 1"                                                                                           )               #   ...
  LO    <- uj::p0(uj::f0(lo == 0, "= ", "≤ "), base::round(100 * lo), "%")                                                           # label for lo  proportion successful
  HI    <- uj::p0(uj::f0(hi == 1, "= ", "≥ "), base::round(100 * hi), "%")                                                           # label for hi  proportion successful
  MID   <- uj::p0("> ", base::round(100 * lo), "% and < ", base::round(100 * hi), "%")                                               # label for mid proportion successful
  ps    <- base::rep(MID, base::nrow(x))                                                                                             # default to mid proportion successful
  i     <- x$pS <= lo                                                                                                                # index lo proportion successful
  j     <- x$pS >= hi                                                                                                                # index hi proportion successful
  ps[i] <- LO                                                                                                                        # store the label for lo proportion successful where appropriate
  ps[j] <- HI                                                                                                                        # store the label for hi proportion successful where appropriate
  x1    <- x2 <- x                                                                                                                   # copy the data for the two plots
  x1$pS <- base::factor(ps, levels = base::c(LO, HI, MID), ordered = T)                                                              # convert x$pS to ordered
  x1    <- x1[base::order(x$pS)      , ]                                                                                             # data for first  plot is sorted   on x$pS
  x2    <- x2[x2$pS > lo & x2$pS < hi, ]                                                                                             # data for second plot is filtered on x$pS
  for (i in 1:2) {                                                                                                                   # FOR each of the two plots
    if (i == 1) {p <- ggplot2::ggplot(data = x1, mapping = ggplot2::aes(x = x, y = y, color = pS, label = lab))}                     # : IF this is the first (unfiltered) plot, use a color aesthetic
    if (i == 2) {p <- ggplot2::ggplot(data = x2, mapping = ggplot2::aes(x = x, y = y,             label = lab))}                     # : IF this is the second (filtered) plot, don't use a color aesthetic
    p <- p + ggplot2::geom_hline(yintercept = 0, color = "grey50", linetype = "dashed", linewidth = 0.25)                            # : add a horizontal reference line at E = 0
    p <- p + ggplot2::geom_vline(xintercept = 0, color = "grey50", linetype = "dashed", linewidth = 0.25)                            # : add a vertical reference line at E = 0
    p <- p + ggplot2::geom_abline(slope = 1, intercept = 0, color = "grey50", linetype = "dashed", linewidth = 0.25)                 # : add an identity line
    if (i == 1) {                                                                                                                    # : IF this is the first unfiltered plot
      p <- p + ggplot2::geom_point(shape = ".", na.rm = T, alpha = 1 / 3, show.legend = T)                                           # : : add plot points
      p <- p + ggplot2::scale_color_manual(values = base::c("blue", "red", "grey20"))                                                # : : set the color scale for the three point types
      p <- p + ggplot2::labs(title = title)                                                                                          # : : add the title
      p <- p + ggplot2::geom_text(data = t1, color = "black", hjust = labH, vjust = labV, na.rm = T, size = 2, parse = T)            # : : add the annotation labels
    } else {                                                                                                                         # : ELSE
      p <- p + ggplot2::geom_point(shape = ".", na.rm = T, alpha = 1 / 3, show.legend = T, color = "blue")                           # : : add plot points
      p <- p + ggplot2::labs(title = title, caption = cap)                                                                           # : : add the title and caption
      p <- p + ggplot2::geom_text(data = t2, color = "black", hjust = labH, vjust = labV, na.rm = T, size = 2, parse = T)            # : : add the annotation labels
    }                                                                                                                                # : END IF
    p <- p + ggplot2::scale_x_continuous(xlab, limits = base::c(lb, ub), expand = ggplot2::expansion())                              # : set the x-axis scale
    p <- p + ggplot2::scale_y_continuous(ylab, limits = base::c(lb, ub), expand = ggplot2::expansion())                              # : set the y-axis scale
    p <- p + ggplot2::facet_wrap(ggplot2::vars(`z(cut)`), ncol = 6, labeller = "label_both")                                         # : facet on z-score cut score label with both variable name and value
    p <- p + ggplot2::guides(color = ggplot2::guide_legend(title = leg, override.aes = base::list(shape = 15, size = 2, alpha = 1))) # : override legend size, shape, and alpha
    p <- p + ggplot2::theme_minimal()                                                                                                # : set the theme to minimal
    p <- p + ggplot2::theme(                                                                                                         # : APPLY custom theme elements
      plot.title.position = "plot",                                                                                                  # : : place title at top left
      legend.position     = "bottom",                                                                                                # : : place legend at the bottom
      plot.background     = ggplot2::element_rect(color = "black" , fill = "grey95", linewidth = 0.75),                              # : : format plot background
      panel.background    = ggplot2::element_rect(color = "black" , fill = "white" , linewidth = 0.25),                              # : : format panel background
      panel.border        = ggplot2::element_rect(color = "black" , fill =  NA     , linewidth = 0.25),                              # : : format panel border (drawn on top after everything is done)
      strip.background    = ggplot2::element_rect(color = "black" , fill = "grey35", linewidth = 0.25),                              # : : format facet-strip background
      plot.title          = ggplot2::element_text(color = "black" , face = "bold"  , size = 9),                                      # : : format title
      strip.text          = ggplot2::element_text(color = "white" , face = "bold"  , size = 7),                                      # : : format facet-strip text
      axis.title          = ggplot2::element_text(color = "black" , face = "bold"  , size = 8),                                      # : : format axis titles
      axis.text           = ggplot2::element_text(color = "black" , face = "plain" , size = 5),                                      # : : format tick labels
      legend.title        = ggplot2::element_text(color = "black" , face = "bold"  , size = 7),                                      # : : format legend title
      legend.text         = ggplot2::element_text(color = "black" , face = "plain" , size = 7),                                      # : : format legend text
      plot.caption        = ggplot2::element_text(color = "grey35", face = "italic", size = 7),                                      # : : format plot caption
      axis.ticks          = ggplot2::element_line(color = "black" , linewidth = 0.25),                                               # : : format tick marks
      panel.grid.major    = ggplot2::element_line(color = "grey95", linewidth = 0.25),                                               # : : format major panel gridlines
      panel.spacing.y     = ggplot2::unit(0.1, "lines"),                                                                             # : : reduce spacing between panels
      panel.grid.minor    = ggplot2::element_blank(),                                                                                # : : remove minor panel gridlines
      legend.background   = ggplot2::element_blank(),                                                                                # : : remove legend background rect
      legend.box.spacing  = ggplot2::unit(0, "in"),                                                                                  # : : set legend box spacing to none
      legend.box.margin   = ggplot2::margin(),                                                                                       # : : set legend box margin to none
      legend.margin       = ggplot2::margin()                                                                                        # : : set legend margin to none
    )                                                                                                                                # : END APPLY
    if (i == 1) {file <- uj::p0(dir, sep, "recovery_all_data", "." , dev)}                                                           # : IF first plot: create the file name for the first plot
    if (i == 2) {file <- uj::p0(dir, sep, "recovery_filtered", "." , dev)}                                                           # : IF second plot: create the file name for the second plot
    ggplot2::ggsave(file, plot = p, device = dev, width = w, height = h, units = "in", dpi = dpi)                                    # : save the plot
  }                                                                                                                                  # END FOR
}

#' @encoding UTF-8
#' @title Simulate an SRT Analysis to Evaluate the Effect of Partial Jackknifing on Standard Errors of Probability-Based Entity Capacity Measures
#' @param ni Positive integer scalar number of iterations per sample size.
#' @param mn.mn.z Numeric scalar mean of entity-aggregate mean current-year z-scores.
#' @param sd.mn.z Numeric scalar standard deviation of entity-aggregate mean current-year z-scores.
#' @param mn.sd.z Numeric scalar mean of entity-aggregate standard deviations of current-year z-scores.
#' @param sd.sd.z Numeric scalar standard deviation of entity-aggregate standard deviations of current-year z-scores.
#' @param mn.E Numeric scalar mean entity capacity.
#' @param sd.E Numeric scalar standard deviation of entity capacity.
#' @param lo,hi Numeric scalar lower and upper bound on proportion student success to identify for which entities to estimate capacity.
#' @param r.prev.curr Numeric scalar correlation between current-year and previous-year scores.
#' @param clear Logical scalar indicating whether to clear the console for major user updates.
#' @param dir Character scalar directory in which to save interim results.
#' @param n.core Integer scalar number of cores to use for parallel processing.
#' @param maj.jacks Integer vector of maximum jackknife draws for which to run analyses.
#' @return A data frame with the following structure:
#' ```
#'   $ entity.id  Entity's integer ID number
#'   $ ns         Sample size (number of students)
#'   $ E.known    Entity's known capacity value
#'   $ D.known    Entity's known discrimination value
#'   $ nj.all     Integer number of jackknife draws for all possible pairs
#'   $ nj.max     Integer number of jackknife draws for bounded number of pairs
#'   $ E.hat.all  Estimated entity capacity using all possible jackknife draws
#'   $ E.hat.max  Estimated entity capacity using a bounded number of jackknife draws
#'   $ D.hat.all  Estimated entity discrimination using all possible jackknife draws
#'   $ D.hat.max  Estimated entity discrimination using a bounded number of jackknife draws
#'   $ se.all     Estimated SE(E) using all possible jackknife draws
#'   $ se.max     Estimated SE(E) using a bounded number of jackknife draws
#'   $ d          Proportional deflation of SE(E) associated with using a bounded number of jackknife draws
#' ```
#' @export
sim_srts_bounding_p <- function(ni = 30, mn.mn.z = 0, sd.mn.z = 0.6, mn.sd.z = 0.85, sd.sd.z = 0.15, mn.E = 50, sd.E = 15, lo = 0, hi = 1, r.prev.curr = 0.8, clear = T, dir = getwd(), n.core = 6, max.jacks = 10000) {
  NI <- NA_integer_; NR <- NA_real_; NC <- NA_character_                                                                 # for convenience
  y <- tibble::tibble(entity.id = NI, ns = NI, E.known = NR, D.known = NR, nj.all = NI, nj.max = NI, E.hat.all = NR,     # init the return tibble
                      E.hat.max = NR, D.hat.all = NR, D.hat.max = NR, se.all = NR, se.max = NR, d = NR, .rows = 0)       #   ...
  for (max.jack in max.jacks) {                                                                                          # FOR each value of [max.jacks]
    uj::say("Conduct simulation to evaluate the effect of partial jackknifing [max.jack = ", max.jack, "]", lev = 1)     # : user update, level 1
    n    <- 1                                                                                                            # : start with a sample size of one student
    while (n * (n - 1) / 2 < max.jack) {n <- n + 1}                                                                      # : WHILE the total number of pairs is less than max.jack, increment sample size
    ns   <- base::rep(n, 30)                                                                                             # : replicate the sample size 30 times (ultimately for 30 different sample sizes)
    add  <- base::cumsum(0:29)                                                                                           # : cum sum of 0:29 to create different sample sizes
    ns   <- ns + add                                                                                                     # : add together to create 30 different sample sizes of increasing counts
    ns   <- base::rep(ns, each = ni)                                                                                     # : replicate each for the specified number of iterations
    ne   <- uj::N(ns)                                                                                                    # : get the total number of entities
    cut  <- 0                                                                                                            # : set the z-score cut score to 0
    ids  <- 1:ne                                                                                                         # : generate entity IDs
    mnZs <- stats::rnorm(ne, mean = mn.mn.z, sd = sd.mn.z)                                                               # : generate within-entity means of previous z-scores
    sdZs <- base::pmax(0.25, stats::rnorm(ne, mean = mn.sd.z, sd = sd.sd.z))                                             # : generate within-entity standard deviations of previous z-scores
    Es   <- base::pmin(100, base::pmax(0, stats::rnorm(ne, mn.E, sd.E)))                                                 # : generate known Es
    Ds   <- base::rep(0.063, ne)                                                                                         # : generate known Ds (the median from real data)
    x    <- tibble::tibble(entity.id = NI, year = NI, subject = NC, z.curr = NR, z.prev = NR, C = NR, S = NI, .rows = 0) # : init input data frame
    uj::say("Simulate scaled scores for [", ne, "] entities with [max.jack = ", max.jack, "]", lev = 2)                  # : user update, level 2
    for (id in ids) {                                                                                                    # : FOR each entity
      if (id == 1 | id %% 30 == 0) {uj::say()}                                                                           # : : periodic minor user update
      n   <- ns[id]                                                                                                      # : : get its sample size
      mnZ <- mnZs[id]                                                                                                    # : : get its mean previous z-score
      sdZ <- sdZs[id]                                                                                                    # : : get its standard deviation of previous z-scores
      mu  <- base::c(mnZ, mnZ)                                                                                           # : : mean vector for generating z-scores
      var <- sdZ ^ 2                                                                                                     # : : variance for generating z-scores
      cov <- base::matrix(base::c(var, r.prev.curr * var, r.prev.curr * var, var), nrow = 2)                             # : : covariance matrix for generating z-scores
      zs  <- MASS::mvrnorm(n, mu, cov)                                                                                   # : : calculate within-entity current z-scores
      x   <- base::rbind(x, tibble::tibble(entity.id = id, year = 1, subject = "x", z.curr = uj::av(zs[ , 1]),           # : : store this entity's input data
                                           z.prev = uj::av(zs[ , 2]), C = NR, S = NI))                                   # : :   ...
    }                                                                                                                    # : END FOR
    uj::say("Calculate student challenge values with [max.jack = ", max.jack, "]", lev = 2)                              # : user update level 2
    prev <- x$z.prev                                                                                                     # : get previous scaled scores (z-scores)
    S    <- x$z.curr >= cut                                                                                              # : calculate success indicators
    mod  <- Rfast::glm_logistic(prev, S)                                                                                 # : model success as a function of previous scaled scores
    b0   <- uj::av(mod$be[1])                                                                                            # : extract the intercept parameter
    b1   <- uj::av(mod$be[2])                                                                                            # : extract the slope parameter
    x$C  <- 100 * (1 - (1 / (1 + base::exp(-b0 - b1 * prev))))                                                           # : calculate challenge values as 100 times 1 minus the probability of meeting the cut
    uj::say("Simulate student success indicators for [", ne, "] entities with [max.jack = ", max.jack, "]", lev = 2)     # : user update level 2
    for (id in ids) {                                                                                                    # : FOR each entity
      if (id == 1 | id %% 30 == 0) {uj::say()}                                                                           # : : periodic minor user update
      n <- ns[id]                                                                                                        # : : get its sample size
      E <- Es[id]                                                                                                        # : : get its known entity capacity
      D <- Ds[id]                                                                                                        # : : get its known entity discrimination
      C <- x$C[x$entity.id == id]                                                                                        # : : get its students' challenge values
      P <- 1 / (1 + base::exp(-D * (E - C)))                                                                             # : : calculate probability of success
      R <- stats::runif(n)                                                                                               # : : generate uniform random vector
      x$S[x$entity.id == id] <- base::as.integer(R <= P)                                                                 # : : generate and store success measures
    }                                                                                                                    # : END FOR
    uj::say("Estimate SRT models with [max.jack = ", max.jack, "]", lev = 2)                                             # : user update level 2
    sep  <- base::.Platform$file.sep                                                                                     # : get the platform-specific file separator
    file <- uj::p0(dir, sep, "bounding.p.tsv")                                                                           # : build the filename for interim results
    for (id in ids) {                                                                                                    # : FOR each entity
      n  <- ns[id]                                                                                                       # : : get its sample size (number of students)
      E  <- Es[id]                                                                                                       # : : get its known capacity value
      D  <- Ds[id]                                                                                                       # : : get its known discrimination value
      h1 <- uj::p0("Estimate unbounded model for entity [", id, "] of [", ne, "] with [max.jack = ", max.jack, "]\n")    # : : build headers for estimation
      h2 <- uj::p0("Estimate   bounded model for entity [", id, "] of [", ne, "] with [max.jack = ", max.jack, "]\n")    # : :   ...
      xx <- x[x$entity.id == id, ]                                                                                       # : : get its subset of the input data
      pS <- uj::mean0(xx$S)                                                                                              # : : calculate its proportion successful
      if (pS > lo & pS < hi) {                                                                                           # : : IF proportion successful is in bounds
        nn <- n * (n - 1) / 2                                                                                            # : : : calculate total number of student pairs
        yAll <- srt::est_srt(xx, "C", "S", 0, 100, max.jack = nn      , no.pickup = T, n.core = n.core, lo = lo,         # : : : estimate SRT model without a bound on jackknife draws
                             hi = hi, head = h1, dir = dir, clear = clear)$y                                             # : : :   ...
        yMax <- srt::est_srt(xx, "C", "S", 0, 100, max.jack = max.jack, no.pickup = T, n.core = n.core, lo = lo,         # : : : estimate SRT model with a bound on jackknife draws
                             hi = hi, head = h2, dir = dir, clear = clear)$y                                             # : : :   ...
        sAll <- yAll$se.E                                                                                                # : : : get SE(E) without a bound on jackknife draws
        sMax <- yMax$se.E                                                                                                # : : : get SE(E) with a bound on jackknife draws
        d    <- (sAll - sMax) / sAll                                                                                     # : : : get the proportional deflation in SE(E) attributable to a bound on jackknife draws
        yy <- tibble::tibble(entity.id = id, ns = n, E.known = E, D.known = D, nj.all = yAll$nj, nj.max = yMax$nj,       # : : : store this entities results
                             E.hat.all = yAll$E, E.hat.max = yMax$E, D.hat.all = yAll$D, D.hat.max = yMax$D,             # : : :   ...
                             se.all = sAll, se.max = sMax, d = d)                                                        # : : :   ...
        y <- base::rbind(y, yy)                                                                                          # : : : append to the overall results
        readr::write_tsv(y, file)                                                                                        # : : : write the interim results file
      }                                                                                                                  # : : END IF
    }                                                                                                                    # : END FOR
  }                                                                                                                      # END FOR
  y                                                                                                                      # return the result
}

#' @encoding UTF-8
#' @title Simulate an SRT Analysis to Evaluate the Effect of Partial Jackknifing on Standard Errors of Probability-Based Entity Capacity Measures
#' @inheritParams sim_srts_bounding_p
#' @return A data frame with the following structure:
#' ```
#'   $ entity.id  Entity's integer ID number
#'   $ ns         Sample size (number of students)
#'   $ E.known    Entity's known capacity value
#'   $ D.known    Entity's known discrimination value
#'   $ nj.all     Integer number of jackknife draws for all possible pairs
#'   $ nj.max     Integer number of jackknife draws for bounded number of pairs
#'   $ E.hat.all  Estimated entity capacity using all possible jackknife draws
#'   $ E.hat.max  Estimated entity capacity using a bounded number of jackknife draws
#'   $ D.hat.all  Estimated entity discrimination using all possible jackknife draws
#'   $ D.hat.max  Estimated entity discrimination using a bounded number of jackknife draws
#'   $ se.all     Estimated SE(E) using all possible jackknife draws
#'   $ se.max     Estimated SE(E) using a bounded number of jackknife draws
#'   $ d          Proportional deflation of SE(E) associated with using a bounded number of jackknife draws
#' ```
#' @export
sim_srts_bounding_z <- function(ni = 30, mn.mn.z = 0, sd.mn.z = 0.6, mn.sd.z = 0.85, sd.sd.z = 0.15, mn.E = 0, sd.E = 0.5, mn.sd.dz = 0.5, sd.sd.dz = 0.1, lo = 0, hi = 1, r.prev.curr = 0.8, dir = getwd(), clear = T, n.core = 6, max.jacks = 5000) {
  NI <- NA_integer_; NR <- NA_real_; NC <- NA_character_                                                                 # for convenience
  y <- tibble::tibble(entity.id = NI, ns = NI, E.known = NR, D.known = NR, nj.all = NI, nj.max = NI, E.hat.all = NR,     # init the return tibble
                      E.hat.max = NR, D.hat.all = NR, D.hat.max = NR, se.all = NR, se.max = NR, d = NR, .rows = 0)       #   ...
  for (max.jack in max.jacks) {                                                                                          # FOR each value of max.jacks
    uj::say("Conduct simulation to evaluate the effect of partial jackknifing [max.jack = ", max.jack, "]", lev = 1)     # : user update, level 1
    n    <- 1                                                                                                            # : start out with 1 student per entity
    while (n * (n - 1) / 2 < max.jack) {n <- n + 1}                                                                      # : while the number of pairs is less than max.jack, increment number of students
    ns   <- base::rep(n, 30)                                                                                             # : replicate that number 30 times (for 30 different sample sizes)
    add  <- base::cumsum(0:29)                                                                                           # : get cumulative sum of 0:29 to add to replicated numbers (for increasing sample sizes)
    ns   <- ns + add                                                                                                     # : get final set of sample sizes
    ns   <- base::rep(ns, each = ni)                                                                                     # : replicate each to reach the specified number of iterations
    ne   <- uj::N(ns)                                                                                                    # : number of entities for which to simulate data
    cut  <- 0                                                                                                            # : set the z-score cut score to zero
    ids  <- 1:ne                                                                                                         # : generate entity IDs
    mnZs <- stats::rnorm(ne, mean = mn.mn.z, sd = sd.mn.z)                                                               # : generate within-entity means of previous z-scores
    sdZs <- base::pmax(0.25, stats::rnorm(ne, mean = mn.sd.z, sd = sd.sd.z))                                             # : generate within-entity standard deviations of previous z-scores
    Es   <- base::pmin(4, base::pmax(-4, stats::rnorm(ne, mn.E, sd.E)))                                                  # : generate known Es
    Ds   <- base::rep(3.8, ne)                                                                                           # : generate known Ds (the median of real-data values)
    x    <- tibble::tibble(entity.id = NI, year = NI, subject = NC, z.curr = NR, z.prev = NR, C = NR, S = NI, .rows = 0) # : init the input data frame
    uj::say("Simulate scaled scores for [", ne, "] entities with [max.jack = ", max.jack, "]", lev = 2)                  # : user update, level 2
    for (id in ids) {                                                                                                    # : FOR each entity
      if (id == 1 | id %% 30 == 0) {uj::say()}                                                                           # : : periodic minor user update
      n   <- ns[id]                                                                                                      # : : get its sample size
      mnZ <- mnZs[id]                                                                                                    # : : get its previous scaled score z-score mean
      sdZ <- sdZs[id]                                                                                                    # : : get its previous scaled score z-score standard deviation
      mu  <- base::c(mnZ, mnZ)                                                                                           # : : mean vector for generating correlated z-scores
      var <- sdZ ^ 2                                                                                                     # : : variance for generating z-scores
      cov <- base::matrix(base::c(var, r.prev.curr * var, r.prev.curr * var, var), nrow = 2)                             # : : covariance matrix for generating z-scores
      zs  <- MASS::mvrnorm(n, mu, cov)                                                                                   # : : calculate within-entity current z-scores
      x   <- base::rbind(x, tibble::tibble(entity.id = id, year = 1, subject = "x", z.curr = uj::av(zs[ , 1]),           # : : append the entity's previous and current score data
                                           z.prev = uj::av(zs[ , 2]), C = NR, S = NI))                                   # : :   ...
    }                                                                                                                    # : END FOR
    x$z.curr <- (x$z.curr - base::mean(x$z.curr)) / stats::sd(x$z.curr)                                                  # : recenter the z-scored current scaled scores
    x$z.prev <- (x$z.prev - base::mean(x$z.prev)) / stats::sd(x$z.prev)                                                  # : recenter the z-scored previous scaled scores
    uj::say("Apply known entity capaacity values with [max.jack = ", max.jack, "]", lev = 2)                             # : user update, level 2
    for (id in ids) {                                                                                                    # : FOR each entity
      if (id == 1 | id %% 30 == 0) {uj::say()}                                                                           # : : periodic minor user update
      i     <- x$entity.id == id                                                                                         # : : index matching data rows
      n     <- uj::nw(i)                                                                                                 # : : get the count of matching rows
      mn.dz <- Es[id]                                                                                                    # : : get the mean difference in z-scores
      sd.dz <- base::max(0.05, stats::rnorm(1, mn.sd.dz, sd.sd.dz))                                                      # : : get the standard deviation of mean difference in z-scores
      dz    <- stats::rnorm(n, mn.dz, sd.dz)                                                                             # : : generate deviation of z-scores
      x$z.curr[i] <- x$z.curr[i] + dz                                                                                    # : : set current score to current score plus deviation
    }                                                                                                                    # : END FOR
    uj::say("Calculate student challenge and success values with [max.jack = ", max.jack, "]", lev = 2)                  # : user update, level 2
    ind <- base::as.matrix(tibble::tibble(`(Intercept)` = 1, z = x$z.prev))                                              # : create the independent variable matrix for predicting current scores
    dep <- uj::av(x$z.curr)                                                                                              # : create the dependent variable vector for predicting current scores
    mod <- stats::lm.fit(ind, dep)                                                                                       # : model current score as a function of previous score
    x$S <- base::as.integer(x$z.curr >= cut)                                                                             # : set success variable to whether current score is greater than or equal to the cut score
    x$C <- cut - mod$fitted.values                                                                                       # : calculate challenge as z-score distance from predicted current score to cut score
    uj::say("Estimate SRT models with [max.jack = ", max.jack, "]", lev = 2)                                             # : user update, level 2
    sep  <- base::.Platform$file.sep                                                                                     # : get the file separator value for the current computing platform
    file <- uj::p0(dir, sep, "bounding.z.tsv")                                                                           # : build the file name for saving interim results
    for (id in ids) {                                                                                                    # : FOR each entity
      n  <- ns[id]                                                                                                       # : : get its number of students
      E  <- Es[id]                                                                                                       # : : get its known E value
      D  <- Ds[id]                                                                                                       # : : get its known D value
      h1 <- uj::p0("Estimate unbounded model for entity [", id, "] of [", ne, "] with [max.jack = ", max.jack, "]\n")    # : : build headers for user updates
      h2 <- uj::p0("Estimate   bounded model for entity [", id, "] of [", ne, "] with [max.jack = ", max.jack, "]\n")    # : :   ...
      xx <- x[x$entity.id == id, ]                                                                                       # : : get its subset of the input data
      pS <- uj::mean0(xx$S)                                                                                              # : : get its proportion successful
      if (pS > lo & pS < hi) {                                                                                           # : : IF its proportion successful in in bounds
        nn <- n * (n - 1) / 2                                                                                            # : : : get the number of pairs of students
        yAll <- srt::est_srt(xx, "C", "S", -6, 6, max.jack = nn      , no.pickup = T, n.core = n.core, lo = lo, hi = hi, # : : : estimate an SRT model without a limit on the number of jackknife draws
                             head = h1, dir = dir, clear = clear)$y                                                      # : : :   ...
        yMax <- srt::est_srt(xx, "C", "S", -6, 6, max.jack = max.jack, no.pickup = T, n.core = n.core, lo = lo, hi = hi, # : : : estimate an SRT model with a limit on the the number of jackknife draws
                             head = h2, dir = dir, clear = clear)$y                                                      # : : :   ...
        sAll <- yAll$se.E                                                                                                # : : : get the standard error of E estimated with the full set of jackknife draws
        sMax <- yMax$se.E                                                                                                # : : : get the standard error of E estimated with the bounded set of jackknife draws
        d    <- (sAll - sMax) / sAll                                                                                     # : : : get the proportional deflation in SE(E) associated with bounding jackknife draws
        yy <- tibble::tibble(entity.id = id, ns = n, E.known = E, D.known = D, nj.all = yAll$nj, nj.max = yMax$nj,       # : : : store the results for this entity
                             E.hat.all = yAll$E, E.hat.max = yMax$E, D.hat.all = yAll$D, D.hat.max = yMax$D,             # : : :   ...
                             se.all = sAll, se.max = sMax, d = d)                                                        # : : :   ...
        y <- base::rbind(y, yy)                                                                                          # : : : append to the overall results
        readr::write_tsv(y, file)                                                                                        # : : : write the interim results file
      }                                                                                                                  # : : END IF
    }                                                                                                                    # : END FOR
  }                                                                                                                      # END FOR
  y                                                                                                                      # return the result
}

#' @encoding UTF-8
#' @title Simulate an SRT Analysis to Evaluate the Effect of Partial Jackknifing on Standard Errors of Probability-Based Entity Capacity Measures
#' @param ni Positive integer scalar number of iterations per sample size.
#' @param mn.mn.z Numeric scalar mean of entity-aggregate mean current-year z-scores.
#' @param sd.mn.z Numeric scalar standard deviation of entity-aggregate mean current-year z-scores.
#' @param mn.sd.z Numeric scalar mean of entity-aggregate standard deviations of current-year z-scores.
#' @param sd.sd.z Numeric scalar standard deviation of entity-aggregate standard deviations of current-year z-scores.
#' @param mn.E Numeric scalar mean entity capacity.
#' @param sd.E Numeric scalar standard deviation of entity capacity.
#' @param lo,hi Numeric scalar lower and upper bound on proportion student success to identify for which entities to estimate capacity.
#' @param r.prev.curr Numeric scalar correlation between current-year and previous-year scores.
#' @param clear Logical scalar indicating whether to clear the console for major user updates.
#' @param dir Character scalar directory in which to save interim results.
#' @param n.core Integer scalar number of cores to use for parallel processing.
#' @param maj.jacks Integer vector of maximum jackknife draws for which to run analyses.
#' @return A data frame with the following structure:
#' ```
#'   $ entity.id  Entity's integer ID number
#'   $ ns         Sample size (number of students)
#'   $ E.known    Entity's known capacity value
#'   $ D.known    Entity's known discrimination value
#'   $ nj.all     Integer number of jackknife draws for all possible pairs
#'   $ nj.max     Integer number of jackknife draws for bounded number of pairs
#'   $ E.hat.all  Estimated entity capacity using all possible jackknife draws
#'   $ E.hat.max  Estimated entity capacity using a bounded number of jackknife draws
#'   $ D.hat.all  Estimated entity discrimination using all possible jackknife draws
#'   $ D.hat.max  Estimated entity discrimination using a bounded number of jackknife draws
#'   $ se.all     Estimated SE(E) using all possible jackknife draws
#'   $ se.max     Estimated SE(E) using a bounded number of jackknife draws
#'   $ d          Proportional deflation of SE(E) associated with using a bounded number of jackknife draws
#' ```
#' @export
sim_srts_p <- function(ni = 30, mn.mn.z = 0, sd.mn.z = 0.6, mn.sd.z = 0.85, sd.sd.z = 0.15, mn.E = 50, sd.E = 15, lo = NULL, hi = NULL, z.cuts = round(seq(-2, 2, by = 0.1), 1), r.prev.curr = 0.8, save.n = 500, say.n = 50, clear = T, dir = getwd(), n.core = 6, max.jack = 5000) {
  uj::say("Simulate entity parameters", lev = 1)                                                                         # user update, level 1
  NI   <- NA_integer_; NR <- NA_real_; NC <- NA_character_                                                               # for convenience
  ns   <- base::c(10, 11, 13, 16, 20, 25, 31, 38, 46, 54, 64, 75, 87, 90, 104, 119, 135, 152, 170, 189, 199)             # sample sizes
  ns   <- base::rep(ns, each = ni)                                                                                       # replicate the sample size 30 times (ultimately for 30 different sample sizes)
  ne   <- uj::N(ns)                                                                                                      # get the total number of entities
  ids  <- 1:ne                                                                                                           # generate entity IDs
  mnZs <- stats::rnorm(ne, mean = mn.mn.z, sd = sd.mn.z)                                                                 # generate within-entity means of previous z-scores
  sdZs <- base::pmax(0.25, stats::rnorm(ne, mean = mn.sd.z, sd = sd.sd.z))                                               # generate within-entity standard deviations of previous z-scores
  Es   <- base::pmin(100, base::pmax(0, stats::rnorm(ne, mn.E, sd.E)))                                                   # generate known Es
  Ds   <- base::rep(0.063, ne)                                                                                           # generate known Ds (the median from real data)
  x    <- tibble::tibble(entity.id = NI, year = NI, subject = NC, z.curr = NR, z.prev = NR, .rows = 0)                   # init input data frame
  uj::say("Simulate scaled scores for [", ne, "] entities", lev = 1)                                                     # user update, level 1
  for (id in ids) {                                                                                                      # FOR each entity
    if (id == 1 | id %% 30 == 0) {uj::say()}                                                                             # : periodic minor user update
    n   <- ns[id]                                                                                                        # : get its sample size
    mnZ <- mnZs[id]                                                                                                      # : get its mean previous z-score
    sdZ <- sdZs[id]                                                                                                      # : get its standard deviation of previous z-scores
    mu  <- base::c(mnZ, mnZ)                                                                                             # : mean vector for generating z-scores
    var <- sdZ ^ 2                                                                                                       # : variance for generating z-scores
    cov <- base::matrix(base::c(var, r.prev.curr * var, r.prev.curr * var, var), nrow = 2)                               # : covariance matrix for generating z-scores
    zs  <- MASS::mvrnorm(n, mu, cov)                                                                                     # : calculate within-entity current z-scores
    x   <- base::rbind(x, tibble::tibble(entity.id = id, year = 1, subject = "x", z.curr = uj::av(zs[ , 1]),             # : store this entity's input data
                                         z.prev = uj::av(zs[ , 2])))                                                     # :  ...
  }                                                                                                                      # END FOR
  uj::say("Calculate student challenge values for various cut scores", lev = 1)                                          # user update level 2
  for (z.cut in z.cuts) {                                                                                                # FOR each cut score
    pref <- uj::p0(uj::f0(z.cut < 0, ".neg", "."), base::abs(z.cut))                                                     # : prefix for the cut score (for variable labels)
    prev <- x$z.prev                                                                                                     # : get previous scaled scores (z-scores)
    S    <- base::as.integer(x$z.curr >= z.cut)                                                                          # : calculate success indicators
    mod  <- Rfast::glm_logistic(prev, S)                                                                                 # : model success as a function of previous scaled scores
    b0   <- uj::av(mod$be[1])                                                                                            # : extract the intercept parameter
    b1   <- uj::av(mod$be[2])                                                                                            # : extract the slope parameter
    C    <- 100 * (1 - (1 / (1 + base::exp(-b0 - b1 * prev))))                                                           # : calculate challenge values as 100 times 1 minus the probability of meeting the cut
    CS   <- uj::name_cs(tibble::tibble(C = C, S = S), uj::p0(base::c("C", "S"), pref))                                   # : build the tibble with C and S
    uj::say("Simulate student success indicators for [", ne, "] entities with [z.cut = ", z.cut, "]", lev = 2)           # : user update level 2
    for (id in ids) {                                                                                                    # : FOR each entity
      if (id == 1 | id %% 30 == 0) {uj::say()}                                                                           # : : periodic minor user update
      i <- x$entity.id == id                                                                                             # : : index rows of [x] for this entity
      n <- uj::nw(i)                                                                                                     # : : get its sample size
      E <- Es[id]                                                                                                        # : : get its known entity capacity
      D <- Ds[id]                                                                                                        # : : get its known entity discrimination
      P <- 1 / (1 + base::exp(-D * (E - C[i])))                                                                          # : : calculate probability of success
      R <- stats::runif(n)                                                                                               # : : generate uniform random vector
      CS[i, 2] <- base::as.integer(R <= P)                                                                               # : : generate and store success measures
    }                                                                                                                    # : END FOR
    x <- base::cbind(x, CS)                                                                                              # : append the tibble with C and S variables
  }                                                                                                                      # END FOR
  prefs <- base::rep(".", uj::N(z.cuts))                                                                                 # build prefixes for vectors of names of C and S variables
  prefs[z.cuts < 0] <- ".neg"                                                                                            #   ...
  prefs <- uj::p0(prefs, base::abs(z.cuts))                                                                              #   ...
  Cs <- uj::p0("C", prefs)                                                                                               # build vector of names of C variables
  Ss <- uj::p0("S", prefs)                                                                                               # build vector of names of S variables
  LBs <- base::rep(0, uj::N(Cs))                                                                                         # build vector of lower bounds
  UBs <- base::rep(100, uj::N(Cs))                                                                                       # build vector of upper bounds
  base::list(E = Es, y = srt::srt(x, Cs, Ss, LBs, UBs, max.jack = max.jack, min.n = 10, dir = dir, clear = clear,        # run SRT and return the result
             say.n = say.n, no.pickup = T, n.core = n.core, lo = lo, hi = hi, E.known = Es))                             #   ...
}

#' @encoding UTF-8
#' @title Plot Simulated SRT Recovery Results
#' @param x The output of a call to \code{\link{sim_srts_prob}}.
#' @inheritParams plot_C_densities
#' @return `NULL`. Called for the side effect of building and saving plots.
#' @export
plot_sim_srts_p <- function(x, lo = 0.05, hi = 0.95, min.n = 30, dir = getwd(), sub.dir = "plots", sub.sub = "simulation", w = 6.5, h = 8.2, dpi = 500, dev = "png", clear = T) {
  uj::say("Build simulation recovery plots", lev = 1, clear = clear)                                                                 # top level update
  sep   <- base::.Platform$file.sep                                                                                                  # file separator for file paths
  dir   <- uj::p0(base::c(dir, sub.dir, sub.sub), collapse = sep)                                                                    # create directory for saving plots
  if (!base::dir.exists(dir)) {base::dir.create(dir, recursive = T)}                                                                 # IF directory does not exist: create it recursively
  x     <- x$y$y
  x$C   <- uj::fsub(x$C, "C." , "" )                                                                                                 # remove "C." prefix
  x$C   <- uj::fsub(x$C, "neg", "-")                                                                                                 # replace "neg" prefix with negative sign
  x$C   <- uj::fsub(x$C, "pos", "" )                                                                                                 # remove "pos" prefix
  x$C   <- base::round(base::as.numeric(x$C), 1)                                                                                     # convert to numeric and round to one decimal
  cuts  <- uj::suv(x$C)                                                                                                              # cuts are sorted unique values
  ncut  <- uj::N(cuts)                                                                                                               # number of cuts
  xlab  <- "Known Entity Capacity\n"                                                                                                 # x-axis title
  ylab  <- "Estimated Entity Capacity"                                                                                               # y-axis title
  title <- "Recovery of Entity Capacity via Simulation by Threshold Score"                                                           # plot title
  NC    <- NA_character_; NR <- NA_real_                                                                                             # for convenience
  lb    <- 0                                                                                                                         # lower bound on E
  ub    <- 100                                                                                                                       # upper bound on E
  nTL   <- 1 + base::floor(ncut / 2)                                                                                                 # number of labels to place in top left of panels
  nBR   <- ncut - nTL                                                                                                                # number of labels to place in bottom right
  labLO <- lb + 0.05 * (ub - lb)                                                                                                     # label vert location when in bottom right
  labHI <- ub - 0.05 * (ub - lb)                                                                                                     # label vert location when in upper left
  labX  <- base::c(base::rep(labLO, nTL), base::rep(labHI, nBR))                                                                     # label horz location vector
  labY  <- base::c(base::rep(labHI, nTL), base::rep(labLO, nBR))                                                                     # label vert location vector
  labH  <- base::c(base::rep(0    , nTL), base::rep(1    , nBR))                                                                     # label horz justification vector
  labV  <- base::c(base::rep(1    , nTL), base::rep(0    , nBR))                                                                     # label vert justification vector
  genE  <- x$E.known; E <- x$E; C <- x$C; pS <- x$p.S; N <- x$ns                                                                     # get vectors of generating E, estimated E, estimated C, and proportion successful
  x     <- tibble::tibble(x = genE, y = E   , `z(cut)` = C , p.S = pS, lab = NC, N = N)                                              # init point plotting data frame
  t1    <- tibble::tibble(x = labX, y = labY, `z(cut)` = NC, p.S = NR, lab = NC, .rows = ncut)                                       # init labeling data frame for plot 1 (unfiltered)
  t2    <- tibble::tibble(x = labX, y = labY, `z(cut)` = NC, p.S = NR, lab = NC, .rows = ncut)                                       # init labeling data frame for plot 2 (filtered)
  for (i in 1:ncut) {                                                                                                                # FOR each cut score
    cut  <- cuts[i]                                                                                                                  # : get the current cut score
    xx   <- x[x$`z(cut)` == cut, ]                                                                                                   # : subset the data for this cut score
    tmp1 <- tibble::tibble(x = xx$x, y = xx$y, p.S = xx$p.S, N = xx$N)                                                               # : store data for plot 1 regression
    tmp2 <- tmp1[tmp1$p.S > lo & tmp1$p.S < hi & tmp1$N >= min.n, ]                                                                  # : filter out lo and high proportion successful for plot 2
    lrg1 <- stats::lm(y ~ x, data = tmp1)                                                                                            # : estimate linear regression of estimated E on known E for plot 1
    lrg2 <- stats::lm(y ~ x, data = tmp2)                                                                                            # : estimate linear regression of estimated E on known E for plot 2
    Rsq1 <- uj::spf("%0.2f", 1 - (stats::var(lrg1$residuals)) / stats::var(tmp1$y))                                                  # : calculate and store R2 for this cut's panel for plot 1
    Rsq2 <- uj::spf("%0.2f", 1 - (stats::var(lrg2$residuals)) / stats::var(tmp2$y))                                                  # : calculate and store R2 for this cut's panel for plot 2
    t1$lab[i] <- base::paste0("~italic(R)^2 == ", Rsq1)                                                                              # : store the label for this cut's panel for plot 1
    t2$lab[i] <- base::paste0("~italic(R)^2 == ", Rsq2)                                                                              # : store the label for this cut's panel for plot 2
    t1$`z(cut)`[i] <- cut                                                                                                            # : store the cut score in the plot-1 labeling data frame
    t2$`z(cut)`[i] <- cut                                                                                                            # : store the cut score in the plot-2 labeling data frame
  }                                                                                                                                  # END FOR
  x$`z(cut)`  <- base::factor( x$`z(cut)`, levels = cuts, ordered = T)                                                               # convert cut score to ordered factor for data
  t1$`z(cut)` <- base::factor(t1$`z(cut)`, levels = cuts, ordered = T)                                                               # convert cut score to ordered factor for plot-1 labeling data frame
  t2$`z(cut)` <- base::factor(t2$`z(cut)`, levels = cuts, ordered = T)                                                               # convert cut score to ordered factor for plot-2 labeling data frame
  cap   <- uj::p0("data removed for entities with ", uj::f0(lo == 0, "0%" , uj::p0("≤ ", base::round(100 * lo), "%")),               # build plot caption
                  " S = 1 or "                     , uj::f0(lo == 1, "100", uj::p0("≥ ", base::round(100 * hi), "%")),               #   ...
                  " S = 1 or N < ", min.n                                                                            )               #   ...
  x1    <- x2 <- x                                                                                                                   # copy the data for the two plots
  x2    <- x2[x2$p.S > lo & x2$p.S < hi & x2$N >= min.n, ]                                                                           # data for second plot is filtered on x$pS
  for (i in 1:2) {                                                                                                                   # FOR each of the two plots
    if (i == 1) {p <- ggplot2::ggplot(data = x1, mapping = ggplot2::aes(x = x, y = y, label = lab))}                    # : IF this is the first (unfiltered) plot, use a color aesthetic
    if (i == 2) {p <- ggplot2::ggplot(data = x2, mapping = ggplot2::aes(x = x, y = y, label = lab))}                    # : IF this is the second (filtered) plot, don't use a color aesthetic
    p <- p + ggplot2::geom_hline(yintercept = 50, color = "grey50", linetype = "dashed", linewidth = 0.25)                           # : add a horizontal reference line at E = 0
    p <- p + ggplot2::geom_vline(xintercept = 50, color = "grey50", linetype = "dashed", linewidth = 0.25)                           # : add a vertical reference line at E = 0
    p <- p + ggplot2::geom_abline(slope = 1, intercept = 0, color = "grey50", linetype = "dashed", linewidth = 0.25)                 # : add an identity line
    p <- p + ggplot2::geom_point(shape = ".", na.rm = T, alpha = 1 / 3, show.legend = T, color = "blue")                           # : : add plot points
    if (i == 1) {                                                                                                                    # : IF this is the first unfiltered plot
      p <- p + ggplot2::labs(title = title)                                                                                          # : : add the title
      p <- p + ggplot2::geom_text(data = t1, color = "black", hjust = labH, vjust = labV, na.rm = T, size = 2, parse = T)            # : : add the annotation labels
    } else {                                                                                                                         # : ELSE
      p <- p + ggplot2::labs(title = title, caption = cap)                                                                           # : : add the title and caption
      p <- p + ggplot2::geom_text(data = t2, color = "black", hjust = labH, vjust = labV, na.rm = T, size = 2, parse = T)            # : : add the annotation labels
    }                                                                                                                                # : END IF
    p <- p + ggplot2::scale_x_continuous(xlab, limits = base::c(lb, ub), expand = ggplot2::expansion())                              # : set the x-axis scale
    p <- p + ggplot2::scale_y_continuous(ylab, limits = base::c(lb, ub), expand = ggplot2::expansion())                              # : set the y-axis scale
    p <- p + ggplot2::facet_wrap(ggplot2::vars(`z(cut)`), ncol = 6, labeller = "label_both")                                         # : facet on z-score cut score label with both variable name and value
    p <- p + ggplot2::theme_minimal()                                                                                                # : set the theme to minimal
    p <- p + ggplot2::theme(                                                                                                         # : APPLY custom theme elements
      plot.title.position = "plot",                                                                                                  # : : place title at top left
      legend.position     = "bottom",                                                                                                # : : place legend at the bottom
      plot.background     = ggplot2::element_rect(color = "black" , fill = "grey95", linewidth = 0.75),                              # : : format plot background
      panel.background    = ggplot2::element_rect(color = "black" , fill = "white" , linewidth = 0.25),                              # : : format panel background
      panel.border        = ggplot2::element_rect(color = "black" , fill =  NA     , linewidth = 0.25),                              # : : format panel border (drawn on top after everything is done)
      strip.background    = ggplot2::element_rect(color = "black" , fill = "grey35", linewidth = 0.25),                              # : : format facet-strip background
      plot.title          = ggplot2::element_text(color = "black" , face = "bold"  , size = 9),                                      # : : format title
      strip.text          = ggplot2::element_text(color = "white" , face = "bold"  , size = 7),                                      # : : format facet-strip text
      axis.title          = ggplot2::element_text(color = "black" , face = "bold"  , size = 8),                                      # : : format axis titles
      axis.text           = ggplot2::element_text(color = "black" , face = "plain" , size = 5),                                      # : : format tick labels
      legend.title        = ggplot2::element_text(color = "black" , face = "bold"  , size = 7),                                      # : : format legend title
      legend.text         = ggplot2::element_text(color = "black" , face = "plain" , size = 7),                                      # : : format legend text
      plot.caption        = ggplot2::element_text(color = "grey35", face = "italic", size = 7),                                      # : : format plot caption
      axis.ticks          = ggplot2::element_line(color = "black" , linewidth = 0.25),                                               # : : format tick marks
      panel.grid.major    = ggplot2::element_line(color = "grey95", linewidth = 0.25),                                               # : : format major panel gridlines
      panel.spacing.y     = ggplot2::unit(0.1, "lines"),                                                                             # : : reduce spacing between panels
      panel.grid.minor    = ggplot2::element_blank(),                                                                                # : : remove minor panel gridlines
      legend.background   = ggplot2::element_blank(),                                                                                # : : remove legend background rect
      legend.box.spacing  = ggplot2::unit(0, "in"),                                                                                  # : : set legend box spacing to none
      legend.box.margin   = ggplot2::margin(),                                                                                       # : : set legend box margin to none
      legend.margin       = ggplot2::margin()                                                                                        # : : set legend margin to none
    )                                                                                                                                # : END APPLY
    if (i == 1) {file <- uj::p0(dir, sep, "recovery_p_all_data", "." , dev)}                                                         # : IF first plot: create the file name for the first plot
    if (i == 2) {file <- uj::p0(dir, sep, "recovery_p_lo=", lo, "_hi=", hi, "_N=", min.n, "." , dev)}                                # : IF second plot: create the file name for the second plot
    ggplot2::ggsave(file, plot = p, device = dev, width = w, height = h, units = "in", dpi = dpi)                                    # : save the plot
  }                                                                                                                                  # END FOR
}
