#' @encoding UTF-8
#' @title Build a Caterpillar Plot for Entity Group Capacity
#' @param x The output of a call to \code{\link{srt}}.
#' @param Cs Character vector of names of the student challenge variables for which to plot entity-group capacity.
#' @param n.se Numeric scalar number of standard errors above and below point estimates to go in forming confidence intervals.
#' @inheritParams plot_entity_group
#' @return `NULL`. Called for the side effect of building and saving plots.
#' @export
plot_caterpillars <- function(x, n.se = 1, lo = 0.05, hi = 0.95, min.n = 20, dir = getwd(), sub.dir = "plots", sub.sub = "caterpillars", w = 6.5, h = 8.5, dpi = 500, dev = "png", clear = T) {
  uj::say("Build and save caterpillar plots of entity capacity", lev = 1, clear = clear)                                 # user update
  x1   <- x$y                                                                                                            # first of two data sets for plotting
  if (!base::is.null(lo)) {                                                                                              # IF filtering is requested
    x1 <- x1[x1$p.S > lo   , ]                                                                                           # : filter lo proportion successful if specified
    x1 <- x1[x1$p.S < hi   , ]                                                                                           # : filter hi proportion successful if specified
    x1 <- x1[x1$ns >= min.n, ]                                                                                           # : filter small sample size if specified
    cap <- uj::p0("data removed when proportion successful ≤ ", lo, " or ≥ ", hi, " or N < ", min.n)                     # : build caption
  } else {cap <- NULL}                                                                                                   # ELSE no caption
  x1   <- x1[!base::is.na(x1$E), ]                                                                                       # filter out any missing value Es
  x1   <- x1[ base::order(x1$E), ]                                                                                       # order on E
  x1$x <- NA_integer_                                                                                                    # x-location is an index
  levs <- base::c("C[2]^'AGP'", "C[2]^'P1'", "C[2]^'P2'",                                                                # student challenge variable labels
                  "C[3]^'AGP'", "C[3]^'P1'", "C[3]^'P2'",                                                                #   ...
                  "C[4]^'AGP'", "C[4]^'P1'", "C[4]^'P2'")                                                                #   ...
  x1$C[x1$C == "C2.agp"] <- "C[2]^'AGP'"; x1$C[x1$C == "C3.agp"] <- "C[3]^'AGP'"; x1$C[x1$C == "C4.agp"] <- "C[4]^'AGP'" # replace the old values with the new values
  x1$C[x1$C == "C2.p"  ] <- "C[2]^'P1'" ; x1$C[x1$C == "C3.p"  ] <- "C[3]^'P1'" ; x1$C[x1$C == "C4.p"  ] <- "C[4]^'P1'"  #   ...
  x1$C[x1$C == "C2.pp" ] <- "C[2]^'P2'" ; x1$C[x1$C == "C3.pp" ] <- "C[3]^'P2'" ; x1$C[x1$C == "C4.pp" ] <- "C[4]^'P2'"  #   ...
  subs <- uj::suv(x1$subject)                                                                                            # sorted unique values of subject
  yrs  <- uj::suv(x1$year)                                                                                               # sorted unique values of year
  for (l in levs) {for (s in subs) {for (y in yrs) {                                                                     # FOR each level, subject, and year
    i <- x1$C == l & x1$subject == s & x1$year == y                                                                      # : index the matching rows of data
    x1$x[i] <- 1:uj::nw(i)                                                                                               # : store the index numbers as x locations
  }}}                                                                                                                    # END FOR x3
  x1$C  <- base::factor(x1$C, levels = levs, ordered = T)                                                                # challenge type as ordered factor
  x2    <- x1                                                                                                            # duplicate the data for confidence interval lines
  x2$lb <- base::pmax(0  , x2$E - n.se * x2$se.E)                                                                        # calculate the lower bounds
  x2$ub <- base::pmin(100, x2$E + n.se * x2$se.E)                                                                        # calculate the upper bounds
  LB    <- 0                                                                                                             # absolute lower bound on [E]
  MID   <- 50                                                                                                            # absolute midpoint of [E]
  UB    <- 100                                                                                                           # absolute upper bound on [E]
  yBase <- base::c(LB, MID, UB)                                                                                          # y-axis base value limits
  yLims <- base::c(-3, 103)                                                                                              # y-axis limits
  yBrks <- base::seq(0, 100, by = 25)                                                                                    # y-axis breaks
  g20   <- scales::alpha("black", 0.2)                                                                                   # 20% opaque black
  g50   <- scales::alpha("black", 0.5)                                                                                   # 20% opaque black
  yLab  <- uj::p0("Entity Capacity (± ", n.se, " SE)")                                                                   # y-axis label
  sep   <- base::.Platform$file.sep                                                                                      # platform-specific file-separator character
  dir   <- uj::p0(dir, uj::f0(base::is.null(sub.dir), "", sep), uj::f0(base::is.null(sub.dir), "", sub.dir))             # directory for saving plots
  dir   <- uj::p0(dir, uj::f0(base::is.null(sub.sub), "", sep), uj::f0(base::is.null(sub.sub), "", sub.sub))             # directory for saving plots
  if (!base::dir.exists(dir)) {base::dir.create(dir, recursive = T)}                                                     # create directory if needed
  for (s in subs) {for (y in yrs) {                                                                                      # FOR each subject and year
    uj::say("For [subject = ", s, "] and [year = ", y, "]", lev = 2)                                                     # : user update
    ttl <- uj::p0("Caterpillar Plots for [year = ", y, "] and [subject = ", s, "]")                                      # : build title
    X1 <- x1[x1$subject == s & x1$year == y, ]                                                                           # : get data subset for points
    X2 <- x2[x2$subject == s & x2$year == y, ]                                                                           # : get data subset for CIs
    p <- ggplot2::ggplot(data = X1, mapping = ggplot2::aes(x = x, y = E))                                                # : initialize the plot
    p <- p + ggplot2::geom_hline(yintercept = yBase, linetype = "dashed", color = g50, linewidth = 0.25, na.rm = T)      # : draw dashed horizontal reference lines at min and max probabilities
    p <- p + ggplot2::geom_segment(data = X2, color = g50, linewidth = 0.1, na.rm = T,                                   # : draw confidence interval segments
                                   mapping = ggplot2::aes(x = x, xend = x, y = lb, yend = ub))                           # :   ...
    p <- p + ggplot2::geom_point(shape = 18, color = "black", size = 0.15, na.rm = T)                                    # : draw point estimate dots
    p <- p + ggplot2::facet_wrap(. ~ C, nrow = 3, labeller = "label_parsed", scales = "free_x")                          # : facet the plot, allow x-scale to vary
    p <- p + ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.02))                                       # : set the x-axis scale
    p <- p + ggplot2::scale_y_continuous(name = yLab, breaks = yBrks, limits = yLims, expand = ggplot2::expansion())     # : set the y-axis scale
    p <- p + ggplot2::labs(title = ttl, caption = cap)                                                                   # : apply the title and subtitle
    p <- p + ggplot2::theme_minimal()                                                                                    # : set the default theme to minimal
    p <- p + ggplot2::theme(                                                                                             # : CUSTOM theme elements
      plot.title.position = "plot",                                                                                      # : : position title at left edge of plot
      axis.text.x         = ggplot2::element_blank(),                                                                    # : : don't draw x-axis labels
      axis.title.x        = ggplot2::element_blank(),                                                                    # : : don't draw x-axis title
      axis.ticks.x        = ggplot2::element_blank(),                                                                    # : : don't draw x-axis tick marks
      axis.line           = ggplot2::element_blank(),                                                                    # : : don't draw axis lines
      panel.grid.major.x  = ggplot2::element_blank(),                                                                    # : : don't draw x-axis gridlines
      panel.grid.minor.x  = ggplot2::element_blank(),                                                                    # : :   ...
      panel.spacing.y     = ggplot2::unit(0.1, "lines"),                                                                             # : : reduce spacing between panels
      plot.title          = ggplot2::element_text(face = "bold" ),                                                       # : : show plot title in bold font
      plot.subtitle       = ggplot2::element_text(face = "plain"),                                                       # : : show plot subtitle in plain font
      axis.title.y        = ggplot2::element_text(face = "bold"),                                                        # : : show y-axis title in bold
      strip.text          = ggplot2::element_text(face = "bold", color = "white", size = 7),                             # : : show strip text in bold and white
      axis.text.y         = ggplot2::element_text(face = "plain", color = "black", size = 7),                            # : : show strip text in bold and white
      axis.ticks.y        = ggplot2::element_line(color = "black" , linewidth = 0.25),                                   # : : show y-axis tick marks in black and thin lines
      panel.grid.major.y  = ggplot2::element_line(color = "grey95", linewidth = 0.25),                                   # : : show y-axis gridlines as very light thin lines
      panel.grid.minor.y  = ggplot2::element_line(color = "grey95", linewidth = 0.25),                                   # : :   ...
      strip.background    = ggplot2::element_rect(color = "black" , linewidth = 0.50, fill = "grey35"),                  # : : dark grey background for faceting strips
      plot.background     = ggplot2::element_rect(color = "black" , linewidth = 0.75, fill = "grey95"),                  # : : plot background light grey fill, thick black border
      panel.background    = ggplot2::element_rect(color = "black" , linewidth = 0.50, fill = "white"),                   # : : panel background white file, medium black border
      panel.border        = ggplot2::element_rect(color = "black" , linewidth = 0.50, fill = NA)                         # : : panel border redrawn after everything with medium black border
    )                                                                                                                    # : END CUSTOM
    file <- uj::p0(ttl, uj::f0(base::is.null(cap), " (unfiltered)", " (filtered)"), ".", dev)                            # : build plot filename
    ggplot2::ggsave(file, plot = p, path = dir, device = dev, width = w, height = h, units = "in", dpi = dpi)            # : save plot to file
  }}                                                                                                                     # END FOR x2
}
