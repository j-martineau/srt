#' @encoding UTF-8
#' @title Build and Save a Student Success Plots for a Single Entity Group
#' @param x The output of a call to \code{\link{srt}}.
#' @param entity.id Optional character scalar entity ID.
#' @param Ss Character vector of student success variable names.
#' @param Cs Character vector of student challenge variable names of the same length as `S`.
#' @param min.n Integer scalar minimum number of students for an entity-group to be plotted.
#' @param dir Character scalar directory in which to save plots.
#' @param sub.dir Character scalar sub-directory in which to save plots.
#' @param sub.sub Character scalar sub-sub-directory in which to save plots.
#' @param w,h Numeric scalar width and height of plots in inches.
#' @param dpi Integer scalar dots per inch of resolution.
#' @param dev Character scalar graphics device to use in saving plots.
#' @param clear Logical scalar indicating whether to clear the console for user updates.
#' @return `NULL`. Called for the side effect of building and saving plots
#' @export
plot_entity_group <- function(x, Ss, Cs, entity.id = NULL, min.n = 30, lo = 0.05, hi = 0.95, dir = getwd(), sub.dir = "plots", sub.sub = "entities", w = 6.5, h = 4.5, dpi = 500, dev = "png", clear = T, mask = T) {
  # E < 50, D ≥ 0 top right stats label corner placement
  # E < 50, D < 0 bot right stats label corner placement
  # E ≥ 50, D ≥ 0 bot left stats label corner placement
  # E ≥ 50, D < 0 top left stats label corner placement
  lab_loc <- function(E, D) {uj::f0(E < 50, uj::f0(D >= 0, "tr", "br"), uj::f0(D >= 0, "bl", "tl"))}                                                          # function to identify the corner of the plot to place the label in
  lab_hj <- function(loc) {uj::f0(loc == "tr" | loc == "br", 1.00, 0.00)}                                                                                     # function to get the horizontal stats label justification
  lab_vj <- function(loc) {uj::f0(loc == "tr" | loc == "tl", 1.00, 0.00)}                                                                                     # function to get the vertical stats label justification
  lab_y  <- function(loc) {uj::f0(loc == "bl" | loc == "br", 0.02, 0.98)}                                                                                     # function to get the y-axis stats label placement
  lab_x  <- function(loc) {uj::f0(loc == "tl" | loc == "bl", 0.00, 100)}                                                                                      # function to get the x-axis stats label placement
  LB    <- 0                                                                                                                                                  # lower bound on C/E
  UB    <- 100                                                                                                                                                # upper bound on C/E
  y0    <- -0.065                                                                                                                                             # vertical location for S = 0
  y1    <-  1.065                                                                                                                                             # vertical location for S = 1
  yLab0 <- -0.15                                                                                                                                              # vertical location for S = 0 label
  yLab1 <-  1.15                                                                                                                                              # vertical location for S = 1 label
  hs    <-  0.08                                                                                                                                              # jitter height for S = 0 and S = 1 points
  lab0  <- " students not meeting performance level "                                                                                                         # label for below student dots
  lab1  <- " students meeting performance level "                                                                                                             # label for above student dots
  xLab  <- "Level of Student Challenge"                                                                                                                       # x-axis label
  yLab  <- "Probability of Student Success"                                                                                                                   # y-axis label
  col0  <- "darkorange4"                                                                                                                                      # color for S = 0
  col1  <- "darkblue"                                                                                                                                         # color for S = 1
  colE  <- "darkred"                                                                                                                                          # color associated with E
  colG  <- "grey50"                                                                                                                                           # color associated with guide lines
  colW  <- "white"                                                                                                                                            # color associated with plot panel background
  colK  <- "black"                                                                                                                                            # default line color
  col90 <- "grey90"                                                                                                                                           # color associated with gridlines
  col95 <- "grey95"                                                                                                                                           # color associated with plot background
  col97 <- scales::alpha(col95, 0.5)                                                                                                                          # color associated with annotation background
  colSE <- scales::alpha(colE, 0.1)                                                                                                                           # color associated with standard error of E
  wide  <-  0.75                                                                                                                                              # wide linewidth
  mid   <-  0.5                                                                                                                                               # medium linewidth
  thin  <-  0.25                                                                                                                                              # thin linewidth
  lbY   <-  0.0                                                                                                                                               # minimum probability value
  ubY   <-  1.0                                                                                                                                               # maximum probability value
  minY  <- -0.2                                                                                                                                               # minimum y-value of plotting panel
  maxY  <-  1.2                                                                                                                                               # maximum y-value of plotting panel
  yBase <-  0:1                                                                                                                                               # y-axis base values
  yLims <- base::c(-0.2, 1.2)                                                                                                                                 # y-axis limits
  yBrks <- base::seq(0, 1, by = 0.1)                                                                                                                          # y-axis breaks
  yLabs <- uj::p0(100 * yBrks, "%")                                                                                                                           # y-axis break labels
  sep   <- base::.Platform$file.sep                                                                                                                           # file separator char
  n     <- base::nrow(x$y)                                                                                                                                    # number of rows of SRT output data
  if (base::is.null(entity.id)) {entity.id <- x$y$entity.id[base::sample(1:n, 1)]}                                                                            # randomly sample an entity ID if none is supplied
  dir   <- uj::p0(dir, uj::f0(base::is.null(sub.dir), "", sep), uj::f0(base::is.null(sub.dir), "", sub.dir))                                                  # directory for saving plots
  dir   <- uj::p0(dir, uj::f0(base::is.null(sub.sub), "", sep), uj::f0(base::is.null(sub.sub), "", sub.sub))                                                  # directory for saving plots
  dir   <- uj::p0(dir, sep, entity.id)                                                                                                                        # directory for saving plots
  if (!base::dir.exists(dir)) {base::dir.create(dir)}                                                                                                         # create directory if necessary
  i     <- x$x$entity.id == entity.id                                                                                                                         # initialize indices for matching input data rows
  j     <- x$y$entity.id == entity.id                                                                                                                         # initialize indices for matching output data rows
  xx    <- x$x[i, base::c("year", "subject", Ss, Cs)]                                                                                                         # get the matching input  data rows and just the needed vars
  yy    <- x$y[j, base::c("year", "subject", "C", "E", "D", "se.E")]                                                                                          # get the matching output data rows and just the needed vars
  pref  <- "Student Success Plot with "                                                                                                                       # plot title prefix
  sub   <- uj::p0("for entity ID = ", uj::f0(mask, "xxxxx", entity.id))                                                                                       # plot subtitle prefix
  rows  <- base::unique(yy[ , base::c("year", "subject")])                                                                                                    # get unique combinations of year and subject
  uj::say("Plot student success plots ", sub, lev = 1, clear = clear)                                                                                         # user update
  for (r in 1:uj::nr(rows)) {                                                                                                                                 # FOR each unique combination
    yr   <- rows$year[r]                                                                                                                                      # : get the year
    subj <- rows$subject[r]                                                                                                                                   # : get the subject
    subt <- uj::p0(sub, ", subject = ", subj, ", and year = ", yr)                                                                                            # : build the subtitle
    x    <- xx[xx$subject == subj & xx$year == yr, ]                                                                                                          # : extract the relevant input data rows
    y    <- yy[yy$subject == subj & yy$year == yr, ]                                                                                                          # : extract the relevant output data rows
    for (i in 1:uj::N(Cs)) {                                                                                                                                  # : FOR each challenge variable
      C     <- Cs[i]                                                                                                                                          # : : get its variable name
      S     <- Ss[i]                                                                                                                                          # : : get its associated success variable name
      padX  <- 0.01 * (UB - LB)                                                                                                                               # : : calculate a pad for the x-axis
      minX  <- LB - padX                                                                                                                                      # : : minimum x-value of plotting panel
      maxX  <- UB + padX                                                                                                                                      # : : maximum x-value of plotting panel
      xBase <- base::c(LB, UB)                                                                                                                                # : : x-axis base values
      xLims <- base::c(minX, maxX)                                                                                                                            # : : x-axis limits
      cc    <- uj::av(x[ , C])                                                                                                                                # : : get the entity-group's student challenge values from the input data
      ss    <- uj::av(x[ , S])                                                                                                                                # : : get the entity-group's student success values from the output data
      j     <- uj::ok(cc) & uj::ok(ss)                                                                                                                        # : : index students with complete data
      n     <- uj::nw(j)                                                                                                                                      # : : get the number of students with complete data
      if (n >= min.n) {                                                                                                                                       # : : IF there are enough students with complete data
        uj::say()                                                                                                                                             # : : : minor user update
        cc <- cc[j]                                                                                                                                           # : : : get just the non-NA values
        ss <- ss[j]                                                                                                                                           # : : :   ...
        c0 <- cc[ss == 0] ; has0 <- uj::N(c0) > 0                                                                                                             # : : : get challenge values for students not meeting the performance level cut
        c1 <- cc[ss == 1] ; has1 <- uj::N(c1) > 0                                                                                                             # : : : get challenge values for students meeting the performance level cut
        if (!base::is.null(lo)) {pS <- uj::mean0(ss); okPS <- lo < pS & pS < hi} else {okPS <- T}
        if (has0 & has1 & okPS) {                                                                                                                             # : : : IF there are students both meeting and not meeting the cut
          sy0  <- y0 + stats::runif(uj::N(c0), -hs / 2, hs / 2)                                                                                               # : : : : jittered vertical locations for S = 0 points
          sy1  <- y1 + stats::runif(uj::N(c1), -hs / 2, hs / 2)                                                                                               # : : : : jittered vertical locations for S = 1 points
          k    <- y$C == C                                                                                                                                    # : : : : index the row of the subset of output data matching the current challenge variable name
          e    <- y$E[k]                                                                                                                                      # : : : : extract the associated entity-group capacity value
          d    <- y$D[k]                                                                                                                                      # : : : : extract the associated entity-group discrimination value
          se   <- y$se.E[k]                                                                                                                                   # : : : : extract the associated entity-group standard error of capacity
          cVec <- base::seq(LB, UB, length.out = 5000)                                                                                                        # : : : : build a 5000-element sequence of challenge values
          pVec <- 1 / (1 + base::exp(-d * (e - cVec)))                                                                                                        # : : : : calculate the probability of student success at the 5000 challenge values
          path <- tibble::tibble(x = cVec, y = pVec)                                                                                                          # : : : : build the probability path data frame
          lab  <- uj::spf(" N = %0.0f \n D = %0.2f \n E = %0.2f \n se(E) = %0.2f \n mean(S) = %0.2f ", n, d, e, se, pS)                                       # : : : : build the plot annotation label
          ttl  <- uj::p0(pref, "[C = ", C, "]")                                                                                                               # : : : : complete the plot title
          loc  <- lab_loc(e, d)                                                                                                                               # : : : : location for stats label
          hj   <- lab_hj(loc)                                                                                                                                 # : : : : horizontal justification for stats label
          vj   <- lab_vj(loc)                                                                                                                                 # : : : : vertical justification for stats label
          lx   <- lab_x(loc)                                                                                                                                  # : : : : x-axis value for stats label
          ly   <- lab_y(loc)                                                                                                                                  # : : : : y-axis value for stats label
          p <- ggplot2::ggplot(data = path, mapping = ggplot2::aes(x = x, y = y))                                                                             # : : : : initialize the plot
          p <- p + ggplot2::annotate("rect", xmin = minX, xmax = LB, ymin = minY, ymax = maxY, color = NA, fill = col97, na.rm = T)                           # : : : : draw a grey rect on the left   edge of the plot area
          p <- p + ggplot2::annotate("rect", xmin = UB, xmax = maxX, ymin = minY, ymax = maxY, color = NA, fill = col97, na.rm = T)                           # : : : : draw a grey rect on the right  edge of the plot area
          p <- p + ggplot2::annotate("rect", xmin = minX, xmax = maxX, ymin = minY, ymax = lbY, color = NA, fill = col97, na.rm = T)                          # : : : : draw a grey rect on the bottom edge of the plot area
          p <- p + ggplot2::annotate("rect", xmin = minX, xmax = maxX, ymin = ubY, ymax = maxY, color = NA, fill = col97, na.rm = T)                          # : : : : draw a grey rect on the top    edge of the plot area
          p <- p + ggplot2::geom_hline(yintercept = yBase, linetype = "dashed", color = colG, linewidth = thin, na.rm = T)                                    # : : : : draw dashed horizontal reference lines at min and max probabilities
          p <- p + ggplot2::geom_vline(xintercept = xBase, linetype = "dashed", color = colG, linewidth = thin, na.rm = T)                                    # : : : : draw dashed vertical reference lines at LB and UB of challenge values
          p <- p + ggplot2::geom_line(color = colK, linetype = "solid", linewidth = mid, na.rm = T)                                                           # : : : : draw the probability curve (the student characteristic curve)
          p <- p + ggplot2::annotate("point", x = c0, y = sy0, shape = 5, color = col0, alpha = 0.5, size = 0.5, na.rm = T)                                   # : : : : draw the S = 0 points with some vertical jitter
          p <- p + ggplot2::annotate("point", x = c1, y = sy1, shape = 5, color = col1, alpha = 0.5, size = 0.5, na.rm = T)                                   # : : : : draw the S = 1 points with some vertical jitter
          p <- p + ggplot2::annotate("text", x = UB, y = yLab0, label = lab0, color = col0, hjust = 1, vjust = 0.5, na.rm = T)                                # : : : : draw S = 0 points label below the vertically-jittered points
          p <- p + ggplot2::annotate("text", x = LB, y = yLab1, label = lab1, color = col1, hjust = 0, vjust = 0.5, na.rm = T)                                # : : : : draw S = 1 points label above the vertically-jittered points
          p <- p + ggplot2::annotate("rect", xmin = e - se, xmax = e + se, ymin = 0.48, ymax = 0.52, color = colE, fill = colSE, linewidth = thin, na.rm = T) # : : : : draw the confidence rect around E
          p <- p + ggplot2::annotate("point", x = e, y = 0.5, shape = 16, color = colE, size = 2, na.rm = T)                                                  # : : : : draw the E point estimate
          p <- p + ggplot2::annotate("text", x = lx, y = ly, label = lab, hjust = hj, vjust = vj, na.rm = T)                                                  # : : : : draw the annotate label
          p <- p + ggplot2::scale_x_continuous(name = xLab, limits = xLims, expand = ggplot2::expansion())                                                    # : : : : set the x-axis scale
          p <- p + ggplot2::scale_y_continuous(name = yLab, limits = yLims, expand = ggplot2::expansion(), breaks = yBrks, labels = yLabs)                    # : : : : set the y-axis scale
          p <- p + ggplot2::ggtitle(ttl, subtitle = subt)                                                                                                     # : : : : add the plot title and subtitle
          p <- p + ggplot2::theme_minimal()                                                                                                                   # : : : : set the base theme to minimal
          p <- p + ggplot2::theme(                                                                                                                            # : : : : CUSTOM theme elements
            plot.title.position = "plot",                                                                                                                     # : : : : : position title and subtitle at the edge of the plot
            plot.title          = ggplot2::element_text(face = "bold"),                                                                                       # : : : : : bold the title
            plot.subtitle       = ggplot2::element_text(face = "plain"),                                                                                      # : : : : : plain the subtitle
            axis.title          = ggplot2::element_text(face = "bold"),                                                                                       # : : : : : bold the axis titles
            panel.background    = ggplot2::element_rect(color = colK , fill = colW , linewidth = mid ),                                                       # : : : : : black outline, white fill, mid- width border for panel background
            plot.background     = ggplot2::element_rect(color = colK , fill = col95, linewidth = wide),                                                       # : : : : : black outline, g95   fill, wide-width border for plot  background
            panel.border        = ggplot2::element_rect(color = colK , fill = NA   , linewidth = mid ),                                                       # : : : : : black outline, no    fill, mid- width panel border drawn over the top of everything
            panel.grid.major    = ggplot2::element_line(color = col90,               linewidth = thin),                                                       # : : : : : thin grey95 major x- and y-axis gridlines
            panel.grid.minor.x  = ggplot2::element_line(color = col90,               linewidth = thin),                                                       # : : : : : thin grey95 minor x-axis gridlines
            axis.ticks          = ggplot2::element_line(color = colK ,               linewidth = thin),                                                       # : : : : : thin black axis tick marks
            panel.grid.minor.y  = ggplot2::element_blank()                                                                                                    # : : : : : no y-axis minor gridlines
          )                                                                                                                                                   # : : : : END CUSTOM
          file <- uj::p0(ttl, " ", subt, ".", dev)                                                                                                            # : : : : build filename
          ggplot2::ggsave(file, plot = p, device = dev, path = dir, width = w, height = h, units = "in", dpi = dpi)                                           # : : : : save the plot
        }                                                                                                                                                     # : : : END IF
      }                                                                                                                                                       # : : END IF
    }                                                                                                                                                         # : END FOR
  }                                                                                                                                                           # END FOR
}
