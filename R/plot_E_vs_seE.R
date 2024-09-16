#' @encoding UTF-8
#' @title Plot Jackknifed Estimates of \eqn{E} vs. \eqn{}
#' @inheritParams plot_C_densities
#' @return `NULL`. Called for the side effect of building and saving plots.
#' @export
plot_E_vs_seE <- function(x, dir = getwd(), sub.dir = "plots", sub.sub = "E_vs_se(E)", w = 6.5, h = 3.5, dpi = 500, dev = "png", clear = T) {
  uj::say("Plot entity capacity vs. standard error", lev = 1)
  x <- x[x$cond == "ok" , base::c("year", "subject", "C", "E", "se.E")]
  levs <- base::c("C[2]^'AGP'", "C[2]^'P1'", "C[2]^'P2'",
                  "C[3]^'AGP'", "C[3]^'P1'", "C[3]^'P2'",
                  "C[4]^'AGP'", "C[4]^'P1'", "C[4]^'P2'")
  x$C[x$C == "C2.agp"] <- "C[2]^'AGP'"
  x$C[x$C == "C3.agp"] <- "C[3]^'AGP'"
  x$C[x$C == "C4.agp"] <- "C[4]^'AGP'"
  x$C[x$C == "C2.p"  ] <- "C[2]^'P1'"
  x$C[x$C == "C3.p"  ] <- "C[3]^'P1'"
  x$C[x$C == "C4.p"  ] <- "C[4]^'P1'"
  x$C[x$C == "C2.pp" ] <- "C[2]^'P2'"
  x$C[x$C == "C3.pp" ] <- "C[3]^'P2'"
  x$C[x$C == "C4.pp" ] <- "C[4]^'P2'"
  x$C    <- base::factor(x$C   , levels = levs           , ordered = T)
  x$year <- base::factor(x$year, levels = uj::suv(x$year), ordered = T)
  xe <- x[x$subject == "ela" , ]
  xm <- x[x$subject == "math", ]
  ex   <- ggplot2::expansion()
  eBase <- base::c(0, 50, 100)
  eLims <- base::c(-4, 104)
  eBrks <- base::c(0, 25, 50, 75, 100)
  sBase <- base::c(0, 50)
  sLims <- base::c(-3, 53)
  sBrks <- base::c(0, 10, 20, 30, 40, 50)
  colE <- scales::alpha("cyan4"  , 2 / 3)
  colM <- scales::alpha("orange4", 2 / 3)
  pe <- ggplot2::ggplot(data = xe, mapping = ggplot2::aes(x = E, y = se.E))
  pm <- ggplot2::ggplot(data = xm, mapping = ggplot2::aes(x = E, y = se.E))
  pe <- pe + ggplot2::geom_vline(xintercept = eBase, color = "grey50", linetype = "dashed", linewidth = 0.15)
  pm <- pm + ggplot2::geom_vline(xintercept = eBase, color = "grey50", linetype = "dashed", linewidth = 0.15)
  pe <- pe + ggplot2::geom_hline(yintercept = sBase, color = "grey50", linetype = "dashed", linewidth = 0.15)
  pm <- pm + ggplot2::geom_hline(yintercept = sBase, color = "grey50", linetype = "dashed", linewidth = 0.15)
  pe <- pe + ggplot2::geom_point(shape = ".", color = colE, na.rm = T)
  pm <- pm + ggplot2::geom_point(shape = ".", color = colM, na.rm = T)
  pe <- pe + ggplot2::facet_grid(year ~ C, scales = "fixed", labeller = "label_parsed")
  pm <- pm + ggplot2::facet_grid(year ~ C, scales = "fixed", labeller = "label_parsed")
  pe <- pe + ggplot2::scale_x_continuous(name = "E"    , limits = eLims, breaks = eBrks, expand = ex)
  pm <- pm + ggplot2::scale_x_continuous(name = "E"    , limits = eLims, breaks = eBrks, expand = ex)
  pe <- pe + ggplot2::scale_y_continuous(name = "se(E)", limits = sLims, breaks = sBrks, expand = ex)
  pm <- pm + ggplot2::scale_y_continuous(name = "se(E)", limits = sLims, breaks = sBrks, expand = ex)
  pe <- pe + ggplot2::ggtitle("Entity Capacity vs. Standard Error in ELA")
  pm <- pm + ggplot2::ggtitle("Entity Capacity vs. Standard Error in Math")
  pe <- pe + ggplot2::theme_minimal()
  pm <- pm + ggplot2::theme_minimal()
  pe <- pe + ggplot2::theme(
    plot.title.position = "plot",
    plot.title          = ggplot2::element_text(face = "bold"   , size = 20 / ggplot2::.pt, color = "black"),
    axis.title          = ggplot2::element_text(face = "bold"   , size = 19 / ggplot2::.pt, color = "black"),
    strip.text          = ggplot2::element_text(face = "bold"   , size = 18 / ggplot2::.pt, color = "white"),
    axis.text           = ggplot2::element_text(face = "plain"  , size = 12 / ggplot2::.pt, color = "black"),
    axis.ticks          = ggplot2::element_line(color = "black" , linewidth = 0.25),
    panel.grid.major    = ggplot2::element_line(color = "grey95", linewidth = 0.25),
    panel.grid.minor    = ggplot2::element_blank(),
    plot.background     = ggplot2::element_rect(color = "black", linewidth = 0.75, fill = "grey95"),
    panel.background    = ggplot2::element_rect(color = "black", linewidth = 0.50, fill = "white" ),
    strip.background    = ggplot2::element_rect(color = "black", linewidth = 0.50, fill = "black" ),
    panel.border        = ggplot2::element_rect(color = "black", linewidth = 0.50, fill = NA      )
  )
  pm <- pm + ggplot2::theme(
    plot.title.position = "plot",
    plot.title          = ggplot2::element_text(face = "bold"   , size = 20 / ggplot2::.pt, color = "black"),
    axis.title          = ggplot2::element_text(face = "bold"   , size = 19 / ggplot2::.pt, color = "black"),
    strip.text          = ggplot2::element_text(face = "bold"   , size = 18 / ggplot2::.pt, color = "white"),
    axis.text           = ggplot2::element_text(face = "plain"  , size = 12 / ggplot2::.pt, color = "black"),
    axis.ticks          = ggplot2::element_line(color = "black" , linewidth = 0.25),
    panel.grid.major    = ggplot2::element_line(color = "grey95", linewidth = 0.25),
    panel.grid.minor    = ggplot2::element_blank(),
    plot.background     = ggplot2::element_rect(color = "black", linewidth = 0.75, fill = "grey95"),
    panel.background    = ggplot2::element_rect(color = "black", linewidth = 0.50, fill = "white" ),
    strip.background    = ggplot2::element_rect(color = "black", linewidth = 0.50, fill = "black" ),
    panel.border        = ggplot2::element_rect(color = "black", linewidth = 0.50, fill = NA      )
  )
  uj::say("Save plots", lev = 1)
  sep <- base::.Platform$file.sep
  dir <- uj::p0(base::c(dir, sub.dir, sub.sub), collapse = sep)
  fe <- uj::p0(dir, sep, "ela_E_vs_se(E)." , dev)
  fm <- uj::p0(dir, sep, "math_E_vs_se(E).", dev)
  ggplot2::ggsave(fe, plot = pe, device = dev, width = w, height = h, units = "in", dpi = dpi)
  ggplot2::ggsave(fm, plot = pm, device = dev, width = w, height = h, units = "in", dpi = dpi)
  uj::say("DONE!", lev = 1)
}

#' @encoding UTF-8
#' @title Plot Jackknifed Estimates of \eqn{E} vs. \eqn{}
#' @inheritParams plot_C_densities
#' @return `NULL`. Called for the side effect of building and saving plots.
#' @export
plot_n_vs_seE <- function(x, dir = getwd(), sub.dir = "plots", sub.sub = "N_vs_se(E)", w = 6.5, h = 3.5, dpi = 500, dev = "png", clear = T) {
  uj::say("Plot entity capacity vs. standard error", lev = 1)
  x <- x[x$cond == "ok" , base::c("year", "subject", "C", "ns", "se.E")]
  levs <- base::c("C[2]^'AGP'", "C[2]^'P1'", "C[2]^'P2'",
                  "C[3]^'AGP'", "C[3]^'P1'", "C[3]^'P2'",
                  "C[4]^'AGP'", "C[4]^'P1'", "C[4]^'P2'")
  x$C[x$C == "C2.agp"] <- "C[2]^'AGP'"
  x$C[x$C == "C3.agp"] <- "C[3]^'AGP'"
  x$C[x$C == "C4.agp"] <- "C[4]^'AGP'"
  x$C[x$C == "C2.p"  ] <- "C[2]^'P1'"
  x$C[x$C == "C3.p"  ] <- "C[3]^'P1'"
  x$C[x$C == "C4.p"  ] <- "C[4]^'P1'"
  x$C[x$C == "C2.pp" ] <- "C[2]^'P2'"
  x$C[x$C == "C3.pp" ] <- "C[3]^'P2'"
  x$C[x$C == "C4.pp" ] <- "C[4]^'P2'"
  x$C    <- base::factor(x$C   , levels = levs           , ordered = T)
  x$year <- base::factor(x$year, levels = uj::suv(x$year), ordered = T)
  xe <- x[x$subject == "ela" , ]
  xm <- x[x$subject == "math", ]
  ex   <- ggplot2::expansion()
  sBase <- base::c(0, 50)
  sLims <- base::c(-3, 53)
  sBrks <- base::c(0, 10, 20, 30, 40, 50)
  colE <- scales::alpha("cyan4"  , 2 / 3)
  colM <- scales::alpha("orange4", 2 / 3)
  pe <- ggplot2::ggplot(data = xe, mapping = ggplot2::aes(x = ns, y = se.E))
  pm <- ggplot2::ggplot(data = xm, mapping = ggplot2::aes(x = ns, y = se.E))
  pe <- pe + ggplot2::geom_hline(yintercept = sBase, color = "grey50", linetype = "dashed", linewidth = 0.15)
  pm <- pm + ggplot2::geom_hline(yintercept = sBase, color = "grey50", linetype = "dashed", linewidth = 0.15)
  pe <- pe + ggplot2::geom_point(shape = ".", color = colE, na.rm = T)
  pm <- pm + ggplot2::geom_point(shape = ".", color = colM, na.rm = T)
  pe <- pe + ggplot2::facet_grid(year ~ C, scales = "fixed", labeller = "label_parsed")
  pm <- pm + ggplot2::facet_grid(year ~ C, scales = "fixed", labeller = "label_parsed")
  pe <- pe + ggplot2::scale_x_continuous(name = "Number of Students", expand = ex)
  pm <- pm + ggplot2::scale_x_continuous(name = "Number of Students", expand = ex)
  pe <- pe + ggplot2::scale_y_continuous(name = "se(E)", limits = sLims, breaks = sBrks, expand = ex)
  pm <- pm + ggplot2::scale_y_continuous(name = "se(E)", limits = sLims, breaks = sBrks, expand = ex)
  pe <- pe + ggplot2::ggtitle("Sample Size vs. Standard Error of Entity Capacity in ELA")
  pm <- pm + ggplot2::ggtitle("Sample Size vs. Standard Error of Entity Capacity in Math")
  pe <- pe + ggplot2::theme_minimal()
  pm <- pm + ggplot2::theme_minimal()
  pe <- pe + ggplot2::theme(
    plot.title.position = "plot",
    plot.title          = ggplot2::element_text(face = "bold"   , size = 20 / ggplot2::.pt, color = "black"),
    axis.title          = ggplot2::element_text(face = "bold"   , size = 19 / ggplot2::.pt, color = "black"),
    strip.text          = ggplot2::element_text(face = "bold"   , size = 18 / ggplot2::.pt, color = "white"),
    axis.text           = ggplot2::element_text(face = "plain"  , size = 12 / ggplot2::.pt, color = "black"),
    axis.ticks          = ggplot2::element_line(color = "black" , linewidth = 0.25),
    panel.grid.major    = ggplot2::element_line(color = "grey95", linewidth = 0.25),
    panel.grid.minor    = ggplot2::element_blank(),
    plot.background     = ggplot2::element_rect(color = "black", linewidth = 0.75, fill = "grey95"),
    panel.background    = ggplot2::element_rect(color = "black", linewidth = 0.50, fill = "white" ),
    strip.background    = ggplot2::element_rect(color = "black", linewidth = 0.50, fill = "black" ),
    panel.border        = ggplot2::element_rect(color = "black", linewidth = 0.50, fill = NA      )
  )
  pm <- pm + ggplot2::theme(
    plot.title.position = "plot",
    plot.title          = ggplot2::element_text(face = "bold"   , size = 20 / ggplot2::.pt, color = "black"),
    axis.title          = ggplot2::element_text(face = "bold"   , size = 19 / ggplot2::.pt, color = "black"),
    strip.text          = ggplot2::element_text(face = "bold"   , size = 18 / ggplot2::.pt, color = "white"),
    axis.text           = ggplot2::element_text(face = "plain"  , size = 12 / ggplot2::.pt, color = "black"),
    axis.ticks          = ggplot2::element_line(color = "black" , linewidth = 0.25),
    panel.grid.major    = ggplot2::element_line(color = "grey95", linewidth = 0.25),
    panel.grid.minor    = ggplot2::element_blank(),
    plot.background     = ggplot2::element_rect(color = "black", linewidth = 0.75, fill = "grey95"),
    panel.background    = ggplot2::element_rect(color = "black", linewidth = 0.50, fill = "white" ),
    strip.background    = ggplot2::element_rect(color = "black", linewidth = 0.50, fill = "black" ),
    panel.border        = ggplot2::element_rect(color = "black", linewidth = 0.50, fill = NA      )
  )
  uj::say("Save plots", lev = 1)
  sep <- base::.Platform$file.sep
  dir <- uj::p0(base::c(dir, sub.dir, sub.sub), collapse = sep)
  fe <- uj::p0(dir, sep, "ela_N_vs_se(E)." , dev)
  fm <- uj::p0(dir, sep, "math_N_vs_se(E).", dev)
  ggplot2::ggsave(fe, plot = pe, device = dev, width = w, height = h, units = "in", dpi = dpi)
  ggplot2::ggsave(fm, plot = pm, device = dev, width = w, height = h, units = "in", dpi = dpi)
  uj::say("DONE!", lev = 1)
}
