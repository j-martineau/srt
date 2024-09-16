#' @encoding UTF-8
#' @title Plot Jackknifed Estimates of \eqn{E} vs. \eqn{}
#' @inheritParams plot_C_densities
#' @return `NULL`. Called for the side effect of building and saving plots.
#' @export
plot_N_vs_seE <- function(x, dir = getwd(), sub.dir = "plots", sub.sub = "N_vs_se(E)", w = 6.5, h = 4, dpi = 500, dev = "png", clear = T) {
  x <- x$y[ , base::c("year", "subject", "C", "ns", "se.E")]
  levs <- base::c("C(2,AGP)", "C(2,P1)", "C(2,P2)",
                  "C(3,AGP)", "C(3,P1)", "C(3,P2)",
                  "C(4,AGP)", "C(4,P1)", "C(4,P2)")
  x$C[x$C == "C2.agp"] <- "C(2,AGP)"
  x$C[x$C == "C3.agp"] <- "C(3,AGP)"
  x$C[x$C == "C4.agp"] <- "C(4,AGP)"
  x$C[x$C == "C2.p"  ] <- "C(2,P1)"
  x$C[x$C == "C3.p"  ] <- "C(3,P1)"
  x$C[x$C == "C4.p"  ] <- "C(4,P1)"
  x$C[x$C == "C2.pp" ] <- "C(2,P2)"
  x$C[x$C == "C3.pp" ] <- "C(3,P2)"
  x$C[x$C == "C4.pp" ] <- "C(4,P2)"
  x$C    <- base::factor(x$C   , levels = levs           , ordered = T)
  x$year <- base::factor(x$year, levels = uj::suv(x$year), ordered = T)
  xe     <- x[x$subject == "ela" , ]
  xm     <- x[x$subject == "math", ]
  ex     <- ggplot2::expansion()
  exE    <- ggplot2::expansion(mult = 0.02)
  sBase  <- base::c(0, 50)
  sLims  <- base::c(-2, 52)
  sBrks  <- base::c(0, 10, 20, 30, 40, 50)
  colE   <- scales::alpha("cyan4"  , 1 / 3)
  colM   <- scales::alpha("orange4", 1 / 3)
  pe <- ggplot2::ggplot(data = xe, mapping = ggplot2::aes(x = ns, y = se.E))
  pm <- ggplot2::ggplot(data = xm, mapping = ggplot2::aes(x = ns, y = se.E))
  pe <- pe + ggplot2::geom_hline(yintercept = sBase, color = "grey50", linetype = "dashed", linewidth = 0.15)
  pm <- pm + ggplot2::geom_hline(yintercept = sBase, color = "grey50", linetype = "dashed", linewidth = 0.15)
  pe <- pe + ggplot2::geom_point(shape = ".", color = colE, na.rm = T)
  pm <- pm + ggplot2::geom_point(shape = ".", color = colM, na.rm = T)
  pe <- pe + ggplot2::facet_grid(rows = ggplot2::vars(year), cols = ggplot2::vars(C), scales = "fixed")
  pm <- pm + ggplot2::facet_grid(rows = ggplot2::vars(year), cols = ggplot2::vars(C), scales = "fixed")
  pe <- pe + ggplot2::scale_x_continuous(name = "N(students)", expand = exE)
  pm <- pm + ggplot2::scale_x_continuous(name = "N(students)", expand = exE)
  pe <- pe + ggplot2::scale_y_continuous(name = "se(E)"      , limits = sLims, breaks = sBrks, expand = ex)
  pm <- pm + ggplot2::scale_y_continuous(name = "se(E)"      , limits = sLims, breaks = sBrks, expand = ex)
  pe <- pe + ggplot2::ggtitle("Entity Capacity vs. Standard Error in ELA")
  pm <- pm + ggplot2::ggtitle("Entity Capacity vs. Standard Error in Math")
  pe <- pe + ggplot2::theme_minimal()
  pm <- pm + ggplot2::theme_minimal()
  pe <- pe + ggplot2::theme(
    plot.title.position = "plot",
    plot.title          = ggplot2::element_text(face = "bold"   , size = 14 / ggplot2::.pt, color = "black"),
    axis.title          = ggplot2::element_text(face = "bold"   , size = 14 / ggplot2::.pt, color = "black"),
    strip.text          = ggplot2::element_text(face = "bold"   , size = 13 / ggplot2::.pt, color = "white"),
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
    plot.title          = ggplot2::element_text(face = "bold"   , size = 14 / ggplot2::.pt, color = "black"),
    axis.title          = ggplot2::element_text(face = "bold"   , size = 14 / ggplot2::.pt, color = "black"),
    strip.text          = ggplot2::element_text(face = "bold"   , size = 13 / ggplot2::.pt, color = "white"),
    axis.text           = ggplot2::element_text(face = "plain"  , size = 12 / ggplot2::.pt, color = "black"),
    axis.ticks          = ggplot2::element_line(color = "black" , linewidth = 0.25),
    panel.grid.major    = ggplot2::element_line(color = "grey95", linewidth = 0.25),
    panel.grid.minor    = ggplot2::element_blank(),
    plot.background     = ggplot2::element_rect(color = "black", linewidth = 0.75, fill = "grey95"),
    panel.background    = ggplot2::element_rect(color = "black", linewidth = 0.50, fill = "white" ),
    strip.background    = ggplot2::element_rect(color = "black", linewidth = 0.50, fill = "black" ),
    panel.border        = ggplot2::element_rect(color = "black", linewidth = 0.50, fill = NA      )
  )
  sep <- base::.Platform$file.sep
  dir <- uj::p0(base::c(dir, sub.dir, sub.sub), collapse = sep)
  fe  <- uj::p0(dir, sep, "ela_N_vs_se(E)." , dev)
  fm  <- uj::p0(dir, sep, "math_N_vs_se(E).", dev)
  ggplot2::ggsave(fe, plot = pe, device = dev, width = w, height = h, units = "in", dpi = dpi)
  ggplot2::ggsave(fm, plot = pm, device = dev, width = w, height = h, units = "in", dpi = dpi)
}
