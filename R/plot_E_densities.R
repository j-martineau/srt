#' @encoding UTF-8
#' @title Build Student Challenge Density Plots
#' @param x The input data for a call to \code{\link{srt}}.
#' @param Cs Character vector of names of the student challenge variables for which to plot entity-group capacity.
#' @inheritParams plot_entity_group
#' @return `NULL`. Called for the side effect of building and saving plots.
#' @export
plot_E_densities <- function(x, Cs, conds, dir = getwd(), sub.dir = "plots", sub.sub = "E_densities", w = 6.5, h = 4, dpi = 500, dev = "png", clear = T) {
  uj::say("Calculate estimated densities", lev = 1)
  eCol <- "cyan4"  ; eFill <- scales::alpha(eCol, 0.2)
  mCol <- "orange4"; mFill <- scales::alpha(mCol, 0.2)
  NAC <- NA_character_; NAI <- NA_integer_; NAR <- NA_real_
  y <- tibble::tibble(subject = NAC, year = NAI, C = NAC, x = NAR, y = NAR, .rows = 0)
  for (S in uj::suv(x$subject)) {
    iS <- x$subject == S
    for (Y in uj::suv(x$year)) {
      iY <- x$year == Y
      for (j in 1:uj::N(Cs)) {
        uj::say()
        C    <- Cs[j]
        cond <- conds[j]
        iC   <- x[[cond]] == "ok"
        i    <- iS & iY & iC
        i[uj::na(i)] <- F
        E <- uj::av(x$E[i])
        E <- E[uj::ok(E)]
        E <- stats::density(E, cut = 1)
        y <- base::rbind(y, tibble::tibble(subject = S, year = Y, C = C, x = base::c(0, E$x, 100, 0), y = base::c(0, E$y, 0, 0)))
      }
    }
  }
  uj::say("Build plots", lev = 1)
  levs <- base::c("C[2]^'AGP'", "C[2]^'P1'", "C[2]^'P2'",
                  "C[3]^'AGP'", "C[3]^'P1'", "C[3]^'P2'",
                  "C[4]^'AGP'", "C[4]^'P1'", "C[4]^'P2'")
  y$x <- base::pmin(100, base::pmax(0, y$x))
  y$C[y$C == "C2.agp"] <- "C[2]^'AGP'"
  y$C[y$C == "C3.agp"] <- "C[3]^'AGP'"
  y$C[y$C == "C4.agp"] <- "C[4]^'AGP'"
  y$C[y$C == "C2.p"  ] <- "C[2]^'P1'"
  y$C[y$C == "C3.p"  ] <- "C[3]^'P1'"
  y$C[y$C == "C4.p"  ] <- "C[4]^'P1'"
  y$C[y$C == "C2.pp" ] <- "C[2]^'P2'"
  y$C[y$C == "C3.pp" ] <- "C[3]^'P2'"
  y$C[y$C == "C4.pp" ] <- "C[4]^'P2'"
  y$C <- base::factor(y$C, levels = levs, labels = levs, ordered = T)
  y$year <- base::factor(y$year, levels = uj::suv(y$year), ordered = T)
  xLims <- base::c(-2, 102)
  xBase <- base::c(0, 50, 100)
  xBrks <- base::c(0, 25, 50, 75, 100)
  xe <- y[y$subject == "ela" , ]
  xm <- y[y$subject == "math", ]
  pe <- ggplot2::ggplot(data = xe, mapping = ggplot2::aes(x = x, y = y))
  pm <- ggplot2::ggplot(data = xm, mapping = ggplot2::aes(x = x, y = y))
  pe <- pe + ggplot2::geom_vline(xintercept = xBase, color = "grey50", linetype = "dashed", linewidth = 0.25)
  pm <- pm + ggplot2::geom_vline(xintercept = xBase, color = "grey50", linetype = "dashed", linewidth = 0.25)
  pe <- pe + ggplot2::geom_hline(yintercept = 0    , color = "grey50", linetype = "dashed", linewidth = 0.25)
  pm <- pm + ggplot2::geom_hline(yintercept = 0    , color = "grey50", linetype = "dashed", linewidth = 0.25)
  pe <- pe + ggplot2::geom_polygon(color = eCol, linewidth = 0.25, fill = eFill)
  pm <- pm + ggplot2::geom_polygon(color = mCol, linewidth = 0.25, fill = mFill)
  pe <- pe + ggplot2::facet_grid(C ~ year, scales = "free", labeller = "label_parsed")
  pm <- pm + ggplot2::facet_grid(C ~ year, scales = "free", labeller = "label_parsed")
  pe <- pe + ggplot2::scale_x_continuous(name = "E", limits = xLims, breaks = xBrks, expand = ggplot2::expansion())
  pm <- pm + ggplot2::scale_x_continuous(name = "E", limits = xLims, breaks = xBrks, expand = ggplot2::expansion())
  pe <- pe + ggplot2::scale_y_continuous(name = "Estimated Density", expand = ggplot2::expansion(mult = 0.1))
  pm <- pm + ggplot2::scale_y_continuous(name = "Estimated Density", expand = ggplot2::expansion(mult = 0.1))
  pe <- pe + ggplot2::ggtitle("Estimated Densities of Entity Capacity in ELA")
  pm <- pm + ggplot2::ggtitle("Estimated Densities of Entity Capacity in Math")
  pe <- pe + ggplot2::theme_minimal()
  pm <- pm + ggplot2::theme_minimal()
  pe <- pe + ggplot2::theme(
    plot.title.position = "plot",
    plot.title          = ggplot2::element_text(face = "bold"   , size = 20 / ggplot2::.pt, color = "black"),
    axis.title          = ggplot2::element_text(face = "bold"   , size = 19 / ggplot2::.pt, color = "black"),
    strip.text          = ggplot2::element_text(face = "bold"   , size = 18 / ggplot2::.pt, color = "white"),
    axis.text           = ggplot2::element_text(face = "plain"  , size = 12 / ggplot2::.pt, color = "black"),
    axis.ticks          = ggplot2::element_line(color = "black" , linewidth = 0.50),
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
    axis.ticks          = ggplot2::element_line(color = "black" , linewidth = 0.50),
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
  fe <- uj::p0(dir, sep, "ela_E_densities.", dev)
  fm <- uj::p0(dir, sep, "math_E_densities.", dev)
  ggplot2::ggsave(fe, plot = pe, device = dev, width = w, height = h, units = "in", dpi = dpi)
  ggplot2::ggsave(fm, plot = pm, device = dev, width = w, height = h, units = "in", dpi = dpi)
  uj::say("DONE!", lev = 1)
}
