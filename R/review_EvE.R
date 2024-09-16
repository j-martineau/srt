#' @encoding UTF-8
#' @title Graphically Review Different Variants of Entity-Group Capacity
#' @param ID Character vector of entity-group identifying variable names.
#' @param C Character vector of student challenge variable names.
#' @return `NULL`. Called for the side effect of plotting two-variable relationships for review.
#' @export
review_EvE <- function(x, ID, C) {
  z  <- x$y[ , base::c(ID, "C", "E")]                                                                                    # extract the ID vars, C var, and E var from the output
  cn <- base::colnames(z)                                                                                                # col names for later
  z  <- uj::run("dplyr::group_by(z, ", uj::p0(ID, collapse = ", "), ")")                                                 # group [z] by ID vars
  n  <- uj::N(C)                                                                                                         # number of different challenge variables
  for (i in 1:(n - 1)) {                                                                                                 # FOR each challenge variable except the last
    Ci  <- C[i]                                                                                                          # : get its name
    Ei  <- uj::p0("E.", Ci)                                                                                              # : create a name for its E variable
    zi  <- z[z$C == Ci, ]                                                                                                # : subset its rows
    cni <- cn                                                                                                            # : copy col names
    cni[cni == "E"] <- Ei                                                                                                # : substitute the E-variable name
    base::colnames(zi) <- cni                                                                                            # : rename columns
    for (j in (i + 1):n) {                                                                                               # : FOR each following challenge variable
      Cj  <- C[j]                                                                                                        # : : get its name
      Ej  <- uj::p0("E.", Cj)                                                                                            # : : create a name for its E variable
      zj  <- z[z$C == Cj, ]                                                                                              # : : subset its rows
      cnj <- cn                                                                                                          # : : copy col names
      cnj[cnj == "E"] <- Ej                                                                                              # : : substitute the E-variable name
      base::colnames(zj) <- cnj                                                                                          # : : rename columns
      zz  <- dplyr::left_join(zi, zj, by = ID)                                                                           # : : join the subsets
      xx  <- zz[ , Ei]                                                                                                   # : : x-variable
      yy  <- zz[ , Ej]                                                                                                   # : : y-variable
      ii  <- uj::ok(xx) & uj::ok(yy)                                                                                     # : : index non-NA on both x- and y-variables
      nn  <- uj::nw(ii)                                                                                                  # : : number of cases with non-NA on both
      r1  <- uj::cor0(xx, yy)                                                                                            # : : calculate correlation
      r2  <- r1 ^ 2                                                                                                      # : : and R-squared
      ttl <- uj::spf("r2 = %0.3f | r = %0.3f | N = %0.0f", r2, r1, nn)                                                   # : : build label
      p   <- uj::run(uj::p0("ggplot2::ggplot(data = zz, mapping = ggplot2::aes(x = ", Ei, ", y = ", Ej, "))"))           # : : initialize the plot
      p   <- p + ggplot2::geom_point(shape = ".", na.rm = T)                                                             # : : add points
      p   <- p + ggplot2::ggtitle(ttl)                                                                                   # : : add the label
      uj::xplot(p)                                                                                                       # : : close all plots and print the new one
      uj::continue()                                                                                                     # : : wait for user
    }                                                                                                                    # : END FOR
  }                                                                                                                      # END FOR
}
