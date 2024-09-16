#' @encoding UTF-8
#' @title Conduct Hierarchical Linear Value Added Model Analyses
#' @description Conducts one analysis per unique combination of year and subject.
#' @param x A data frame containing entity.id, year, subject, and z-scored scaled scores.
#' @param z.curr Character scalar name of the variable containing \eqn{z}-scored current scaled scores.
#' @param z.priors Character vector of names of variables containing \eqn{z}-scored prior scaled scores.
#' @param demos Optional character vector of names of variables containing demographics.
#' @param clear Logical scalar indicating whether to clear the console for user updates.
#' @return A data frame.
#' @export
hlm_vam <- function(x, z.curr, z.priors, demos = NULL, clear = T) {
  nllD <- base::is.null(demos)
  uj::say("Run VAM analyses", lev = 1, clear = clear)                                                                    # user update
  NC <- NA_character_; NI <- NA_integer_; NR <- NA_real_                                                                 # for convenience
  if (nllD) {out <- tibble::tibble(entity.ID = NC, year = NI, subject = NC, vam  = NR,            .rows = 0)}            # init the return data frame
  else      {out <- tibble::tibble(entity.id = NC, year = NI, subject = NC, vam1 = NR, vam2 = NR, .rows = 0)}            #   ...based on whether there are demographics
  for (y in uj::suv(x$year)) {                                                                                           # FOR each (sorted) unique value of year
    for (s in uj::suv(x$subject)) {                                                                                      # : FOR each (sorted) unique value of subject
      uj::say("VAM for [year = ", y, "] and [subject, = ", s, "]", lev = 2)                                              # : : user update
      i <- x$year == y & x$subject == s                                                                                  # : : index matching rows of input data
      i[base::is.na(i)] <- F                                                                                             # : : set [NA] index values to [FALSE]
      xx <- x[i, ]                                                                                                       # : : extract just the matching rows
      if (nllD) {                                                                                                        # : : IF there are no demographics
        code <- uj::p0("lme4::lmer(", z.curr, " ~ ",                                                                     # : : : code to run HLM VAM using
                       uj::p0(       z.priors         , collapse = " + "), " + ",                                        # : : :   ... priors as predictors
                       uj::p0(uj::p0(z.priors, " ^ 2"), collapse = " + "), " + ",                                        # : : :   ... squared priors as predictors
                       uj::p0("(1 | entity.id), data = xx)"))                                                            # : : :   ... with random intercepts
        uj::say(); mod  <- uj::run(code)                                                                                 # : : : run the 1st VAM
        uj::say(); beta <- coef(mod)$entity.id                                                                           # : : : extract coefficients for the 1st VAM
        uj::say(); ids  <- base::rownames(beta)                                                                          # : : : get entity IDs
        uj::say(); vam  <- beta$`(Intercept)`                                                                            # : : : extract random intercepts of 1st VAM
        uj::say(); temp <- tibble::tibble(entity.id = ids, year = y, subject = s, vam = vam)                             # : : : store the results
      } else {                                                                                                           # : : ELSE
        code1 <- uj::p0("lme4::lmer(", z.curr, " ~ ",                                                                    # : : : code to run HLM VAM using
                        uj::p0(       z.priors         , collapse = " + "), " + ",                                       # : : :   ... priors as predictors
                        uj::p0(uj::p0(z.priors, " ^ 2"), collapse = " + "), " + ",                                       # : : :   ... squared priors as predictors
                        uj::p0("(1 | entity.id), data = xx)"))                                                           # : : :   ... with random intercepts
        code2 <- uj::p0("lme4::lmer(", z.curr, " ~ ",                                                                    # : : : code to run HLM VAM using
                        uj::p0(       z.priors         , collapse = " + "), " + ",                                       # : : :   ... priors as predictors
                        uj::p0(uj::p0(z.priors, " ^ 2"), collapse = " + "), " + ",                                       # : : :   ... squared priors as predictors
                        uj::p0(       demos            , collapse = " + "), " + ",                                       # : : :   ... demographics as predictors
                        uj::p0("(1 | entity.id), data = xx)"))                                                           # : : :   ... with random intercepts
        uj::say(); mod1  <- uj::run(code1)                                                                               # : : : run the 1st VAM
        uj::say(); mod2  <- uj::run(code2)                                                                               # : : : rum the 2nd VAM
        uj::say(); beta1 <- coef(mod1)$entity.id                                                                         # : : : extract coefficients for the 1st VAM
        uj::say(); beta2 <- coef(mod2)$entity.id                                                                         # : : : extract coefficients for the 2nd VAM
        uj::say(); ids   <- base::rownames(beta1)                                                                        # : : : get entity IDs
        uj::say(); vam1  <- beta1$`(Intercept)`                                                                          # : : : extract random intercepts of 1st VAM
        uj::say(); vam2  <- beta2$`(Intercept)`                                                                          # : : : extract random intercepts of 2nd VAM
        uj::say(); temp  <- tibble::tibble(entity.id = ids, year = y, subject = s, vam1 = vam1, vam2 = vam2)             # : : : store the results
      }                                                                                                                  # : : END FOR
      out <- base::rbind(out, temp)                                                                                      # : : row bind to the output tibble
    }                                                                                                                    # : END FOR
  }                                                                                                                      # END FOR
  out                                                                                                                    # return the result
}
