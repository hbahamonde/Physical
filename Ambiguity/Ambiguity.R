# Ambiguity: gendered appearance configurations and personal votes
# Authors: Outi Sarpila and Hector Bahamonde
# Start: 12 October 2026. Natural experiment under an explicit observational assignment framework.
# Natural-experiment classification does not establish exchangeability or causal effects.
# Run: Rscript Ambiguity.R
# Knit: Rscript -e 'knitr::knit("Ambiguity.rnw")'
# Named chunks below are imported by knitr::read_chunk(). No cleaning is rerun.

cat("\014")
rm(list=ls())


## ---- setup
options(stringsAsFactors = FALSE, width = 100)
required <- c("ggplot2", "knitr", "gridExtra", "scales")
missing_packages <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Install required packages: ", paste(missing_packages, collapse = ", "))
library(ggplot2)
# Fixed study design and shared analysis settings. Observed values are added below.
# The saved candidate-level file does not contain the original respondent sample.
paper_values <- list(
  election_year = 2017L, rating_year = 2020L, n_respondents = 7920L,
  respondent_age_min = 18L, respondent_age_max = 64L, images_per_respondent = 50L,
  scale_min = 1, scale_max = 5, confidence_level = .95,
  prediction_percentiles = c(10, 90), effect_sd = 1, zero_cue = 0,
  trim_percent = c(1, 5), prediction_grid_points = 61L,
  support_radius = .65, support_min_candidates = 5L,
  raw_balance_tolerance = .25,
  surface_axis_limits = c(-2, 2), surface_grid_points = 65L
)
paper_values$scale_midpoint <- mean(c(paper_values$scale_min, paper_values$scale_max))
paper_values$confidence_pct <- 100 * paper_values$confidence_level
paper_values$ci_quantile <- (1 + paper_values$confidence_level) / 2
paper_values$prediction_quantiles <- paper_values$prediction_percentiles / 100
paper_values$trim_quantiles <- 1 - paper_values$trim_percent / 100
# Aliases retained for captions that refer to the local support rule.
paper_values$local_support_radius <- paper_values$support_radius
paper_values$local_support_min <- paper_values$support_min_candidates
find_project <- function() {
  override <- Sys.getenv("AMBIGUITY_DIR")
  if (nzchar(override)) {
    if (!file.exists(file.path(override, "Ambiguity.R"))) {
      stop("AMBIGUITY_DIR does not point to the Ambiguity project folder.")
    }
    return(normalizePath(override, mustWork = TRUE))
  }
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(arg)) {
    script_path <- sub("^--file=", "", arg[1])
    if (basename(script_path) == "Ambiguity.R" && file.exists(script_path)) {
      return(dirname(normalizePath(script_path)))
    }
  }
  for (fr in rev(sys.frames())) {
    if (is.character(fr$ofile) && length(fr$ofile) == 1L &&
        basename(fr$ofile) == "Ambiguity.R" && file.exists(fr$ofile)) {
      return(dirname(normalizePath(fr$ofile)))
    }
  }
  if (file.exists("Ambiguity.R")) return(normalizePath("."))
  # Editor selections have no source filename; use this project's known location.
  default_project <- "/Users/hectorbahamonde/research/Physical/Ambiguity"
  if (file.exists(file.path(default_project, "Ambiguity.R"))) {
    return(normalizePath(default_project))
  }
  stop("Cannot locate the Ambiguity project. Check its folder or set AMBIGUITY_DIR.")
}
project_dir <- find_project()
data_path <- Sys.getenv("AMBIGUITY_DATA", file.path(dirname(project_dir), "dat.Rdata"))
stopifnot(file.exists(data_path))
for (subdir in c("figures", "results")) dir.create(file.path(project_dir, subdir), showWarnings = FALSE)
source_hash_before <- unname(tools::md5sum(data_path))
set.seed(20261012)

## ---- load-data
# Load the saved clean object in isolation. Never source Voting_Phys_App.R.
data_env <- new.env(parent = baseenv())
loaded_names <- load(data_path, envir = data_env)
stopifnot("dat" %in% loaded_names, is.data.frame(data_env$dat))
dat_source <- data_env$dat
required_vars <- c("id", "candidate_number", "city", "party", "gender", "turnout", "age",
                   "femininity", "masculinity", "attractiveness")
stopifnot(all(required_vars %in% names(dat_source)))
stopifnot(!anyDuplicated(dat_source$id),
          !anyDuplicated(paste(dat_source$city, dat_source$candidate_number)))
# The original saved counts double-counted precinct and municipal totals; a surname-only
# merge also attached another person's votes to 67 photograph identities. Reuse all saved
# appearance/control data, and repair ONLY the in-memory electoral outcome from the audited
# Ministry crosswalk. The original dat.Rdata is never rewritten and cleaning is not rerun.
audit_path <- file.path(project_dir, "audit", "verified_photo_candidate_crosswalk.csv")
stopifnot(file.exists(audit_path))
vote_audit <- read.csv(audit_path, stringsAsFactors = FALSE)
stopifnot(nrow(vote_audit) == nrow(dat_source), !anyDuplicated(vote_audit$id),
          all(vote_audit$metadata_city_candidate_verified == "True"),
          all(vote_audit$appearance_photo_id_verified == "True"))
audit_index <- match(dat_source$id, vote_audit$id)
stopifnot(!anyNA(audit_index))
vote_audit <- vote_audit[audit_index, ]
stopifnot(all(dat_source$city == vote_audit$city),
          all(dat_source$candidate_number == vote_audit$candidate_number),
          all(as.character(dat_source$party) == vote_audit$official_party),
          all(is.na(dat_source$age) | dat_source$age == vote_audit$official_age),
          all(is.na(dat_source$gender) | as.character(dat_source$gender) == vote_audit$official_gender),
          all(vote_audit$official_votes >= 0 & vote_audit$official_votes <= vote_audit$city_valid_votes))
counts_wrong_person <- sum(dat_source$turnout / 2 != vote_audit$official_votes)
names_wrong_person <- sum(vote_audit$saved_name_belongs_other_candidate == "True")
# Full corrected data: keep the saved appearance/control columns, repair official names
# and votes, and retain the old corrupted values in clearly labelled provenance columns.
dat_fixed <- dat_source
dat_fixed$firstname_saved <- dat_source$firstname
dat_fixed$turnout_saved <- dat_source$turnout
dat_fixed$firstname <- vote_audit$official_firstname
dat_fixed$lastname <- vote_audit$official_lastname
dat_fixed$turnout <- vote_audit$official_votes
dat_fixed$official_votes <- vote_audit$official_votes
dat_fixed$city_valid_votes <- vote_audit$city_valid_votes
dat_fixed$vote_share <- dat_fixed$turnout / dat_fixed$city_valid_votes
dat_fixed$elected <- vote_audit$elected
dat_fixed$incumbent_councillor <- vote_audit$official_incumbent_councillor
dat_fixed$national_mp <- vote_audit$official_member_national_parliament
main_vars <- setdiff(required_vars, c("id", "candidate_number"))
keep <- complete.cases(dat_fixed[, main_vars])
dat_fixed$analysis_included <- keep
analysis_vars <- unique(c(required_vars, "firstname", "lastname", "turnout_saved", "official_votes",
                          "city_valid_votes", "vote_share", "elected", "incumbent_councillor", "national_mp"))
d <- droplevels(dat_fixed[keep, analysis_vars])
stopifnot(all(d$gender %in% c("Male", "Female")), all(d$turnout >= 0),
          all(d$turnout == floor(d$turnout)),
          all(d$femininity >= paper_values$scale_min & d$femininity <= paper_values$scale_max),
          all(d$masculinity >= paper_values$scale_min & d$masculinity <= paper_values$scale_max))
d$gender <- factor(d$gender, levels = c("Male", "Female"))
d$city <- factor(d$city) # Actual municipality. The source 'municipality' is a regional code.
d$party <- factor(d$party)
d$list_id <- interaction(d$city, d$party, drop = TRUE)
d$age_c <- d$age - mean(d$age)
sample_flow <- data.frame(stage = c("Saved clean dataset", "Complete primary variables"),
                          n = c(nrow(dat_source), nrow(d)))
write.csv(sample_flow, file.path(project_dir, "results", "sample_flow.csv"), row.names = FALSE)
missingness <- data.frame(variable = names(dat_source), missing = colSums(is.na(dat_source)))
write.csv(missingness, file.path(project_dir, "results", "missingness.csv"), row.names = FALSE)

## ---- construct-measures
# Ratings are made relative to others of the same age and gender, by separate rater samples.
# Hence these indices describe perceived cue configurations; they do NOT measure identity,
# nonbinary expression, or directly observed sex-category uncertainty.
# Standardize F and M within recorded gender on the SAME complete primary sample.
within_z <- function(x, group) ave(x, group, FUN = function(v) as.numeric(scale(v)))
d$zF <- within_z(d$femininity, d$gender)
d$zM <- within_z(d$masculinity, d$gender)
sign_gender <- ifelse(d$gender == "Male", 1, -1)
d$A0 <- sign_gender * (d$zF - d$zM) / 2
d$S0 <- (d$zF + d$zM) / 2
d$atypicality <- within_z(d$A0, d$gender)
d$coelevation <- within_z(d$S0, d$gender)
# A increases for relatively feminine/less masculine men, and masculine/less feminine women.
# S is the standardized sum of the two gender-referenced ratings. At fixed A,
# increasing S raises both standardized ratings equally; high S alone need not
# put both ratings above their within-gender means.
# A and S are an invertible rotation of F and M. The linear model is not a novel estimator.
# Raw balance is a sensitivity/proxy only; equal ratings do not establish perceptual ambiguity.
d$cue_balance <- 1 - abs(d$femininity - d$masculinity) /
  (paper_values$scale_max - paper_values$scale_min)
d$cue_intensity <- (d$femininity + d$masculinity) / 2
d$balance_z <- within_z(d$cue_balance, d$gender)
d$intensity_z <- within_z(d$cue_intensity, d$gender)
d$A2 <- d$atypicality^2
d$S2 <- d$coelevation^2
d$AS <- d$atypicality * d$coelevation
scale_constants <- do.call(rbind, lapply(levels(d$gender), function(g) {
  z <- d[d$gender == g, ]
  data.frame(gender = g, n = nrow(z), F_mean = mean(z$femininity), F_sd = sd(z$femininity),
             M_mean = mean(z$masculinity), M_sd = sd(z$masculinity),
             A0_sd = sd(z$A0), S0_sd = sd(z$S0),
             F_M_correlation = cor(z$femininity, z$masculinity),
             A_S_correlation = cor(z$atypicality, z$coelevation),
             both_raw_at_least_3 = sum(z$femininity >= paper_values$scale_midpoint &
                                       z$masculinity >= paper_values$scale_midpoint),
             raw_gap_less_than_quarter = sum(abs(z$femininity - z$masculinity) <
                                             paper_values$raw_balance_tolerance))
}))
stopifnot(max(abs(scale_constants$A_S_correlation)) < 1e-10)
write.csv(scale_constants, file.path(project_dir, "results", "scale_constants.csv"), row.names = FALSE)

## ---- export-datasets
# These files are written immediately before estimation. turnout and official_votes both
# contain the FIXED official counts; turnout_saved is retained only to document the repair.
fixed_csv_path <- file.path(project_dir, "Ambiguity_fixed.csv")
analysis_csv_path <- file.path(project_dir, "Ambiguity_analysis.csv")
write.csv(dat_fixed, fixed_csv_path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
write.csv(d, analysis_csv_path, row.names = FALSE, na = "", fileEncoding = "UTF-8")

# Read back the exported files and check every candidate against the verified crosswalk.
verify_official_votes <- function(x) {
  j <- match(x$id, vote_audit$id)
  stopifnot(!anyNA(j), !anyDuplicated(x$id),
            !anyDuplicated(paste(x$city, x$candidate_number)),
            all(x$city == vote_audit$city[j]),
            all(x$candidate_number == vote_audit$candidate_number[j]),
            all(x$turnout == vote_audit$official_votes[j]),
            all(x$official_votes == vote_audit$official_votes[j]),
            all(x$firstname == vote_audit$official_firstname[j]),
            all(x$lastname == vote_audit$official_lastname[j]))
  invisible(TRUE)
}
fixed_csv <- read.csv(fixed_csv_path, stringsAsFactors = FALSE, na.strings = "",
                      check.names = FALSE, fileEncoding = "UTF-8")
analysis_csv <- read.csv(analysis_csv_path, stringsAsFactors = FALSE, na.strings = "",
                         check.names = FALSE, fileEncoding = "UTF-8")
stopifnot(nrow(fixed_csv) == nrow(dat_fixed), nrow(analysis_csv) == nrow(d),
          identical(names(analysis_csv), names(d)))
verify_official_votes(fixed_csv)
verify_official_votes(analysis_csv)

# The actual analyses use the reloaded analysis CSV. Restore factor levels/reference
# categories that CSV cannot store, so the econometric specifications stay the same.
factor_vars <- names(d)[vapply(d, is.factor, logical(1))]
for (v in factor_vars) {
  analysis_csv[[v]] <- factor(analysis_csv[[v]], levels = levels(d[[v]]), ordered = is.ordered(d[[v]]))
}
rownames(analysis_csv) <- rownames(d)
stopifnot(isTRUE(all.equal(d, analysis_csv, tolerance = 1e-12, check.attributes = FALSE)))
d <- analysis_csv
writeLines(c(
  paste("Full corrected CSV:", nrow(fixed_csv), "candidates; all official vote/name checks passed."),
  paste("Analysis CSV:", nrow(d), "candidates; reloaded and used for estimation."),
  "turnout = corrected official votes; turnout_saved = original corrupted counts.",
  "All appearance/control measurements retained from the original saved data."
), file.path(project_dir, "results", "dataset_export_checks.txt"))

## ---- estimation
# PPML models the conditional mean, rather than assuming equidispersion or log-transforming votes.
# Common complete-case sample for the gender-cue models and attractiveness benchmark.
f_baseline <- turnout ~ gender * atypicality + age_c + party + city
f_beauty <- update(f_baseline, . ~ . + attractiveness)
f_main <- f_beauty
# Online Appendix adjustment for joint cue level, with the same age, attractiveness,
# municipality and party controls as the primary model.
f_joint <- update(f_main, . ~ gender * (atypicality + coelevation) +
                    age_c + party + city + attractiveness)
f_balance <- turnout ~ gender * (balance_z + intensity_z) + age_c + attractiveness +
  party + city
f_surface <- update(f_joint, . ~ . + gender * (A2 + S2 + AS))
f_raw <- turnout ~ gender * (femininity + masculinity) + age_c + attractiveness +
  party + city
fit_ppml <- function(formula, data) {
  m <- glm(formula, family = poisson(link = "log"), data = data,
           na.action = na.fail, control = glm.control(maxit = 100, epsilon = 1e-10))
  if (!m$converged || any(!is.finite(coef(m)[!is.na(coef(m))]))) stop("PPML did not converge.")
  m
}
models <- list(baseline = fit_ppml(f_baseline, d), beauty = fit_ppml(f_beauty, d),
               main = fit_ppml(f_main, d), joint = fit_ppml(f_joint, d),
               balance = fit_ppml(f_balance, d),
               surface = fit_ppml(f_surface, d), raw = fit_ppml(f_raw, d))
# City FE cannot absorb candidate-specific incumbency, recognition, campaign resources,
# or local party strength. A city-by-party FE sensitivity improves within-list comparison.
list_size <- table(d$list_id)
d_list <- droplevels(d[as.integer(list_size[as.character(d$list_id)]) > 1, ])
f_list <- turnout ~ gender * atypicality + age_c + attractiveness +
  list_id
models$within_list <- fit_ppml(f_list, d_list)
top_cut <- unname(quantile(d$turnout, paper_values$trim_quantiles[1]))
d_trim <- droplevels(d[d$turnout <= top_cut, ])
models$trim <- fit_ppml(f_main, d_trim)
top5_cut <- unname(quantile(d$turnout, paper_values$trim_quantiles[2]))
d_trim5 <- droplevels(d[d$turnout <= top5_cut, ])
models$trim5 <- fit_ppml(f_main, d_trim5)
city_size <- table(d$city)
d_informative <- droplevels(d[as.integer(city_size[as.character(d$city)]) > 1, ])
models$informative <- fit_ppml(f_main, d_informative)
models$experience <- fit_ppml(update(f_main, . ~ . + incumbent_councillor + national_mp), d)
models$surface_trim <- fit_ppml(f_surface, d_trim)
models$surface_trim5 <- fit_ppml(f_surface, d_trim5)
sex_data <- lapply(levels(d$gender), function(g) droplevels(d[d$gender == g, ]))
names(sex_data) <- levels(d$gender)
f_split <- turnout ~ atypicality + age_c + attractiveness +
  party + city
models$male <- fit_ppml(f_split, sex_data$Male)
models$female <- fit_ppml(f_split, sex_data$Female)
# Secondary pooled model: party-specific gendered A slopes and intercepts.
# Keep the primary-sample standardization of A. The secondary restriction only
# removes parties without enough candidates in both recorded gender groups.
party_cell_counts <- do.call(rbind, lapply(levels(d$party), function(p) {
  do.call(rbind, lapply(levels(d$gender), function(g) {
    z <- d[d$party == p & d$gender == g, ]
    data.frame(party = p, gender = g, n = nrow(z),
               city_clusters = length(unique(z$city)),
               A_min = if (nrow(z)) min(z$atypicality) else NA_real_,
               A_max = if (nrow(z)) max(z$atypicality) else NA_real_,
               A_sd = if (nrow(z) > 1L) sd(z$atypicality) else NA_real_)
  }))
}))
eligible_party_levels <- levels(d$party)[vapply(levels(d$party), function(p) {
  all(party_cell_counts$n[party_cell_counts$party == p] >= paper_values$support_min_candidates)
}, logical(1))]
party_cell_counts$eligible <- party_cell_counts$party %in% eligible_party_levels
d_party <- droplevels(d[d$party %in% eligible_party_levels, ])
party_model_n <- nrow(d_party)
party_model_dropped_n <- nrow(d) - party_model_n
excluded_party_levels <- setdiff(levels(d$party), eligible_party_levels)
stopifnot(length(eligible_party_levels) >= 2L,
          all(table(d_party$party, d_party$gender) >= paper_values$support_min_candidates),
          identical(d_party$atypicality, d$atypicality[d$party %in% eligible_party_levels]))
f_party <- update(f_main, . ~ . + gender * atypicality * party)
models$party_heterogeneity <- fit_ppml(f_party, d_party)
# Targeted appendix diagnostics retain the same eligible parties and A scaling.
# The trimming threshold is defined once in the primary sample, not within party.
models$party_experience <- fit_ppml(
  update(f_party, . ~ . + incumbent_councillor + national_mp), d_party)
d_party_trim <- droplevels(d_party[d_party$turnout <= top_cut, ])
stopifnot(identical(levels(d_party_trim$party), eligible_party_levels),
          all(table(d_party_trim$party, d_party_trim$gender) > 0L),
          identical(d_party_trim$atypicality, d_party$atypicality[d_party$turnout <= top_cut]))
models$party_trim <- fit_ppml(f_party, d_party_trim)
party_sensitivity_models <- c("party_heterogeneity", "party_experience", "party_trim")
stopifnot(all(vapply(models[party_sensitivity_models],
                    function(m) !anyNA(coef(m)), logical(1))))
write.csv(party_cell_counts, file.path(project_dir, "results", "party_cell_counts.csv"), row.names = FALSE)
model_data <- list(baseline = d, beauty = d, main = d, joint = d, balance = d, surface = d, raw = d,
                   within_list = d_list, trim = d_trim, trim5 = d_trim5, informative = d_informative,
                   experience = d,
                   surface_trim = d_trim, surface_trim5 = d_trim5,
                   male = sex_data$Male, female = sex_data$Female,
                   party_heterogeneity = d_party,
                   party_experience = d_party, party_trim = d_party_trim)
# Every substantive model, including restricted-sample sensitivities, must use official votes.
for (nm in names(models)) {
  verify_official_votes(model_data[[nm]])
  stopifnot(identical(as.numeric(model.response(model.frame(models[[nm]]))),
                      as.numeric(model_data[[nm]]$turnout)))
}

## ---- robust-inference
# Municipality-clustered sandwich covariance, CR1 correction, t reference with G-1 df.
# Poisson score_i = x_i (y_i - mu_i); bread = inverse(sum_i mu_i x_i x_i').
# Keep all estimable columns including FE, so uncertainty reflects their estimation.
robust_vcov <- function(m, cluster = NULL) {
  estimable <- !is.na(coef(m))
  X <- model.matrix(m)[, estimable, drop = FALSE]
  n <- nrow(X); k <- ncol(X)
  bread <- chol2inv(chol(crossprod(X * sqrt(fitted(m)))))
  score <- X * as.numeric(residuals(m, type = "response"))
  if (is.null(cluster)) {
    meat <- crossprod(score) * n / (n - k)
    df <- n - k; G <- n
  } else {
    stopifnot(length(cluster) == n)
    U <- rowsum(score, as.character(cluster), reorder = FALSE)
    G <- nrow(U)
    stopifnot(G > 1, n > k)
    meat <- crossprod(U) * (G / (G - 1)) * ((n - 1) / (n - k))
    df <- G - 1
  }
  V <- bread %*% meat %*% bread
  dimnames(V) <- list(colnames(X), colnames(X))
  attr(V, "df") <- df; attr(V, "clusters") <- G
  V
}
vcovs <- lapply(names(models), function(nm) robust_vcov(models[[nm]], model_data[[nm]]$city))
names(vcovs) <- names(models)
party_model_clusters <- attr(vcovs$party_heterogeneity, "clusters")
V_main_HC1 <- robust_vcov(models$main)
linear_contrast <- function(m, V, weights, label = "") {
  L <- setNames(numeric(ncol(V)), colnames(V))
  stopifnot(all(names(weights) %in% names(L)))
  L[names(weights)] <- weights
  b <- sum(L * coef(m)[names(L)])
  se <- sqrt(max(0, as.numeric(t(L) %*% V %*% L)))
  critical <- qt(paper_values$ci_quantile, attr(V, "df"))
  data.frame(label = label, log_ratio = b, se = se, ratio = exp(b),
             lower = exp(b - critical * se), upper = exp(b + critical * se),
             p = 2 * pt(-abs(b / se), attr(V, "df")))
}
gender_contrast <- function(m, V, variable, g) {
  w <- setNames(1, variable)
  if (g == "Female") {
    candidates <- c(paste0("genderFemale:", variable), paste0(variable, ":genderFemale"))
    term <- candidates[candidates %in% colnames(V)]
    if (length(term)) w <- c(w, setNames(1, term[1]))
  }
  linear_contrast(m, V, w, paste(g, variable))
}
# Party/gender slopes from the one pooled secondary model, using its full
# coefficient covariance rather than independent party regressions.
party_contrast_weights <- function(g, p, model_name = "party_heterogeneity") {
  m <- models[[model_name]]; z <- model_data[[model_name]]
  stopifnot(g %in% levels(z$gender), p %in% eligible_party_levels)
  nd <- z[rep(1L, 2L), ]
  nd$gender <- factor(rep(g, 2L), levels = levels(z$gender))
  nd$party <- factor(rep(p, 2L), levels = levels(z$party))
  nd$atypicality <- c(0, 1)
  X <- model.matrix(delete.response(terms(m)), nd, contrasts.arg = m$contrasts)
  L <- X[2L, ] - X[1L, ]
  stopifnot(identical(names(L), colnames(vcovs[[model_name]])))
  L
}
party_slope_contrast <- function(g, p, model_name = "party_heterogeneity") {
  linear_contrast(models[[model_name]], vcovs[[model_name]],
                  party_contrast_weights(g, p, model_name), paste(p, g, "atypicality"))
}
party_contrasts_for_model <- function(model_name) {
  z_model <- model_data[[model_name]]
  out <- do.call(rbind, lapply(eligible_party_levels, function(p) {
    do.call(rbind, lapply(levels(d_party$gender), function(g) {
      r <- party_slope_contrast(g, p, model_name)
      z <- z_model[z_model$party == p & z_model$gender == g, ]
      r$party <- p; r$gender <- g; r$model <- model_name
      r$n_cell <- nrow(z); r$cell_city_clusters <- length(unique(z$city))
      r$model_n <- nrow(z_model); r$model_clusters <- attr(vcovs[[model_name]], "clusters")
      r$A_min <- min(z$atypicality); r$A_max <- max(z$atypicality)
      r
    }))
  }))
  out$percent <- 100 * (out$ratio - 1)
  out$percent_lower <- 100 * (out$lower - 1)
  out$percent_upper <- 100 * (out$upper - 1)
  out
}
party_contrasts <- party_contrasts_for_model("party_heterogeneity")
party_sensitivity_contrasts <- do.call(rbind, lapply(party_sensitivity_models,
                                                   party_contrasts_for_model))
write.csv(party_contrasts, file.path(project_dir, "results", "party_contrasts.csv"), row.names = FALSE)
write.csv(party_sensitivity_contrasts,
          file.path(project_dir, "results", "party_sensitivity_contrasts.csv"), row.names = FALSE)

# Null: all eligible parties share the same A slope within each gender. Allow
# gender-by-party intercept differences under the null as well as the alternative.
party_reference <- eligible_party_levels[1L]
party_restrictions_for_model <- function(model_name) {
  R <- lapply(levels(d_party$gender), function(g) {
    do.call(rbind, lapply(setdiff(eligible_party_levels, party_reference), function(p) {
      party_contrast_weights(g, p, model_name) -
        party_contrast_weights(g, party_reference, model_name)
    }))
  })
  names(R) <- levels(d_party$gender)
  R
}
party_restrictions <- party_restrictions_for_model("party_heterogeneity")
party_joint_wald <- function(R, label, model_name = "party_heterogeneity") {
  V <- vcovs[[model_name]]
  b <- coef(models[[model_name]])[colnames(V)]
  q <- nrow(R); delta <- as.numeric(R %*% b)
  VR <- R %*% V %*% t(R)
  stopifnot(qr(R)$rank == q, qr(VR)$rank == q)
  W <- as.numeric(t(delta) %*% solve(VR, delta))
  data.frame(test = label, F = W / q, df1 = q, df2 = attr(V, "df"),
             p = pf(W / q, q, attr(V, "df"), lower.tail = FALSE),
             n = nobs(models[[model_name]]), clusters = attr(V, "clusters"),
             eligible_parties = length(eligible_party_levels),
             reference_party = party_reference)
}
party_tests_for_model <- function(model_name) {
  R <- party_restrictions_for_model(model_name)
  rbind(party_joint_wald(do.call(rbind, R), "All genders", model_name),
        party_joint_wald(R$Male, "Men", model_name),
        party_joint_wald(R$Female, "Women", model_name))
}
party_slope_test <- party_tests_for_model("party_heterogeneity")
party_sensitivity_tests <- do.call(rbind, lapply(party_sensitivity_models, function(nm) {
  r <- party_tests_for_model(nm); r$model <- nm; r
}))
write.csv(party_slope_test, file.path(project_dir, "results", "party_slope_test.csv"), row.names = FALSE)
write.csv(party_sensitivity_tests,
          file.path(project_dir, "results", "party_sensitivity_tests.csv"), row.names = FALSE)
extract_effects <- function(nm, variable = "atypicality") {
  do.call(rbind, lapply(levels(d$gender), function(g) {
    res <- gender_contrast(models[[nm]], vcovs[[nm]], variable, g)
    res$model <- nm; res$gender <- g; res$variable <- variable
    res$n <- nobs(models[[nm]]); res$clusters <- attr(vcovs[[nm]], "clusters")
    res
  }))
}
effects <- do.call(rbind, lapply(c("baseline", "beauty", "main", "joint", "within_list", "trim", "trim5", "informative", "experience"), extract_effects))
S_effects <- extract_effects("joint", "coelevation")
balance_effects <- extract_effects("balance", "balance_z")
split_effects <- do.call(rbind, lapply(c("male", "female"), function(nm) {
  r <- linear_contrast(models[[nm]], vcovs[[nm]], c(atypicality = 1))
  r$model <- nm; r$gender <- ifelse(nm == "male", "Male", "Female")
  r$variable <- "atypicality"; r$n <- nobs(models[[nm]])
  r$clusters <- attr(vcovs[[nm]], "clusters"); r
}))
all_effects <- rbind(effects, S_effects, balance_effects, split_effects)
all_effects$percent <- 100 * (all_effects$ratio - 1)
all_effects$percent_lower <- 100 * (all_effects$lower - 1)
all_effects$percent_upper <- 100 * (all_effects$upper - 1)
write.csv(all_effects, file.path(project_dir, "results", "contrasts.csv"), row.names = FALSE)
gender_difference <- linear_contrast(models$main, vcovs$main,
                                     c("genderFemale:atypicality" = 1), "Female minus male A slope")
write.csv(gender_difference, file.path(project_dir, "results", "gender_difference.csv"), row.names = FALSE)
coefficient_table <- do.call(rbind, lapply(names(models), function(nm) {
  b <- coef(models[[nm]])[colnames(vcovs[[nm]])]
  se <- sqrt(diag(vcovs[[nm]]))
  data.frame(model = nm, term = names(b), estimate = b, cluster_se = se,
             df = attr(vcovs[[nm]], "df"), p = 2 * pt(-abs(b / se), attr(vcovs[[nm]], "df")))
}))
write.csv(coefficient_table, file.path(project_dir, "results", "coefficients.csv"), row.names = FALSE)

## ---- attractiveness-checks
# Isolate the attractiveness comparisons from the existing substantive models.
# All specifications retain the same primary sample, cue scaling, and other controls.
paper_values$attractiveness_spline_df <- 3L
attractiveness_reference_coefficients <- lapply(models, coef)
attractiveness_reference_vcovs <- vcovs
attractiveness_reference_data <- d
attractiveness_reference_frame <- model.frame(models$main)
attractiveness_other_controls <- setdiff(
  all.vars(delete.response(terms(models$main))), "attractiveness")
stopifnot(nrow(d) == nobs(models$main),
          all(c("atypicality", "attractiveness", "gender", "city") %in% names(d)),
          all(is.finite(d$atypicality)), all(is.finite(d$attractiveness)),
          identical(as.numeric(model.response(attractiveness_reference_frame)),
                    as.numeric(d$turnout)))

attractiveness_models <- list(
  omitted = fit_ppml(update(f_main, . ~ . - attractiveness), d),
  linear = models$main,
  flexible = fit_ppml(
    update(f_main, . ~ . - attractiveness +
             gender * splines::ns(attractiveness,
                                  df = paper_values$attractiveness_spline_df)), d))
attractiveness_vcovs <- lapply(names(attractiveness_models), function(nm) {
  if (nm == "linear") vcovs$main else robust_vcov(attractiveness_models[[nm]], d$city)
})
names(attractiveness_vcovs) <- names(attractiveness_models)

for (nm in names(attractiveness_models)) {
  m <- attractiveness_models[[nm]]
  mf <- model.frame(m)
  stopifnot(m$converged, !anyNA(coef(m)),
            length(coef(m)) == m$rank,
            nobs(m) == nrow(d),
            identical(rownames(mf), rownames(attractiveness_reference_frame)),
            identical(as.numeric(model.response(mf)), as.numeric(d$turnout)),
            all(attractiveness_other_controls %in% names(mf)),
            all(vapply(attractiveness_other_controls, function(v) {
              identical(mf[[v]], attractiveness_reference_frame[[v]])
            }, logical(1))),
            attr(attractiveness_vcovs[[nm]], "clusters") == length(unique(d$city)),
            identical(colnames(attractiveness_vcovs[[nm]]), names(coef(m))))
}
stopifnot(!("attractiveness" %in% all.vars(formula(attractiveness_models$omitted))),
          identical(attractiveness_models$linear, models$main),
          identical(attractiveness_vcovs$linear, vcovs$main),
          "attractiveness" %in% all.vars(formula(attractiveness_models$flexible)),
          any(grepl("genderFemale:splines::ns|splines::ns.*:genderFemale",
                    names(coef(attractiveness_models$flexible)))))

attractiveness_effects <- do.call(rbind, lapply(names(attractiveness_models), function(nm) {
  do.call(rbind, lapply(levels(d$gender), function(g) {
    r <- gender_contrast(attractiveness_models[[nm]], attractiveness_vcovs[[nm]],
                         "atypicality", g)
    data.frame(model = nm, gender = g,
               log_ratio = r$log_ratio, se = r$se, ratio = r$ratio,
               lower = r$lower, upper = r$upper, p = r$p,
               n = nobs(attractiveness_models[[nm]]),
               cities = attr(attractiveness_vcovs[[nm]], "clusters"))
  }))
}))

# Positive signed balance denotes relatively feminine cues in both gender groups.
# Candidate pairing is preserved for correlations; neither d nor its CSV is modified.
attractiveness_pairs <- data.frame(
  gender = d$gender,
  signed_balance = ifelse(d$gender == "Male", d$atypicality, -d$atypicality),
  attractiveness = d$attractiveness)
attractiveness_correlations <- do.call(rbind, lapply(levels(d$gender), function(g) {
  z <- attractiveness_pairs[attractiveness_pairs$gender == g, ]
  data.frame(gender = g, n = nrow(z),
             pearson = cor(z$signed_balance, z$attractiveness, method = "pearson"),
             spearman = cor(z$signed_balance, z$attractiveness, method = "spearman"))
}))

# Empirical Q-Q data compare marginal distributions, rather than candidate pairs.
# Attractiveness retains its original 1--5 units; signed balance retains its SD units.
attractiveness_qq <- do.call(rbind, lapply(levels(d$gender), function(g) {
  z <- attractiveness_pairs[attractiveness_pairs$gender == g, ]
  p <- ppoints(nrow(z))
  data.frame(gender = g, probability = p,
             signed_balance = quantile(z$signed_balance, p, type = 7, names = FALSE),
             attractiveness = quantile(z$attractiveness, p, type = 7, names = FALSE))
}))
attractiveness_diagnostics <- data.frame(
  model = names(attractiveness_models),
  n = vapply(attractiveness_models, nobs, numeric(1)),
  rank = vapply(attractiveness_models, function(m) m$rank, numeric(1)),
  cities = vapply(attractiveness_vcovs, function(V) attr(V, "clusters"), numeric(1)),
  converged = vapply(attractiveness_models, function(m) m$converged, logical(1)))

stopifnot(identical(d, attractiveness_reference_data),
          identical(lapply(models, coef), attractiveness_reference_coefficients),
          identical(vcovs, attractiveness_reference_vcovs),
          nrow(attractiveness_pairs) == nrow(d),
          sum(attractiveness_correlations$n) == nrow(d),
          nrow(attractiveness_qq) == nrow(d),
          all(attractiveness_qq$probability > 0 & attractiveness_qq$probability < 1),
          all(attractiveness_pairs$attractiveness >= paper_values$scale_min &
                attractiveness_pairs$attractiveness <= paper_values$scale_max))
write.csv(attractiveness_effects,
          file.path(project_dir, "results", "attractiveness_effects.csv"), row.names = FALSE)
write.csv(attractiveness_correlations,
          file.path(project_dir, "results", "attractiveness_correlations.csv"), row.names = FALSE)
write.csv(attractiveness_pairs,
          file.path(project_dir, "results", "attractiveness_pairs.csv"), row.names = FALSE)
write.csv(attractiveness_qq,
          file.path(project_dir, "results", "attractiveness_qq.csv"), row.names = FALSE)
write.csv(attractiveness_diagnostics,
          file.path(project_dir, "results", "attractiveness_diagnostics.csv"), row.names = FALSE)


## ---- diagnostics
# The appendix joint A+S model is an invertible rotation of the raw F/M model.
# The A-only primary model is a restricted, different specification.
rotation_error <- max(abs(fitted(models$joint) - fitted(models$raw)))
stopifnot(rotation_error < 1e-5)
# Separate uniform scaling from the surname-merge error. Constant scaling preserves slope
# contrasts, but replacing misassigned outcomes need not. Audit all 1389 photograph keys.
all_saved_counts_even <- all(dat_source$turnout %% 2 == 0)
official_check <- data.frame(city = "Vantaa", candidate_number = 2, official_votes = 4653,
  saved_votes = dat_source$turnout[dat_source$city == "Vantaa" & dat_source$candidate_number == 2],
  source_url = "https://paatokset.vantaa.fi/ktwebscr/fileshow?docid=1155746&doctype=3&version=1")
write.csv(official_check, file.path(project_dir, "results", "vote_provenance_check.csv"), row.names = FALSE)
d_scaled <- d; d_scaled$turnout <- d$turnout * 2
m_scaled <- fit_ppml(f_main, d_scaled)
V_scaled <- robust_vcov(m_scaled, d_scaled$city)
slope_terms <- setdiff(colnames(vcovs$main), c("(Intercept)", grep("^city", colnames(vcovs$main), value = TRUE)))
scale_slope_error <- max(abs(coef(m_scaled)[slope_terms] - coef(models$main)[slope_terms]))
scale_vcov_error <- max(abs(V_scaled - vcovs$main))
stopifnot(scale_slope_error < 1e-7, scale_vcov_error < 1e-7)
d_legacy <- d; d_legacy$turnout <- d$turnout_saved
m_legacy <- fit_ppml(f_main, d_legacy)
V_legacy <- robust_vcov(m_legacy, d_legacy$city)
legacy_effects <- do.call(rbind, lapply(levels(d$gender), function(g) {
  r <- gender_contrast(m_legacy, V_legacy, "atypicality", g); r$gender <- g; r
}))
write.csv(legacy_effects, file.path(project_dir, "results", "legacy_saved_outcome_contrasts.csv"), row.names = FALSE)
nonlinear_terms <- grep("(^|:)(A2|S2|AS)$", colnames(vcovs$surface), value = TRUE)
bn <- coef(models$surface)[nonlinear_terms]
Vn <- vcovs$surface[nonlinear_terms, nonlinear_terms, drop = FALSE]
nonlinear_wald <- as.numeric(t(bn) %*% solve(Vn, bn))
nonlinear_df <- length(bn)
nonlinear_p <- pf(nonlinear_wald / nonlinear_df, nonlinear_df,
                  attr(vcovs$surface, "df"), lower.tail = FALSE)
nonlinear_test <- function(nm, V = vcovs[[nm]]) {
  terms <- grep("(^|:)(A2|S2|AS)$", colnames(V), value = TRUE)
  b <- coef(models[[nm]])[terms]
  W <- as.numeric(t(b) %*% solve(V[terms, terms, drop = FALSE], b))
  data.frame(model = nm, F = W / length(terms), df1 = length(terms), df2 = attr(V, "df"),
             p = pf(W / length(terms), length(terms), attr(V, "df"), lower.tail = FALSE),
             n = nobs(models[[nm]]))
}
nonlinear_tests <- do.call(rbind, lapply(c("surface", "surface_trim", "surface_trim5"), nonlinear_test))
nonlinear_HC1 <- nonlinear_test("surface", robust_vcov(models$surface))
nonlinear_HC1$model <- "surface_HC1"
nonlinear_tests <- rbind(nonlinear_tests, nonlinear_HC1)
write.csv(nonlinear_tests, file.path(project_dir, "results", "nonlinear_tests.csv"), row.names = FALSE)
diagnostics <- data.frame(model = names(models),
  n = vapply(models, nobs, numeric(1)),
  rank = vapply(models, function(m) m$rank, numeric(1)),
  cities = vapply(vcovs, function(V) attr(V, "clusters"), numeric(1)),
  converged = vapply(models, function(m) m$converged, logical(1)),
  pearson_dispersion = vapply(models, function(m) sum(residuals(m, "pearson")^2) / df.residual(m), numeric(1)),
  max_leverage = vapply(models, function(m) max(hatvalues(m)), numeric(1)))
write.csv(diagnostics, file.path(project_dir, "results", "diagnostics.csv"), row.names = FALSE)
support_summary <- data.frame(gender = levels(d$gender),
  balanced = vapply(sex_data, function(z) sum(abs(z$femininity - z$masculinity) <
                                              paper_values$raw_balance_tolerance), integer(1)),
  F_and_M_at_least_3 = vapply(sex_data, function(z) sum(z$femininity >= paper_values$scale_midpoint &
                                                       z$masculinity >= paper_values$scale_midpoint), integer(1)),
  opposite_cue_dominates = vapply(sex_data, function(z) sum(if (as.character(z$gender[1]) == "Male") z$femininity > z$masculinity else z$masculinity > z$femininity), integer(1)))
write.csv(support_summary, file.path(project_dir, "results", "support_summary.csv"), row.names = FALSE)

## ---- figures
gender_names <- c(Male = "Men", Female = "Women")
palette <- c(Male = "#245B78", Female = "#A55145")
# Shared styling: square panels, a visible border, and no background grid.
square_panel_theme <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(panel.grid = element_blank(),
          panel.border = element_rect(fill = NA, color = "grey25", linewidth = .65),
          aspect.ratio = 1,
          strip.text = element_text(face = "bold", size = base_size + 1),
          axis.text = element_text(color = "grey20"),
          plot.margin = margin(8, 12, 8, 8))
}
model_names <- c(baseline = "Age, party, municipality", beauty = "+ Attractiveness",
                 main = "Baseline controls", joint = "+ Joint cue level",
                 within_list = "Municipality x party fixed effects",
                 experience = "+ Incumbency and parliamentary office",
                 trim = "Excluding top 1% of votes", trim5 = "Excluding top 5% of votes")
plot_effects <- effects[effects$model %in% names(model_names), ]
plot_effects$model_label <- factor(model_names[plot_effects$model], levels = rev(unname(model_names)))
plot_effects$gender_label <- factor(gender_names[plot_effects$gender], levels = c("Men", "Women"))
oa_estimation_plot <- ggplot(plot_effects, aes(x = 100 * (ratio - 1), y = model_label, color = gender)) +
  geom_vline(xintercept = 0, color = "grey65", linetype = 2, linewidth = .45) +
  geom_errorbar(aes(xmin = 100 * (lower - 1), xmax = 100 * (upper - 1)),
                orientation = "y", width = .13, linewidth = .65) +
  geom_point(size = 2.5) +
  facet_wrap(~ gender_label, nrow = 1) +
  scale_color_manual(values = palette, guide = "none") +
  labs(x = "Association with expected votes for +1 SD in atypical cue contrast (%)", y = NULL) +
  square_panel_theme()
main_plot_effects <- effects[effects$model == "main", ]
cue_names <- c(atypicality = "Atypical contrast (A)", coelevation = "Joint cue level (S)")
main_plot_effects$cue_label <- factor(cue_names[main_plot_effects$variable],
                                      levels = rev(unname(cue_names)))
main_plot_effects$gender_label <- factor(gender_names[main_plot_effects$gender],
                                         levels = c("Men", "Women"))
main_plot <- ggplot(main_plot_effects,
                    aes(x = 100 * (ratio - 1), y = cue_label, color = gender)) +
  geom_vline(xintercept = 0, color = "grey65", linetype = 2, linewidth = .45) +
  geom_errorbar(aes(xmin = 100 * (lower - 1), xmax = 100 * (upper - 1)),
                orientation = "y", width = .10, linewidth = .65) +
  geom_point(size = 2.5) +
  facet_wrap(~ gender_label, nrow = 1) +
  scale_color_manual(values = palette, guide = "none") +
  labs(x = "Association with expected votes for +1 within-gender SD (%)", y = NULL) +
  square_panel_theme()
ggsave(file.path(project_dir, "figures", "estimation.pdf"), main_plot, width = 8, height = 4.5)
ggsave(file.path(project_dir, "figures", "estimation.png"), main_plot, width = 8, height = 4.5, dpi = 300)
ggsave(file.path(project_dir, "figures", "oa_estimation.pdf"), oa_estimation_plot, width = 8.8, height = 5.2)
ggsave(file.path(project_dir, "figures", "oa_estimation.png"), oa_estimation_plot, width = 8.8, height = 5.2, dpi = 300)

## ---- prediction-curves
# PPML predictions are expected vote COUNTS. Dividing each contextual prediction by the
# verified municipal valid-vote total yields its predicted ballot share, not election probability.
# At each A, average over observed controls/municipalities/parties WITHIN gender.
# A-only models use one-dimensional support and do not condition on joint cue level.
# Only the appendix joint model holds S=0 and checks two-dimensional A/S support.
# Male and female baselines reflect different compositions.
predict_standardized <- function(g, model_name = "main") {
  z <- d[d$gender == g, ]
  range_A <- quantile(z$atypicality, paper_values$prediction_quantiles, names = FALSE)
  grid_A <- sort(unique(c(seq(range_A[1], range_A[2],
                              length.out = paper_values$prediction_grid_points),
                          paper_values$zero_cue)))
  m <- models[[model_name]]; V <- vcovs[[model_name]]
  critical <- qt(paper_values$ci_quantile, attr(V, "df"))
  b <- coef(m)[colnames(V)]
  adjusts_joint_level <- "coelevation" %in% names(b)
  do.call(rbind, lapply(grid_A, function(a) {
    n_near <- if (adjusts_joint_level) {
      sum((z$atypicality - a)^2 +
            (z$coelevation - paper_values$zero_cue)^2 <= paper_values$support_radius^2)
    } else {
      sum(abs(z$atypicality - a) <= paper_values$support_radius)
    }
    if (n_near < paper_values$support_min_candidates) return(NULL)
    nd <- z; nd$atypicality <- a
    if (adjusts_joint_level) nd$coelevation <- paper_values$zero_cue
    X <- model.matrix(delete.response(terms(m)), nd, contrasts.arg = m$contrasts)
    X <- X[, names(b), drop = FALSE]
    mu <- as.numeric(exp(X %*% b))
    individual_share <- mu / z$city_valid_votes
    # A mean model need not be globally bounded; verify displayed contextual probabilities.
    stopifnot(all(individual_share >= 0 & individual_share <= 1))
    votes <- mean(mu)
    grad_log_votes <- colMeans(X * mu) / votes
    se_log_votes <- sqrt(as.numeric(t(grad_log_votes) %*% V %*% grad_log_votes))
    share <- mean(individual_share)
    grad_logit_share <- colMeans(X * individual_share) / (share * (1 - share))
    se_logit_share <- sqrt(as.numeric(t(grad_logit_share) %*% V %*% grad_logit_share))
    contrast <- gender_contrast(m, V, "atypicality", g)
    se_log_index <- abs(a) * contrast$se
    data.frame(model = model_name, gender = g, atypicality = a, n_local = n_near,
      support_dimensions = if (adjusts_joint_level) 2L else 1L,
      joint_level_fixed = if (adjusts_joint_level) paper_values$zero_cue else NA_real_,
      expected_votes = votes,
      votes_lower = exp(log(votes) - critical * se_log_votes),
      votes_upper = exp(log(votes) + critical * se_log_votes),
      ballot_probability = share,
      probability_lower = plogis(qlogis(share) - critical * se_logit_share),
      probability_upper = plogis(qlogis(share) + critical * se_logit_share),
      relative_vote_index = 100 * exp(a * contrast$log_ratio),
      index_lower = 100 * exp(a * contrast$log_ratio - critical * se_log_index),
      index_upper = 100 * exp(a * contrast$log_ratio + critical * se_log_index))
  }))
}
prediction_curves <- do.call(rbind, lapply(c("main", "experience", "joint"), function(nm)
  do.call(rbind, lapply(levels(d$gender), predict_standardized, model_name = nm))))
prediction_curves$gender <- factor(prediction_curves$gender, levels = c("Male", "Female"))
prediction_curves$model_label <- factor(prediction_curves$model,
  levels = c("main", "experience", "joint"),
  labels = c("Baseline controls", "+ Incumbency and parliamentary office", "+ Joint cue level"))
write.csv(prediction_curves, file.path(project_dir, "results", "prediction_curves.csv"), row.names = FALSE)
main_prediction_curves <- prediction_curves[prediction_curves$model == "main", ]
office_prediction_curves <- droplevels(prediction_curves[prediction_curves$model %in% c("main", "experience"), ])
joint_prediction_curves <- droplevels(prediction_curves[prediction_curves$model %in% c("main", "joint"), ])
write.csv(main_prediction_curves, file.path(project_dir, "results", "main_prediction_curves.csv"), row.names = FALSE)
write.csv(joint_prediction_curves, file.path(project_dir, "results", "oa_joint_prediction_curves.csv"), row.names = FALSE)
plot_prediction <- function(outcome, lower, upper, ylabel,
                            curves = main_prediction_curves, show_model_legend = FALSE) {
  panel_labels <- gender_names
  p <- ggplot(curves, aes(x = atypicality, y = .data[[outcome]], color = gender, fill = gender,
                          group = interaction(gender, model))) +
    geom_ribbon(aes(ymin = .data[[lower]], ymax = .data[[upper]]), alpha = .10, linewidth = 0, linetype = 0) +
    geom_line(aes(linetype = model_label), linewidth = .9) +
    facet_wrap(~ gender, labeller = as_labeller(panel_labels), nrow = 1) +
    scale_color_manual(values = palette, guide = "none") +
    scale_fill_manual(values = palette, guide = "none") +
    labs(x = "Gender-atypical cue contrast (within-gender SD)", y = ylabel, linetype = NULL) +
    square_panel_theme(12)
  if (show_model_legend) {
    line_labels <- unique(as.character(curves$model_label))
    line_values <- setNames(c("solid", "dashed", "dotdash")[seq_along(line_labels)], line_labels)
    p <- p + scale_linetype_manual(values = line_values) +
      theme(legend.position = "bottom", legend.text = element_text(size = 10))
  } else {
    p <- p + scale_linetype_manual(values = "solid", guide = "none") +
      theme(legend.position = "none")
  }
  p
}
# The main figures use independent ggplot panels and aligned rug strips. Observed
# extremes therefore cannot train the fitted panel's x scale or distort its geometry.
# Appendix figures retain the existing faceted plotting function above.
main_cue_limits <- function(curves) {
  radius <- ceiling((max(abs(curves$atypicality)) + .1) / .25) * .25
  c(-radius, radius)
}
main_panel_parts <- function(curves, observations, title, x_limits, y_limits,
                             show_ci = FALSE, base_size = 11, two_rug_rows = FALSE) {
  curve <- ggplot(curves, aes(atypicality, expected_votes, color = gender,
                              group = gender))
  if (show_ci) {
    curve <- curve + geom_ribbon(aes(ymin = votes_lower, ymax = votes_upper,
                                    fill = gender), alpha = .13, linewidth = 0,
                                show.legend = FALSE)
  }
  curve <- curve + geom_line(linetype = "solid", linewidth = .8, show.legend = FALSE) +
    scale_color_manual(values = palette, guide = "none") +
    scale_fill_manual(values = palette, guide = "none") +
    scale_x_continuous(breaks = c(-1, 0, 1)) +
    coord_cartesian(xlim = x_limits, ylim = y_limits, expand = FALSE) +
    labs(title = title, x = NULL, y = NULL) +
    square_panel_theme(base_size) +
    theme(plot.title = element_text(hjust = .5, face = "bold", size = base_size,
                                    lineheight = 1.1, margin = margin(b = 7)),
          axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          plot.margin = margin(7, 7, 6, 5))
  # Filter the displayed rug only; the model, predictions, and reported N use all
  # eligible candidates. The caption identifies this central-window display.
  rug_data <- observations[observations$atypicality >= x_limits[1] &
                             observations$atypicality <= x_limits[2], ]
  rug_data$rug_y <- if (two_rug_rows) ifelse(rug_data$gender == "Male", .73, .27) else .5
  rug <- ggplot(rug_data, aes(atypicality, color = gender)) +
    geom_segment(aes(xend = atypicality, y = rug_y - .17, yend = rug_y + .17),
                 alpha = .42, linewidth = .25, show.legend = FALSE) +
    scale_color_manual(values = palette, guide = "none") +
    scale_x_continuous(breaks = c(-1, 0, 1)) +
    scale_y_continuous(breaks = if (two_rug_rows) c(.27, .73) else NULL,
                       labels = if (two_rug_rows) c("W", "M") else NULL) +
    coord_cartesian(xlim = x_limits, ylim = c(0, 1), expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = base_size, base_family = "sans") +
    theme(panel.grid = element_blank(), axis.title = element_blank(),
          axis.text.x = element_text(color = "grey20", size = base_size - 1),
          axis.text.y = element_text(color = "grey40", size = 7),
          axis.ticks = element_blank(), plot.margin = margin(0, 7, 0, 5))
  list(curve = ggplotGrob(curve), rug = ggplotGrob(rug))
}
assemble_main_panels <- function(parts, ncol, rug_height = .36, base_size = 11,
                                 show_gender_key = FALSE) {
  # Matching gtable widths aligns every rug exactly with its fitted panel, and
  # aligns candidate-count titles and vote axes across the panel grid.
  common_widths <- do.call(grid::unit.pmax, lapply(parts, function(z) z$curve$widths))
  common_widths <- do.call(grid::unit.pmax,
                           c(list(common_widths), lapply(parts, function(z) z$rug$widths)))
  panels <- lapply(parts, function(z) {
    z$curve$widths <- common_widths; z$rug$widths <- common_widths
    gridExtra::arrangeGrob(z$curve, z$rug, ncol = 1,
                           heights = grid::unit.c(grid::unit(1, "null"),
                                                  grid::unit(rug_height, "in")))
  })
  if (length(panels) %% ncol) {
    panels <- c(panels, rep(list(grid::nullGrob()), ncol - length(panels) %% ncol))
  }
  cue_label <- grid::textGrob("Gender-atypical cue contrast (within-gender SD)",
                              gp = grid::gpar(fontfamily = "sans", fontsize = base_size))
  bottom <- if (show_gender_key) {
    key <- grid::grobTree(
      grid::segmentsGrob(x0 = .37, x1 = .41, y0 = .5, y1 = .5,
                         gp = grid::gpar(col = palette["Male"], lwd = 2)),
      grid::textGrob("Men", x = .42, just = "left",
                      gp = grid::gpar(fontfamily = "sans", fontsize = base_size - 1)),
      grid::segmentsGrob(x0 = .53, x1 = .57, y0 = .5, y1 = .5,
                         gp = grid::gpar(col = palette["Female"], lwd = 2)),
      grid::textGrob("Women", x = .58, just = "left",
                      gp = grid::gpar(fontfamily = "sans", fontsize = base_size - 1)))
    gridExtra::arrangeGrob(cue_label, key, ncol = 1,
                           heights = grid::unit(c(.28, .28), "in"))
  } else cue_label
  gridExtra::arrangeGrob(grobs = panels, ncol = ncol,
                         left = grid::textGrob("Predicted expected personal votes", rot = 90,
                           gp = grid::gpar(fontfamily = "sans", fontsize = base_size)),
                         bottom = bottom, padding = grid::unit(.12, "in"))
}
main_plot_x_limits <- main_cue_limits(main_prediction_curves)
main_plot_y_limits <- range(main_prediction_curves[, c("votes_lower", "votes_upper")])
main_plot_y_limits <- main_plot_y_limits + c(-1, 1) * diff(main_plot_y_limits) * .05
main_plot_parts <- lapply(levels(d$gender), function(g) {
  curves <- main_prediction_curves[main_prediction_curves$gender == g, ]
  curves$support_segment <- 1L
  observations <- d[d$gender == g, ]
  title <- paste0(gender_names[g], " (N = ", format(nrow(observations), big.mark = ",",
                                                    trim = TRUE), ")")
  main_panel_parts(curves, observations, title, main_plot_x_limits, main_plot_y_limits,
                   show_ci = TRUE, base_size = 12)
})
predicted_votes_plot <- assemble_main_panels(main_plot_parts, ncol = 2, base_size = 12)
oa_baseline_share_plot <- plot_prediction("ballot_probability", "probability_lower", "probability_upper",
                                         "Average predicted share\nof municipal valid votes") +
  scale_y_continuous(labels = scales::label_percent(accuracy = .1))
relative_votes_plot <- plot_prediction("relative_vote_index", "index_lower", "index_upper", "Expected-vote index (A = 0: 100)") +
  geom_hline(yintercept = 100, linewidth = .35, color = "grey65")
oa_predicted_votes_plot <- plot_prediction("expected_votes", "votes_lower", "votes_upper",
                                          "Predicted expected personal votes",
                                          office_prediction_curves, show_model_legend = TRUE)
oa_predicted_probability_plot <- plot_prediction("ballot_probability", "probability_lower", "probability_upper",
                                                "Average predicted share\nof municipal valid votes",
                                                office_prediction_curves, show_model_legend = TRUE) +
  scale_y_continuous(labels = scales::label_percent(accuracy = .1))
oa_joint_predicted_votes_plot <- plot_prediction("expected_votes", "votes_lower", "votes_upper",
                                                "Predicted expected personal votes",
                                                joint_prediction_curves, show_model_legend = TRUE)
oa_joint_predicted_probability_plot <- plot_prediction("ballot_probability", "probability_lower", "probability_upper",
                                                      "Average predicted share\nof municipal valid votes",
                                                      joint_prediction_curves, show_model_legend = TRUE) +
  scale_y_continuous(labels = scales::label_percent(accuracy = .1))
for (nm in c("predicted_votes", "oa_baseline_predicted_vote_share", "relative_predicted_votes",
             "oa_predicted_votes", "oa_predicted_vote_probability",
             "oa_joint_predicted_votes", "oa_joint_predicted_vote_probability")) {
  p <- switch(nm, predicted_votes = predicted_votes_plot,
              oa_baseline_predicted_vote_share = oa_baseline_share_plot, relative_predicted_votes = relative_votes_plot,
              oa_predicted_votes = oa_predicted_votes_plot,
              oa_predicted_vote_probability = oa_predicted_probability_plot,
              oa_joint_predicted_votes = oa_joint_predicted_votes_plot,
              oa_joint_predicted_vote_probability = oa_joint_predicted_probability_plot)
  figure_height <- if (nm == "predicted_votes") 4.4 else
    if (startsWith(nm, "oa_") && nm != "oa_baseline_predicted_vote_share") 4.8 else 4.4
  ggsave(file.path(project_dir, "figures", paste0(nm, ".pdf")), p, width = 7.6,
         height = figure_height, bg = if (nm == "predicted_votes") "white" else NULL)
  ggsave(file.path(project_dir, "figures", paste0(nm, ".png")), p, width = 7.6,
         height = figure_height, dpi = 300, bg = if (nm == "predicted_votes") "white" else NULL)
}

## ---- party-prediction-curves
# Candidate party is an observed organizational context, not measured voter ideology.
# These names follow the official English 2017 election results. The secondary model
# uses the eligible parties defined in estimation; all primary data remain unchanged.
party_names <- c(KD = "Christian Democrats", KESK = "Centre Party of Finland",
                 KOK = "National Coalition Party", PIR = "Pirate Party", PS = "Finns Party",
                 RKP = "Swedish People's Party", SDP = "Social Democratic Party",
                 SKP = "Communist Party of Finland", VAS = "Left Alliance", VIHR = "Green League")
stopifnot(all(levels(d$party) %in% names(party_names)))
predict_party_standardized <- function(party_code, g) {
  z <- d_party[as.character(d_party$party) == party_code & d_party$gender == g, ]
  stopifnot(nrow(z) >= paper_values$support_min_candidates)
  a_range <- unname(quantile(z$atypicality, paper_values$prediction_quantiles))
  a_grid <- sort(unique(c(seq(a_range[1], a_range[2],
                            length.out = paper_values$prediction_grid_points),
                        if (paper_values$zero_cue >= a_range[1] &&
                            paper_values$zero_cue <= a_range[2]) paper_values$zero_cue)))
  m <- models$party_heterogeneity; V <- vcovs$party_heterogeneity
  b <- coef(m)[colnames(V)]
  critical <- qt(paper_values$ci_quantile, attr(V, "df"))
  out <- do.call(rbind, lapply(a_grid, function(a) {
    n_near <- sum(abs(z$atypicality - a) <= paper_values$support_radius)
    nd <- z; nd$atypicality <- a
    X <- model.matrix(delete.response(terms(m)), nd, contrasts.arg = m$contrasts,
                      xlev = m$xlevels)[, colnames(V), drop = FALSE]
    mu <- as.numeric(exp(X %*% b))
    stopifnot(all(is.finite(mu)))
    votes <- mean(mu)
    gradient <- colMeans(X * mu) / votes
    se <- sqrt(max(0, as.numeric(t(gradient) %*% V %*% gradient)))
    data.frame(party = party_code, gender = g, atypicality = a,
               n = nrow(z), n_local = n_near,
               supported = n_near >= paper_values$support_min_candidates,
               expected_votes = votes,
               votes_lower = exp(log(votes) - critical * se),
               votes_upper = exp(log(votes) + critical * se))
  }))
  # Support flags describe local observation density; the parametric model predicts
  # throughout the group's observed central range, including sparse locations.
  out$support_segment <- cumsum(c(TRUE, diff(out$supported) != 0))
  out
}
party_prediction_curves <- do.call(rbind, lapply(eligible_party_levels, function(p)
  do.call(rbind, lapply(levels(d$gender), function(g) predict_party_standardized(p, g)))))
party_prediction_curves$party <- factor(party_prediction_curves$party, levels = eligible_party_levels)
party_prediction_curves$gender <- factor(party_prediction_curves$gender, levels = levels(d$gender))
write.csv(party_prediction_curves, file.path(project_dir, "results", "party_prediction_curves.csv"),
          row.names = FALSE)
party_plot_n <- with(d_party, table(party, gender))
party_plot_x_limits <- main_cue_limits(party_prediction_curves)
party_plot_parts <- lapply(eligible_party_levels, function(p) {
  curves <- party_prediction_curves[party_prediction_curves$party == p, ]
  observations <- d_party[as.character(d_party$party) == p, ]
  title <- paste0(party_names[p], "\n(", p, ")  Men ", party_plot_n[p, "Male"],
                   ", Women ", party_plot_n[p, "Female"])
  main_panel_parts(curves, observations, title, party_plot_x_limits,
                   y_limits = c(0, max(curves$expected_votes) * 1.08),
                   show_ci = FALSE, base_size = 10, two_rug_rows = TRUE)
})
party_votes_plot <- assemble_main_panels(party_plot_parts, ncol = 3, rug_height = .42,
                                        base_size = 11, show_gender_key = TRUE)
ggsave(file.path(project_dir, "figures", "party_predicted_votes.pdf"), party_votes_plot,
       width = 8.8, height = 10.7, bg = "white")
ggsave(file.path(project_dir, "figures", "party_predicted_votes.png"), party_votes_plot,
       width = 8.8, height = 10.7, dpi = 300, bg = "white")

## ---- support-figures
support_plot <- ggplot(d, aes(femininity, masculinity, color = gender)) +
  geom_abline(slope = 1, intercept = 0, color = "grey55", linetype = 2, linewidth = .4) +
  geom_point(alpha = .32, size = .8) +
  facet_wrap(~ gender, labeller = as_labeller(gender_names)) +
  coord_equal(xlim = c(paper_values$scale_min, paper_values$scale_max),
               ylim = c(paper_values$scale_min, paper_values$scale_max)) +
  scale_color_manual(values = palette, guide = "none") +
  labs(x = paste0("Perceived femininity (", paper_values$scale_min, "-", paper_values$scale_max, ")"),
       y = paste0("Perceived masculinity (", paper_values$scale_min, "-", paper_values$scale_max, ")")) +
  square_panel_theme()
ggsave(file.path(project_dir, "figures", "cue_support.pdf"), support_plot, width = 6.8, height = 4.0)
ggsave(file.path(project_dir, "figures", "cue_support.png"), support_plot, width = 6.8, height = 4.0, dpi = 300)
# Online Appendix quadratic surface: show only nearby empirical support under the shared
# local-count/radius settings. Predictions relative to group mean cues at the SAME
# reference controls; ratios cancel nuisance FE. This is a model-based association surface.
surface_predictions <- do.call(rbind, lapply(levels(d$gender), function(g) {
  z <- d[d$gender == g, ]
  grid <- expand.grid(atypicality = seq(paper_values$surface_axis_limits[1],
                                       paper_values$surface_axis_limits[2],
                                       length.out = paper_values$surface_grid_points),
                      coelevation = seq(paper_values$surface_axis_limits[1],
                                        paper_values$surface_axis_limits[2],
                                        length.out = paper_values$surface_grid_points))
  counts <- vapply(seq_len(nrow(grid)), function(j) sum(
    (z$atypicality - grid$atypicality[j])^2 +
      (z$coelevation - grid$coelevation[j])^2 <= paper_values$support_radius^2), integer(1))
  nd <- z[rep(1, nrow(grid)), ]
  nd$atypicality <- grid$atypicality; nd$coelevation <- grid$coelevation
  nd$A2 <- nd$atypicality^2; nd$S2 <- nd$coelevation^2; nd$AS <- nd$atypicality * nd$coelevation
  ref <- nd; ref$atypicality <- ref$coelevation <- paper_values$zero_cue
  ref$A2 <- ref$atypicality^2; ref$S2 <- ref$coelevation^2
  ref$AS <- ref$atypicality * ref$coelevation
  X <- model.matrix(delete.response(terms(models$surface)), nd, contrasts.arg = models$surface$contrasts)
  X0 <- model.matrix(delete.response(terms(models$surface)), ref, contrasts.arg = models$surface$contrasts)
  L <- (X - X0)[, colnames(vcovs$surface), drop = FALSE]
  b <- as.numeric(L %*% coef(models$surface)[colnames(vcovs$surface)])
  se <- sqrt(pmax(0, rowSums((L %*% vcovs$surface) * L)))
  critical <- qt(paper_values$ci_quantile, attr(vcovs$surface, "df"))
  data.frame(grid, gender = g, n_local = counts, supported = counts >= paper_values$support_min_candidates,
             ratio = exp(b), lower = exp(b - critical * se), upper = exp(b + critical * se))
}))
write.csv(surface_predictions, file.path(project_dir, "results", "surface_predictions.csv"), row.names = FALSE)
surface_predictions$gender <- factor(surface_predictions$gender, levels = c("Male", "Female"))
surface_plot <- ggplot(surface_predictions[surface_predictions$supported, ],
                       aes(atypicality, coelevation, fill = log(ratio))) +
  geom_raster() + facet_wrap(~ gender, labeller = as_labeller(gender_names)) +
  scale_fill_gradient2(low = "#255B78", mid = "#FAFAF5", high = "#A55145", midpoint = 0,
                       name = "Log vote ratio") +
  coord_equal() + labs(x = "Appearance atypicality (within-gender SD)",
                       y = "Joint cue level (within-gender SD)") +
  square_panel_theme()
ggsave(file.path(project_dir, "figures", "response_surface.pdf"), surface_plot, width = 7.8, height = 4.4)
ggsave(file.path(project_dir, "figures", "response_surface.png"), surface_plot, width = 7.8, height = 4.4, dpi = 300)

## ---- attractiveness-figures
# Preserve each candidate's two ratings to show their actual relationship.
# The gender panels use the same scales and square geometry.
attractiveness_panel_labels <- setNames(vapply(levels(d$gender), function(g) {
  paste0(gender_names[g], " (N = ", sum(d$gender == g), ")")
}, character(1)), levels(d$gender))
attractiveness_x_limits <- main_cue_limits(data.frame(
  atypicality = attractiveness_pairs$signed_balance))
attractiveness_candidate_plot <- ggplot(attractiveness_pairs,
  aes(signed_balance, attractiveness, color = gender, group = gender)) +
  geom_point(alpha = .22, size = .8, show.legend = FALSE) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
    linewidth = .8, show.legend = FALSE) +
  facet_wrap(~ gender, ncol = 2,
    labeller = as_labeller(attractiveness_panel_labels)) +
  scale_color_manual(values = palette, guide = "none") +
  scale_x_continuous(breaks = c(-2, 0, 2),
    labels = c("-2\nMore masculine", "0", "2\nMore feminine")) +
  scale_y_continuous(breaks = seq(paper_values$scale_min, paper_values$scale_max)) +
  coord_cartesian(xlim = attractiveness_x_limits,
    ylim = c(paper_values$scale_min, paper_values$scale_max), expand = FALSE) +
  labs(x = "Gender cue balance (within-gender SD)", y = "Attractiveness (mean rating)") +
  square_panel_theme(11) +
  theme(axis.text.x = element_text(size = 9, lineheight = 1.05),
        strip.text = element_text(size = 10),
        panel.spacing = grid::unit(.7, "lines"),
        plot.margin = margin(8, 9, 8, 8))
ggsave(file.path(project_dir, "figures", "attractiveness_cue_pairs.pdf"),
       attractiveness_candidate_plot, width = 7.6, height = 4.4, bg = "white")
ggsave(file.path(project_dir, "figures", "attractiveness_cue_pairs.png"),
       attractiveness_candidate_plot, width = 7.6, height = 4.4, dpi = 300, bg = "white")

## ---- paper-values
# Values consumed by the Rnw; they update automatically when the analysis is rerun.
primary <- effects[effects$model == "main", ]
primary_interval_description <- function(g) {
  r <- primary[primary$gender == g, ]
  stopifnot(nrow(r) == 1L)
  if (r$lower > 1) return("lies entirely above zero")
  if (r$upper < 1) return("lies entirely below zero")
  "includes zero"
}
primary_interval_text <- paste0(
  "The interval for men ", primary_interval_description("Male"), ". ",
  "The interval for women ", primary_interval_description("Female"), ".")
primary_evidence_text <- if (any(primary$lower > 1)) {
  paste0(
    if (primary$lower[primary$gender == "Male"] > 1)
      "The estimated appearance-associated advantage indicates an unequal distribution of expected personal votes within male candidacies."
    else
      "The estimated appearance-associated advantage indicates an unequal distribution of expected personal votes within female candidacies.",
    " These estimates do not support a uniform electoral penalty for a gender-atypical cue balance.")
} else if (all(primary$upper < 1)) {
  "Both estimates locate an appearance-associated disadvantage in the distribution of expected personal votes within gender categories. They are consistent with fewer expected personal votes for a more gender-atypical cue balance."
} else {
  "The estimates do not establish a common direction of association across gender groups. This uncertainty concerns the appearance-associated differences in expected personal votes. It does not establish an absence of inequality within either gender category."
}
# Interpret the primary contrasts from their current confidence intervals, so the
# manuscript's substantive statements change with the estimates as well as its numbers.
primary_profile_description <- function(g) {
  r <- primary[primary$gender == g, ]
  stopifnot(nrow(r) == 1L)
  group <- if (g == "Male") "men" else "women"
  cue <- if (g == "Male") "feminine-relative-to-masculine" else "masculine-relative-to-feminine"
  if (r$lower > 1 || r$upper < 1) {
    direction <- if (r$lower > 1) "greater" else "lower"
    paste0("Among ", group, ", the baseline model associates ", direction,
           " expected personal votes with more ", cue,
           " cue profiles at the same values of the observed controls. ",
           "This association describes an unequal distribution of personal votes within this gender category.")
  } else {
    paste0("For ", group, ", the confidence interval accommodates both advantages and disadvantages ",
           "in expected personal votes. The cue association remains imprecisely estimated. ",
           "This uncertainty does not establish an absence of appearance-associated inequality within this gender category.")
  }
}
primary_inequality_text <- paste(primary_profile_description("Male"),
                                 primary_profile_description("Female"))
male_primary <- primary[primary$gender == "Male", ]
female_primary <- primary[primary$gender == "Female", ]
primary_abstract_text <- if (male_primary$lower > 1 && female_primary$lower <= 1 && female_primary$upper >= 1) {
  "The baseline pattern is consistent with an unequal distribution of expected personal votes within male candidacies, with an appearance-associated advantage for more gender-atypical cue profiles. The female cue association remains imprecisely estimated. Its uncertainty does not establish an absence of inequality within female candidacies."
} else {
  primary_evidence_text
}
primary_theory_abstract_text <- if (male_primary$lower > 1) {
  "Warmth and cooperative leadership provide a possible interpretation of this appearance-associated inequality within male candidacies."
} else ""
# Compact abstract statements and submission highlights follow the current estimates.
primary_abstract_short_male <- if (male_primary$lower > 1) {
  "Appearance-associated vote inequality favors more gender-atypical male profiles."
} else if (male_primary$upper < 1) {
  "Appearance-associated vote inequality disadvantages more gender-atypical male profiles."
} else {
  "The male appearance association remains uncertain, without establishing electoral equality."
}
primary_abstract_short_female <- if (female_primary$lower > 1) {
  "Appearance-associated vote inequality favors more gender-atypical female profiles."
} else if (female_primary$upper < 1) {
  "Appearance-associated vote inequality disadvantages more gender-atypical female profiles."
} else {
  "The female estimate remains uncertain, without establishing electoral equality."
}
primary_abstract_short_text <- paste(primary_abstract_short_male, primary_abstract_short_female)
primary_theory_text <- if (male_primary$lower > 1) {
  paste0("The positive baseline association locates an appearance-associated advantage in the distribution ",
         "of expected personal votes within male candidacies. A leadership fit account offers a possible ",
         "interpretation of this inequality. A more feminine-relative-to-masculine cue balance could ",
         "suggest warmth or reduced dominance that some voters value.")
} else {
  "The baseline association within male candidacies does not establish the advantage for a more feminine-relative-to-masculine cue balance predicted by a cooperative leadership fit account. It therefore leaves this proposed interpretation of the distribution of personal votes unconfirmed."
}
fmt <- function(x, digits = 1) formatC(x, digits = digits, format = "f")

# Published survey-design counts and a constructed illustration of Equation 1.
# The illustration changes one standardized cue and leaves the other at its group mean.
paper_values$photo_pool_min <- 10000L
paper_values$candidate_universe <- 33618L
paper_values$photos_selected <- 1500L
paper_values$n_invited <- 26500L
paper_values$recognition_exclusions <- 4L
cue_example_A <- setNames(1 / (2 * scale_constants$A0_sd), scale_constants$gender)
paper_values$cue_example_A <- cue_example_A
format_effect_text <- function(r) {
  stopifnot(nrow(r) == 1L)
  paste0(fmt(100 * (r$ratio - 1)), "\\% (", fmt(paper_values$confidence_pct, 0), "\\% CI ",
         fmt(100 * (r$lower - 1)), "\\% to ", fmt(100 * (r$upper - 1)), "\\%)")
}
effect_text <- function(g, nm = "main") {
  r <- effects[effects$gender == g & effects$model == nm, ]
  format_effect_text(r)
}
s_effect_text <- function(g) format_effect_text(S_effects[S_effects$gender == g, ])
effect_text_balance <- function(g) format_effect_text(balance_effects[balance_effects$gender == g, ])
party_effect_text <- function(p, g, nm = "party_heterogeneity") {
  r <- party_sensitivity_contrasts
  format_effect_text(r[r$party == p & r$gender == g & r$model == nm, ])
}
party_point_text <- function(p, g, nm = "party_heterogeneity") {
  r <- party_sensitivity_contrasts
  r <- r[r$party == p & r$gender == g & r$model == nm, ]
  stopifnot(nrow(r) == 1L)
  paste0(fmt(100 * (r$ratio - 1)), "\\%")
}
primary_conformity_interpretation <- if (male_primary$lower > 1) {
  "Within male candidacies, the positive baseline association locates an appearance-associated advantage in the distribution of expected personal votes. This finding runs against the conformity expectation that a more gender-atypical cue balance should reduce personal votes."
} else if (male_primary$upper < 1) {
  "Within male candidacies, the negative baseline association links a more gender-atypical cue balance with fewer expected personal votes. This unequal distribution is consistent with the conformity expectation that a more gender-atypical cue balance should reduce personal votes."
} else {
  "Within male candidacies, uncertainty about the baseline association leaves the conformity expectation unresolved. An imprecise cue association does not establish an absence of appearance-associated inequality in the distribution of personal votes."
}
primary_cooperative_benchmark <- if (female_primary$lower <= 1 && female_primary$upper >= 1) {
  "The association within female candidacies leaves the full cooperative-leadership benchmark unresolved. Its interval accommodates both favorable and unfavorable associations in expected personal votes. This uncertainty does not establish an absence of appearance-associated inequality among women."
} else if (female_primary$upper < 1 && male_primary$lower > 1) {
  "The opposite baseline associations within male and female candidacies match the directional benchmark for a shared advantage associated with a more feminine-relative-to-masculine balance. They locate this advantage in unequal distributions of expected personal votes within each gender category."
} else {
  "The association within female candidacies does not complete the directional benchmark for a shared advantage associated with a more feminine-relative-to-masculine balance. The proposed common direction of appearance-associated differences in personal votes consequently remains unestablished."
}
party_named_contrast <- function(p, g) {
  r <- party_contrasts[party_contrasts$party == p & party_contrasts$gender == g, ]
  stopifnot(nrow(r) == 1L)
  r
}
kok_male <- party_named_contrast("KOK", "Male")
kok_female <- party_named_contrast("KOK", "Female")
centre_female <- party_named_contrast("KESK", "Female")
green_female <- party_named_contrast("VIHR", "Female")
party_profile_interpretation <- if (kok_male$ratio > 1 && kok_female$ratio < 1) {
  paste0("Within the National Coalition Party, a more feminine appearance relative to masculine appearance is associated with more expected personal votes among both men and women. This pattern favors departure from masculine appearance expectations among men and conformity to feminine appearance expectations among women. The one-standard-deviation estimates for the gender-atypical index are ",
    party_point_text("KOK", "Male"), " among men and ", party_point_text("KOK", "Female"),
    " among women, with opposite signs reflecting the gender-specific orientation of the index. These associations describe appearance-associated electoral inequality within each gender category and are consistent with a preference for feminine cues in this party.",
    if ((kok_female$lower <= 1 && kok_female$upper >= 1) ||
        (kok_male$lower <= 1 && kok_male$upper >= 1))
      " At least one confidence interval includes zero, so the evidence does not establish this pattern for both genders."
    else "")
} else {
  paste0("Within the National Coalition Party, a one-standard-deviation increase in the gender-atypical index is associated with a change in expected personal votes of ",
    party_point_text("KOK", "Male"), " among men and ", party_point_text("KOK", "Female"),
    " among women. The gender-specific orientation of the index makes these contrasts distinct comparisons of appearance-associated electoral inequality among men and among women.")
}
party_female_context_interpretation <- if (centre_female$ratio > 1 && green_female$ratio < 1) {
  paste0("Among women, a more masculine-relative-to-feminine profile is associated with greater expected personal votes in the Centre Party and fewer in the Green League. The corresponding one-standard-deviation contrasts are ",
    party_point_text("KESK", "Female"), " in the Centre Party and ",
    party_point_text("VIHR", "Female"), " in the Green League. These opposing point estimates illustrate how an imprecise pooled female association can coexist with appearance-associated electoral inequality within party contexts.")
} else {
  paste0("Among women, a one-standard-deviation increase in the gender-atypical index is associated with a change in expected personal votes of ",
    party_point_text("KESK", "Female"), " in the Centre Party and ",
    party_point_text("VIHR", "Female"), " in the Green League. These comparisons describe appearance-associated electoral inequality among women within party contexts. The accompanying confidence intervals delimit the precision of the party-specific associations.")
}
party_test_summary <- function(test) {
  r <- party_slope_test[party_slope_test$test == test, ]
  stopifnot(nrow(r) == 1L)
  p_text <- if (r$p < .001) "<0.001" else paste0("=", fmt(r$p, 3))
  paste0("$F_{", r$df1, ",", r$df2, "}=", fmt(r$F, 2), "$ ($p", p_text, "$)")
}
party_heterogeneity_text <- if (party_slope_test$p[party_slope_test$test == "All genders"] <
                                1 - paper_values$confidence_level) {
  "The joint test provides evidence that appearance-associated differences in expected personal votes vary across parties within one or both gender categories. The pooled within-gender association therefore does not describe a common association in every party context."
} else {
  "The joint test does not provide sufficient evidence that appearance-associated differences in expected personal votes vary across parties within gender categories. Differences between individual point estimates consequently remain uncertain."
}
paper_values$party_heterogeneity_text <- party_heterogeneity_text
party_abstract_context_text <- if (party_slope_test$p[party_slope_test$test == "All genders"] <
                                   1 - paper_values$confidence_level) {
  "Party-specific estimates further indicate that pooled within-gender associations in personal votes conceal variation across electoral contexts."
} else {
  "Party-specific estimates examine whether the pooled within-gender associations in personal votes extend across electoral contexts."
}
paper_values$party_abstract_context_text <- party_abstract_context_text
join_party_names <- function(parties) {
  labels <- paste0("the ", unname(party_names[parties]))
  if (!length(labels)) return("no party")
  if (length(labels) == 1L) return(labels)
  if (length(labels) == 2L) return(paste(labels, collapse = " and "))
  paste0(paste(head(labels, -1L), collapse = ", "), ", and ", tail(labels, 1L))
}
party_direction_text <- paste(vapply(c("Male", "Female"), function(g) {
  r <- party_contrasts[party_contrasts$gender == g, ]
  positive <- r$party[r$ratio > 1]
  negative <- r$party[r$ratio < 1]
  prefix <- if (g == "Male") "Within male candidacies" else "Within female candidacies"
  statements <- character()
  if (length(positive)) statements <- c(statements, paste0(prefix,
    ", the fitted personal-vote association is positive in ",
    join_party_names(positive), "."))
  if (length(negative)) statements <- c(statements, paste0(prefix,
    ", the fitted personal-vote association is negative in ",
    join_party_names(negative), "."))
  if (!length(statements)) statements <- paste0(prefix,
    ", the party-specific fitted personal-vote associations are zero.")
  paste(statements, collapse = " ")
}, character(1)), collapse = " ")
paper_values$party_n_contrasts <- nrow(party_contrasts)
paper_values$party_n_nonsignificant <- sum(party_contrasts$p >= 1 - paper_values$confidence_level)
party_precision_text <- paste0("Of the ", paper_values$party_n_contrasts,
  " party--gender one-SD contrasts in expected personal votes, ", paper_values$party_n_nonsignificant,
  " are not statistically significant at the ", fmt(100 * (1 - paper_values$confidence_level), 0),
  "\\% level.")
party_uncertainty_text <- paste0(party_precision_text,
  " The curves describe appearance-associated differences in the distribution of personal votes within candidacies, while uncertainty limits conclusions about particular parties.")
party_band_note <- paste0("Confidence bands are omitted to make the fitted personal-vote associations easier to compare. ",
  paper_values$party_n_nonsignificant, " of the ", paper_values$party_n_contrasts,
  " party--gender contrasts are not statistically significant at the ",
  fmt(100 * (1 - paper_values$confidence_level), 0), "\\% level.")
paper_values$party_uncertainty_text <- party_uncertainty_text
paper_values$party_precision_text <- party_precision_text
paper_values$primary_conformity_interpretation <- primary_conformity_interpretation
paper_values$primary_cooperative_benchmark <- primary_cooperative_benchmark
paper_values$party_profile_interpretation <- party_profile_interpretation
paper_values$party_female_context_interpretation <- party_female_context_interpretation
paper_values$party_band_note <- party_band_note
paper_values$main_figure_x_limits <- main_plot_x_limits
paper_values$party_figure_x_limits <- party_plot_x_limits
sdp_baseline <- party_sensitivity_contrasts[party_sensitivity_contrasts$party == "SDP" &
  party_sensitivity_contrasts$gender == "Male" &
  party_sensitivity_contrasts$model == "party_heterogeneity", ]
sdp_office <- party_sensitivity_contrasts[party_sensitivity_contrasts$party == "SDP" &
  party_sensitivity_contrasts$gender == "Male" &
  party_sensitivity_contrasts$model == "party_experience", ]
party_sdp_sensitivity_text <- paste0(
  "Within Social Democratic male candidacies, the baseline contrast in expected personal votes of ",
  party_effect_text("SDP", "Male"), " changes to ",
  party_effect_text("SDP", "Male", "party_experience"),
  " after adjustment for prior office. ",
  if (sdp_office$lower <= 1 && sdp_office$upper >= 1)
    "The office-adjusted interval includes zero, so the large baseline magnitude requires particular caution."
  else "The office-adjusted interval excludes zero, although its magnitude depends on the adjustment.")
party_sdp_magnitude_note <- paste0("Within Social Democratic male candidacies, the fitted one-SD contrast in expected personal votes changes from ",
  fmt(100 * (sdp_baseline$ratio - 1)), "\\% to ", fmt(100 * (sdp_office$ratio - 1)),
  "\\% after adjustment for prior office, illustrating the sensitivity of its magnitude.")
party_sensitivity_interpretation <- if (all(party_sensitivity_tests$p[
  party_sensitivity_tests$test == "All genders"] < 1 - paper_values$confidence_level)) {
  "The joint tests continue to provide evidence of variation in within-gender personal-vote associations across parties under both adjustments. This persistence does not imply that individual party contrasts retain the same magnitude."
} else {
  "The evidence for variation in within-gender personal-vote associations across parties depends on the adjustment, as do the magnitudes of individual party contrasts."
}
paper_values$party_direction_text <- party_direction_text
paper_values$party_sdp_sensitivity_text <- party_sdp_sensitivity_text
paper_values$party_sdp_magnitude_note <- party_sdp_magnitude_note
paper_values$party_sensitivity_interpretation <- party_sensitivity_interpretation
n_main <- nrow(d); n_male <- sum(d$gender == "Male"); n_female <- sum(d$gender == "Female")
n_cities <- length(unique(d$city)); n_list <- nrow(d_list)
n_singleton_cities <- sum(table(d$city) == 1)
n_singleton_lists <- sum(list_size == 1)
dispersion_main <- diagnostics$pearson_dispersion[diagnostics$model == "main"]
gender_diff_p <- gender_difference$p
primary_gender_comparison_text <- if (gender_diff_p < 1 - paper_values$confidence_level) {
  "The interaction test indicates that the percentage change in expected personal votes associated with a one-standard-deviation increase in the index differs between men and women."
} else {
  "This test does not provide sufficient evidence that the percentage change in expected personal votes associated with a one-standard-deviation increase in the index differs between men and women."
}
oa_joint_inequality_text <- paste0(
  if (effects$lower[effects$model == "joint" & effects$gender == "Male"] > 1) {
    "With joint cue level held constant, the model associates more gender-atypical cue profiles with greater expected personal votes within male candidacies. This conditional association describes an appearance-associated advantage in the unequal distribution of personal votes."
  } else if (effects$upper[effects$model == "joint" & effects$gender == "Male"] < 1) {
    "With joint cue level held constant, the model associates more gender-atypical cue profiles with fewer expected personal votes within male candidacies. This conditional association describes an appearance-associated disadvantage in the distribution of personal votes."
  } else {
    "With joint cue level held constant, the cue association within male candidacies remains imprecisely estimated. This uncertainty does not establish an absence of appearance-associated inequality in the distribution of personal votes."
  },
  if (all(S_effects$lower <= 1 & S_effects$upper >= 1)) {
    " The joint cue level intervals include zero for both gender categories, leaving their separate associations imprecisely estimated. This uncertainty does not establish that expected personal votes are distributed equally along joint cue level."
  } else {
    " At least one joint cue level interval excludes zero, providing evidence of an appearance-associated difference in expected personal votes within that gender category."
  })
oa_office_inequality_text <- paste0(
  if (effects$lower[effects$model == "experience" & effects$gender == "Male"] <= 1 &&
      effects$upper[effects$model == "experience" & effects$gender == "Male"] >= 1) {
    paste0("With prior office included, the cue association within male candidacies is imprecisely estimated. ",
      if (male_primary$lower > 1 || male_primary$upper < 1)
        "The evidence for the baseline appearance-associated inequality is therefore sensitive to this conditioning choice."
      else "This result leaves the adjusted appearance-associated association unresolved.")
  } else if (effects$lower[effects$model == "experience" & effects$gender == "Male"] > 1) {
    "With prior office included, more gender-atypical cue profiles are associated with greater expected personal votes within male candidacies. The adjusted association describes an appearance-associated advantage conditional on the expanded controls."
  } else {
    "With prior office included, more gender-atypical cue profiles are associated with fewer expected personal votes within male candidacies. The adjusted association describes an appearance-associated disadvantage conditional on the expanded controls."
  },
  if (abs(effects$log_ratio[effects$model == "experience" & effects$gender == "Male"]) <
      abs(male_primary$log_ratio)) {
    " Its point estimate lies closer to zero than in the baseline model."
  } else if (abs(effects$log_ratio[effects$model == "experience" & effects$gender == "Male"]) >
             abs(male_primary$log_ratio)) {
    " Its point estimate lies farther from zero than in the baseline model."
  } else {
    " Its point estimate has the same magnitude as in the baseline model."
  },
  " The adjustment does not identify a causal role for prior office.",
  if (effects$lower[effects$model == "experience" & effects$gender == "Male"] <= 1 &&
      effects$upper[effects$model == "experience" & effects$gender == "Male"] >= 1)
    " The imprecise adjusted association does not establish an absence of inequality within male candidacies."
  else "")
oa_quadratic_trim_p <- nonlinear_tests$p[
  nonlinear_tests$model %in% c("surface_trim", "surface_trim5")]
stopifnot(length(oa_quadratic_trim_p) == 2L)
oa_quadratic_inequality_text <- if (nonlinear_p < 1 - paper_values$confidence_level) {
  paste0("The full-sample joint test supports additional quadratic or interaction structure relative to the joint-level-adjusted linear specification. This omnibus result concerns the added terms jointly. It does not establish nonlinearity in both gender groups. ",
    if (all(oa_quadratic_trim_p >= 1 - paper_values$confidence_level)) {
      "The evidence is not retained under either upper-tail exclusion, making support for nonlinear structure sensitive to the candidate population represented by the estimate. "
    } else if (all(oa_quadratic_trim_p < 1 - paper_values$confidence_level)) {
      "The corresponding joint tests retain this evidence under both upper-tail exclusions. "
    } else {
      "The corresponding joint tests retain this evidence under only one upper-tail exclusion, indicating sensitivity to the estimation sample. "
    },
    "The fitted surface therefore describes conditional differences in expected personal votes whose shape must be assessed alongside these sample comparisons.")
} else {
  "The full-sample joint test does not provide sufficient evidence of additional quadratic or interaction structure at the reported significance level. The fitted surface describes model-implied differences in expected personal votes, but its shapes do not establish nonlinear appearance-associated inequality within gender categories. This qualification concerns the shape of the conditional cue association. It does not establish an absence of inequality."
}
paper_values$n_saved <- nrow(dat_source)
paper_values$n_main <- n_main
paper_values$n_male <- n_male
paper_values$n_female <- n_female
paper_values$n_cities <- n_cities
paper_values$n_within_list <- n_list
paper_values$n_singleton_cities <- n_singleton_cities
paper_values$n_singleton_lists <- n_singleton_lists
paper_values$n_high_high_men <- support_summary$F_and_M_at_least_3[support_summary$gender == "Male"]
paper_values$n_high_high_women <- support_summary$F_and_M_at_least_3[support_summary$gender == "Female"]
paper_values$n_balanced_men <- support_summary$balanced[support_summary$gender == "Male"]
paper_values$n_balanced_women <- support_summary$balanced[support_summary$gender == "Female"]
paper_values$n_opposite_dominates_men <- support_summary$opposite_cue_dominates[support_summary$gender == "Male"]
paper_values$n_opposite_dominates_women <- support_summary$opposite_cue_dominates[support_summary$gender == "Female"]
paper_values$n_trim1_removed <- nrow(d) - nrow(d_trim)
paper_values$n_trim5_removed <- nrow(d) - nrow(d_trim5)
paper_values$trim_vote_cutoffs <- c(top_cut, top5_cut)
paper_values$gender_difference_p <- gender_diff_p
paper_values$dispersion_main <- dispersion_main
paper_values$primary_A_effects <- primary
paper_values$joint_A_effects <- effects[effects$model == "joint", ]
paper_values$joint_S_effects <- S_effects
paper_values$primary_adjusts_joint_level <- FALSE
paper_values$party_model_n <- party_model_n
paper_values$party_model_dropped_n <- party_model_dropped_n
paper_values$eligible_party_levels <- eligible_party_levels
paper_values$excluded_party_levels <- excluded_party_levels
paper_values$n_eligible_parties <- length(eligible_party_levels)
paper_values$party_model_clusters <- party_model_clusters
paper_values$party_min_per_gender <- paper_values$support_min_candidates
paper_values$party_cell_counts <- party_cell_counts
paper_values$party_contrasts <- party_contrasts
paper_values$party_slope_test <- party_slope_test
paper_values$party_sensitivity_contrasts <- party_sensitivity_contrasts
paper_values$party_sensitivity_tests <- party_sensitivity_tests
paper_values$party_trim_removed_n <- nrow(d_party) - nrow(d_party_trim)
paper_values$party_trim_cutoff <- top_cut
paper_values$primary_interval_text <- primary_interval_text
paper_values$primary_evidence_text <- primary_evidence_text
paper_values$primary_abstract_text <- primary_abstract_text
paper_values$primary_abstract_short_text <- primary_abstract_short_text
paper_values$primary_theory_abstract_text <- primary_theory_abstract_text
paper_values$primary_theory_text <- primary_theory_text
paper_values$primary_inequality_text <- primary_inequality_text
paper_values$primary_gender_comparison_text <- primary_gender_comparison_text
paper_values$oa_joint_inequality_text <- oa_joint_inequality_text
paper_values$oa_office_inequality_text <- oa_office_inequality_text
paper_values$oa_quadratic_inequality_text <- oa_quadratic_inequality_text
paper_values$prediction_ranges <- do.call(rbind, lapply(levels(d$gender), function(g) {
  a <- main_prediction_curves$atypicality[main_prediction_curves$gender == g]
  data.frame(gender = g, A_min = min(a), A_max = max(a),
             n_prediction_points = length(a))
}))
paper_values$n_outcome_matches <- nrow(vote_audit)
paper_values$n_wrong_person_counts <- counts_wrong_person
paper_values$n_wrong_person_names <- names_wrong_person
paper_values$n_wrong_person_counts_main <- sum(d$turnout_saved / 2 != d$turnout)
count_noun <- function(n, singular, plural) paste(n, if (n == 1) singular else plural)
party_exclusion_text <- paste(vapply(excluded_party_levels, function(p) {
  counts <- party_cell_counts[party_cell_counts$party == p, ]
  paste0(p, " (",
         count_noun(counts$n[counts$gender == "Male"], "man", "men"), ", ",
         count_noun(counts$n[counts$gender == "Female"], "woman", "women"), ")")
}, character(1)), collapse = " and ")
paper_values$party_exclusion_text <- party_exclusion_text
support_high_text <- paste(count_noun(paper_values$n_high_high_men, "man", "men"), "and",
                           count_noun(paper_values$n_high_high_women, "woman", "women"))
writeLines(c(
  "Ambiguity. Verified analysis summary", paste("Source:", normalizePath(data_path)),
  paste("Source MD5:", source_hash_before),
  paste("Complete cases:", n_main, ". Men:", n_male, ". Women:", n_female, ". Cities:", n_cities),
  paste("Municipality-by-party FE sample:", n_list, ". Dropped singleton lists:", n_singleton_lists),
  paste("Primary +1 SD A, men:", effect_text("Male")),
  paste("Primary +1 SD A, women:", effect_text("Female")),
  paste("Difference in standardized gender slopes p:", signif(gender_diff_p, 4)),
  paste("Secondary party sample:", party_model_n, "candidates;",
        length(eligible_party_levels), "eligible parties;", party_model_dropped_n, "excluded candidates."),
  paste("Secondary joint party-slope heterogeneity p:",
        signif(party_slope_test$p[party_slope_test$test == "All genders"], 4)),
  paste("Quadratic terms joint F p:", signif(nonlinear_p, 4)),
  paste("Main Pearson dispersion:", round(dispersion_main, 2)),
  paste("Raw F/M vs appendix joint A+S model maximum prediction discrepancy:", signif(rotation_error, 3)),
  paste("All saved counts even:", all_saved_counts_even, ". Vantaa no.2 saved/official:",
        official_check$saved_votes, "/", official_check$official_votes),
  paste("Official audit:", nrow(vote_audit), "unique verified photograph matches. Wrong-person counts after halving:", counts_wrong_person,
        ". Other-person first names:", names_wrong_person),
  paste("Wrong-person counts in complete main sample:", sum(d$turnout_saved / 2 != d$turnout)),
  "Main outcome is official_votes joined by verified photograph ID, municipality and candidate number.",
  paste("Exported and reloaded analysis CSV:", analysis_csv_path),
  paste("Uniform outcome x2 slope discrepancy:", signif(scale_slope_error, 3),
        ". Cluster covariance discrepancy:", signif(scale_vcov_error, 3)),
  "No causal interpretation. Uncertainty conditions on saved mean ratings.",
  "Primary specification uses atypicality only, with the existing baseline controls. Joint cue level is an appendix adjustment.",
  "A is relative gendered appearance atypicality. S is joint cue level. Neither is gender identity.",
  primary_interval_text,
  primary_evidence_text,
  paste("Raw high-high support:", support_high_text, ". F and M >=", paper_values$scale_midpoint)
), file.path(project_dir, "results", "analysis_summary.txt"))

## ---- attractiveness-prose
attractiveness_correlation_text <- function(g, method = "pearson") {
  r <- attractiveness_correlations[attractiveness_correlations$gender == g, ]
  stopifnot(nrow(r) == 1L, method %in% c("pearson", "spearman"))
  fmt(r[[method]], 3)
}
attractiveness_effect_text <- function(g, specification) {
  r <- attractiveness_effects[attractiveness_effects$gender == g &
                               attractiveness_effects$model == specification, ]
  format_effect_text(r)
}
attractiveness_point_text <- function(g, specification) {
  r <- attractiveness_effects[attractiveness_effects$gender == g &
                               attractiveness_effects$model == specification, ]
  stopifnot(nrow(r) == 1L)
  paste0(fmt(100 * (r$ratio - 1)), "\\%")
}
attractiveness_male_adjustment_text <- paste0(
  "Among men, the estimated change in expected personal votes for a one-standard-deviation increase in the index is ",
  attractiveness_point_text("Male", "omitted"), " without attractiveness adjustment, ",
  attractiveness_point_text("Male", "linear"), " with the baseline adjustment, and ",
  attractiveness_point_text("Male", "flexible"), " under nonlinear attractiveness adjustment that varies by gender.")
attractiveness_male_flexible <- attractiveness_effects[
  attractiveness_effects$gender == "Male" & attractiveness_effects$model == "flexible", ]
attractiveness_adjustment_interpretation <- if (attractiveness_male_flexible$lower > 1) {
  "The male cue association is positive under nonlinear attractiveness adjustment that varies by gender, with its confidence interval entirely above zero. Appearance-associated electoral inequality among men is therefore evident even under the flexible attractiveness specification. Together with the weak relationship between the two ratings among men, this result weighs against treating the gender-atypical index as a proxy for measured attractiveness."
} else if (attractiveness_male_flexible$upper < 1) {
  "The confidence interval remains entirely below zero under the flexible adjustment. A more gender-atypical appearance among men is therefore associated with fewer expected votes after allowing the attractiveness association to vary by gender and take a nonlinear form. This result describes electoral inequality within male candidacies conditional on the measured attractiveness adjustment."
} else {
  "The confidence interval includes zero under the flexible adjustment. The baseline appearance-associated difference in support among men is therefore sensitive to how attractiveness is modeled. The evidence does not establish whether that difference remains after this adjustment, and this uncertainty does not establish an absence of inequality among men."
}
attractiveness_female_flexible <- attractiveness_effects[
  attractiveness_effects$gender == "Female" & attractiveness_effects$model == "flexible", ]
attractiveness_female_adjustment_text <- paste0(
  "Among women, the estimated change is ",
  attractiveness_point_text("Female", "omitted"), " without attractiveness adjustment and ",
  attractiveness_point_text("Female", "linear"), " with the baseline adjustment. With the flexible adjustment, it is ",
  attractiveness_effect_text("Female", "flexible"), ". ",
  if (attractiveness_female_flexible$lower <= 1 && attractiveness_female_flexible$upper >= 1) {
    "The confidence interval includes both an advantage and a disadvantage in expected votes. Because women rated as more feminine relative to masculine are also commonly rated as more attractive, the data provide less separation between these appearance dimensions among women. The direction of the adjusted difference in support remains uncertain. That uncertainty does not establish an equal distribution of electoral support among female candidates."
  } else {
    "The confidence interval excludes zero. The estimate describes electoral inequality within female candidacies after the flexible adjustment. The close relationship between feminine appearance and attractiveness among women nevertheless limits how sharply the data can distinguish their separate associations with votes."
  })
paper_values$attractiveness_correlations <- attractiveness_correlations
paper_values$attractiveness_effects <- attractiveness_effects
paper_values$attractiveness_male_adjustment_text <- attractiveness_male_adjustment_text
paper_values$attractiveness_adjustment_interpretation <- attractiveness_adjustment_interpretation
paper_values$attractiveness_female_adjustment_text <- attractiveness_female_adjustment_text

## ---- paper-tables
# Ready-to-print tabular fragments. Captions and placement belong to the Rnw.
# Percentage cells report the model contrast and its shared confidence interval.
effect_table_cell <- function(r) {
  stopifnot(nrow(r) == 1L)
  paste0(fmt(100 * (r$ratio - 1)), " [", fmt(100 * (r$lower - 1)), ", ",
         fmt(100 * (r$upper - 1)), "]")
}
p_table_cell <- function(x) {
  if (is.na(x)) return("--")
  if (x < .001) return("$<0.001$")
  fmt(x, 3)
}
latex_tabular <- function(rows, headings, alignment) {
  stopifnot(ncol(rows) == length(headings))
  lines <- apply(rows, 1L, function(x) paste(x, collapse = " & "))
  paste(c(paste0("\\begin{tabular}{", alignment, "}"),
          "\\toprule", paste0(paste(headings, collapse = " & "), " \\\\"),
          "\\midrule", paste0(lines, " \\\\"), "\\bottomrule", "\\end{tabular}"),
        collapse = "\n")
}
# Gender-specific descriptive statistics for the primary-model sample, using uncentered age.
summary_variables <- c(
  "Personal votes" = "turnout",
  "Gender-atypical cue contrast ($A$)" = "atypicality",
  "Femininity rating" = "femininity",
  "Masculinity rating" = "masculinity",
  "Attractiveness rating" = "attractiveness",
  "Candidate age (years)" = "age"
)
stopifnot(nrow(d) == nobs(models$main))
summary_groups <- do.call(rbind, lapply(levels(d$gender), function(g) {
  data.frame(gender = g, label = unname(gender_names[g]), n = sum(d$gender == g))
}))
stopifnot(sum(summary_groups$n) == nrow(d), all(summary_groups$n > 1L))
summary_statistics <- do.call(rbind, lapply(seq_along(summary_variables), function(j) {
  variable <- unname(summary_variables[j])
  do.call(rbind, lapply(summary_groups$gender, function(g) {
    x <- d[[variable]][d$gender == g]
    stopifnot(is.numeric(x), length(x) == summary_groups$n[summary_groups$gender == g],
              all(is.finite(x)))
    quartiles <- unname(quantile(x, c(.25, .75)))
    data.frame(variable = variable, label = names(summary_variables)[j], gender = g,
               n = length(x), mean = mean(x), sd = sd(x), min = min(x), max = max(x),
               median = median(x), q1 = quartiles[1], q3 = quartiles[2],
               range_digits = if (variable %in% c("turnout", "age")) 0L else 2L)
  }))
}))
summary_number <- function(x, digits = 2L) {
  x[abs(x) < 0.5 * 10^(-digits)] <- 0
  formatC(x, format = "f", digits = digits, big.mark = ",")
}
summary_continuous_lines <- vapply(seq_along(summary_variables), function(j) {
  variable <- unname(summary_variables[j])
  cells <- unlist(lapply(summary_groups$gender, function(g) {
    r <- summary_statistics[summary_statistics$variable == variable &
                              summary_statistics$gender == g, ]
    c(summary_number(r$mean), summary_number(r$sd),
      paste0("$", summary_number(r$min, r$range_digits), "$--$",
             summary_number(r$max, r$range_digits), "$"))
  }), use.names = FALSE)
  paste(c(names(summary_variables)[j], cells), collapse = " & ")
}, character(1))
summary_group_headings <- vapply(seq_len(nrow(summary_groups)), function(i) {
  r <- summary_groups[i, ]
  paste0("\\multicolumn{3}{c}{", r$label, " ($N=", summary_number(r$n, 0), "$)}")
}, character(1))
summary_statistics_table_tex <- paste(c(
  "\\begin{tabular}{@{}lrrc@{\\hspace{1.5em}}rrc@{}}", "\\toprule",
  paste0(" & ", paste(summary_group_headings, collapse = " & "), " \\\\"),
  "\\cmidrule(lr){2-4}\\cmidrule(l){5-7}",
  "Variable & Mean & SD & Min.--Max. & Mean & SD & Min.--Max. \\\\", "\\midrule",
  paste0(summary_continuous_lines, " \\\\"), "\\bottomrule", "\\end{tabular}"
), collapse = "\n")
paper_values$summary_statistics <- summary_statistics
paper_values$summary_groups <- summary_groups
paper_values$n_parties <- length(unique(d$party))
paper_values$vote_summary_by_gender <- summary_statistics[
  summary_statistics$variable == "turnout", c("gender", "n", "median", "q1", "q3")]
rownames(paper_values$vote_summary_by_gender) <- paper_values$vote_summary_by_gender$gender
write.csv(summary_statistics, file.path(project_dir, "results", "summary_statistics_by_gender.csv"),
          row.names = FALSE)

main_contrast_table <- data.frame(
  cue = "Gender-atypical contrast ($A$)",
  men = effect_table_cell(primary[primary$gender == "Male", ]),
  women = effect_table_cell(primary[primary$gender == "Female", ])
)
main_contrast_table_tex <- latex_tabular(
  main_contrast_table,
  c("Cue", "Men (\\%)", "Women (\\%)"), "lcc")

# Coefficients of the exact baseline PPML model used for Figure 1.
# Report substantive terms; summarize the nuisance fixed effects as inclusion rows.
oa_baseline_labels <- c(
  "\\texttt{Atypicality}" = "atypicality",
  "\\texttt{Woman}" = "genderFemale",
  "\\texttt{Woman} $\\times$ \\texttt{Atypicality}" = "genderFemale:atypicality",
  "\\texttt{Age} (mean centered)" = "age_c",
  "\\texttt{Attractiveness}" = "attractiveness"
)
oa_baseline_terms <- unname(oa_baseline_labels)
stopifnot(all(oa_baseline_terms %in% colnames(vcovs$main)))
oa_baseline_estimates <- unname(coef(models$main)[oa_baseline_terms])
oa_baseline_se <- unname(sqrt(diag(vcovs$main))[oa_baseline_terms])
oa_baseline_df <- attr(vcovs$main, "df")
oa_baseline_critical <- qt(paper_values$ci_quantile, oa_baseline_df)
oa_baseline_regression <- data.frame(
  model = "main", term = oa_baseline_terms, label = names(oa_baseline_labels),
  estimate = oa_baseline_estimates, cluster_se = oa_baseline_se,
  lower = oa_baseline_estimates - oa_baseline_critical * oa_baseline_se,
  upper = oa_baseline_estimates + oa_baseline_critical * oa_baseline_se,
  p = 2 * pt(-abs(oa_baseline_estimates / oa_baseline_se), oa_baseline_df),
  df = oa_baseline_df
)
oa_baseline_metadata <- list(
  n = nobs(models$main), clusters = attr(vcovs$main, "clusters"), df = oa_baseline_df,
  age_reference = mean(d$age),
  municipality_fe = "city" %in% attr(terms(models$main), "term.labels"),
  party_fe = "party" %in% attr(terms(models$main), "term.labels")
)
oa_baseline_regression_lines <- vapply(seq_len(nrow(oa_baseline_regression)), function(i) {
  r <- oa_baseline_regression[i, ]
  paste(c(r$label, fmt(r$estimate, 3), fmt(r$cluster_se, 3),
          paste0("[", fmt(r$lower, 3), ", ", fmt(r$upper, 3), "]"),
          p_table_cell(r$p)), collapse = " & ")
}, character(1))
oa_baseline_metadata_values <- c(
  "Municipality fixed effects" = if (oa_baseline_metadata$municipality_fe) "Yes" else "No",
  "Party fixed effects" = if (oa_baseline_metadata$party_fe) "Yes" else "No",
  "Candidates ($N$)" = summary_number(oa_baseline_metadata$n, 0),
  "Municipalities ($G$)" = summary_number(oa_baseline_metadata$clusters, 0)
)
oa_baseline_metadata_lines <- paste0(
  names(oa_baseline_metadata_values), " & \\multicolumn{4}{c}{",
  unname(oa_baseline_metadata_values), "} \\\\")
oa_baseline_regression_table_tex <- paste(c(
  "\\begin{tabular}{@{}lrrcr@{}}", "\\toprule",
  paste0("Variable & Coefficient & Clustered SE & ",
         paper_values$confidence_pct, "\\% CI & $p$ \\\\"),
  "\\midrule", paste0(oa_baseline_regression_lines, " \\\\"),
  "\\midrule", oa_baseline_metadata_lines, "\\bottomrule", "\\end{tabular}"
), collapse = "\n")
paper_values$oa_baseline_regression <- oa_baseline_regression
paper_values$oa_baseline_metadata <- oa_baseline_metadata
write.csv(oa_baseline_regression,
          file.path(project_dir, "results", "oa_baseline_regression.csv"), row.names = FALSE)

attractiveness_adjustment_labels <- c(
  omitted = "Attractiveness omitted",
  linear = "Baseline linear attractiveness",
  flexible = "Gender-specific attractiveness spline")
attractiveness_adjustment_table <- do.call(rbind,
  lapply(names(attractiveness_adjustment_labels), function(nm) {
    r <- attractiveness_effects[attractiveness_effects$model == nm, ]
    data.frame(specification = unname(attractiveness_adjustment_labels[nm]),
               n = as.character(unique(r$n)),
               men = effect_table_cell(r[r$gender == "Male", ]),
               women = effect_table_cell(r[r$gender == "Female", ]))
  }))
attractiveness_adjustment_table_tex <- latex_tabular(
  attractiveness_adjustment_table,
  c("Attractiveness adjustment", "$N$", "Men (\\%)", "Women (\\%)"),
  "p{0.40\\linewidth}rcc")
paper_values$attractiveness_adjustment_table <- attractiveness_adjustment_table

sensitivity_specs <- c(main = "Baseline controls",
                       joint = "+ Joint cue level",
                       experience = "+ Incumbency and parliamentary office",
                       within_list = "Municipality-by-party fixed effects",
                       trim = paste0("Excluding top ", paper_values$trim_percent[1], "\\% of votes"),
                       trim5 = paste0("Excluding top ", paper_values$trim_percent[2], "\\% of votes"))
oa_sensitivity_table <- do.call(rbind, lapply(names(sensitivity_specs), function(nm) {
  r <- effects[effects$model == nm, ]
  data.frame(specification = unname(sensitivity_specs[nm]),
             n = as.character(nobs(models[[nm]])),
             cities = as.character(attr(vcovs[[nm]], "clusters")),
             men = effect_table_cell(r[r$gender == "Male", ]),
             women = effect_table_cell(r[r$gender == "Female", ]))
}))
oa_sensitivity_table <- rbind(oa_sensitivity_table,
  do.call(rbind, lapply(c("male", "female"), function(nm) {
    r <- split_effects[split_effects$model == nm, ]
    data.frame(specification = paste0("Separate model for ", if (nm == "male") "men" else "women"),
               n = as.character(nobs(models[[nm]])),
               cities = as.character(attr(vcovs[[nm]], "clusters")),
               men = if (nm == "male") effect_table_cell(r) else "--",
               women = if (nm == "female") effect_table_cell(r) else "--")
  })))
oa_sensitivity_table_tex <- latex_tabular(
  oa_sensitivity_table,
  c("Specification", "$N$", "Cities", "Men (\\%)", "Women (\\%)"), "p{0.32\\linewidth}rrcc")

# Six gender-specific quadratic coefficients from one pooled surface model.
# Women's coefficients and SEs combine each base term with its gender interaction.
quadratic_names <- c(A2 = "$A^2$", S2 = "$S^2$", AS = "$A S$")
oa_quadratic_effects <- do.call(rbind, lapply(levels(d$gender), function(g) {
  do.call(rbind, lapply(names(quadratic_names), function(v) {
    r <- gender_contrast(models$surface, vcovs$surface, v, g)
    data.frame(gender = g, term = v, coefficient = r$log_ratio,
               cluster_se = r$se, p = r$p)
  }))
}))
oa_quadratic_table <- data.frame(
  gender = unname(gender_names[oa_quadratic_effects$gender]),
  term = unname(quadratic_names[oa_quadratic_effects$term]),
  coefficient = fmt(oa_quadratic_effects$coefficient, 3),
  cluster_se = fmt(oa_quadratic_effects$cluster_se, 3),
  p = vapply(oa_quadratic_effects$p, p_table_cell, character(1))
)
oa_quadratic_table_tex <- latex_tabular(
  oa_quadratic_table, c("Gender", "Term", "Coefficient", "Cluster SE", "$p$"), "llrrr")
nonlinear_labels <- c(surface = "Quadratic, full sample",
                      surface_trim = paste0("Excluding top ", paper_values$trim_percent[1], "\\% of votes"),
                      surface_trim5 = paste0("Excluding top ", paper_values$trim_percent[2], "\\% of votes"))
oa_nonlinear_tests <- nonlinear_tests[nonlinear_tests$model != "surface_HC1", ]
oa_nonlinear_test_table <- data.frame(
  specification = unname(nonlinear_labels[oa_nonlinear_tests$model]),
  n = as.character(oa_nonlinear_tests$n),
  F = fmt(oa_nonlinear_tests$F, 3),
  df1 = as.character(oa_nonlinear_tests$df1),
  df2 = as.character(oa_nonlinear_tests$df2),
  p = vapply(oa_nonlinear_tests$p, p_table_cell, character(1))
)
oa_nonlinear_test_table_tex <- latex_tabular(
  oa_nonlinear_test_table,
  c("Specification", "$N$", "$F$", "$df_1$", "$df_2$", "$p$"), "lrrrrr")
oa_party_table <- data.frame(
  party = party_contrasts$party,
  gender = unname(gender_names[party_contrasts$gender]),
  n = as.character(party_contrasts$n_cell),
  cities = as.character(party_contrasts$cell_city_clusters),
  association = vapply(seq_len(nrow(party_contrasts)), function(j) {
    effect_table_cell(party_contrasts[j, ])
  }, character(1)),
  p = vapply(party_contrasts$p, p_table_cell, character(1))
)
oa_party_table_tex <- latex_tabular(
  oa_party_table,
  c("Party", "Gender", "Candidates", "Cell cities", "Association (\\%)", "$p$"), "llrrcr")
party_sensitivity_specs <- c(
  party_heterogeneity = "Party-specific baseline",
  party_experience = "+ Incumbency and parliamentary office",
  party_trim = paste0("Excluding top ", paper_values$trim_percent[1], "\\% of votes"))
oa_party_sensitivity_table <- do.call(rbind, lapply(names(party_sensitivity_specs), function(nm) {
  tests <- party_sensitivity_tests[party_sensitivity_tests$model == nm, ]
  sdp_men <- party_sensitivity_contrasts[
    party_sensitivity_contrasts$model == nm &
      party_sensitivity_contrasts$party == "SDP" &
      party_sensitivity_contrasts$gender == "Male", ]
  kok_men <- party_sensitivity_contrasts[
    party_sensitivity_contrasts$model == nm &
      party_sensitivity_contrasts$party == "KOK" &
      party_sensitivity_contrasts$gender == "Male", ]
  green_men <- party_sensitivity_contrasts[
    party_sensitivity_contrasts$model == nm &
      party_sensitivity_contrasts$party == "VIHR" &
      party_sensitivity_contrasts$gender == "Male", ]
  data.frame(specification = unname(party_sensitivity_specs[nm]),
             n = as.character(nobs(models[[nm]])),
             cities = as.character(attr(vcovs[[nm]], "clusters")),
             joint_p = p_table_cell(tests$p[tests$test == "All genders"]),
             men_p = p_table_cell(tests$p[tests$test == "Men"]),
             women_p = p_table_cell(tests$p[tests$test == "Women"]),
             sdp_men = effect_table_cell(sdp_men),
             kok_men = effect_table_cell(kok_men),
             green_men = effect_table_cell(green_men))
}))
oa_party_sensitivity_table_tex <- paste0(
  "\\textit{Panel A. Party heterogeneity tests}\\par\\smallskip\n",
  latex_tabular(oa_party_sensitivity_table[, c("specification", "n", "cities", "joint_p", "men_p", "women_p")],
    c("Specification", "$N$", "Cities", "Joint $p$", "Men $p$", "Women $p$"),
    "p{0.32\\linewidth}rrccc"),
  "\\par\\medskip\\textit{Panel B. Selected male contrasts (\\%)}\\par\\smallskip\n",
  latex_tabular(oa_party_sensitivity_table[, c("specification", "sdp_men", "kok_men", "green_men")],
    c("Specification", "Social Democrats", "National Coalition", "Green League"),
    "p{0.25\\linewidth}p{0.22\\linewidth}p{0.22\\linewidth}p{0.22\\linewidth}"))
paper_values$main_contrast_table <- main_contrast_table
paper_values$oa_sensitivity_table <- oa_sensitivity_table
paper_values$oa_quadratic_effects <- oa_quadratic_effects
paper_values$oa_nonlinear_tests <- oa_nonlinear_tests
paper_values$oa_party_table <- oa_party_table
paper_values$oa_party_sensitivity_table <- oa_party_sensitivity_table
table_fragments <- list(attractiveness_adjustment_table = attractiveness_adjustment_table_tex,
                        summary_statistics_table = summary_statistics_table_tex,
                        main_contrast_table = main_contrast_table_tex,
                        oa_baseline_regression_table = oa_baseline_regression_table_tex,
                        oa_sensitivity_table = oa_sensitivity_table_tex,
                        oa_quadratic_table = oa_quadratic_table_tex,
                        oa_nonlinear_test_table = oa_nonlinear_test_table_tex,
                        oa_party_table = oa_party_table_tex,
                        oa_party_sensitivity_table = oa_party_sensitivity_table_tex)
for (nm in names(table_fragments)) {
  writeLines(table_fragments[[nm]], file.path(project_dir, "results", paste0(nm, ".tex")))
}
write.csv(oa_quadratic_effects, file.path(project_dir, "results", "oa_quadratic_effects.csv"),
          row.names = FALSE)

## ---- submission-highlights
# Submission highlights are regenerated from the same models as the manuscript.
# They are separate from the abstract and manuscript word-count regions.
submission_highlights <- c(
  "An original dataset links separate human appearance ratings to personal votes.",
  "Natural-experiment classification assumes external variation in gendered appearance.",
  primary_abstract_short_male,
  if (party_slope_test$p[party_slope_test$test == "All genders"] <
      1 - paper_values$confidence_level) {
    "Appearance-associated vote inequality varies across parties within gender."
  } else {
    "Party comparisons examine the context of appearance-associated vote inequality."
  },
  "The assignment assumption does not establish exchangeability or causal effects."
)
writeLines(paste0("- ", submission_highlights), file.path(project_dir, "Highlights.txt"))

## ---- save-results
stopifnot(identical(source_hash_before, unname(tools::md5sum(data_path))))
saveRDS(list(models = models, vcov_city = vcovs, vcov_main_HC1 = V_main_HC1,
             attractiveness_models = attractiveness_models,
             attractiveness_vcov_city = attractiveness_vcovs,
             attractiveness_pairs = attractiveness_pairs,
             attractiveness_qq = attractiveness_qq,
             attractiveness_correlations = attractiveness_correlations,
             attractiveness_effects = attractiveness_effects,
             contrasts = all_effects, gender_difference = gender_difference,
             diagnostics = diagnostics, scale_constants = scale_constants,
             source_path = normalizePath(data_path), source_md5 = source_hash_before,
             outcome_crosswalk_path = normalizePath(audit_path),
             outcome_crosswalk_md5 = unname(tools::md5sum(audit_path)),
             analysis_dataset_path = normalizePath(analysis_csv_path),
             analysis_dataset_md5 = unname(tools::md5sum(analysis_csv_path)),
             predictions = prediction_curves, main_predictions = main_prediction_curves,
             party_predictions = party_prediction_curves,
             party_contrasts = party_contrasts, party_cell_counts = party_cell_counts,
             party_slope_test = party_slope_test,
             party_sensitivity_contrasts = party_sensitivity_contrasts,
             party_sensitivity_tests = party_sensitivity_tests,
             party_model_n = party_model_n, eligible_party_levels = eligible_party_levels,
             joint_predictions = joint_prediction_curves,
             office_predictions = office_prediction_curves,
             paper_values = paper_values, table_fragments = table_fragments,
             legacy_saved_outcome_contrasts = legacy_effects,
             nonlinear_test = c(F = nonlinear_wald / nonlinear_df, df1 = nonlinear_df,
                                df2 = attr(vcovs$surface, "df"), p = nonlinear_p)),
        file.path(project_dir, "results", "models.rds"))
capture.output(sessionInfo(), file = file.path(project_dir, "results", "sessionInfo.txt"))
cat(readLines(file.path(project_dir, "results", "analysis_summary.txt")), sep = "\n")

## ---- word-counts
# Count the knitted TeX after all inline R expressions and tables have resolved.
# The manuscript count includes the abstract, headings, captions, footnotes,
# and figure/table notes.
# It excludes title metadata, keywords, references, the Online Appendix, citation
# keys, cross-reference labels, table cells, and mathematical expressions.
word_count_tokens <- c(manuscript = "AMBIGUITYMANUSCRIPTWORDCOUNT",
                       abstract = "AMBIGUITYABSTRACTWORDCOUNT")
word_count_region <- function(tex, name) {
  start <- paste0("% AMBIGUITY_", name, "_START")
  end <- paste0("% AMBIGUITY_", name, "_END")
  starts <- gregexpr(start, tex, fixed = TRUE)[[1L]]
  ends <- gregexpr(end, tex, fixed = TRUE)[[1L]]
  if (length(starts) != 1L || length(ends) != 1L || starts < 0L ||
      ends < starts + nchar(start)) {
    stop("Word-count markers are missing, repeated, or out of order for ", name, ".")
  }
  substr(tex, starts + nchar(start), ends - 1L)
}
count_tex_words <- function(tex) {
  executable <- unname(Sys.which("texcount"))
  if (!nzchar(executable) && file.exists("/Library/TeX/texbin/texcount")) {
    executable <- "/Library/TeX/texbin/texcount"
  }
  if (!nzchar(executable)) {
    stop("Dynamic word counts require TeXcount, supplied with TeX Live or MacTeX.")
  }
  citation_macros <- c("cite", "parencite", "textcite", "autocite", "footcite",
                       "citet", "citep", "ref", "eqref", "autoref", "pageref")
  rules <- c(paste0("%TC:macro \\", citation_macros, " 1"),
             "%TC:envir table [] text", "%TC:envir figure [] text")
  input <- tempfile("ambiguity-word-count-", fileext = ".tex")
  diagnostics <- tempfile("ambiguity-word-count-", fileext = ".log")
  on.exit(unlink(c(input, diagnostics)), add = TRUE)
  writeLines(c(rules, tex), input, useBytes = TRUE)
  output <- system2(executable,
    c("-sum=1,1,1,0,0,0,0", "-1", "-utf8", "-q", shQuote(input)),
    stdout = TRUE, stderr = diagnostics)
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop("TeXcount failed. ", paste(readLines(diagnostics, warn = FALSE), collapse = " "))
  }
  output <- trimws(output)
  numeric_output <- output[grepl("^[0-9]+$", output)]
  unexpected_output <- output[nzchar(output) &
    !grepl("^[0-9]+$|^SUMWEIGHTS:", output)]
  if (length(numeric_output) != 1L || length(unexpected_output)) {
    stop("TeXcount returned an unexpected result. ", paste(output, collapse = " "))
  }
  as.integer(numeric_output)
}
manuscript_word_counts <- function(tex) {
  abstract <- word_count_region(tex, "ABSTRACT")
  main <- word_count_region(tex, "MAIN")
  c(manuscript = count_tex_words(paste(abstract, main, sep = "\n")),
    abstract = count_tex_words(abstract))
}
install_word_count_hook <- function() {
  previous <- knitr::knit_hooks$get("document")
  if (isTRUE(attr(previous, "ambiguity_word_count_hook"))) {
    previous <- attr(previous, "previous_document_hook")
  }
  hook <- local({
    previous_hook <- previous
    function(tex) {
      tex <- paste(previous_hook(tex), collapse = "\n")
      if (!grepl(word_count_tokens[["manuscript"]], tex, fixed = TRUE)) return(tex)
      counts <- manuscript_word_counts(tex)
      for (name in names(counts)) {
        token <- word_count_tokens[[name]]
        occurrences <- gregexpr(token, tex, fixed = TRUE)[[1L]]
        if (length(occurrences) != 1L || occurrences < 0L) {
          stop("Expected one dynamic word-count placeholder for ", name, ".")
        }
        tex <- gsub(token, formatC(counts[[name]], format = "d", big.mark = ","),
                    tex, fixed = TRUE)
      }
      write.csv(data.frame(scope = names(counts), words = unname(counts)),
        file.path(project_dir, "results", "word_counts.csv"), row.names = FALSE)
      tex
    }
  })
  attr(hook, "ambiguity_word_count_hook") <- TRUE
  attr(hook, "previous_document_hook") <- previous
  knitr::knit_hooks$set(document = hook)
  invisible(hook)
}
install_word_count_hook()
