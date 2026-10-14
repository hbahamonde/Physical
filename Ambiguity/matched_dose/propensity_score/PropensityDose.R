# Propensity-score matched continuous-dose analysis, separate from the paper.
# Run Rscript PropensityDose.R or source this file from an editor.
args <- grep("^--file=", commandArgs(FALSE), value = TRUE)
analysis_dir <- if (length(args) && basename(sub("^--file=", "", args[1])) == "PropensityDose.R") {
  dirname(normalizePath(sub("^--file=", "", args[1])))
} else "/Users/hectorbahamonde/research/Physical/Ambiguity/matched_dose/propensity_score"
parent_dir <- dirname(analysis_dir)
project_dir <- dirname(parent_dir)
.libPaths(c(file.path(parent_dir, ".Rlibrary"), .libPaths()))
stopifnot(all(vapply(c("nbpMatching", "nbpInference", "lpSolve", "ggplot2", "jsonlite"),
  requireNamespace, logical(1), quietly = TRUE)))
library(ggplot2)
source(file.path(parent_dir, "inference_helpers.R"))
for (s in c("results", "figures")) dir.create(file.path(analysis_dir, s), showWarnings = FALSE)
out <- function(x) file.path(analysis_dir, "results", x)
fig <- function(x) file.path(analysis_dir, "figures", x)

# These design rules are specified before vote outcomes enter matching.
config <- list(score_formula = paste("atypicality ~ age_c + attractiveness + coelevation +",
  "incumbent_councillor + national_mp + factor(party) + log_city_ballots"),
  exact = "gender", score_sd_caliper = .2, allocation_probability_bounds = c(.45, .55),
  min_dose_gap = .5, max_dose_gap = 1.5, age_caliper = 10,
  attractiveness_caliper = .5, joint_cue_caliper = .75,
  electoral_market_ratio_caliper = 1.5, max_abs_smd = .10,
  scopes = list(overlap = NULL, central = c(1.75, 3.25)), seed = 20261014L)
jsonlite::write_json(config, out("design_settings.json"), auto_unbox = TRUE, pretty = TRUE)
d <- read.csv(file.path(project_dir, "Ambiguity_analysis.csv"), stringsAsFactors = FALSE)
fixed <- read.csv(file.path(project_dir, "Ambiguity_fixed.csv"), stringsAsFactors = FALSE)
official <- read.csv(file.path(project_dir, "audit", "verified_photo_candidate_crosswalk.csv"))
j <- match(d$id, official$id); jf <- match(d$id, fixed$id)
stopifnot(nrow(d) == 1384L, !anyDuplicated(d$id), !anyNA(j), !anyNA(jf),
  all(d$turnout == d$official_votes), all(d$turnout == official$official_votes[j]),
  all(d$turnout == fixed$official_votes[jf]), all(d$city == official$city[j]),
  all(d$candidate_number == official$candidate_number[j]), all(d$turnout > 0))
writeLines(c("Fixed analysis CSV and official outcome crosswalk agree for all 1384 candidates.",
  paste("MD5", unname(tools::md5sum(file.path(project_dir, "Ambiguity_analysis.csv"))))),
  out("data_checks.txt"))

# Only these fields enter score fitting and pairing. City ID is recorded for diagnostics.
fields <- c("id", "gender", "city", "party", "age_c", "age", "attractiveness", "coelevation",
  "incumbent_councillor", "national_mp", "city_valid_votes", "atypicality", "femininity", "masculinity")
design <- d[, fields]
design$log_city_ballots <- log(design$city_valid_votes)
design$score <- design$gps_density <- NA_real_
score_fits <- list(); model_matrices <- list(); balance_matrices <- list(); sigma <- numeric(); score_checks <- list()
for (g in c("Male", "Female")) {
  ix <- which(design$gender == g); z <- design[ix, ]
  fit <- lm(as.formula(config$score_formula), data = z)
  design$score[ix] <- fitted(fit)
  # Match nbpInference::make.pmatrix's working Gaussian residual scale exactly.
  sigma[g] <- sd(residuals(fit))
  design$gps_density[ix] <- dnorm(z$atypicality, fitted(fit), sigma[g])
  score_fits[[g]] <- fit
  model_matrices[[g]] <- model.matrix(fit)[, -1, drop = FALSE]
  # Balance every party, including the omitted regression reference category.
  # Regression and allocation fitting retain the usual full-rank coding.
  balance_matrices[[g]] <- cbind(model_matrices[[g]][, !grepl("factor(party)",
    colnames(model_matrices[[g]]), fixed = TRUE), drop = FALSE],
    model.matrix(~factor(party) - 1, data = z))
  varying <- apply(balance_matrices[[g]], 2, sd) > 0
  balance_matrices[[g]] <- balance_matrices[[g]][, varying, drop = FALSE]
  # Verify our stable Gaussian GPS allocation formula against the package API.
  package_p <- nbpInference::make.pmatrix(z$atypicality, model_matrices[[g]])
  stable_p <- plogis(outer(z$atypicality, z$atypicality, "-") *
    outer(fitted(fit), fitted(fit), "-") / sigma[g]^2)
  diag(stable_p) <- 0
  stopifnot(max(abs(package_p - stable_p)) < 1e-9)
  score_checks[[g]] <- data.frame(gender = g, n = nrow(z), model_r_squared = summary(fit)$r.squared,
    dose_residual_sd = sigma[g], score_sd = sd(fitted(fit)),
    maximum_package_probability_difference = max(abs(package_p - stable_p)))
}
stopifnot(all(is.finite(design$score)), all(design$gps_density > 0))
saveRDS(score_fits, out("gps_models.rds"))
write.csv(do.call(rbind, score_checks), out("gps_model_checks.csv"), row.names = FALSE)
write.csv(design, out("outcome_free_design.csv"), row.names = FALSE)

empty_pairs <- function() data.frame(high_id = integer(), low_id = integer(), cost = numeric(),
  probability_observed_allocation = numeric(), gender = character(), scope = character())
make_match <- function(g, scope, window) {
  full <- design[design$gender == g, ]; z <- full
  Xfull <- balance_matrices[[g]]
  if (!is.null(window)) z <- z[z$femininity >= window[1] & z$femininity <= window[2] &
    z$masculinity >= window[1] & z$masculinity <= window[2], ]
  starting_n <- nrow(z)
  dose_delta <- outer(z$atypicality, z$atypicality, "-")
  score_delta <- outer(z$score, z$score, "-")
  pmat <- plogis(dose_delta * score_delta / sigma[g]^2)
  allowed <- abs(score_delta) <= config$score_sd_caliper * sd(full$score) &
    pmat >= config$allocation_probability_bounds[1] & pmat <= config$allocation_probability_bounds[2] &
    abs(dose_delta) >= config$min_dose_gap & abs(dose_delta) <= config$max_dose_gap &
    abs(outer(z$age, z$age, "-")) <= config$age_caliper &
    abs(outer(z$attractiveness, z$attractiveness, "-")) <= config$attractiveness_caliper &
    abs(outer(z$coelevation, z$coelevation, "-")) <= config$joint_cue_caliper &
    abs(outer(z$log_city_ballots, z$log_city_ballots, "-")) <= log(config$electoral_market_ratio_caliper)
  diag(allowed) <- FALSE
  usable <- which(rowSums(allowed) > 0L)
  if (!length(usable)) return(list(pairs = empty_pairs(), starting_n = starting_n, eligible_n = 0L, initial_pairs = 0L))
  allowed <- allowed[usable, usable, drop = FALSE]
  pmat <- pmat[usable, usable, drop = FALSE]
  z <- z[usable, ]; n <- nrow(z)
  # Under the constant-variance Gaussian GPS, its conditional mean is a balancing
  # score for the whole dose distribution. Match on distance in that score.
  cost <- abs(outer(z$score, z$score, "-")) / sd(full$score)
  scaled <- matrix(0, n, n)
  scaled[allowed] <- 1 + round(999 * cost[allowed] / max(cost[allowed]))
  penalty <- 1001 * (n + 1)
  dm <- matrix(penalty, 2*n, 2*n)
  dm[seq_len(n), seq_len(n)] <- penalty * (2*n + 1)
  dm[seq_len(n), seq_len(n)][allowed] <- scaled[allowed]
  dm[(n+1):(2*n), (n+1):(2*n)] <- 0; diag(dm) <- 0
  h <- nbpMatching::nonbimatch(nbpMatching::distancematrix(dm), precision = 9)$halves
  h <- h[h$Group1.Row <= n & h$Group2.Row <= n, ]
  stopifnot(nrow(h) > 0L, all(allowed[cbind(h$Group1.Row, h$Group2.Row)]))
  hi <- ifelse(z$atypicality[h$Group1.Row] > z$atypicality[h$Group2.Row], h$Group1.Row, h$Group2.Row)
  lo <- ifelse(hi == h$Group1.Row, h$Group2.Row, h$Group1.Row)
  initial <- length(hi)
  X <- Xfull[match(z$id, full$id), , drop = FALSE]
  sx <- apply(Xfull, 2, sd)
  delta <- sweep(X[hi, , drop = FALSE] - X[lo, , drop = FALSE], 2, sx, "/")
  B <- rbind(t(delta) - config$max_abs_smd, -t(delta) - config$max_abs_smd)
  opt <- lpSolve::lp("max", rep(1, initial), B, rep("<=", nrow(B)), rep(0, nrow(B)), all.bin = TRUE)
  stopifnot(opt$status == 0L)
  k <- as.integer(round(sum(opt$solution)))
  if (!k) return(list(pairs = empty_pairs(), starting_n = starting_n, eligible_n = n, initial_pairs = initial))
  pair_cost <- cost[cbind(hi, lo)]
  opt2 <- lpSolve::lp("min", pair_cost / max(pair_cost) + seq_along(pair_cost)*1e-8,
    rbind(B, rep(1, initial)), c(rep("<=", nrow(B)), "="), c(rep(0, nrow(B)), k), all.bin = TRUE)
  stopifnot(opt2$status == 0L)
  take <- which(opt2$solution > .5)
  list(pairs = data.frame(high_id = z$id[hi[take]], low_id = z$id[lo[take]], cost = pair_cost[take],
    probability_observed_allocation = pmat[cbind(hi[take], lo[take])], gender = g, scope = scope),
    starting_n = starting_n, eligible_n = n, initial_pairs = initial)
}

matches <- list(); flow <- list()
for (scope in names(config$scopes)) for (g in c("Male", "Female")) {
  key <- paste(scope, g, sep = "_")
  cat("Matching", key, "\n"); flush.console()
  m <- make_match(g, scope, config$scopes[[scope]])
  matches[[key]] <- m$pairs
  flow[[key]] <- data.frame(scope = scope, gender = g, starting_n = m$starting_n,
    eligible_n = m$eligible_n, initial_pairs = m$initial_pairs, retained_pairs = nrow(m$pairs))
}
pair_ids <- do.call(rbind, matches); rownames(pair_ids) <- NULL
flow <- do.call(rbind, flow); rownames(flow) <- NULL
write.csv(pair_ids, out("pair_ids_outcome_free.csv"), row.names = FALSE)
write.csv(flow, out("design_flow.csv"), row.names = FALSE)
print(flow, row.names = FALSE)

balance <- list(); pair_context <- list()
for (key in names(matches)) {
  p <- matches[[key]]; if (!nrow(p)) next
  g <- p$gender[1]; full <- design[design$gender == g, ]; X <- balance_matrices[[g]]
  ih <- match(p$high_id, full$id); il <- match(p$low_id, full$id)
  delta <- X[ih, , drop = FALSE] - X[il, , drop = FALSE]
  balance[[key]] <- data.frame(scope = p$scope[1], gender = g, variable = colnames(X),
    high_mean = colMeans(X[ih, , drop = FALSE]), low_mean = colMeans(X[il, , drop = FALSE]),
    smd = colMeans(delta) / apply(X, 2, sd), mean_absolute_pair_gap = colMeans(abs(delta)))
  pair_context[[key]] <- data.frame(scope = p$scope[1], gender = g, n_pairs = nrow(p),
    same_city = sum(full$city[ih] == full$city[il]), same_party = sum(full$party[ih] == full$party[il]),
    same_incumbency = sum(full$incumbent_councillor[ih] == full$incumbent_councillor[il]),
    maximum_ballot_ratio = max(exp(abs(full$log_city_ballots[ih] - full$log_city_ballots[il]))),
    allocation_probability_min = min(p$probability_observed_allocation),
    allocation_probability_max = max(p$probability_observed_allocation))
}
balance <- do.call(rbind, balance); rownames(balance) <- NULL
stopifnot(all(abs(balance$smd) <= config$max_abs_smd + 1e-7))
write.csv(balance, out("covariate_balance.csv"), row.names = FALSE)
write.csv(do.call(rbind, pair_context), out("pair_context.csv"), row.names = FALSE)

# Outcome join occurs only after the complete, outcome-free design is saved.
hi <- d[match(pair_ids$high_id, d$id), ]; lo <- d[match(pair_ids$low_id, d$id), ]
pairs <- cbind(pair_ids, dose_high = hi$atypicality, dose_low = lo$atypicality,
  dose_gap = hi$atypicality - lo$atypicality, city_high = hi$city, city_low = lo$city,
  party_high = hi$party, party_low = lo$party,
  votes_high = hi$turnout, votes_low = lo$turnout,
  share_high = 10000*hi$vote_share, share_low = 10000*lo$vote_share)
for (scope in names(config$scopes)) for (g in c("Male", "Female")) {
  p <- pairs[pairs$scope == scope & pairs$gender == g, ]
  stopifnot(!anyDuplicated(c(p$high_id, p$low_id)))
}
write.csv(pairs, out("matched_pairs.csv"), row.names = FALSE)

# Fit-informed dose-swap allocation probabilities enter the nbpInference estimate.
effects <- list()
for (scope in names(config$scopes)) for (g in c("Male", "Female")) {
  p <- pairs[pairs$scope == scope & pairs$gender == g, ]; I <- nrow(p)
  if (I < 2L) next
  ids <- as.vector(rbind(p$high_id, p$low_id))
  z <- design[match(ids, design$id), ]; Z <- z$atypicality
  X <- model_matrices[[g]][match(ids, design$id[design$gender == g]), , drop = FALSE]
  # Score-model probabilities use the full gender fit, not a rematched-subset fit.
  pm <- plogis(outer(Z, Z, "-") * outer(z$score, z$score, "-") / sigma[g]^2)
  pp <- cbind(seq(1L, 2L*I, by = 2L), seq(2L, 2L*I, by = 2L))
  stopifnot(max(abs(pm[pp] - p$probability_observed_allocation)) < 1e-12)
  for (outcome in c("raw_votes", "log_votes", "votes_per_10000_ballots")) {
    yh <- switch(outcome, raw_votes = p$votes_high, log_votes = log(p$votes_high), votes_per_10000_ballots = p$share_high)
    yl <- switch(outcome, raw_votes = p$votes_low, log_votes = log(p$votes_low), votes_per_10000_ballots = p$share_low)
    Y <- as.vector(rbind(yh, yl))
    estimate <- nbpInference::bias.corrected.neyman(Y, Z, pp, pm, xi = 0)
    variance <- as.numeric(nbpInference::covAdj.variance(Y, Z, pairs = pp, pmat = pm,
      xi = 0, Q = matrix(1, I, 1)))
    algebra <- sum((yh - yl) / p$probability_observed_allocation) / (2*sum(p$dose_gap))
    stopifnot(abs(estimate - algebra) < 1e-9, variance >= 0)
    se <- sqrt(variance)
    effects[[paste(scope, g, outcome)]] <- data.frame(scope = scope, gender = g, outcome = outcome,
      n_pairs = I, n_candidates = 2*I, n_cities = length(unique(c(p$city_high, p$city_low))),
      mean_dose_gap = mean(p$dose_gap), mean_votes_high = mean(p$votes_high), mean_votes_low = mean(p$votes_low),
      estimate = estimate, standard_error = se, lower = estimate - qnorm(.975)*se, upper = estimate + qnorm(.975)*se)
  }
}
effects <- do.call(rbind, effects); rownames(effects) <- NULL
write.csv(effects, out("effect_estimates.csv"), row.names = FALSE)
print(effects, row.names = FALSE)
saveRDS(list(config = config, score_fits = score_fits, flow = flow, balance = balance,
  pair_ids = pair_ids, pairs = pairs, effects = effects), out("propensity_dose_analysis.rds"))
writeLines(capture.output(sessionInfo()), out("sessionInfo.txt"))

# Optional first pass stops after the design and package effect estimates.
if (identical(Sys.getenv("GPS_DESIGN_ONLY"), "1")) quit(save = "no", status = 0L)
source(file.path(analysis_dir, "gps_inference_helpers.R"))
gps_inference_self_test()
gamma_grid <- sort(unique(c(seq(1, 2, by = .1), 1.25, 2.5, 3, 4, 5)))
sensitivity <- list()
for (scope in names(config$scopes)) for (g in c("Male", "Female")) {
  p <- pairs[pairs$scope == scope & pairs$gender == g, ]; if (!nrow(p)) next
  for (outcome in c("raw_votes", "log_votes", "votes_per_10000_ballots")) {
    delta <- switch(outcome, raw_votes = p$votes_high - p$votes_low,
      log_votes = log(p$votes_high) - log(p$votes_low), votes_per_10000_ballots = p$share_high - p$share_low)
    s <- gps_signed_rank_sensitivity(delta, p$probability_observed_allocation, p$dose_gap, gamma_grid)
    s$scope <- scope; s$gender <- g; s$outcome <- outcome
    sensitivity[[paste(scope, g, outcome)]] <- s
  }
}
sensitivity <- do.call(rbind, sensitivity); rownames(sensitivity) <- NULL
write.csv(sensitivity, out("hidden_bias_sensitivity.csv"), row.names = FALSE)

colors <- c(Male = "#245B78", Female = "#D27A9E")
nice_theme <- theme_classic(base_size = 12) + theme(panel.border = element_rect(color = "#444444", fill = NA),
  panel.grid = element_blank(), plot.title = element_text(face = "bold", size = 16),
  plot.subtitle = element_text(size = 11, margin = margin(b = 12)),
  plot.caption = element_text(hjust = 0, size = 9, lineheight = 1.15, margin = margin(t = 12)),
  legend.position = "none", plot.margin = margin(15, 22, 14, 15))
ef <- effects[effects$scope == "overlap" & effects$outcome == "raw_votes", ]
ef$gender_label <- factor(ifelse(ef$gender == "Male", "Men", "Women"), levels = c("Women", "Men"))
ef$label <- sprintf("%+.1f votes  [%.1f, %.1f]\n%d matched pairs", ef$estimate, ef$lower, ef$upper, ef$n_pairs)
p_effect <- ggplot(ef, aes(x = estimate, y = gender_label, color = gender)) +
  geom_vline(xintercept = 0, color = "#888888", linetype = "dashed", linewidth = .45) +
  geom_segment(aes(x = lower, xend = upper, yend = gender_label), linewidth = 1.15) +
  geom_point(size = 3.7) + geom_text(aes(label = label), nudge_y = .24, size = 3.5, lineheight = 1.15) +
  scale_color_manual(values = colors) + scale_y_discrete(expand = expansion(add = .65)) +
  scale_x_continuous(expand = expansion(mult = .15)) +
  labs(title = "Propensity-score matched appearance contrasts",
    subtitle = "Higher gender-atypical cues versus lower cues among comparable candidates",
    x = "Personal-vote contrast per one SD of the appearance dose", y = NULL,
    caption = paste("Dots show continuous-dose matched estimates. Bars show approximate 95% Neyman intervals.",
      "Scores and pairs are fixed before vote outcomes enter the analysis. Men and women are matched separately.",
      "A causal interpretation requires conditional exchangeability and the working dose-assignment model.", sep = "\n")) + nice_theme
ss <- sensitivity[sensitivity$scope == "overlap" & sensitivity$outcome == "raw_votes" & sensitivity$gamma_per_SD <= 3, ]
p_sensitivity <- ggplot(ss, aes(x = gamma_per_SD, y = p_two_sided_upper_bound, color = gender)) +
  geom_hline(yintercept = .05, linetype = "dashed", color = "#888888") +
  geom_line(linewidth = 1.1) + scale_color_manual(values = colors) +
  facet_wrap(~factor(gender, levels = c("Male", "Female"), labels = c("Men", "Women"))) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0,.05,.25,.5,.75,1)) +
  scale_x_continuous(breaks = seq(1,3,.5)) +
  labs(title = "Sensitivity to hidden differences between candidates",
    subtitle = "Exact signed-rank bounds allow for fitted dose-assignment probabilities",
    x = "Hidden assignment-odds multiplier per one SD of appearance (Gamma)",
    y = "Two-sided p-value upper bound",
    caption = paste("Gamma = 1 allows no additional hidden bias beyond the fitted assignment score.",
      "For a pair with dose gap d, hidden assignment odds can change by Gamma^d.",
      "The dashed line marks .05. These bounds test a sharp dose-swap null, conditional on the selected pairs.", sep = "\n")) + nice_theme + theme(aspect.ratio = 1)
bl <- balance[balance$scope == "overlap", ]
bl$variable_label <- sub("factor(party)", "Party ", bl$variable, fixed = TRUE)
bl$variable_label <- sub("log_city_ballots", "Log municipal ballots", bl$variable_label, fixed = TRUE)
bl$variable_label <- sub("age_c", "Age", bl$variable_label, fixed = TRUE)
bl$variable_label <- sub("coelevation", "Joint cue level", bl$variable_label, fixed = TRUE)
bl$variable_label <- sub("incumbent_councillor", "Incumbent councillor", bl$variable_label, fixed = TRUE)
bl$variable_label <- sub("national_mp", "Member of Parliament", bl$variable_label, fixed = TRUE)
bl$variable_label <- sub("attractiveness", "Attractiveness", bl$variable_label, fixed = TRUE)
p_balance <- ggplot(bl, aes(x = smd, y = reorder(variable_label, smd), color = gender)) +
  geom_vline(xintercept = c(-.1,.1), color = "#888888", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "#BBBBBB", linewidth = .3) + geom_point(size = 2.5) +
  facet_wrap(~factor(gender, levels = c("Male", "Female"), labels = c("Men", "Women"))) +
  scale_color_manual(values = colors) + coord_cartesian(xlim = c(-.125,.125)) +
  labs(title = "Balance after propensity-score matching", subtitle = "Higher-dose minus lower-dose candidates",
    x = "Standardized mean difference", y = NULL,
    caption = "Every score-model covariate and party indicator is within 0.10 full-gender-sample SDs.") + nice_theme
pdf(fig("propensity_dose_results.pdf"), width = 8.8, height = 6.0, useDingbats = FALSE)
print(p_effect); print(p_sensitivity); print(p_balance); dev.off()
ggsave(fig("propensity_dose_effects.png"), p_effect, width = 8.8, height = 5.7, dpi = 220)
ggsave(fig("hidden_bias_sensitivity.png"), p_sensitivity, width = 8.8, height = 5.7, dpi = 220)
ggsave(fig("covariate_balance.png"), p_balance, width = 8.8, height = 6.0, dpi = 220)
summary_lines <- c("PROPENSITY-SCORE MATCHED CONTINUOUS-DOSE ANALYSIS", "",
  capture.output(print(flow, row.names = FALSE)), "",
  capture.output(print(effects, row.names = FALSE)), "",
  capture.output(print(sensitivity[sensitivity$scope == "overlap" & sensitivity$outcome == "raw_votes" &
    sensitivity$gamma_per_SD %in% c(1,1.25,1.5,2), ], row.names = FALSE)), "",
  "The fixed corrected dataset is used. The manuscript and its figures are unchanged.",
  "The Gaussian conditional dose mean is a balancing score only under its working assignment model.",
  "This is disjoint nonbipartite propensity-score pairing, not the CausalGPS exposure-grid ERF estimator.",
  "Neither matching nor measured covariate balance establishes as-if random assignment.",
  "The target is a dose-gap-weighted local average finite contrast for retained candidates.",
  "This is not an instrumental-variable LATE and does not identify a universal one-SD intervention.",
  "Neyman intervals condition on the selected matches and estimated assignment scores.",
  "They are not municipality-clustered and do not incorporate score-estimation or selection uncertainty.",
  "Cross-city matches balance observed ballot volume but cannot remove unmeasured municipal/list differences.",
  "Ballot volume is realized election context, not a strictly preexposure population measurement.",
  "Appearance controls define similar beauty and joint-level profiles, not automatically a total facial effect.",
  "Candidate appearance must correspond to a sufficiently well-defined intervention for a causal reading.",
  "Competition and interference remain additional identification concerns.",
  "Sensitivity results are exact conditional on known fitted allocation probabilities and independent swaps.",
  "The fitted probabilities are estimated, so this is model-based sensitivity, not an actual randomized experiment.",
  "The raw-vote, log-vote, and ballot-normalized outcomes have different estimands.")
writeLines(summary_lines, out("analysis_summary.txt"))
cat("Completed separate propensity-score analysis.\n")
