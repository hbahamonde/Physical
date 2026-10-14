# Separate matched-dose analysis. No main-paper files or saved data are modified.
# Run Rscript MatchedDose.R from this folder, or source this file from an editor.
args <- grep("^--file=", commandArgs(FALSE), value = TRUE)
analysis_dir <- if (length(args) && basename(sub("^--file=", "", args[1])) == "MatchedDose.R") {
  dirname(normalizePath(sub("^--file=", "", args[1])))
} else "/Users/hectorbahamonde/research/Physical/Ambiguity/matched_dose"
project_dir <- dirname(analysis_dir)
.libPaths(c(file.path(analysis_dir, ".Rlibrary"), .libPaths()))
packages <- c("nbpMatching", "nbpInference", "lpSolve", "sensitivitymv", "ggplot2", "jsonlite")
stopifnot(all(vapply(packages, requireNamespace, logical(1), quietly = TRUE)))
library(ggplot2)
source(file.path(analysis_dir, "inference_helpers.R"))
for (s in c("results", "figures")) dir.create(file.path(analysis_dir, s), showWarnings = FALSE)
out <- function(name) file.path(analysis_dir, "results", name)
fig <- function(name) file.path(analysis_dir, "figures", name)

# Freeze the design independently of outcomes. A retains the full sample's SD unit.
config <- list(min_dose_gap = .5, max_dose_gap = 1.5, age_caliper = 10,
  attractiveness_caliper = .5, joint_cue_caliper = .75, max_abs_smd = .10,
  exact = c("gender", "city", "party", "incumbent_councillor", "national_mp"),
  bootstrap_repetitions = 4999L, seed = 20261014L,
  scopes = list(primary = NULL, central_wide = c(1.75, 3.25), central_tight = c(2, 3)))
jsonlite::write_json(config, out("design_settings.json"), auto_unbox = TRUE, pretty = TRUE)
data_path <- file.path(project_dir, "Ambiguity_analysis.csv")
d <- read.csv(data_path, stringsAsFactors = FALSE)
fixed <- read.csv(file.path(project_dir, "Ambiguity_fixed.csv"), stringsAsFactors = FALSE)
official <- read.csv(file.path(project_dir, "audit", "verified_photo_candidate_crosswalk.csv"))
j <- match(d$id, official$id); jf <- match(d$id, fixed$id)
stopifnot(nrow(d) == 1384L, !anyDuplicated(d$id), !anyNA(j), !anyNA(jf),
  all(d$turnout == d$official_votes), all(d$turnout == official$official_votes[j]),
  all(d$turnout == fixed$official_votes[jf]), all(d$city == official$city[j]),
  all(d$candidate_number == official$candidate_number[j]), all(d$turnout > 0))
writeLines(c(paste("Source", normalizePath(data_path)),
  paste("Source MD5", unname(tools::md5sum(data_path))),
  paste("Rows", nrow(d)), "Every vote count equals the fixed file and official ID crosswalk."),
  out("data_checks.txt"))

# Only these fields enter matching. Votes, election status and saved outcomes do not.
covariates <- c("age", "attractiveness", "coelevation")
design <- d[, c("id", config$exact, "femininity", "masculinity", "atypicality", covariates)]
design$score <- NA_real_
score_fits <- list()
for (g in c("Male", "Female")) {
  ix <- which(design$gender == g)
  z <- design[ix, ]
  # Lu et al. allow a scalar Gaussian dose regression under constant variance.
  # Exact matching makes the city/party/office offsets identical within pairs.
  fit <- lm(atypicality ~ age + attractiveness + coelevation + incumbent_councillor +
    national_mp + factor(party) + factor(city), data = z)
  design$score[ix] <- fitted(fit)
  score_fits[[g]] <- fit
}
stopifnot(all(is.finite(design$score)))

empty_pairs <- function() data.frame(high_id = integer(), low_id = integer(),
  cost = numeric(), gender = character(), scope = character())

make_match <- function(g, scope, window) {
  z <- design[design$gender == g, ]
  full_z <- z
  if (!is.null(window)) z <- z[z$femininity >= window[1] & z$femininity <= window[2] &
    z$masculinity >= window[1] & z$masculinity <= window[2], ]
  if (nrow(z) < 2L) return(list(pairs = empty_pairs(), eligible_n = nrow(z), initial_pairs = 0L))
  dose_gap <- abs(outer(z$atypicality, z$atypicality, "-"))
  exact_group <- interaction(z$city, z$party, z$incumbent_councillor, z$national_mp, drop = TRUE)
  allowed <- outer(as.character(exact_group), as.character(exact_group), "==") &
    abs(outer(z$age, z$age, "-")) <= config$age_caliper &
    abs(outer(z$attractiveness, z$attractiveness, "-")) <= config$attractiveness_caliper &
    abs(outer(z$coelevation, z$coelevation, "-")) <= config$joint_cue_caliper &
    dose_gap >= config$min_dose_gap & dose_gap <= config$max_dose_gap
  diag(allowed) <- FALSE
  usable <- which(rowSums(allowed) > 0L)
  if (!length(usable)) return(list(pairs = empty_pairs(), eligible_n = nrow(z), initial_pairs = 0L))
  allowed <- allowed[usable, usable, drop = FALSE]; z <- z[usable, ]
  n <- nrow(z)
  # Lu's implemented distance uses midranks of both balancing score and dose.
  rs <- rank(z$score, ties.method = "average")
  ra <- rank(z$atypicality, ties.method = "average")
  lu_distance <- (outer(rs, rs, "-")^2 + 1e-6) / pmax(outer(ra, ra, "-")^2, 1e-12)
  allowed_cost <- lu_distance[allowed]
  scaled_cost <- matrix(0, n, n)
  scaled_cost[allowed] <- 1 + round(999 * allowed_cost / max(allowed_cost))
  # Dummy nodes allow discarded candidates. The penalty makes cardinality the
  # first objective and total Lu distance the second. Forbidden pairs never win.
  penalty <- 1001 * (n + 1)
  dm <- matrix(penalty, 2*n, 2*n)
  dm[seq_len(n), seq_len(n)] <- penalty * (2*n + 1)
  dm[seq_len(n), seq_len(n)][allowed] <- scaled_cost[allowed]
  dm[(n+1):(2*n), (n+1):(2*n)] <- 0
  diag(dm) <- 0
  matched <- nbpMatching::nonbimatch(nbpMatching::distancematrix(dm), precision = 9)
  h <- matched$halves
  h <- h[h$Group1.Row <= n & h$Group2.Row <= n, ]
  if (!nrow(h)) return(list(pairs = empty_pairs(), eligible_n = nrow(full_z), initial_pairs = 0L))
  stopifnot(all(allowed[cbind(h$Group1.Row, h$Group2.Row)]))
  hi <- ifelse(z$atypicality[h$Group1.Row] > z$atypicality[h$Group2.Row], h$Group1.Row, h$Group2.Row)
  lo <- ifelse(hi == h$Group1.Row, h$Group2.Row, h$Group1.Row)
  initial_pairs <- length(hi)
  # Retain the largest balanced subset of Lu pairs. This selection uses only
  # covariates, never votes. Balance is measured in the full gender group's SDs.
  delta <- sapply(covariates, function(v) (z[[v]][hi] - z[[v]][lo]) / sd(full_z[[v]]))
  delta <- matrix(delta, nrow = length(hi), ncol = length(covariates))
  B <- rbind(t(delta) - config$max_abs_smd, -t(delta) - config$max_abs_smd)
  opt <- lpSolve::lp("max", rep(1, length(hi)), B, rep("<=", nrow(B)),
    rep(0, nrow(B)), all.bin = TRUE)
  stopifnot(opt$status == 0L)
  k <- as.integer(round(sum(opt$solution)))
  if (!k) return(list(pairs = empty_pairs(), eligible_n = nrow(full_z), initial_pairs = initial_pairs))
  cost <- lu_distance[cbind(hi, lo)]
  opt2 <- lpSolve::lp("min", cost / max(cost) + seq_along(cost)*1e-8,
    rbind(B, rep(1, length(hi))), c(rep("<=", nrow(B)), "="), c(rep(0, nrow(B)), k),
    all.bin = TRUE)
  stopifnot(opt2$status == 0L)
  take <- which(opt2$solution > .5)
  list(pairs = data.frame(high_id = z$id[hi[take]], low_id = z$id[lo[take]],
    cost = cost[take], gender = g, scope = scope),
    eligible_n = nrow(full_z), initial_pairs = initial_pairs)
}

# Finish and save every design before outcomes are joined.
matches <- list(); designs <- list()
for (scope in names(config$scopes)) for (g in c("Male", "Female")) {
  key <- paste(scope, g, sep = "_")
  z <- make_match(g, scope, config$scopes[[scope]])
  matches[[key]] <- z$pairs
  designs[[key]] <- data.frame(scope = scope, gender = g,
    starting_n = if (is.null(config$scopes[[scope]])) sum(design$gender == g) else
      sum(design$gender == g & design$femininity >= config$scopes[[scope]][1] &
        design$femininity <= config$scopes[[scope]][2] &
        design$masculinity >= config$scopes[[scope]][1] & design$masculinity <= config$scopes[[scope]][2]),
    initial_pairs = z$initial_pairs, retained_pairs = nrow(z$pairs))
}
pair_ids <- do.call(rbind, matches)
write.csv(pair_ids, out("pair_ids_outcome_free.csv"), row.names = FALSE)
design_flow <- do.call(rbind, designs)
write.csv(design_flow, out("design_flow.csv"), row.names = FALSE)
print(design_flow)
if (!nrow(pair_ids)) stop("No balanced matching is feasible under the frozen rules.")

# Outcome join after matching. Verify all pair constraints on the retained data.
hi <- d[match(pair_ids$high_id, d$id), ]; lo <- d[match(pair_ids$low_id, d$id), ]
pairs <- cbind(pair_ids, city = hi$city, party = hi$party,
  dose_high = hi$atypicality, dose_low = lo$atypicality,
  dose_gap = hi$atypicality - lo$atypicality,
  votes_high = hi$turnout, votes_low = lo$turnout,
  vote_difference = hi$turnout - lo$turnout,
  log_vote_difference = log(hi$turnout) - log(lo$turnout))
stopifnot(all(hi$city == lo$city), all(hi$party == lo$party),
  all(hi$gender == lo$gender), all(hi$incumbent_councillor == lo$incumbent_councillor),
  all(hi$national_mp == lo$national_mp), all(pairs$dose_gap >= config$min_dose_gap),
  all(pairs$dose_gap <= config$max_dose_gap),
  all(abs(hi$age - lo$age) <= config$age_caliper),
  all(abs(hi$attractiveness - lo$attractiveness) <= config$attractiveness_caliper),
  all(abs(hi$coelevation - lo$coelevation) <= config$joint_cue_caliper))
balance <- do.call(rbind, lapply(unique(paste(pair_ids$scope, pair_ids$gender)), function(key) {
  ix <- which(paste(pair_ids$scope, pair_ids$gender) == key)
  g <- pair_ids$gender[ix[1]]
  do.call(rbind, lapply(covariates, function(v) data.frame(scope = pair_ids$scope[ix[1]], gender = g,
    variable = v, high_mean = mean(hi[[v]][ix]), low_mean = mean(lo[[v]][ix]),
    smd = mean(hi[[v]][ix] - lo[[v]][ix]) / sd(d[[v]][d$gender == g]),
    mean_absolute_pair_gap = mean(abs(hi[[v]][ix] - lo[[v]][ix])))))
}))
stopifnot(all(abs(balance$smd) <= config$max_abs_smd + 1e-7))
write.csv(balance, out("covariate_balance.csv"), row.names = FALSE)
write.csv(pairs, out("matched_pairs.csv"), row.names = FALSE)

effects <- list(); sensitivity <- list(); package_checks <- list()
gamma_grid <- sort(unique(c(seq(1, 3, by = .025), 4, 5)))
for (scope in names(config$scopes)) for (g in c("Male", "Female")) {
  z <- pairs[pairs$scope == scope & pairs$gender == g, ]
  if (nrow(z) < 2L) next
  for (trans in c("raw", "log")) {
    r <- md_nbp_effect_ratio(z$votes_high, z$votes_low, z$dose_high, z$dose_low,
      transformation = trans, library_path = file.path(analysis_dir, ".Rlibrary"))
    effects[[paste(scope,g,trans)]] <- data.frame(scope = scope, gender = g, outcome = trans,
      n_pairs = nrow(z), n_candidates = 2*nrow(z), n_cities = length(unique(z$city)),
      mean_dose_gap = mean(z$dose_gap), mean_votes_high = mean(z$votes_high),
      mean_votes_low = mean(z$votes_low), mean_vote_difference = mean(z$vote_difference),
      estimate = r$estimate, standard_error = r$standard_error,
      neyman_lower = r$interval[1], neyman_upper = r$interval[2])
  }
  if (length(unique(z$city)) >= 2L) {
    boot <- md_city_bootstrap(z$votes_high,z$votes_low,z$dose_high,z$dose_low,z$city,
      repetitions = config$bootstrap_repetitions, seed = config$seed)
    saveRDS(boot, out(paste0("bootstrap_",scope,"_",g,".rds")))
    for (trans in c("raw", "log")) {
      key <- paste(scope,g,trans)
      row <- boot$intervals[boot$intervals$estimand == paste0("neyman_",trans,"_vote_effect_ratio"), ]
      effects[[key]]$city_bootstrap_lower <- row$lower
      effects[[key]]$city_bootstrap_upper <- row$upper
    }
  }
  for (trans in c("raw", "log")) {
    diff <- if (trans == "raw") z$vote_difference else z$log_vote_difference
    s <- md_signed_rank_sensitivity(diff, gamma_grid, dose_gaps = z$dose_gap)
    s$scope <- scope; s$gender <- g; s$outcome <- trans
    sensitivity[[paste(scope,g,trans)]] <- s
  }
  # Established package M-statistic sensitivity with a common pairwise Gamma,
  # reported separately from exact, continuous-dose-aware primary bounds.
  package_checks[[paste(scope,g)]] <- do.call(rbind, lapply(c(1,1.25,1.5,2), function(ga) {
    a <- sensitivitymv::senmv(z$vote_difference, gamma = ga, method = "h")
    reverse <- sensitivitymv::senmv(-z$vote_difference, gamma = ga, method = "h")
    data.frame(scope = scope, gender = g, gamma_common_pair = ga,
      p_increase_upper_bound = a$pval, p_decrease_upper_bound = reverse$pval,
      p_two_sided_upper_bound = min(1, 2*min(a$pval,reverse$pval)),
      package_statistic = a$statistic)
  }))
}
effects <- do.call(rbind, effects)
sensitivity <- do.call(rbind, sensitivity)
write.csv(effects, out("effect_estimates.csv"), row.names = FALSE)
write.csv(sensitivity, out("hidden_bias_sensitivity.csv"), row.names = FALSE)
write.csv(do.call(rbind,package_checks), out("package_common_pair_sensitivity.csv"), row.names = FALSE)
summary_lines <- c("MATCHED-DOSE ANALYSIS", "",
  "Corrected official personal votes are used for the photographed candidates.",
  "Matching is separate by recorded gender and never uses vote outcomes.",
  "A retains the main analysis sample's within-gender standard-deviation scale.", "",
  "Design", capture.output(print(design_flow, row.names=FALSE)), "",
  "Primary continuous-dose estimates", capture.output(print(effects[effects$scope=="primary",],row.names=FALSE)), "",
  "Primary exact, dose-aware sensitivity bounds", capture.output(print(
    sensitivity[sensitivity$scope=="primary" & sensitivity$outcome=="raw" &
      sensitivity$gamma %in% c(1,1.25,1.5,2),
      c("gender","gamma","n_pairs","p_two_sided_upper_bound")],row.names=FALSE)), "",
  "Interpretation",
  "The estimator sum(vote differences)/sum(dose gaps) is a dose-normalized local",
  "average finite-contrast effect under conditional as-if random dose assignment.",
  "It targets only retained candidates over their observed dose gaps, not all candidates",
  "or a point-specific derivative at raw F=M=2.5. No IV/complier LATE is estimated.",
  "Neyman intervals are conservative, approximate, and conditional on fixed selected matches.",
  "City-cluster bootstrap intervals are saved separately and also condition on fixed matches.",
  "A causal interpretation additionally requires a sufficiently well-defined profile intervention",
  "and an explicit model of electoral competition/interference. Matching does not establish these.",
  "Attractiveness matching defines a profile comparison at similar measured beauty, not an",
  "automatically identified direct effect independent of a beauty pathway.",
  "The continuous-dose Gaussian balancing score is a working assignment model with constant variance.",
  "The exact signed-rank test is a sharp-null dose-swap test, not a test of zero average slope.",
  "For primary sensitivity, Gamma is per one SD of A and pair j has odds bound Gamma^gap_j.",
  "This mapping assumes an exponential-tilt/additive-Gaussian hidden assignment model.",
  "The common-pair sensitivitymv Huber M test is a distinct, approximate supplemental test.",
  "The log-vote estimate describes a geometric dose contrast and can differ from the arithmetic",
  "vote-count estimate. It is not the manuscript's Poisson expected-vote ratio.", "",
  "The strict central windows retain few matched men and no women after the prespecified",
  "balance selection. That describes this two-stage design, not global infeasibility of every match.")
writeLines(summary_lines,out("analysis_summary.txt"))
saveRDS(list(settings=config, score_fits=score_fits, pairs=pairs, effects=effects,
  sensitivity=sensitivity, balance=balance), out("matched_dose_analysis.rds"))

colors <- c(Male = "#245B78", Female = "#D27A9E")
gender_names <- c(Male = "Men", Female = "Women")
theme_md <- function(size=12) theme_minimal(base_size=size) + theme(panel.grid=element_blank(),
  panel.border=element_rect(color="grey30",fill=NA,linewidth=.6),
  axis.text=element_text(color="grey20"), legend.position="top", plot.title=element_text(face="bold"),
  plot.caption=element_text(hjust=0,color="grey35",size=9), plot.margin=margin(12,16,12,12))
e <- effects[effects$scope == "primary" & effects$outcome == "raw", ]
e$group <- factor(paste0(gender_names[e$gender],"\n",e$n_pairs," pairs (",e$n_candidates," candidates)"),
  levels=rev(paste0(gender_names[e$gender],"\n",e$n_pairs," pairs (",e$n_candidates," candidates)")))
effect_plot <- ggplot(e,aes(estimate,group,color=gender)) +
  geom_vline(xintercept=0,color="grey60",linetype=2,linewidth=.45) +
  geom_errorbar(aes(xmin=neyman_lower,xmax=neyman_upper),orientation="y",width=.20,linewidth=1) +
  geom_point(size=3.8) + scale_color_manual(values=colors,guide="none") +
  labs(title="Gender-atypical appearance and personal votes",
    subtitle="Local contrasts from matched male and female candidates",
    x="Dose-normalized vote difference (votes per within-gender SD)",y=NULL,
    caption=paste("Dots show within-pair dose contrasts. Bars show approximate 95% Neyman intervals.",
      "Estimates apply to retained candidates over their observed dose differences.",
      "Causal interpretation assumes as-if random dose assignment within matched pairs.",sep="\n")) + theme_md()
s <- sensitivity[sensitivity$scope == "primary" & sensitivity$outcome == "raw" & sensitivity$gamma<=3, ]
s$gender <- factor(s$gender, levels=c("Male","Female"))
sensitivity_plot <- ggplot(s,aes(gamma,p_two_sided_upper_bound,color=gender)) +
  geom_hline(yintercept=.05,color="grey55",linetype=2,linewidth=.5) + geom_line(linewidth=1) +
  scale_color_manual(values=colors,labels=gender_names,name=NULL) +
  scale_y_continuous(limits=c(0,1),breaks=c(0,.05,.25,.5,.75,1)) +
  labs(title="Sensitivity to hidden assignment bias",
    subtitle="Exact signed-rank randomization calculation with dose-aware bounds",
    x=expression(Gamma["1 SD"]~"(hidden-bias parameter)"),y="Upper bound on two-sided p-value",
    caption=paste("Gamma = 1 assumes fair within-pair dose assignment. Larger values allow hidden bias.",
      "Each pair's allocation-odds bound is Gamma raised to its actual A-dose gap.",
      "The dashed line marks p = 0.05. The test concerns a sharp null of no dose effects.",sep="\n")) + theme_md() + theme(aspect.ratio=.7)
b <- balance[balance$scope=="primary", ]
b$gender <- factor(b$gender, levels=c("Male","Female"))
b$variable <- factor(b$variable,levels=rev(c("age","attractiveness","coelevation")),
  labels=rev(c("Age","Attractiveness","Joint cue level")))
balance_plot <- ggplot(b,aes(smd,variable,color=gender)) +
  geom_vline(xintercept=c(-.1,.1),linetype=2,color="grey65") +
  geom_vline(xintercept=0,color="grey75") + geom_point(size=3,position=position_dodge(width=.35)) +
  scale_color_manual(values=colors,labels=gender_names,name=NULL) +
  scale_x_continuous(limits=c(-.15,.15),breaks=c(-.1,0,.1)) +
  labs(title="Covariate balance after matching", x="High-dose minus low-dose standardized mean difference",y=NULL,
    caption="Municipality, party, incumbent status and parliamentary office are matched exactly.") + theme_md()
ggsave(fig("matched_dose_effects.png"),effect_plot,width=9,height=5,dpi=300,bg="white")
ggsave(fig("hidden_bias_sensitivity.png"),sensitivity_plot,width=7.5,height=6.2,dpi=300,bg="white")
ggsave(fig("covariate_balance.png"),balance_plot,width=8,height=4.7,dpi=300,bg="white")
pdf(fig("matched_dose_results.pdf"),width=9,height=6.5,useDingbats=FALSE)
print(effect_plot); print(sensitivity_plot); print(balance_plot); dev.off()
capture.output(sessionInfo(),file=out("sessionInfo.txt"))
cat("\nPRIMARY ESTIMATES\n"); print(effects[effects$scope=="primary", ],row.names=FALSE)
cat("\nPRIMARY SENSITIVITY\n"); print(sensitivity[sensitivity$scope=="primary" &
  sensitivity$outcome=="raw" & sensitivity$gamma %in% c(1,1.25,1.5,2),
  c("gender","gamma","n_pairs","p_two_sided_upper_bound")],row.names=FALSE)
