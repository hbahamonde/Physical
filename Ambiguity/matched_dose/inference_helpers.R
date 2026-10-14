# Package-free inference helpers for a fixed, outcome-blind matched-dose design.
# Source this file to load helpers. Running it directly executes fixtures only.
# No data are loaded, matched, fitted, or written by this file.
#
# For pair j, orient the observed outcome difference as Y_high_dose - Y_low_dose.
# Exact randomization inference conditions on fixed pairs and unordered doses,
# with independent fair dose-label flips across pairs under the sharp null.
# Rosenbaum bounds replace the fair probability by pair-specific probabilities
# in [1/(1+Gamma), Gamma/(1+Gamma)]. These are assumptions about assignment.
# Outcomes need not be statistically independent for a sharp-null randomization
# test, but independent pair assignments must be justified. Causal mean-effect
# interpretation also needs a well-defined profile treatment and an explicit
# treatment-interference assumption for competition within municipalities.

md_weighted_bernoulli_pmf <- function(weights, probability = 0.5) {
  if (!is.numeric(weights) || anyNA(weights) || any(!is.finite(weights)) ||
      any(weights <= 0 | weights != round(weights))) {
    stop("weights must be positive finite integers (or numeric(0)).")
  }
  n <- length(weights)
  if (length(probability) == 1L) probability <- rep(probability, n)
  if (length(probability) != n || anyNA(probability) ||
      any(!is.finite(probability)) || any(probability < 0 | probability > 1)) {
    stop("probability must be in [0,1], with length one or length(weights).")
  }
  pmf <- 1
  for (j in seq_len(n)) {
    w <- as.integer(weights[j])
    index <- seq_along(pmf)
    next_pmf <- numeric(length(pmf) + w)
    next_pmf[index] <- (1 - probability[j]) * pmf
    next_pmf[index + w] <- next_pmf[index + w] + probability[j] * pmf
    pmf <- next_pmf
  }
  # pmf[t + 1] = Pr(sum_j weights[j] * Bernoulli_j = t).
  if (abs(sum(pmf) - 1) > 1e-9) stop("PMF mass check failed.")
  pmf
}

md_upper_tail <- function(pmf, threshold) {
  if (length(threshold) != 1L || !is.finite(threshold)) stop("Invalid threshold.")
  threshold <- ceiling(threshold)
  if (threshold <= 0) return(1)
  if (threshold > length(pmf) - 1L) return(0)
  min(1, max(0, sum(pmf[seq.int(threshold + 1L, length(pmf))])))
}

md_signed_rank_sensitivity <- function(differences,
    gammas = c(1, 1.1, 1.25, 1.5, 2, 3), dose_gaps = NULL) {
  if (!is.numeric(differences) || anyNA(differences) ||
      any(!is.finite(differences))) stop("differences must be finite numeric values.")
  if (!is.numeric(gammas) || !length(gammas) || anyNA(gammas) || any(gammas < 1)) {
    stop("gammas must contain numbers >= 1; Inf is allowed.")
  }
  if (!is.null(dose_gaps) && (!is.numeric(dose_gaps) ||
      length(dose_gaps) != length(differences) || anyNA(dose_gaps) ||
      any(!is.finite(dose_gaps)) || any(dose_gaps <= 0))) {
    stop("dose_gaps must be finite, positive, and aligned with all differences.")
  }
  nonzero <- differences[differences != 0]
  gaps <- if (is.null(dose_gaps)) rep(1, length(nonzero)) else
    dose_gaps[differences != 0]
  # Doubled average ranks are exact integers, including absolute-value ties.
  weights <- as.integer(round(2 * rank(abs(nonzero), ties.method = "average")))
  observed <- sum(weights[nonzero > 0])
  total <- sum(weights)
  observed_extreme <- max(observed, total - observed)
  result <- lapply(gammas, function(gamma) {
    # With supplied gaps, Gamma is the assignment-odds bound for a ONE-SD
    # dose difference. Pair j then has Gamma_j = Gamma ^ dose_gap_j.
    # This is a different sensitivity model from a common pairwise Gamma.
    probability <- if (is.infinite(gamma)) rep(1, length(gaps)) else
      plogis(log(gamma) * gaps)
    pmf <- md_weighted_bernoulli_pmf(weights, probability)
    upper <- md_upper_tail(pmf, observed)
    lower <- md_upper_tail(pmf, total - observed)
    # For Gamma=1, symmetry gives the exact inclusive two-sided tail.
    # For Gamma>1, each one-sided tail has its own exact worst-case bound.
    # Doubling the bound in the observed extreme direction is a valid
    # Bonferroni upper bound for the two-sided randomization p-value.
    # It is NOT asserted to equal the exact joint supremum of both tails.
    # max(upper,lower) would make the doubled result always equal one.
    two_sided <- min(1, 2 * md_upper_tail(pmf, observed_extreme))
    data.frame(gamma = gamma,
      bias_model = if (is.null(dose_gaps)) "common_pair_Gamma" else "Gamma_per_one_SD",
      n_pairs = length(differences),
      n_nonzero = length(nonzero), n_zero = sum(differences == 0),
      doubled_positive_rank_sum = observed, doubled_total_rank_sum = total,
      positive_rank_sum = observed / 2,
      min_favorable_assignment_probability = if (length(probability)) min(probability) else NA_real_,
      max_favorable_assignment_probability = if (length(probability)) max(probability) else NA_real_,
      p_greater_upper_bound = upper, p_less_upper_bound = lower,
      p_two_sided_upper_bound = two_sided,
      two_sided_is_exact_randomization_p = gamma == 1)
  })
  do.call(rbind, result)
}

md_pair_effects <- function(y_high, y_low, a_high, a_low) {
  vectors <- list(y_high, y_low, a_high, a_low)
  if (!length(y_high) || any(lengths(vectors) != length(y_high)) ||
      !all(vapply(vectors, is.numeric, logical(1))) ||
      any(vapply(vectors, function(x) anyNA(x) || any(!is.finite(x)), logical(1)))) {
    stop("Outcomes and doses must be equally sized, nonempty, finite numeric vectors.")
  }
  if (any(y_high < 0 | y_low < 0)) stop("Vote counts must be nonnegative.")
  gap <- a_high - a_low
  if (any(gap <= 0)) stop("Each pair must have a_high strictly greater than a_low.")
  raw <- y_high - y_low
  log_plus_one <- log1p(y_high) - log1p(y_low)
  logs_available <- all(y_high > 0 & y_low > 0)
  log_ratio <- if (logs_available) log(y_high) - log(y_low) else rep(NA_real_, length(raw))
  c(n_pairs = length(raw), mean_dose_gap = mean(gap), min_dose_gap = min(gap),
    max_dose_gap = max(gap), mean_raw_vote_difference = mean(raw),
    median_raw_vote_difference = median(raw), mean_log_vote_ratio = mean(log_ratio),
    geometric_vote_ratio = exp(mean(log_ratio)),
    neyman_raw_vote_effect_ratio = sum(raw) / sum(gap),
    neyman_log_vote_effect_ratio = sum(log_ratio) / sum(gap),
    neyman_geometric_vote_factor_per_SD = exp(sum(log_ratio) / sum(gap)),
    mean_log_plus_one_ratio = mean(log_plus_one),
    geometric_plus_one_ratio = exp(mean(log_plus_one)),
    mean_dose_normalized_log_ratio = mean(log_ratio / gap),
    geometric_vote_factor_per_SD = exp(mean(log_ratio / gap)))
}

# Geometric_vote_ratio compares geometric vote levels, not arithmetic expected
# counts and not the manuscript's adjusted PPML mean ratio. The per-SD factor
# additionally assumes a common constant log-linear response across the local
# doses before it can be interpreted as a common causal factor for +1 SD.
# Log1p ratios concern votes-plus-one, not ordinary vote ratios.
# Signed-rank tests target a sharp zero-effect null, not merely a zero mean.

# This adapter calls the installed package, rather than reconstructing the
# package estimator. It also verifies its intercept-only variance identity.
# No main-paper model is fit. The normal CI is approximate, not an exact
# randomization inversion and not a municipality-clustered interval.
md_nbp_effect_ratio <- function(y_high, y_low, a_high, a_low,
    transformation = c("raw", "log", "log1p"), library_path = NULL,
    confidence = 0.95) {
  details <- md_pair_effects(y_high, y_low, a_high, a_low)
  transformation <- match.arg(transformation)
  if (length(y_high) < 2L) stop("Package variance requires at least two matched pairs.")
  if (transformation == "log" && any(y_high <= 0 | y_low <= 0)) {
    stop("Log-vote inference requires strictly positive vote counts.")
  }
  if (length(confidence) != 1L || !is.finite(confidence) ||
      confidence <= 0 || confidence >= 1) stop("Invalid confidence level.")
  transform <- switch(transformation, raw = identity, log = log, log1p = log1p)
  yh <- transform(y_high); yl <- transform(y_low)
  n <- length(yh)
  Y <- as.vector(rbind(yh, yl))
  Z <- as.vector(rbind(a_high, a_low))
  pairs <- data.frame(first = seq.int(1L, 2L * n, by = 2L),
    second = seq.int(2L, 2L * n, by = 2L))
  ns <- loadNamespace("nbpInference", lib.loc = library_path)
  estimator <- get("classic.neyman", envir = ns)
  variance_estimator <- get("covAdj.variance", envir = ns)
  estimate <- estimator(Y = Y, Z = Z, pairs = pairs)
  variance <- as.numeric(variance_estimator(Y = Y, Z = Z, pairs = pairs,
    xi = 0, Q = matrix(1, nrow = n, ncol = 1)))
  manual_estimate <- sum(yh - yl) / sum(a_high - a_low)
  manual_variance <- n * var(yh - yl) / sum(a_high - a_low)^2
  if (!isTRUE(all.equal(estimate, manual_estimate, tolerance = 1e-10)) ||
      !isTRUE(all.equal(variance, manual_variance, tolerance = 1e-10))) {
    stop("Installed nbpInference estimator/variance differed from audited formula.")
  }
  se <- sqrt(variance)
  critical <- qnorm((1 + confidence) / 2)
  interval <- c(lower = estimate - critical * se, upper = estimate + critical * se)
  list(transformation = transformation, estimate = estimate, variance = variance,
    standard_error = se, interval = interval,
    exponentiated_estimate = if (transformation == "raw") NA_real_ else exp(estimate),
    exponentiated_interval = if (transformation == "raw") c(lower = NA_real_, upper = NA_real_) else exp(interval),
    n_pairs = n, mean_dose_gap = unname(details["mean_dose_gap"]),
    package = paste0("nbpInference ", as.character(getNamespaceVersion("nbpInference"))),
    interval_type = "Approximate normal CI from conservative Neyman variance, intercept Q, independent pair assignment")
}

md_city_bootstrap <- function(y_high, y_low, a_high, a_low, city,
    repetitions = 2000L, confidence = 0.95, seed = 20261014L) {
  point <- md_pair_effects(y_high, y_low, a_high, a_low)
  if (length(city) != length(y_high) || anyNA(city)) stop("city must identify every pair.")
  if (length(repetitions) != 1L || !is.finite(repetitions) ||
      repetitions < 2 || repetitions != as.integer(repetitions)) stop("Invalid repetitions.")
  if (length(confidence) != 1L || !is.finite(confidence) ||
      confidence <= 0 || confidence >= 1) stop("Invalid confidence level.")
  blocks <- split(seq_along(city), as.character(city))
  if (length(blocks) < 2L) stop("A city bootstrap requires at least two city clusters.")
  targets <- c("mean_raw_vote_difference", "mean_log_vote_ratio", "geometric_vote_ratio",
    "neyman_raw_vote_effect_ratio", "neyman_log_vote_effect_ratio", "neyman_geometric_vote_factor_per_SD",
    "mean_log_plus_one_ratio", "geometric_plus_one_ratio",
    "mean_dose_normalized_log_ratio", "geometric_vote_factor_per_SD")
  # Preserve the caller's random-number state.
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv) else
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
  set.seed(seed)
  draws <- vapply(seq_len(repetitions), function(b) {
    sampled_cities <- sample.int(length(blocks), length(blocks), replace = TRUE)
    index <- unlist(blocks[sampled_cities], use.names = FALSE)
    md_pair_effects(y_high[index], y_low[index], a_high[index], a_low[index])[targets]
  }, numeric(length(targets)))
  rownames(draws) <- targets
  alpha <- (1 - confidence) / 2
  limits <- t(vapply(targets, function(nm) {
    if (!is.finite(point[nm])) return(c(NA_real_, NA_real_))
    quantile(draws[nm, ], c(alpha, 1 - alpha), names = FALSE, type = 7)
  }, numeric(2)))
  list(estimates = point,
    intervals = data.frame(estimand = targets, estimate = unname(point[targets]),
      lower = limits[, 1], upper = limits[, 2], row.names = NULL),
    n_cities = length(blocks), repetitions = repetitions, seed = seed,
    interval_type = "City-cluster percentile bootstrap conditional on fixed selected matches",
    draws = t(draws))
}

# This bootstrap describes sampling variability across cities conditional on the
# selected matches. It is not an exact finite-population randomization CI, does
# not account for rematching, and does not address hidden bias. Few city clusters
# can make it unreliable. Do not describe it as a common-shift exact CI.

md_self_test <- function() {
  enumerate <- function(w, p) {
    if (!length(w)) return(list(score = 0, probability = 1, pmf = 1))
    z <- as.matrix(expand.grid(rep(list(0:1), length(w))))
    score <- as.vector(z %*% w)
    probability <- apply(z, 1, function(a) prod(ifelse(a == 1, p, 1 - p)))
    pmf <- numeric(sum(w) + 1L)
    for (i in seq_along(score)) pmf[score[i] + 1L] <- pmf[score[i] + 1L] + probability[i]
    list(score = score, probability = probability, pmf = pmf)
  }
  close <- function(x, y) stopifnot(isTRUE(all.equal(x, y, tolerance = 1e-12,
    check.attributes = FALSE)))
  for (p in list(0, 0.5, 1, c(0.1, 0.8, 0.4, 0.7))) {
    w <- c(1L, 2L, 2L, 5L)
    pp <- rep(p, length.out = length(w))
    close(md_weighted_bernoulli_pmf(w, p), enumerate(w, pp)$pmf)
  }
  close(md_weighted_bernoulli_pmf(numeric(0), 0.5), 1)
  close(md_signed_rank_sensitivity(rep(0, 4), c(1, 2, Inf))$p_two_sided_upper_bound,
    rep(1, 3))
  for (d in list(1:6, -(1:6), c(1, -2, 3, -4, 6), c(-1, 2, -3, 5, 8, 9))) {
    close(md_signed_rank_sensitivity(d, 1)$p_two_sided_upper_bound,
      wilcox.test(d, exact = TRUE, correct = FALSE)$p.value)
  }
  close(md_signed_rank_sensitivity(1:6, 1)$p_two_sided_upper_bound, 2 / 2^6)
  # The rejected 2*max(tails) formula would return one in this fixture.
  r <- md_signed_rank_sensitivity(1:6, 1)
  stopifnot(min(1, 2 * max(r$p_greater_upper_bound, r$p_less_upper_bound)) == 1)
  d <- c(0, 2, -2, 4, -4, -4)
  r <- md_signed_rank_sensitivity(d, 1)
  stopifnot(r$n_nonzero == 5, r$n_zero == 1, r$doubled_positive_rank_sum == 11,
    r$doubled_total_rank_sum == 30)
  w <- as.integer(2 * rank(abs(d[d != 0])))
  e <- enumerate(w, rep(0.5, length(w)))
  threshold <- max(r$doubled_positive_rank_sum, sum(w) - r$doubled_positive_rank_sum)
  extreme <- e$score >= threshold | e$score <= sum(w) - threshold
  close(r$p_two_sided_upper_bound, sum(e$probability[extreme]))
  close(md_signed_rank_sensitivity(d, c(1, 2))$p_two_sided_upper_bound,
    md_signed_rank_sensitivity(-d, c(1, 2))$p_two_sided_upper_bound)
  # Enumerate every extreme assignment-probability configuration for a small
  # design, verifying exact one-sided supremum and conservative two-sided bound.
  w <- c(2, 4, 6, 8)
  threshold <- 16
  for (gamma in c(1, 1.2, 2, 4)) {
    low <- 1 / (1 + gamma); high <- gamma / (1 + gamma)
    vertices <- as.matrix(expand.grid(rep(list(c(low, high)), length(w))))
    tails <- apply(vertices, 1, function(p) {
      ee <- enumerate(w, p)
      c(upper = sum(ee$probability[ee$score >= threshold]),
        two = sum(ee$probability[ee$score >= threshold | ee$score <= sum(w) - threshold]))
    })
    bound <- md_upper_tail(md_weighted_bernoulli_pmf(w, high), threshold)
    close(max(tails["upper", ]), bound)
    stopifnot(max(tails["two", ]) <= min(1, 2 * bound) + 1e-12)
    # Repeat with pair-specific Gamma_j = Gamma_unit ^ gap_j.
    gaps <- c(0.4, 0.7, 1, 1.3)
    favorable <- plogis(log(gamma) * gaps)
    vertices_dose <- as.matrix(expand.grid(lapply(favorable, function(p) c(1 - p, p))))
    tails_dose <- apply(vertices_dose, 1, function(p) {
      ee <- enumerate(w, p)
      c(upper = sum(ee$probability[ee$score >= threshold]),
        two = sum(ee$probability[ee$score >= threshold | ee$score <= sum(w) - threshold]))
    })
    dose_bound <- md_upper_tail(md_weighted_bernoulli_pmf(w, favorable), threshold)
    close(max(tails_dose["upper", ]), dose_bound)
    stopifnot(max(tails_dose["two", ]) <= min(1, 2 * dose_bound) + 1e-12)
  }
  close(md_signed_rank_sensitivity(c(1, -2, 3, 4), c(1, 2), rep(1, 4))$p_two_sided_upper_bound,
    md_signed_rank_sensitivity(c(1, -2, 3, 4), c(1, 2))$p_two_sided_upper_bound)
  dose_test <- md_signed_rank_sensitivity(c(1, -2, 3, 4), 2, c(0.5, 1, 2, 0.75))
  close(dose_test$min_favorable_assignment_probability, plogis(log(2) * 0.5))
  close(dose_test$max_favorable_assignment_probability, plogis(log(2) * 2))
  sensitivity <- md_signed_rank_sensitivity(c(1:9, -10), seq(1, 3, by = 0.25))
  stopifnot(all(diff(sensitivity$p_two_sided_upper_bound) >= -1e-12))
  effects <- md_pair_effects(c(20, 30), c(10, 10), c(1, 1), c(0, 0))
  close(effects["mean_raw_vote_difference"], 15)
  close(effects["geometric_vote_ratio"], sqrt(6))
  close(effects["geometric_vote_factor_per_SD"], sqrt(6))
  close(md_pair_effects(c(0, 2), c(0, 1), c(1, 1), c(0, 0))["mean_raw_vote_difference"], 0.5)
  stopifnot(is.na(md_pair_effects(c(0, 2), c(0, 1), c(1, 1), c(0, 0))["geometric_vote_ratio"]))
  boot <- md_city_bootstrap(c(20, 30, 8, 4), c(10, 10, 4, 2), rep(1, 4), rep(0, 4),
    c("A", "A", "B", "C"), repetitions = 100, seed = 1)
  stopifnot(boot$n_cities == 3, all(dim(boot$draws) == c(100, 10)))
  cat("All matched-dose inference fixtures passed.\n")
  invisible(TRUE)
}

if (sys.nframe() == 0L) md_self_test()
