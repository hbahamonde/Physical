# Inference helpers for outcome-free Gaussian dose-assignment-score matching.
# Every pair is oriented with the OBSERVED higher-dose candidate first.
# q_j is the fitted probability that this particular candidate receives the
# higher of the pair's two doses, conditional on X and their unordered doses.
# The q_j must NOT be replaced by 0.5 after matching.
#
# make.pmatrix() returns symmetric observed-allocation probabilities despite
# ambiguous package documentation. High-first orientation is essential when
# handing those values to nbpInference's bias-corrected estimator/variance.
#
# Exact DP below is conditional on FIXED supplied allocation probabilities,
# fixed matches, and independent pair allocation. Fitted-GPS uncertainty and
# matching-selection uncertainty are not propagated. It is model-conditional
# randomization inference, not a claim that estimated probabilities are known.

gps_weighted_bernoulli_pmf <- function(weights, probabilities) {
  if (!is.numeric(weights) || anyNA(weights) || any(!is.finite(weights)) ||
      any(weights <= 0 | weights != round(weights))) stop("Invalid integer weights.")
  n <- length(weights)
  if (length(probabilities) == 1L) probabilities <- rep(probabilities, n)
  if (length(probabilities) != n || anyNA(probabilities) ||
      any(!is.finite(probabilities)) || any(probabilities < 0 | probabilities > 1)) {
    stop("Invalid Bernoulli probabilities.")
  }
  pmf <- 1
  for (i in seq_len(n)) {
    w <- as.integer(weights[i]); at <- seq_along(pmf)
    updated <- numeric(length(pmf) + w)
    updated[at] <- (1 - probabilities[i]) * pmf
    updated[at + w] <- updated[at + w] + probabilities[i] * pmf
    pmf <- updated
  }
  if (abs(sum(pmf) - 1) > 1e-9) stop("PMF mass check failed.")
  pmf
}

gps_upper_tail <- function(pmf, threshold) {
  threshold <- ceiling(threshold)
  if (length(threshold) != 1L || !is.finite(threshold)) stop("Invalid threshold.")
  if (threshold <= 0) return(1)
  if (threshold >= length(pmf)) return(0)
  min(1, max(0, sum(pmf[seq.int(threshold + 1L, length(pmf))])))
}

gps_gaussian_observed_allocation <- function(a_high, a_low, mean_high, mean_low, sigma) {
  if (length(sigma) != 1L || !is.finite(sigma) || sigma <= 0) stop("Invalid sigma.")
  vectors <- list(a_high, a_low, mean_high, mean_low)
  if (any(lengths(vectors) != length(a_high)) ||
      any(vapply(vectors, function(x) !is.numeric(x) || anyNA(x) ||
        any(!is.finite(x)), logical(1))) || any(a_high <= a_low)) stop("Invalid high-first vectors.")
  # Stable equivalent to the density-product ratio in make.pmatrix().
  plogis((a_high - a_low) * (mean_high - mean_low) / sigma^2)
}

gps_signed_rank_sensitivity <- function(differences, observed_allocation_probabilities,
    dose_gaps, gammas = c(1, 1.1, 1.25, 1.5, 2, 3)) {
  q <- observed_allocation_probabilities
  n <- length(differences)
  vectors <- list(differences, q, dose_gaps)
  if (any(lengths(vectors) != n) ||
      any(vapply(vectors, function(x) !is.numeric(x) || anyNA(x) ||
        any(!is.finite(x)), logical(1)))) stop("Invalid aligned pair vectors.")
  if (any(q <= 0 | q >= 1) || any(dose_gaps <= 0)) {
    stop("Allocation probabilities must lie strictly inside (0,1); gaps must be positive.")
  }
  if (!is.numeric(gammas) || !length(gammas) || anyNA(gammas) || any(gammas < 1)) {
    stop("gammas must be >=1; Inf is allowed.")
  }
  keep <- differences != 0
  d <- differences[keep]; q <- q[keep]; gap <- dose_gaps[keep]
  weights <- as.integer(round(2 * rank(abs(d), ties.method = "average")))
  observed <- sum(weights[d > 0]); total <- sum(weights)
  # Under the sharp null, whichever candidate has the larger fixed outcome
  # contributes a positive sign if assigned high dose. Its baseline assignment
  # log odds are sign(d_j)*logit(q_j), not automatically zero.
  positive_logodds <- sign(d) * qlogis(q)
  do.call(rbind, lapply(gammas, function(gamma) {
    hidden_shift <- if (is.infinite(gamma)) rep(Inf, length(gap)) else log(gamma) * gap
    positive_upper <- plogis(positive_logodds + hidden_shift)
    negative_upper <- plogis(-positive_logodds + hidden_shift)
    greater <- gps_upper_tail(gps_weighted_bernoulli_pmf(weights, positive_upper), observed)
    less <- gps_upper_tail(gps_weighted_bernoulli_pmf(weights, negative_upper), total - observed)
    # These nulls need not be symmetric. Compute the two directions with their
    # own probability vectors, then use the doubled-minimum bound. Reusing a
    # single probability vector with max(T,W-T) is invalid when q_j != 0.5.
    data.frame(gamma_per_SD = gamma, n_pairs = n, n_nonzero = length(d),
      n_zero = sum(!keep), doubled_positive_rank_sum = observed,
      doubled_total_rank_sum = total,
      p_greater_upper_bound = greater, p_less_upper_bound = less,
      p_two_sided_upper_bound = min(1, 2 * min(greater, less)),
      exact_equal_tailed_p_conditional_on_supplied_q = gamma == 1)
  }))
}

gps_ht_effects <- function(y_high, y_low, a_high, a_low, observed_allocation_probabilities) {
  q <- observed_allocation_probabilities
  vectors <- list(y_high, y_low, a_high, a_low, q)
  n <- length(y_high)
  if (!n || any(lengths(vectors) != n) ||
      any(vapply(vectors, function(x) !is.numeric(x) || anyNA(x) ||
        any(!is.finite(x)), logical(1))) || any(a_high <= a_low) ||
      any(q <= 0 | q >= 1) || any(y_high < 0 | y_low < 0)) stop("Invalid pair inputs.")
  gap <- a_high - a_low; raw <- y_high - y_low
  logs <- if (all(y_high > 0 & y_low > 0)) log(y_high) - log(y_low) else rep(NA_real_, n)
  c(n_pairs = n, mean_dose_gap = mean(gap),
    unweighted_descriptive_mean_vote_difference = mean(raw),
    HT_mean_finite_vote_contrast = sum(raw / q) / (2 * n),
    HT_raw_vote_effect_ratio = sum(raw / q) / (2 * sum(gap)),
    HT_mean_finite_log_vote_contrast = sum(logs / q) / (2 * n),
    HT_log_vote_effect_ratio = sum(logs / q) / (2 * sum(gap)),
    HT_geometric_dose_factor = exp(sum(logs / q) / (2 * sum(gap))))
}

# The HT log effect ratio is a finite-dose response summary. Interpreting its
# exponential as a common causal factor for +1 SD additionally assumes a common
# constant log-linear response. It is not the main paper's PPML mean ratio.

gps_inference_self_test <- function() {
  close <- function(x, y) stopifnot(isTRUE(all.equal(x, y, tolerance = 1e-12,
    check.attributes = FALSE)))
  enumerate <- function(weights, fixed_outcome_signs, q) {
    n <- length(weights)
    if (!n) return(list(score = 0, probability = 1, pmf = 1))
    assignment <- as.matrix(expand.grid(rep(list(0:1), n)))
    positive_sign <- assignment
    positive_sign[, fixed_outcome_signs < 0] <- 1 - assignment[, fixed_outcome_signs < 0, drop = FALSE]
    score <- as.vector(positive_sign %*% weights)
    probability <- apply(assignment, 1, function(a) prod(ifelse(a == 1, q, 1 - q)))
    pmf <- numeric(sum(weights) + 1L)
    for (i in seq_along(score)) pmf[score[i] + 1L] <- pmf[score[i] + 1L] + probability[i]
    list(score = score, probability = probability, pmf = pmf)
  }
  d <- c(1, 2, -3, 4); q <- c(0.52, 0.48, 0.57, 0.43)
  gap <- c(0.5, 0.75, 1.25, 1.5)
  w <- as.integer(2 * rank(abs(d))); observed <- sum(w[d > 0])
  e <- enumerate(w, sign(d), q)
  close(gps_weighted_bernoulli_pmf(w, ifelse(d > 0, q, 1 - q)), e$pmf)
  r <- gps_signed_rank_sensitivity(d, q, gap, 1)
  close(r$p_greater_upper_bound, sum(e$probability[e$score >= observed]))
  close(r$p_less_upper_bound, sum(e$probability[e$score <= observed]))
  close(r$p_two_sided_upper_bound, min(1, 2 * min(r$p_greater_upper_bound, r$p_less_upper_bound)))
  # Biased assignment differs sharply from fair flips, even at Gamma=1.
  close(gps_signed_rank_sensitivity(1:6, rep(0.9, 6), rep(1, 6), 1)$p_two_sided_upper_bound,
    min(1, 2 * 0.9^6))
  close(gps_signed_rank_sensitivity(1:6, rep(0.5, 6), rep(1, 6), 1)$p_two_sided_upper_bound,
    2 / 2^6)
  # Enumerate all extreme allowed allocation probability configurations.
  for (gamma in c(1, 1.2, 2, 4)) {
    lo <- plogis(qlogis(q) - log(gamma) * gap)
    hi <- plogis(qlogis(q) + log(gamma) * gap)
    vertices <- as.matrix(expand.grid(lapply(seq_along(q), function(i) c(lo[i], hi[i]))))
    tails <- apply(vertices, 1, function(prob) {
      ee <- enumerate(w, sign(d), prob)
      c(greater = sum(ee$probability[ee$score >= observed]),
        less = sum(ee$probability[ee$score <= observed]))
    })
    rr <- gps_signed_rank_sensitivity(d, q, gap, gamma)
    close(rr$p_greater_upper_bound, max(tails["greater", ]))
    close(rr$p_less_upper_bound, max(tails["less", ]))
    stopifnot(max(pmin(1, 2 * pmin(tails["greater", ], tails["less", ]))) <=
      rr$p_two_sided_upper_bound + 1e-12)
  }
  # Zero differences are omitted with q and gap kept aligned; ties remain exact.
  dt <- c(0, 2, -2, 4, -4, -4)
  qt <- c(0.51, 0.49, 0.54, 0.46, 0.55, 0.45)
  gt <- c(0.5, 0.6, 0.7, 0.8, 0.9, 1)
  rr <- gps_signed_rank_sensitivity(dt, qt, gt, c(1, 2))
  stopifnot(all(rr$n_zero == 1), all(rr$n_nonzero == 5),
    all(rr$doubled_positive_rank_sum == 11), all(rr$doubled_total_rank_sum == 30))
  close(gps_signed_rank_sensitivity(-dt, qt, gt, c(1, 2))$p_two_sided_upper_bound,
    rr$p_two_sided_upper_bound)
  close(gps_signed_rank_sensitivity(rep(0, 3), c(0.4, 0.5, 0.6), rep(1, 3), c(1, 2, Inf))$p_two_sided_upper_bound,
    rep(1, 3))
  close(gps_gaussian_observed_allocation(2, 0, 1.5, 0.5, 1), plogis(2))
  # Direct Gaussian density ratio agrees with the stable formula.
  numerator <- dnorm(2, 1.5, 1) * dnorm(0, 0.5, 1)
  swapped <- dnorm(0, 1.5, 1) * dnorm(2, 0.5, 1)
  close(gps_gaussian_observed_allocation(2, 0, 1.5, 0.5, 1), numerator / (numerator + swapped))
  h <- gps_ht_effects(c(20, 30), c(10, 10), c(1, 2), c(0, 0), c(0.6, 0.4))
  close(h["HT_mean_finite_vote_contrast"], (10 / 0.6 + 20 / 0.4) / 4)
  close(h["HT_raw_vote_effect_ratio"], (10 / 0.6 + 20 / 0.4) / 6)
  cat("All GPS assignment-aware inference fixtures passed.\n")
  invisible(TRUE)
}

if (sys.nframe() == 0L) gps_inference_self_test()
