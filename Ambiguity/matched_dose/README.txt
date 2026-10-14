Separate matched-dose analysis

This folder leaves Ambiguity.R, Ambiguity.rnw, the paper PDF, the original data,
the fixed CSV files and all existing manuscript figures unchanged.

Run
  Rscript MatchedDose.R

R packages
  nbpMatching 1.5.6 performs optimal nonbipartite matching.
  nbpInference 1.0.3 supplies classic.neyman and covAdj.variance.
  lpSolve selects the largest covariate-balanced subset of the Lu pairs.
  sensitivitymv 1.4.4 supplies a supplementary common-pair hidden-bias analysis.
  ggplot2 generates the figures. jsonlite records the settings.

Packages were installed in the local .Rlibrary directory, which is ignored by Git.
On another machine install these packages into that directory, or the normal R
library. The script prepends .Rlibrary to .libPaths(). No global packages are removed.

Files
  MatchedDose.R is the complete analysis driver.
  inference_helpers.R contains the independently checked inference functions.
  results/design_settings.json freezes all matching constraints.
  results/pair_ids_outcome_free.csv records the pairs before vote outcomes are joined.
  results/matched_pairs.csv contains pair identities, doses and fixed personal votes.
  results/covariate_balance.csv reports balance and within-pair covariate gaps.
  results/effect_estimates.csv contains raw and log dose contrasts and intervals.
  results/hidden_bias_sensitivity.csv gives exact dose-aware sensitivity bounds.
  results/package_common_pair_sensitivity.csv contains the separate package M test.
  results/analysis_summary.txt gives the current results and their interpretation.
  figures/matched_dose_results.pdf contains effect, sensitivity and balance plots.

Design
  Existing within-gender A scaling is preserved. The dose remains continuous.
  Match exactly on recorded gender, municipality, party, councillor incumbency and
  parliamentary office. Permit age gaps up to 10 years, attractiveness gaps up to
  0.5 rating points, joint cue level gaps up to 0.75 SD, and A gaps of 0.5 to 1.5 SD.
  No outcome enters the matching score, edge restrictions or subset selection.

  Lu's distance is squared midrank balancing-score distance divided by squared
  midrank A-dose distance, with a small tie-resolving constant. The balancing score
  comes from a continuous Gaussian A regression on age, attractiveness, joint cue
  level, political office, party and municipality. Its scalar interpretation requires
  constant dose-error variance. The matched data also obey direct covariate calipers.

  nbpMatching uses dummy nodes to discard unmatched candidates. Penalties prioritize
  maximum admissible pair count, then minimum Lu distance, with nine-digit precision.
  A second outcome-free step retains the largest subset of these Lu pairs with
  absolute high-minus-low standardized mean differences no greater than 0.10 for
  age, attractiveness and joint cue level. Ties minimize total Lu distance.
  This two-stage selection is not a globally optimal balanced re-pairing of all edges.

  The primary scope includes all candidates with admissible counterparts. Two
  prespecified checks restrict both raw ratings to 1.75-3.25 and 2-3 respectively.
  These raw coordinates are gender-referenced ratings, not classification boundaries.

Estimand and inference
  For pair j, g_j = A_high - A_low and d_j = votes_high - votes_low.
  nbpInference::classic.neyman computes sum(d_j)/sum(g_j). Under fair within-pair
  assignment it estimates a dose-gap-weighted local average causal slope over the
  observed finite contrasts. It is not the mean of d_j/g_j, a global average treatment
  effect, a point derivative, or an IV complier LATE.

  Neyman uncertainty uses nbpInference::covAdj.variance with intercept-only Q and
  fair pair probabilities. Fixed-match city-cluster percentile bootstrap intervals
  are also reported. Neither interval accounts for reconstructing matches in new
  samples, appearance-rating error, or hidden confounding. The log outcome is a
  separate geometric-mean contrast, rather than an arithmetic expected-vote ratio.

  Conditional exchangeability is an assumption about assignment to cue profiles.
  Balance makes it more assessable but does not prove it. A profile intervention and
  assumptions concerning competition between candidates are also needed for a causal
  interpretation. Holding attractiveness similar can condition on a possible pathway
  from gendered appearance, so it is not automatically a pure ambiguity effect.

Hidden bias
  The primary test conditions on fixed pairs, unordered doses and the complete vote
  vector under the sharp null of no effects under any allowable dose swap. Independent
  pair assignments are assumed. Doubled Wilcoxon midranks handle tied vote differences
  exactly. Zero differences are removed. Dynamic programming computes the weighted
  Bernoulli distribution without a normal approximation.

  The primary continuous selection model is f(a|x,u) proportional to h(a,x)exp(gamma*a*u),
  0<=u<=1, with observed assignment information sufficiently matched. For pair j the
  allocation odds bound is Gamma_j = Gamma_1SD ^ g_j. Worst-case one-sided tails are
  exact under this model. Their doubled observed-direction tail is a conservative
  two-sided bound. At Gamma=1 it equals the exact fair-allocation two-sided p-value.

  Standard sensitivity packages do not accept heterogeneous pair-specific dose bounds.
  The custom calculation is therefore necessary for the primary continuous-dose model.
  The sensitivitymv Huber M test uses a common pair-level Gamma and a normal approximation;
  it is supplementary and is not an interchangeable implementation of the primary test.

Primary methodological sources
  Lu, Zanutto, Hornik and Rosenbaum (2001), Journal of the American Statistical Association.
  https://doi.org/10.1198/016214501753381896
  Lu, Greevy, Xu and Beck (2011), The American Statistician.
  https://doi.org/10.1198/tast.2011.09294
  Rosenbaum (1987), Biometrika.
  https://doi.org/10.1093/biomet/74.1.13
  Heng, Zhang, Han, Lorch and Small (2023), JRSS Series A, section 4.3.
  https://doi.org/10.1093/jrsssa/qnad075

Package documentation
  https://CRAN.R-project.org/package=nbpMatching
  https://CRAN.R-project.org/package=nbpInference
  https://CRAN.R-project.org/package=sensitivitymv
  nbpInference documents the Frazier, Heng and Zhou (2024) preprint for its newer
  bias-corrected methods. This analysis uses its classical estimator and intercept-only
  conservative Neyman variance, with their algebra independently verified.
