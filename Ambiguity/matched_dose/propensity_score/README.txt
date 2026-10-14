PROPENSITY-SCORE MATCHED CONTINUOUS-DOSE ANALYSIS

Run
  Rscript PropensityDose.R
or source the script from R. It finds its project directory without requiring
AMBIGUITY_DIR. The parent matched_dose/.Rlibrary supplies nbpMatching 1.5.6,
nbpInference 1.0.3 and lpSolve. ggplot2 produces the separate figures.

This directory is a separate analysis. It does not alter Ambiguity.R,
Ambiguity.rnw, the paper PDF, its figures, or either fixed candidate CSV.
The earlier exact-context design remains in the parent directory.

DATA AND DOSE
The source is Ambiguity_analysis.csv, containing 1384 complete candidates,
810 men and 574 women. Every vote count is checked against Ambiguity_fixed.csv
and the verified candidate-ID outcome crosswalk before analysis.

A is the existing within-gender standardized atypicality measure. Higher A
means relatively more feminine or less masculine cues among men, and relatively
more masculine or less feminine cues among women. Its original full-gender SD
unit is retained in all subsets. It is not a gender-identity measurement.

The primary population is the retained overlap sample. A secondary analysis
restricts both original femininity and masculinity ratings to 1.75--3.25 before
pairing. This is a central appearance window, not a discontinuity threshold.

PROPENSITY MODEL AND MATCHING
Separate Gaussian conditional dose models are fitted for men and women.
  A | X ~ Normal(mu(X), sigma^2)
X includes age, attractiveness, joint cue level, councillor incumbency,
parliamentary office, party indicators, and log municipal valid ballots.
The residual SD follows nbpInference::make.pmatrix's implementation exactly.

Under this homoskedastic Gaussian model, mu(X) is a scalar balancing score
for the entire conditional dose distribution. The script stores the observed
GPS density separately. Matching two observed GPS DENSITIES at different doses
would not provide the same guarantee, so those densities are not naively paired.

nbpMatching performs optimal nonbipartite pairing without replacement, minimizing
absolute score distance. No city, party, or office equality is required.
Candidates must satisfy all of the following outcome-free calipers.
  Score distance <= 0.20 full-gender-sample score SDs
  Fitted probability of the observed unordered-dose allocation in [0.45,0.55]
  A difference from 0.50 to 1.50 original SDs
  Age difference <= 10 years
  Attractiveness difference <= 0.50 rating points
  Joint cue level difference <= 0.75 SDs
  Ratio of municipal valid-ballot totals <= 1.50

Dummy nodes allow candidate exclusion. Maximum feasible pair cardinality is the
first objective, followed by score proximity. Integer rounding of score costs
is documented in the script. A second, outcome-free lpSolve step selects the
largest balanced subset of these fixed pairs, then minimizes score distance.
Every party indicator, including the regression reference party, and every
other score covariate must have absolute standardized mean difference <= 0.10.
The denominator is its full-gender-sample SD. This is not a search over all
possible balanced re-pairings. Pair IDs are saved before joining vote outcomes.

ESTIMAND AND R PACKAGE INFERENCE
Each pair is explicitly oriented higher dose first. If q_j is the fitted
probability of its observed allocation and g_j is the positive dose gap,
  q_j = logistic[g_j * (mu_high - mu_low) / sigma^2].
The formula is checked against nbpInference::make.pmatrix for all candidates.
High-first orientation also avoids an inconsistency between that function's
symmetric observed-allocation matrix and downstream estimator branch ordering.

nbpInference::bias.corrected.neyman estimates
  sum_j[(Y_high - Y_low)/q_j] / [2 * sum_j(g_j)].
nbpInference::covAdj.variance supplies the corresponding approximate Neyman
interval, with an intercept-only Q matrix. The script verifies the algebra.

If the assignment model and causal assumptions hold, the target is the
dose-gap-weighted average finite-dose response among retained candidates.
Formally it is
  sum_j sum_{k=1}^2 [Y_jk(a_high) - Y_jk(a_low)] / [2 * sum_j(g_j)].
This is local to these candidates and observed dose intervals. Reporting it
per SD normalizes these contrasts, rather than identifying a universal +1 SD
intervention or a derivative at an arbitrary facial coordinate. It is not an
instrumental-variable LATE and involves no instrument-induced compliers.

Raw personal votes are the primary outcome, matching the substantive question.
Log votes summarize a geometric response, not the paper's Poisson arithmetic
expected-vote ratio. Votes per 10,000 municipal valid ballots assess support
relative to the municipal voting pool. These are distinct outcome estimands.

The intervals condition on selected pairs and supplied fitted probabilities.
They do not propagate score estimation, matching selection, or municipality
dependence. Cross-city pairs cannot use the former same-city bootstrap without
redesigning the resampling. Independent pair assignment is an additional model
condition. Good measured balance does not establish that assumption.

HIDDEN-BIAS SENSITIVITY
gps_inference_helpers.R supplies exact integer-rank Bernoulli calculations,
verified against brute-force enumeration fixtures with asymmetric probabilities,
ties, zeros, and hidden-bias vertices. This is a custom supplement to the
package estimator, rather than an undocumented package sensitivity function.

Gamma is a multiplicative change in allocation odds per ONE original SD of A.
For gap g_j the hidden log-odds change is bounded by +/- log(Gamma)*g_j,
around the measured Gaussian assignment log odds, not around an automatic 0.5.
The two directional signed-rank bounds use their own probability vectors.
Two-sided bounds are the capped doubled minimum of the directional upper bounds.

These calculations test a sharp null of no outcome change under the permitted
dose swaps. They do not test only a zero average slope. Gamma=1 allows no hidden
bias beyond the fitted assignment model. Exact refers to calculation of that
conditional distribution, not an unconditional guarantee with estimated scores.

IDENTIFICATION LIMITS
Relaxing exact city and party matching increases support but changes which
candidates supply comparisons. Party indicators and ballot volume do not remove
all local party organization, campaigning, networks, or municipality differences.
Ballot volume is realized same-election context, not a strictly preexposure
population measurement. The fixed data contain no separate population variable.

Attractiveness and joint cue level are co-measured facial perceptions. Controlling
them defines comparisons of similar measured appearance profiles and does not
automatically identify a total effect, a biological effect, or a direct effect
excluding every attractiveness pathway. Candidate self-selection, rating error,
a well-defined facial-profile intervention, and electoral interference remain
substantive identification issues. The design does not document randomization.

METHODOLOGICAL REFERENCES
Lu, Zanutto, Hornik, and Rosenbaum (2001). Matching with Doses in an Observational
Study of a Media Campaign Against Drug Abuse. JASA 96(456), 1245--1253.
https://doi.org/10.1198/016214501753381896

Lu, Greevy, Xu, and Beck (2011). Optimal Nonbipartite Matching and Its Statistical
Applications. The American Statistician 65(1), 21--30.
https://doi.org/10.1198/tast.2011.08294

Frazier, Heng, and Zhou (2026). Bias mitigation in matched observational studies
with continuous treatments. Calipered non-bipartite matching and bias-corrected
estimation and inference. Biometrics 82(1), ujag022.
https://doi.org/10.1093/biomtc/ujag022

This implements disjoint GPS-based dose pairing and the bias-corrected estimator,
not Wu et al.'s CausalGPS exposure-grid matching-with-replacement ERF estimator.
The latter has a different target and requires different uncertainty methods.
