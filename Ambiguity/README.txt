Ambiguity
Outi Sarpila (first author) and Hector Bahamonde
Initial project: 12 October 2026

Main deliverables
  Ambiguity.R       Named R code chunks; runs independently or through the RNW.
  Ambiguity.rnw     Editable paper-style draft, with equations and live results.
  Ambiguity.bib     Journal references and Titiunik's methodological book chapter.
  Highlights.txt    Submission highlights regenerated from the manuscript's model objects.
  Ambiguity_fixed.csv
                    All 1,389 candidates, with corrected official names and vote counts.
  Ambiguity_analysis.csv
                    Exact 1,384-row analysis dataset, including all constructed measures.
  Ambiguity.pdf     Compiled main paper and Online Appendix with live quantities and tables.
  build.R           Rebuilds the full analysis and PDF.
  Literature_review.txt and Literature_search_log.txt
                    Extended peer-reviewed scoping review, access tiers, exclusions,
                    67 executed literature searches, and a defensible gap.
  figures/          Predicted votes and municipal ballot-share curves, relative vote index,
                    coefficient estimates, raw joint support, exploratory surface;
                    vector PDF and 300-dpi PNG versions.
  results/          Model objects, clustered covariance matrices, coefficients and
                    contrasts, diagnostics, sample flow, scale constants, source hash,
                    provenance check, nonlinear sensitivities and R session details.

Data and original files
  The analysis loads ../dat.Rdata into an isolated environment. It does not source
  Voting_Phys_App.R, rerun cleaning, or overwrite the original dataset.
  It joins the official electoral outcome in memory using audit/verified_photo_candidate_crosswalk.csv,
  keyed and checked by photograph ID + municipality + official candidate number. All 1,389
  photograph identities were verified against id.xls and phys_cong.dta, and matched uniquely
  to the official 2017 candidate returns. Saved appearance variables and controls are retained.
  Immediately before estimation, it exports Ambiguity_fixed.csv and Ambiguity_analysis.csv.
  The analysis CSV is reloaded and its original factor/reference levels restored before fitting.
  Every exported count and every substantive model response is checked against official votes.
  turnout and official_votes contain corrected counts. turnout_saved and firstname_saved
  preserve the original erroneous values for provenance; the models use corrected turnout.
  Expected source MD5 at project creation: 01ce19c680957edb649e1c35c2a72060
  The saved source has 1,389 candidates; the common primary sample has 1,384
  (810 men, 574 women) across 221 actual municipalities, identified by 'city'.
  The source column called 'municipality' holds region codes and is not used as municipal FE.

Reproduction
  Open this directory in R/RStudio, or in a terminal change to this directory.
  Run the analysis alone: Rscript Ambiguity.R
  Rebuild the analysis and paper: Rscript build.R
  To knit in R: knitr::knit('Ambiguity.rnw', output='Ambiguity.tex')
  The bibliography needs biber; build.R handles an installed macOS universal-binary
  extraction issue locally without changing TeX installation or installing packages.
  The R script detects its source path when sourced from elsewhere. When running selected
  code in an editor, it falls back to /Users/hectorbahamonde/research/Physical/Ambiguity.
  No working-directory change or environment variable is required. AMBIGUITY_DIR and
  AMBIGUITY_DATA remain optional overrides for another project or source-data location.

Interpretation
  The manuscript classifies the design as a natural experiment under Titiunik (2021),
  explicitly as a subtype of observational study. This classification assumes that
  uncontrollable inherited/developmental appearance variation creates a probabilistic
  component in perceived cue profiles among the candidates represented by the study.
  The assignment law is not recovered from the data. The classification does not
  establish equiprobability, exchangeability, or causal identification. Sampling and
  photograph-to-rater randomization are separate measurement procedures.
  The first Online Appendix section formalizes the assignment argument, continuous
  support, measurement, candidate selection, and the additional causal assumptions.
  The primary predictor is the gender-relative atypical cue contrast A. The main model
  includes A and its gender interaction, age, attractiveness, and party and city fixed effects.
  Joint cue level S and its gender interaction enter only an appendix sensitivity model.
  A may increase because the counter-stereotypical cue rises, the stereotypical cue falls,
  or both; primary results do not condition on S. The joint model holds S fixed when
  interpreting A, and the quadratic surface extends that joint-adjusted linear model.
  The two indices jointly are an invertible rotation of F/M; the primary A-only model
  summarizes one targeted aspect of appearance. Neither measures nonbinary identity,
  intentional expression, or category uncertainty. Raters compared candidates with others
  of the same age and gender, and separate samples rated F/M. Intervals condition on
  candidate mean ratings; observational associations do not establish perceptual mechanisms.
  The internal S column remains coelevation. Public appendix labels say Joint cue level.

Initial findings and provenance (updated after full national audit)
  The original source contains 668,400 precinct rows and 33,618 municipal-total rows.
  The original sum grouped and added both, exactly doubling every named candidate's total.
  Its surname/party/city-only merge then attached another person's first name in 68 rows;
  67 have the wrong person's vote count even after halving (66 in the primary model sample).
  A further first-name difference is only quotation formatting. See audit/VOTE_AUDIT_MEMO.txt
  and audit/vote_provenance_memo.txt for the complete evidence chain and published checksum.
  We use verified official votes for the correct photographed candidate, preserving dat.Rdata.
  Current primary, joint-adjusted and office-adjusted estimates are generated in
  results/analysis_summary.txt and results/contrasts.csv. The saved models include primary
  A-only main, optional joint-level adjustment joint, and the joint quadratic surface surface.
  The source MD5 remains 01ce19c680957edb649e1c35c2a72060.
  Official archive: https://tulospalvelu.vaalit.fi/KV-2017/fi/ladattavat_tiedostot.html

Prediction curves
  Main Figure 1 shows expected personal-vote counts from the A-only baseline-controls model,
  without a legend. The curves
  vary A over its gender-specific percentile range and average over observed controls,
  municipalities and parties. They use one-dimensional support in A and do not hold S=0.
  The Online Appendix presents the same model as municipal vote shares, using
  figures/oa_baseline_predicted_vote_share.pdf. These predictions divide each contextual expected count by the municipality's
  total valid votes, then average within gender. These are average local ballot shares,
  not winning probabilities or probabilities for a randomly sampled Finnish voter.
  This alternative scale supplies no independent model or test. The Online Appendix also has
  separate main-versus-joint and main-versus-office comparisons.
  Only joint-adjusted curves hold S=0 and apply two-dimensional A/S support.
  All figure panels are square with visible borders and no inner grid. Confidence and
  support settings are centralized in paper_values. Prediction exports record which
  models fix joint level and whether support is one- or two-dimensional.
  Main figures use individual ggplot2 fitted panels, aligned with separate observation-rug
  strips through gridExtra. Central-range x limits keep outlying rug values from widening
  the fitted display. Rugs show only candidates in the displayed window; headings report
  full-group sample sizes. Figure 1 retains its conditional-mean confidence band.
  Main Figure 2 uses the secondary gender-by-atypicality-by-party PPML specification.
  It includes parties meeting the minimum per-gender support requirement, preserves the
  primary sample's A scaling, and averages predictions within each party-by-gender cell.
  The panels show observed-candidate rugs and cell counts, with separate vertical scales
  and a common cue axis. Figure 2 shows substantive point-estimate patterns without bands;
  the dynamic note reports how many one-SD contrasts are not statistically significant.
  Full uncertainty and joint heterogeneity tests remain in the Online Appendix.
  Party curves are evaluated continuously over each gender-by-party cell's 10th--90th
  percentile range, with one solid curve per party and gender. Local observation counts
  remain diagnostic metadata; they do not filter or segment the plotted lines.
  The rugs indicate the distribution of observed candidates.
  The theory states conformity, cooperative-leadership, and contextual expectations.
  Main results interpret both figures against these expectations and prior research;
  Fig. 2 prediction mechanics are in a footnote. Party-profile interpretation distinguishes
  an atypicality reward from a common feminine cue balance, using the index's gender sign.
  The Online Appendix party-sensitivity table reports the Social Democratic, National Coalition, and Green male contrasts,
  including the Green male direction reversal after upper-tail exclusion.
  Candidate party is not a direct measure of voter ideology. Detailed contrasts and
  targeted office/upper-tail sensitivity checks are reported in the Online Appendix.

Literature boundary
  Gender presentation and real electoral performance are already studied, including
  Hehman2014, Carpinella2016, Wojcik-Mullenax2024, Park-Joo2023 and Naunov2026.
  The contribution is a gender-relative atypicality contrast and a Finnish personal-vote
  test, with joint configurations as appendix sensitivities, not a first-ever elections claim. The search is thorough
  and bounded; it is not a registered systematic review. Some papers were accessible only
  through publisher abstracts, explicitly marked in the evidence review.

Contacts verified on official institutional pages on 12 October 2026
  https://www.utu.fi/en/people/outi-sarpila
  https://www.utu.fi/en/people/hector-bahamonde
  https://www.abo.fi/en/contact/hector-bahamonde/

Publication structure
  The main RNW reports the full baseline-controls specification (R key: main) and a
  secondary party-specific specification (R key: party_heterogeneity).
  The R key baseline means the minimally adjusted model and is a different specification.
  Joint cue level, incumbency/office, other adjustments, and quadratic models appear in the Online
  Appendix, where figures, tables and equations use OA numbering. Study metadata are
  centralized in paper_values, while sample sizes and reported estimates derive from data
  and fitted models. The Rnw consumes generated LaTeX tables from the paper-tables chunk.
  Inequality is interpreted as differences in conditional expected support within gender
  categories, with potential relevance to representation because personal votes rank candidates
  within lists. The models do not estimate seat-winning probabilities or identify discrimination.
  Result-direction and gender-comparison sentences are generated from current estimates and
  confidence intervals in the paper-values chunk.
  Theoretical interpretation develops context-dependent leadership fit using verified
  short quotations from Walker and Wanke (2017), Carpinella and Johnson (2013),
  Spisak and colleagues (2012), and Laustsen and Petersen (2016), with campaign-language
  counterevidence from Wang, Merolla and Manganiello (2023). These motivate hypotheses
  about trait inferences; the observational data do not measure or identify those mechanisms.

Attractiveness comparison (Figure 3)
  Figure 3 compares actual paired candidate ratings by gender. Signed balance B=A for men
  and B=-A for women, so higher B is more feminine relative to masculine cues in both groups.
  Marginal quantile diagnostics remain available as separate analysis outputs.
  The separate attractiveness_models object retains all other primary controls and the same
  complete sample. It compares attractiveness omitted, the existing linear primary model,
  and gender-specific natural cubic splines (3 df per gender). Municipality-clustered inference
  is unchanged. The original models/vcov objects and data exports are not modified.
  Numerical diagnostics are exported as attractiveness_effects.csv, attractiveness_correlations.csv,
  attractiveness_pairs.csv, attractiveness_qq.csv, and attractiveness_diagnostics.csv in results/.
  The new manuscript section is 5.2; its Table 3 and interpretation update dynamically.
