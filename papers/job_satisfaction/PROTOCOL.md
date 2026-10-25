# How personality shapes job satisfaction: analysis decisions (version 2)

Exploratory analysis plan, 15 October 2026. Not a preregistration. Version 1
(14 October; Part I trait associations plus a Part II on reference wages) is kept
in `archive_v1/` with the paper it produced. On 15 October the author dropped
Part II (reference wages, status, envy), whose results did not support a paper,
and asked for Part I to be rebuilt around the mechanisms that link personality
to job satisfaction. Target journal: International Journal of Manpower.

Known before version 2 (from version 1): the five traits predict satisfaction in
all 24 countries (emotional stability ↔ not satisfied in 21; extraversion ↔
extremely satisfied in 22); coarse job blocks account for little of the
emotional-stability association and for part of the extraversion one;
self-reported skill underuse predicts dissatisfaction in 22 countries. Every
result below that uses the new job-condition measures is new. Decisions were
fixed after inspecting variable labels, coding and coverage only.

## Framework

Three channels through which a trait can raise reported job satisfaction,
following the differential exposure–reactivity model (Bolger and Zuckerman 1995),
the job demands–resources model (Bakker and Demerouti 2007) and trait activation
theory (Tett and Burnett 2003):

1. **Exposure** — traits select workers into jobs with fewer demands and more
   resources, and the conditions raise satisfaction.
2. **Reactivity** — the same conditions affect satisfaction differently by trait
   (buffering of demands; amplification of resources the trait makes valuable).
3. **Appraisal** — traits change how the same job is perceived and reported:
   reported conditions differ from the conditions of the job, and part of the
   trait association is general mood rather than evaluation of the job.

The quantity to be explained is the P1 trait coefficient of version 1.

## Countries and sample

Unchanged: employees aged 25–65 in the 24 countries with job satisfaction and the
Big Five (Korea's traits from its separate file); every model separately by
country with all ten plausible values and all declared replicate weights (Fay or
JK2; Chile 28), Rubin pooling; BH adjustment within the families defined below;
no pooled or averaged estimates — summaries count countries.

Core set (24 countries): time pressure, repetitive tasks, physical work, change
in the work environment, autonomy, learning, social tasks and skill underuse are
released everywhere. Extended set (17 countries): support and voice come from a
module not fielded in CAN CHE DEU FIN IRL ISR LTU; models that use them are
estimated in the 17 only.

## Job conditions (each standardised within country, weighted)

| Measure | Items | Coding |
|:--|:--|:--|
| `time_pressure` (demand) | H2_Q12 tight deadlines / very high speed | 1 none of the time … 5 all of the time |
| `repetitive` (demand) | H2_Q17 short repetitive tasks | 1 = yes |
| `physical` (demand) | H2_Q07a working physically for a long period | frequency 1–5 |
| `change` (demand) | H2_Q18a01–a06 areas in which the working environment changed | count 0–6 |
| `autonomy` (resource) | H2_Q08a–d sequence, method, speed, hours | mean, 1 not at all … 5 very high extent |
| `learning` (resource) | H2_Q09a–c learning new things, by doing, keeping up to date | mean frequency 1–5 |
| `social` (social tasks) | H2_Q03a–d, H2_Q05a–b sharing, teaching, presenting, clients, influencing, negotiating | mean frequency 1–5 |
| `support` (resource, extended) | H2_Q13a–c | 0 if no assistance available; else mean of supervisor and co-worker assistance (always 1, sometimes 0.5, never 0) |
| `voice` (resource, extended) | H2_Q14a–b involved in improving work organisation; able to apply own ideas | mean, reversed (always 5 … never 1) |
| `underuse` | H2_Q19a skills higher than required (and `deficit`, skills lower) vs matched | as version 1 |

Item-level missing values: mean of the remaining items of the measure; a measure
with no valid item is mean-imputed with a missing indicator. Structural job
controls are the version 1 blocks: occupation; contract and hours; employer
(firm size, sector); supervising others.

## E. Exposure

E1. Gelbach (2016) decomposition of each P1 trait coefficient with blocks:
structural; demands (time pressure, repetitive, physical, change); autonomy;
learning; social tasks; underuse. In the extended set a further version adds
support and voice. Report each block's contribution, the total share and its
interval.

E2. Trait → condition regressions: each condition on the five traits and P1
controls (which traits select into which conditions).

E3 (13 wage countries). E1 with a pay block (log hourly wage) added last.

Hypotheses: HE1 the exposure share is small (< 25%) for emotional stability;
HE2 it is larger for extraversion and runs through social tasks and autonomy.

## R. Reactivity

Model per country and outcome: five traits, all core conditions, P1 controls,
structural job blocks, and all pre-specified trait × condition products below
entered together (SD × SD units: the change in the condition's satisfaction
slope per SD of the trait). Primary outcome: the 1–5 score; binaries reported.

| Pair | Mechanism | Predicted sign (score) |
|:--|:--|:--|
| R1 EMOS × time_pressure | stable workers are less hurt by pressure | + |
| R2 EMOS × repetitive | stable workers are less hurt by monotony | + |
| R3 EMOS × change | stable workers are less hurt by change | + |
| R4 EXTR × social | social tasks reward extraverts | + |
| R5 OPEM × learning | learning rewards the open | + |
| R6 OPEM × underuse | underuse hurts the open more | − |
| R7 CONS × autonomy | autonomy rewards the conscientious | + |
| R8 AGRE × support (extended) | support rewards the agreeable | + |

Placebo pairs (no mechanism; predicted 0): PL1 EXTR × physical; PL2 AGRE ×
repetitive; PL3 CONS × social.

Verdict rule: a pair is supported if it has the predicted sign with BH q < 0.05
in at least half of the countries where it is estimated and the placebos are
not. Families: tested pairs across countries; placebo pairs across countries.

R-forest (agnostic). Per country, a causal forest (grf, 2000 trees, survey
weights) with emotional stability as a continuous treatment; outcome (score) and
treatment residualised on the other four traits and P1 controls; covariates are
all job conditions plus age, sex and education. Report the calibration test and
the best linear projection of the doubly robust scores on the job conditions
(replicate-weight SEs, scores held fixed). Repeated with extraversion.

## A. Appraisal

A1. Perceived versus actual conditions. For each condition, `actual` = weighted
leave-one-out mean in the worker's job cell: country × finest released
occupation (ISCO 2-digit where released, else 1-digit, else the skill level) ×
industry section (ISIC1C where released); cells with fewer than 10 other workers
fall back to occupation alone. `perceived` = report − actual.
(a) Regress `perceived` on traits and P1 controls. HA1: emotional stability
predicts less perceived time pressure and repetitiveness and more perceived
autonomy (and support) beyond the job cell. (b) E1 with each condition split into
actual and perceived parts: how much of the exposure channel is job-cell and how
much perception.

A2. Facets (12 countries with six-item scales). Replace each trait by its three
facets (EMOS: anxiety, depression, volatility, oriented so that higher = more
stable; EXTR: sociability, assertiveness, energy; and so on). HA2: if the
emotional-stability association is mood (appraisal), the depression facet
dominates; if stress reactivity, the anxiety facet dominates and its product
with time pressure is positive. Report facet coefficients and the anxiety ×
time_pressure and depression × time_pressure products.

A3. Life satisfaction as a lower-bound control (version 1), now with job
conditions in the model.

## V. Verification

- V1 Oster (2019) bounds per trait and country: δ that sets the coefficient to
  zero, from the restricted model (traits, age, sex) and the full model (P1 + all
  job conditions), Rmax = min(1.3 R̃, 1); point estimates; share of countries
  with δ > 1.
- V2 Cognitive comparison: P1 with adaptive problem solving (PVAPS, 10 PVs)
  instead of numeracy, and with both. Prediction: no association.
- V3 Tenure gradient (15 countries with tenure): trait × (tenure ≥ 5 years) in
  P1. If jobs shaped trait reports, associations would strengthen with tenure.
  Prediction: no gradient.
- V4 Acquiescence: the BFI-2-XS balances positively and negatively keyed items
  (stated; raw items are not released).
- Version 1 sensitivities retained: logit / ordered logit, six-item scales,
  literacy, self-employed, life satisfaction.

## Dropped

Part II of version 1 (reference wages, status, envy, rank, Senik comparison,
envy forests) and its scripts move to `archive_v1/`. Wages enter only as the E3
pay block.

## Coverage decisions before estimation (15 October 2026)

- A1 is not estimable in Austria and Germany, which release neither occupation
  nor industry codes; A1 covers 22 countries.
- Facet scores are released in 12 countries (CAN CHL CZE DEU ESP EST HRV ITA NZL
  PRT SVK KOR).
- Tenure is released in 15 countries.

## Changes after estimation (version 2)

**15 October 2026, after reviewing all results.**
- **Causal forests (R-forest) dropped from the paper.** The calibration test
  rejected a common slope in 0 of 24 countries for emotional stability and 2 of
  24 for extraversion (after BH); 1 of 452 best-linear-projection terms was
  significant. The author judged them uninformative beyond the pre-specified
  pairs. Code and results are kept in `archive_v1/`.
- **Causal mediation added** as an alternative to the Gelbach decomposition
  (`R/10_mediation.R`), at the author's request. Imai, Keele and Tingley (2010):
  one mediator at a time, 1–5 score, linear mediator and outcome models, so the
  ACME = a × b. Replicate-weight × plausible-value inference. For time pressure
  and autonomy, the mediator is also split into job-cell (actual) and deviation
  (perceived) parts. Equality with `mediation::mediate` was checked on Czechia.
  Sequential ignorability is a stronger assumption than the accounting reading
  of Gelbach, and the paper says so.
- **Target journal.** International Journal of Manpower (Emerald). The framing
  moves to HRM and labour: Emerald structured abstract, practical implications.

**15 October 2026, second review (author's decisions).**
- **Target journal changed to Social Indicators Research (Springer).**
- **Israel excluded** from all estimates in the paper; this is the author's
  decision. It was taken after results were seen. All Israel results remain in
  `results/` and the exclusion is applied in `R/07_collect.R`, before the BH
  adjustment.
- **Causal mediation now leads the exposure analysis.** It covers per-mediator
  ACMEs, a joint ACME through all conditions (VanderWeele and Vansteelandt
  2014) and the Imai–Keele–Yamamoto sensitivity parameter: the error
  correlation at which each ACME is zero. The actual/perceived split is
  extended to learning, social tasks and voice. Gelbach moves to robustness,
  with a plot of its agreement with mediation, because the sequential
  ignorability assumption may not hold.
- **Hypotheses** are numbered in the framing section, and the empirical
  strategy maps each to its test.

## Version 3 (approved by the author, 15 October 2026)

After the author's review of the SIR draft ("weak, messy at the level of
problem, method and visualisation"), the following changes are proposed.
Nothing below has been estimated yet.

- **H1 becomes the subjective-reward hypothesis:** non-cognitive skills are
  associated with higher job satisfaction holding the objective reward (pay)
  and background fixed. Pay enters as within-country deciles of hourly earnings
  (EARNHRBONUSDCLC2), released for employees in 20 of the 23 countries (not
  CHE, FIN, SVK). Continuous earnings exist in only 12. Author's decision: keep all 23 countries; the pay control enters where deciles are released and CHE, FIN and SVK are flagged in every table and figure. H1 is also reported without the pay control (all 23) so that the two can be compared, and pay is added as a mediator (exposure through objective rewards) in the 20 countries.
- **H2 exposure, H3 appraisal, H4 reactivity** keep their content; all
  channel models add the pay control.
- **Mediation is estimated for all five traits** in all countries. Proportions
  mediated are reported only where the trait's total effect has q < 0.05 in that
  country; the number of such countries is stated per trait.
- **Reactivity stays as pre-specified interactions** (the author accepts this
  for lack of a better cross-sectional test).
- **Every by-country table and figure adds the mean and median across
  countries**, labelled as the one place countries are combined (as in the
  wages paper's scorecard).
- **Figures mark three levels:** q < 0.05 (filled), p < 0.10 (half-tone),
  otherwise hollow. The mediation section opens with a path diagram
  (trait → condition → satisfaction) carrying cross-country means of a, b, the
  indirect effect and the direct effect, with country counts on each arrow.
- **Text**: the problem, the literature progression, the contribution and the
  methods are rewritten as continuous prose, not bullet lists.

**Version 3 estimated, 15 October 2026.** Full rerun with the pay decile in
every model; 16 of 16 validation checks pass. One addition after seeing the
results: `R/11_pay_contrast.R` computes, as a scale benchmark only, the
satisfaction gap between the top and bottom earnings decile from the baseline
model (point estimates, one plausible value). The rendered manuscript is
`output/job_satisfaction.html`; version 2 drafts are in `archive_v1/`.

**Version 4 figures, 16 October 2026** (author's review of the version 3 draft;
no re-estimation). Decisions:

- **Significance marking.** Figures and main-text tables use each country's own
  unadjusted test: filled p < 0.05, grey p < 0.10, hollow otherwise. The text
  states, for every count that matters, how many of the significant countries
  survive Benjamini–Hochberg adjustment ("n (m after adjustment)"), and the
  conclusions rest on the adjusted counts. Online Resource 1 repeats the
  mediation and job-cell count tables with adjusted counts (Tables A3–A4). The
  letter "q" no longer appears in the paper; the methods define the convention.
- **One colour for estimates.** No per-trait colours; traits are panels,
  separated by a grey strip and a panel border.
- **Mean and median rows at the bottom of every by-country figure, with 95%
  intervals** computed as in the wages paper (mean: sqrt(sum se²)/K; median:
  20,000 simulated redraws). Countries in alphabetical order (author's choice
  over ordering by effect size). `manuscript/figure_helpers.R` holds the code.
- **Figure set.** Fig. 1 schematic (unchanged); Fig. 2 H1 dot plot; Fig. 3
  direct and indirect effects by country (total = bar from zero for the joint
  indirect effect through all conditions + grey direct effect; pink where the
  indirect effect has the opposite sign; pale where p ≥ 0.05) for all five
  traits; Fig. 4 mean indirect effect by condition with 95% interval and country
  counts; Fig. 5 the indirect effect split into the job-cell mean (the job) and
  the deviation from it (the view of the job) for ES × time pressure and
  E × voice / autonomy / learning; Fig. 6 facet means with intervals. Dropped:
  the numeric path diagrams (a table drawn as a picture), the jittered ACME plot,
  the dodged appraisal plot and the per-country facet plot (now Fig. A3).
- **Text.** With unadjusted counts the cell-level channel is no longer exactly
  zero for extraversion (voice 4, autonomy 3 of 21 countries); the appraisal
  paragraph now says that the larger part of the mediation runs through the
  worker's view and that a smaller part runs through the job for extraversion.

**Version 5, 16–17 October 2026: response to the editorial assessment**
(`REVISION_PLAN.md` records the assessment, the pushbacks and the 23 changes; the
response matrix is in `RESPONSE_MATRIX.md`). Decisions, before re-estimation:

- **Three estimands for H1**, kept apart in text and tables: pay-adjusted
  (baseline, unchanged), pay-and-occupation-adjusted (new block
  `sens_occupation`), and the direct association at the same reported conditions
  (`ade_joint_core`). The v3/v4 text wrongly said occupation was held fixed in the
  baseline; corrected everywhere. OECD (2025, Figure 3.9) is cited as the closest
  comparator: H1 is a replication of its adjusted result on the same survey.
- **Pay checks**: complete-case with/without pay on one sample
  (`sens_pay_complete`, `sens_nopay_complete`); continuous log earnings against
  deciles on the same sample in the countries that release earnings
  (`sens_logpay`, `sens_decile_wagesample`, `sens_decile_plus_logpay`).
- **Joint decomposition on a common core set** (seven conditions + skill underuse)
  in every country (`acme_joint_core`, `ade_joint_core`), the extended set
  (support, voice) reported separately for the 17 countries. The Gelbach
  implementation check now compares the joint core ACME with the Gelbach
  contribution of the same conditions on the same sample.
- **Vocabulary**: "occupational-group mean" and "individual deviation in reported
  conditions" replace "the job" and "the view of the job"; "indirect/direct part"
  are labels for the decomposition, causal only under the stated assumptions;
  the "upper bound on perception" claim is withdrawn.
- **Group-split diagnostics and sensitivity** (`cell_diagnostics()`, models
  `A1_min20`, `A1_min30`, `A1_1digit`): group counts, sizes, fallback rates,
  between-group variance shares, ICC(k) reliability; minimum group size 20 and 30;
  1-digit occupation in every country.
- **Equivalence tests** (two one-sided tests, 90% interval within the margin):
  ±0.03 SD for the group-mean coefficients, ±0.025 points for the interactions.
  Computed in the manuscript from the estimates and standard errors.
- **Facet contrasts** with their own standard errors (`facet_diff`: DEPR−ANXI,
  DEPR−VOLA, TRUS−COMP, TRUS−RESP, ENER−SOCI, ENER−ASSE).
- **Indicator test (Q5)**, chosen before estimation: within-country satisfaction
  gap between ISCO 1–2 and ISCO 8–9 (top and bottom skill level where only
  ISCOSKIL4 is released), raw, standardised to the pooled personality
  distribution (regression standardisation with group-specific slopes), and with
  common slopes plus pay deciles and plus core conditions (`indicator_gap_estimator`,
  model `IND`; groups need at least 100 employees each).
- **Oster**: δ, bias-adjusted β at δ = 1 and R² movements reported per trait
  (Table A7) instead of pass counts; Masten & Poirier (2022) cited.
- **Hypotheses**: H1 no longer predicts weak conscientiousness/open-mindedness;
  H3a is an observable contrast and its "resources too" clause is marked as added
  after estimation; Q5 added as the indicator question.
- **Text corrections** from the assessment: Judge et al. (2002) ordering;
  conscientiousness described as small and precisely estimated; "group means do
  not differ in any country" replaced by counts and equivalence; both R1 and R5
  lean the predicted way (17 of 23); daggers added to the split figure; the ρ
  column explained as trait-independent; pay documented as a control in the
  joint total.

**Version 5.1, 17 October 2026** (author's review of v5): title changed to "Better Jobs or Brighter Eyes? Non-Cognitive Skills, Working Conditions and Job Satisfaction across 23 Countries"; introduction reframed around the economics of non-cognitive skills (Freeman 1978; Avanesian, Shipitko & Borovskaya 2026) with explicit contributions and a shorter results summary; Q5 becomes the directional hypothesis H5 (composition); hypothesis tags removed from results headings; "return" reserved for wage returns, "reward" elsewhere; Israel no longer mentioned in the paper (the author's exclusion stays recorded here and in `R/07_collect.R`); AI statement moved from Methods to Declarations, software (R, mediation, survey, ggplot2) cited in Methods; shared y-axis in the country figures; schematic redrawn so the direct path clears the boxes.

**Version 5.2, 17 October 2026** (author's second review): introduction reframed around the subjective reward as an unpriced, portable job amenity (Rosen 1986) with the three findings stated as contributions; literature matrix table removed and folded into the narrative; the H3a resources clause is now part of the hypothesis text without a bracketed note (it was added after the first results, 15 October 2026, as recorded above); closing paragraph after H5; Discussion rebuilt hypothesis by hypothesis against Judge et al. 2002, OECD 2025, the occupational-choice literature, the negative-affectivity debate (Spector et al. 2000), trait activation and fit; conclusion shortened; Table 4 without the median column (medians in Table A1); validation table dropped from the Online Resource (stays in results/validation.csv); Tables A6–A7 in portrait; American spelling and lang en-US.

**Version 5.3, 17 October 2026**: the v5.1 introduction restored (hook, economics paragraph) with one new paragraph on what is novel (subjective reward measured beside the objective one; the three routes; the group split and the composition decomposition), the contributions paragraph kept and the findings reduced to one short paragraph. Terminology fixed on the author's instruction: "return" only in the Mincerian wage sense; otherwise objective vs subjective rewards, extrinsic vs intrinsic career success. Abstract, discussion and conclusion now say that the subjective rewards to non-cognitive skills exceed the objective ones, not that a "return on satisfaction" exceeds the wage return.

**Version 6, 17 October 2026: submission package.** New abstract (novelty and methods first). Manuscript split into `manuscript/_setup.R` (shared data and helpers), `job_satisfaction.qmd` (anonymised main text, no date, keywords with semicolons), `_abstract.qmd` (included in the main text and the title page), `appendix.qmd` (Tables A1–A8, Figs A1–A3; 'Online Resource' renamed 'Appendix' throughout) and `title_page.qmd` (author block placeholders, abstract, keywords, JEL, declarations in the author's wording: no funding, no competing interests, code on GitHub public upon publication, AI used for reproducible code and copy-editing). `R/make_submission.sh` renders `output/submission/{title_page.pdf, manuscript_anonymised.pdf, manuscript_anonymised.html, appendix.pdf}`; `make job-satisfaction-render` calls it.

**24 October 2026**: title changed by the author to "Are Non-Cognitive Skills Rewarded with Job Satisfaction Net of Pay? A Comparative Analysis of Well-Being at Work across 23 Economies" (transfer to Applied Research in Quality of Life). Fixed a rendering error introduced by the v5.2 spelling pass: inline R code in Section 5.5 asked for the result key "standardized" instead of "standardised", so four composition numbers printed as NA, -Inf and NaN in the 17 October PDF and Word files.

**25 October 2026: prose pass.** The author asked to humanize the text with the prose-tells audit and chose option A for all 94 edits listed in `HUMANIZE_EDITS.md` (abstract and main text; first person singular throughout; 'this journal' replaced by *Social Indicators Research*; typos 'characteriztics' and 'centerd' from the v5.2 spelling pass fixed). Inline R code, citations and cross-references are unchanged. Scan signal: main text mixed to low, abstract strong to low. The pre-edit sources are kept as `manuscript/job_satisfaction.pre-humanize.qmd` and `manuscript/_abstract.pre-humanize.qmd`. Only the Word files were rebuilt; the PDF and HTML files in `output/submission/` still predate the title change and this pass.

**Prose pass, round 2, 25 October 2026**: re-audited with the updated 22-family prose-tells taxonomy and applied option A of the 72 edits listed in HUMANIZE_EDITS_ROUND2.md. The main text went from Mixed to Low and the abstract from Low to no hits. Inline R, citations and cross-references are unchanged, except for one added `@sec-reactivity` cross-reference. Backups: `manuscript/job_satisfaction.pre-round2.qmd` and `manuscript/_abstract.pre-round2.qmd`. Outputs not rebuilt.

**Grammarly pass, 25 October 2026**: the author's Grammarly-edited Word file was compared with the generated manuscript and about 430 changes were carried into `job_satisfaction.qmd` and `_abstract.qmd` (inline R unchanged; table notes edited in the R strings). Following the author's edits, the citations of Avanesian et al. (2026) and Wickham (2016) were removed. Where Grammarly introduced "we" or changed the meaning, the text was kept in the author's voice ("I"; "not a bound"; "is worth about"; "via the deviation"; the validation sentence; the facet and Oster sentences rebuilt around their inline values). `flagname` in `_setup.R` now lists countries with "and". Backups: `manuscript/*.pre-grammarly.qmd`. Only the Word files were rebuilt.

**Cut and reframe for Applied Research in Quality of Life, 25 October 2026**: manuscript cut from about 11,500 to about 7,500 words (text, tables, captions and references). Six tables moved to the Appendix as Tables A9–A14 (samples, H1 estimands, mediation, deviation vs group mean, reactivity, threats); two new main-text figures replace two of them (`fig-perceived`, `fig-reactivity`). All five hypotheses kept. Reframed around job satisfaction as a domain of quality of life (new citations van Praag et al. 2003 and Bowling et al. 2010, plus Michalos 1985 and Clark 1997 from the existing bibliography), sensitivity analyses gathered in the strategy section, and a Discussion paragraph on satisfaction gaps as quality-of-life indicators. Grammarly changes that broke the claim guardrails were reverted (H1 "No direction is predicted", H2 "moves with", H5 "adjusted for"). Cut from the text: adaptive problem solving comparison, agreeableness facet detail, extended-set mediation share, complete-case and continuous-earnings detail (still in Appendix tables where they exist). Backups: `manuscript/*.pre-cut.qmd`, `references.pre-cut.bib`. Submission files not rebuilt; drafts in `output/*_cut_draft.docx`.

**25 October 2026**: title changed by the author to "Are Non-Cognitive Skills Rewarded with Job Satisfaction Net of Pay? A Cross-Country Analysis of Well-Being at Work in High-Income Economies" (README, main text, appendix and title page). Outputs not rebuilt.

**25 October 2026, evening: author's Word edits and submission package for Applied Research in Quality of Life.** The author's edits to the Word manuscript (introduction, background, data, discussion; restored passages from earlier drafts; new citations Judge & Klinger 2008 and Sousa-Poza & Sousa-Poza 2000) were carried into `job_satisfaction.qmd` with citations and inline R restored. Also applied: Green (2013) for the productive–expandable–social definition of skills; the within-sample pay-share sentence in the Discussion and Conclusion replacing the cross-study comparison with wage returns; "I" for "we/us"; PIIAC → PIAAC; "whose association, holding pay and the other traits fixed". Per the journal guidelines (10,000 words incl. references; abstract 150–250; 4–6 keywords; supplementary material as "Online Resource" in PDF; captions "Fig. 1 Caption" without final full stop; page numbers; consent declaration; LLM use for code documented in Methods): keywords cut to six (Personality dropped), appendix renamed Online Resource 1 (ESM_1.docx and ESM_1.pdf), Word tables rebuilt with flextable (Times New Roman, journal rules, notes as footers, landscape pages for wide tables), captions and page numbers handled in `R/fix_docx.py`. Backups: `manuscript/*.pre-wordedits.*`, `*.pre-ft.*`, `title_page.pre-guidelines.qmd`. Old PDF/HTML outputs moved to `output/submission/old_versions/`.

**25 October 2026, late evening: Codex prose edits.** Adopted `job_satisfaction_codex.qmd` (54 local prose edits; no change to code, inline R, citations or numbers). Added Humphries & Kosse (2017) and Cunha & Heckman (2007) next to Green (2013) on non-cognitive skills as human capital; "socially predetermined" corrected to Green's "socially determined". Corrected a long-standing misstatement flagged by Codex: the median share of the emotional-stability premium accounted for by the joint decomposition refers to the seven core conditions and skill underuse with pay held fixed, not "pay and nine conditions" (abstract, introduction, Discussion). Backups: `manuscript/*.pre-codex.qmd`. Submission package rebuilt.

**25 October 2026, night: outside review edits.** Adopted option A of the 14 edits in `manuscript/job_satisfaction_review_edits.md` (definition of the subjective reward, stakes of the distinction, three mechanisms named early, roadmap for the empirical strategy, reasons for the occupation-by-industry benchmark, the H5 comparison and both equivalence margins, explicit appraisal logic, framing of the low-depression facet, associational wording in place of causal verbs, "little of this premium is reflected in pay" in place of "employers do not pay for this amenity"). No change to code, inline R or citations. Backup: `manuscript/job_satisfaction.pre-review.qmd`. Submission package rebuilt.

**26 October 2026: voice and wording.** Abstract rewritten without the first person (passive voice). Introduction states the three contributions with First, Second, Finally. First person reduced across the introduction (11 to 2 uses) and the empirical strategy (28 to 3) by putting procedures in the passive; runs of sentences starting with "I" removed in the background and discussion. Gelbach sentence rewritten: both methods decompose the same regression coefficients, so the comparison checks the code. The pay-mediator share is no longer described as satisfaction "from wage returns"; the text now says that workers with these traits are more satisfied mainly at the same level of pay. Backups: `manuscript/*.pre-voice.qmd`. Submission package rebuilt.
