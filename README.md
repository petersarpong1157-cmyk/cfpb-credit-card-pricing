# Credit Card Pricing and Product Features in the U.S. Market

Reproducible R materials for an observational study of credit-card pricing and product characteristics using the Consumer Financial Protection Bureau (CFPB) Terms of Credit Card Plans (TCCP) Survey.

## Research question

How are credit-card product characteristics—including targeted credit tier, secured status, fees, introductory offers, and rewards availability—associated with purchase APRs across U.S. credit-card plans?

## Data

The analysis uses the CFPB TCCP release covering July 1–December 31, 2025. The raw workbook contained 663 credit-card plan records from 195 institutions.

The raw CFPB workbook is not redistributed in this repository. See `DATA.md` for the official source and data-processing notes.

## Primary outcome

The primary outcome is a standardized purchase APR for the 620–719 credit-score tier:

- When purchase APR varies by credit tier, the analysis uses `Purchase APR good`.
- When purchase APR does not vary by credit tier, it uses `Purchase APR median`.
- CFPB numeric sentinel values for tiers to which a product is not offered are treated as non-substantive rather than as APRs.

After outcome cleaning, the primary analytical sample contains 620 plans from 172 institutions.

## Analysis

The primary model is ordinary least squares with institution-clustered standard errors. Predictors include secured-card status, introductory APR availability, balance-transfer availability, reported reward types, whether APR varies by credit tier, reported periodic-fee type, and the number of targeted credit tiers.

The analysis is cross-sectional and observational. Coefficients are interpreted as adjusted associations, not causal effects.

## Main results

In the primary analytical sample, mean standardized purchase APR was 23.53% (SD 7.70). With institution-clustered inference:

- Travel rewards reported: β = -4.91 percentage points, 95% CI [-7.39, -2.42], p < .001.
- Periodic fee type reported: β = +4.76 percentage points, 95% CI [2.56, 6.96], p < .001.
- Four targeted credit tiers versus one: β = -4.12 percentage points, 95% CI [-7.79, -0.45], p = .027.

Sensitivity analyses excluding three 0% APR observations and separately excluding 11 range-inconsistent observations produced substantively similar findings.

## Reproduction

1. Download the July 1–December 31, 2025 TCCP workbook from the CFPB.
2. Place the workbook in your local working directory.
3. Run `cfpb-credit-card-pricing.R` in R.
4. The script documents the cleaning, standardized APR construction, primary regression, clustered inference, diagnostics, and sensitivity analyses used in the study.

## Repository contents

- `cfpb-credit-card-pricing.R` — original full R analysis workflow used for the project.
- `DATA.md` — data provenance and variable-construction notes.
- `CITATION.cff` — citation metadata.
- `LICENSE` — MIT license for repository code.

## Citation

A permanent Zenodo DOI will be added after the v3.0.0 GitHub release is archived.

## Author

Peter Sarpong

## License and data

Repository code is released under the MIT License. The CFPB source data remain subject to the terms and documentation of their original publisher.
