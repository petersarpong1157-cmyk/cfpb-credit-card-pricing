# Data documentation

## Source

This project uses the Consumer Financial Protection Bureau (CFPB) Terms of Credit Card Plans (TCCP) Survey release covering July 1–December 31, 2025.

Official survey page: https://www.consumerfinance.gov/data-research/credit-card-data/terms-credit-card-plans-survey/

The analysis began with 663 plan records from 195 institutions.

## Raw data

The original CFPB Excel workbook is not included in this repository. Reproducers should download the corresponding workbook directly from the CFPB so that provenance remains tied to the official source.

## Primary outcome construction

The standardized 620–719 purchase APR is constructed as follows:

- If `Purchase APR Vary by Credit Tier == "Yes"`, use `Purchase APR good`.
- If `Purchase APR Vary by Credit Tier == "No"`, use `Purchase APR median`.
- Otherwise, the standardized outcome is missing.

APR fields are represented as decimal rates in the imported workbook and are converted to percentage points for analysis.

CFPB documentation instructs issuers to enter 999 in numeric credit-tier fields when a product is not offered to that tier. Values imported in the workbook representation as 9.99 are therefore treated as non-substantive sentinel-coded values rather than valid APRs.

There were 43 unusable standardized-outcome records (41 missing and 2 sentinel/non-substantive), leaving 620 plans from 172 institutions in the primary analytical sample.

## Predictors

The analysis uses:

- Secured card
- Introductory APR offered
- Balance transfer offered
- Cashback rewards reported
- Travel-related rewards reported
- Other rewards reported
- Purchase APR varies by credit tier
- Periodic fee type reported
- Number of targeted credit tiers (1, 2, 3, or 4)

Reward categories may overlap. A reward indicator describes whether that reward type is reported in the CFPB field; it should not be interpreted as a standardized reward value.

## Sensitivity checks

Three standardized-outcome observations were 0%. They were retained in the primary model because they were not established as data errors; a sensitivity model excluding them contained 617 plans from 171 institutions.

A separate audit found 11 observations for which the standardized APR exceeded the reported maximum APR. These were retained in the primary model and excluded in a sensitivity analysis (609 plans, 171 institutions). No replacement values were invented.

## Interpretation

The unit of analysis is the credit-card plan/product record, not the individual consumer. The study is observational and cross-sectional. Results describe associations and do not establish causal effects.
