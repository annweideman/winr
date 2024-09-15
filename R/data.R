#' Respiratory dataset
#'
#' The respiratory dataset originates from a randomized clinical trial that
#' evaluated a test treatment against a control in managing a chronic respiratory
#' condition \insertCite{koch_1989,stokes_2012}{winr}. The study involved 111
#' patients—54 receiving the active treatment and 57 on a placebo—distributed
#' across two centers. Assessments were conducted at baseline and during four
#' subsequent follow-up visits. At each visit, patients' respiratory status was
#' evaluated using an ordinal scale where 0 signifies 'terrible', 1 is 'poor',
#' 2 stands for 'fair', 3 represents 'good', and 4 denotes 'excellent'.
#' The participating centers serve as stratification factors, and the baseline
#' variables include age, sex, and initial respiratory status.
#'
#' @format A data frame with 10 variables.
#'
#' \code{ID}:  Participant ID
#'
#' \code{Center}:  Treatment center
#'
#' \code{Sex}:  Participant sex at birth
#'
#' \code{Age}:  Participant age at baseline
#'
#' \code{Treatment}:  T=treatment arm, C=control arm
#'
#' \code{Baseline}:  Ordinal global rating at Baseline: 0 for terrible, 1 for poor, 2 for fair,
#' 3 for good, and 4 for excellent
#'
#' \code{Visit1}:  Ordinal global rating at Visit 1: 0 for terrible, 1 for poor, 2 for fair,
#' 3 for good, and 4 for excellent
#'
#' \code{Visit2}:  Ordinal global rating at Visit 2: 0 for terrible, 1 for poor, 2 for fair,
#' 3 for good, and 4 for excellent
#'
#' \code{Visit3}:  Ordinal global rating at Visit 3: 0 for terrible, 1 for poor, 2 for fair,
#' 3 for good, and 4 for excellent
#'
#' \code{Visit4}:  Ordinal global rating at Visit 4: 0 for terrible, 1 for poor, 2 for fair,
#' 3 for good, and 4 for excellent
#'
#' @import Rdpack
#' @references{
#'    \insertAllCited{}
#' }
#'
"resp"

#' Dermatology dataset
#'
#' The dermatology dataset is derived from a randomized clinical trial that
#' compared a test treatment with a control for managing skin conditions
#' \insertCite{stanish_1978}{winr}. This trial enrolled 172 patients—88 in the
#' test group and 84 in the placebo group—across six clinics. These patients were
#' assessed at three follow-up visits, where the degree of improvement in their
#' skin condition was measured on a five-point scale: 1 for rapidly improving,
#' 2 for slowly improving, 3 for stable, 4 for slowly worsening, and 5 for
#' rapidly worsening. Due to Clinic 9 enrolling only four patients, data from
#' Clinics 8 and 9 were combined, a decision supported by these clinics having
#' the smallest sample sizes in their strata. The primary baseline variable
#' recorded was the stage of disease at the start of the study, categorized as
#' 3 for fair, 4 for poor, and 5 for exacerbation. This dataset also features
#' missing data at follow-up visits, progressively increasing from 2\% (3
#' missing observations) at the first visit to 9\% (16 missing) at the second,
#' and 17\% (30 missing) at the third visit.
#'
#' @format A data frame with 11 variables.
#'
#' \code{INV}:  Investigator identification number (5,6,8,9,10,11)
#'
#' \code{ID}:  Participant ID
#'
#' \code{center}:  Treatment center
#'
#' \code{center2}:  Treatment center that pools centers 3 and 4 due to small sample size
#'
#' \code{TRT}:  1 = Test drug, 2 = Placebo
#'
#' \code{STAGE}:  Initial stage of disease (3 = Fair, 4 = Poor, 5 = Exacerbation)
#'
#' \code{R1}:  Response at Time 1; NA if missing
#'
#' \code{R2}:  Response at Time 2; NA if missing
#'
#' \code{R3}:  Response at Time 3; NA if missing
#'
#' \code{Stage4}:  1 if Stage 4, 0 otherwise
#'
#' \code{Stage5}:  1 if Stage 5, 0 otherwise
#'
#' @import Rdpack
#' @references{
#'    \insertAllCited{}
#' }
#'
"skin"
