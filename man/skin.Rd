% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{skin}
\alias{skin}
\title{Dermatology dataset}
\format{
A data frame with 11 variables.

\code{INV}:  Investigator identification number (5,6,8,9,10,11)

\code{ID}:  Participant ID

\code{center}:  Treatment center

\code{center2}:  Treatment center that pools centers 3 and 4 due to small sample size

\code{TRT}:  1 = Test drug, 2 = Placebo

\code{STAGE}:  Initial stage of disease (3 = Fair, 4 = Poor, 5 = Exacerbation)

\code{R1}:  Response at Time 1; NA if missing

\code{R2}:  Response at Time 2; NA if missing

\code{R3}:  Response at Time 3; NA if missing

\code{Stage4}:  1 if Stage 4, 0 otherwise

\code{Stage5}:  1 if Stage 5, 0 otherwise
}
\usage{
skin
}
\description{
"The dermatology dataset is from a randomized clinical trial comparing a test
treatment to control for skin conditions \insertCite{stanish_1978}{winr}. In this
trial, 172 patients (88 test, 84 placebo) at six clinics were evaluated at
three follow-up visits, and their extent of improvement for their skin
condition was recorded on a five-point scale (1 for rapidly improving,
2 for slowly improving, 3 for stable, 4 for slowly worsening, and 5 for
rapidly worsening). Since clinic 9 only enrolled 4 patients, patients in
clinics 8 and 9 are pooled. This pooling is further justified by clinics 8
and 9 having the smallest stratum sample sizes. The baseline covariable for
patients enrolled in the skin conditions study is disease stage recorded at
baseline (3 = Fair, 4 = Poor, 5 = Exacerbation). Unlike the respiratory
dataset, this dataset is subject to missing data at the follow-up visits, and
the extent of the missing data increases at each visit, with 3 (2\%) missing
observations at visit 1, 16 (9\%) at visit 2, and 30 (17\%) at visit 3"
\insertCite{kowalewski_2023}{winr}.
}
\references{
{
   \insertAllCited{}
}
}
\keyword{datasets}
