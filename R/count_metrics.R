#' Trajectory Measure for Count Data
#'
#' Calculates the percent change of the most current to previous period
#' counts. Values greater than 1 indicate growing; less than one
#' indicate shrinking. \emph{NOTE: Counts must have the same period
#' (e.g. both summed over 7 consecutive days.)} In order to avoid
#' extreme values and dividing by zeros, the ratio uses an offset of 0.5
#' for both the numerator and denominator as pseudocounts and is
#' capped at 5 (i.e. a 400\% increase).
#'
#' The 0.5 offset was inspired by the \code{bayes.poisson.test} function from
#' the {BayesianFirstAid} packages uses a non-informative prior distribution
#' whose mean simplifies to approximately 0.5. For more information, see the
#' reproducibility paper on the OHI Surveillance Team's SharePoint site.
#'
#'
#' @param curr vector of counts for current period (must be non-negative integer)
#' @param prev vector of counts for previous period (must be non-negative integer)
#'
#' @return a numeric vector of percent change
#' @export
#'
#' @examples
#' traj <- score_trajectory(curr = 100L, prev = 80L)
score_trajectory <- function(curr, prev) {
  # requires: pair of non-negative integers, coerce if possible?
  curr <- check_nonneg(curr, "curr")
  prev <- check_nonneg(prev, "prev")

  # effects: returns an estimate of the ratio, moderated by pseudocounts and a cap (500%)
  # comment: A Bayesian estimate would be of higher quality but less transparent
  alpha <- 1

  traj <- ((curr + alpha)/(prev + alpha)) - 1
  100 * ifelse(traj <= 5, traj, 5)
}

#' Calculate p-value of trajectory current counts vs prior counts
#'
#' @inheritParams score_trajectory
#'
#' @return a p.value extracted from the results of \code{\link[stats]{poisson.test}}
#' @export
#'
#' @importFrom stats poisson.test
#' @importFrom purrr map2_dbl
#'
#' @examples
#' traj_pval <- pval_trajectory(curr = 100L, prev = 80L)
pval_trajectory <- function(curr, prev) {
  # requires: pair of non-negative integers, coerce if possible?
  curr <- check_nonneg(curr, "curr")
  prev <- check_nonneg(prev, "prev")

  pval <- purrr::map2_dbl(curr, prev, ~stats::poisson.test(c(.x, .y))$p.value)

  pval
}

#' Calculates the False Discovery Rate for trajectory p-values
#'
#' According to method of Benjamini & Hochberg (1995) via \code{\link[stats]{p.adjust}}
#'
#' @param pval a vector of p-values
#'
#' @return the false discovery rate
#' @export
#'
#' @importFrom stats p.adjust
#'
#' @examples
#' traj_pval <- pval_trajectory(curr = 100L, prev = 80L)
#' traj_fdr <- fdr_trajectory(traj_pval)
fdr_trajectory <- function(pval) {
  fdr <- stats::p.adjust(pval, method = "fdr")
  fdr
}

#' Classify Trajectory Measure for County Data
#'
#' This function tests to see if the count trajectory is statistically
#' distinct from zero with a Poisson test for the ratio of two counts
#' using \code{\link[stats]{poisson.test}} as.calculated by \code{\link{pval_trajectory}}
#' at the two-sided p < .05 level. If it is and the percent change as calculated by
#' \link{score_trajectory} is less than or equal to -10, the trajectory is
#' classified as shrinking. If it is and the percent change is greater than or equal
#' to 10, the trajectory is classified as growing. If the test fails to
#' reject the null hypothesis, the trajectory is classified as not statistically
#' significant.
#'
#'
#' @param traj trajectory as calculated by \code{\link{score_trajectory}}
#' @param pval p-value as calculated by \code{\link{pval_trajectory}}
#'
#' @return an ordered factor "Shrinking" < "No statistically significant change" < "Growing"
#' @export
#'
#' @examples
#' traj <- score_trajectory(curr = 100L, prev = 80L)
#' traj_pval  <- pval_trajectory(curr = 100L, prev = 80L)
#'
#' traj_class <- class_trajectory(traj, traj_pval)
class_trajectory <- function(traj, pval) {
  out <- ifelse(traj <= -10 & pval < 0.025, 1,
         ifelse(traj >=  10 & pval < 0.025, 3, 2))

  out <- factor(out,
                levels = 1:3,
                labels = c("Shrinking", "No significant change", "Growing"),
                ordered = TRUE)

  return(out)
}

#' calculate the burden
#'
#' @inheritParams score_trajectory
#' @param pop vector of population values to standardize counts
#'
#' @return a numeric vector of population adjusted burden
#' @export
#'
#' @examples
#' burd <- score_burden(curr = 100L, prev = 80L, pop = 65432L)
score_burden <- function(curr, prev, pop) {
  # requires: pair of non-negative integers
  curr <- check_nonneg(curr, "curr")
  prev <- check_nonneg(prev, "prev")
  pop <- check_nonneg(pop, "pop")

  #Currently not using pseudocount/offset for burden
  alpha <- 0

  burden <- (1e5 / pop) * (curr + prev + alpha)

  burden
}

#' Classify population adjusted burden
#'
#' Currently, cutoff values are hard coded and adopted straight from
#' the ones used in the CDC State Indicators report:
#' \itemize{
#'   \item "Low" is a burden less than or equal to 10 per 100,000
#'   \item "Moderate" is a burden greater than 10 and less than or equal to 50 per 100,000
#'   \item "Moderately high" is a burden greater than 50 and less than or equal to 100 per 100,000
#'   \item "High" is a burden greater than 100 and less than or equal to 350 per 100,000
#'   \item "Very high" is a burden greater than 350 per 100,000
#' }
#'
#' @param burden numeric vector as calculated by \code{\link{score_burden}}
#'
#' @return an ordered factor
#' @export
#'
#' @examples
#' burd <- score_burden(curr = 100L, prev = 80L, pop = 65432L)
#' burd_class <- class_burden(burd)
class_burden <- function(burden) {
  out <- ifelse(burden <= 10, 1,
         ifelse(burden <= 50, 2,
         ifelse(burden <= 100, 3,
         ifelse(burden <= 350, 4, 5))))

  out <- factor(out,
                levels = 1:5,
                labels = c("Low", "Moderate", "Moderately high", "High", "Very high"),
                ordered = TRUE)

  out
}

#' Calculate the confirmed case composite summary
#'
#' Source: Jeff's Design Doc as confirmed by SEOC
#'
#' @param traj_class trajectory classification as calculated by \code{\link{class_trajectory}}
#' @param burd_class burden classification as calculated by \code{\link{class_burden}}
#'
#' @return an ordered factor for the composite confirmed case summary
#' @export
#'
#' @importFrom dplyr case_when
#'
#' @examples
#' burd <- score_burden(curr = 100L, prev = 80L, pop = 65432L)
#' burd_class <- class_burden(burd)
#' traj <- score_trajectory(curr = 100L, prev = 80L)
#' traj_pval  <- pval_trajectory(curr = 100L, prev = 80L)
#' traj_class <- class_trajectory(traj, traj_pval)
#'
#' composite <- confirmed_case_composite(traj_class, burd_class)
confirmed_case_composite <- function(traj_class, burd_class) {
  out <- dplyr::case_when(
    burd_class == "Low" & traj_class < "Growing" ~ 1,
    burd_class == "Low" & traj_class == "Growing" ~ 2,
    burd_class == "Moderate" & traj_class < "Growing" ~ 2,
    burd_class == "Moderate" & traj_class == "Growing" ~ 3,
    burd_class == "Moderately high" & traj_class == "Shrinking" ~ 2,
    burd_class == "Moderately high" & traj_class > "Shrinking" ~ 3,
    burd_class == "High" & traj_class %in% c("Shrinking", "No significant change", "Growing") ~ 3,
    burd_class == "Very high" & traj_class %in% c("Shrinking", "No significant change", "Growing") ~ 4,
    TRUE ~ NA_real_
  )

  out <- factor(out,
                levels = 1:4,
                labels = c("Low", "Medium", "High", "Very high"),
                ordered = TRUE)

  out
}
