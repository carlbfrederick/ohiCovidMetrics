#' Trajectory Measure for Count Data
#'
#' Calculates the ratio of the most current to previous period
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
#' @return a numeric vector
#' @export
#'
#' @examples
#' traj_2020_06_10 <- score_trajectory(curr = 100L, prev = 80L)
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
#' pval_2020_06_10 <- pval_trajectory(curr = 100L, prev = 80L)
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
#' pval_2020_06_10 <- pval_trajectory(curr = 100L, prev = 80L)
#' fdr_2020_06_10 <- fdr_trajectory(pval_2020_06_10)
fdr_trajectory <- function(pval) {
  fdr <- stats::p.adjust(pval, method = "fdr")
  fdr
}

#' Classify Trajectory Measure for County Data
#'
#' This function tests to see if the count trajectory is statistically
#' distinct from zero with a Poisson test for the ratio of two counts
#' using \code{\link[stats]{poisson.test}} as.calculated by \code{\link{pval_trajectory}}
#' at the two-sided p < .05 level. If it is and the ratio as calculated by
#' \link{score_trajectory} is less than or equal to 0.9, the trajectory is
#' classified as shrinking. If it is and the ration is greather than or equal
#' to 1.1, the trajectory is classified as growing. If the test fails to
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
#' traj_score <- score_trajectory(curr = 100L, prev = 80L)
#' traj_pval  <- pval_trajectory(curr = 100L, prev = 80L)
#'
#' traj_class_2020_06_10 <- class_trajectory(traj_score, traj_pval)
class_trajectory <- function(traj, pval) {
  out <- ifelse(traj <= 0.9 & pval < 0.025, 1,
         ifelse(traj >=  1.1 & pval < 0.025, 3, 2))

  out <- factor(out,
                levels = 1:3,
                labels = c("Shrinking", "No statistically significant change", "Growing"),
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
#' burd <- score_burden(curr = 100L, prev = 80L, population = 65432)
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
#'   \item "Moderately High" is a burden greater than 50 and less than or equal to 100 per 100,000
#'   \item "High" is a burden greater than 100
#' }
#'
#' @param burden numeric vector as calculated by \code{\link{score_burden}}
#'
#' @return an ordered factor
#' @export
#'
#' @examples
#' burd <- score_burden(curr = 100L, prev = 80L, population = 65432)
#' burd_c <- class_burden(burd)
class_burden <- function(burden) {
  out <- ifelse(burden <= 10, 1,
         ifelse(burden <= 50, 2,
         ifelse(burden <= 100, 3, 4)))

  out <- factor(out,
                levels = 1:4,
                labels = c("Low", "Moderate", "Moderately High", "High"),
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
#' burd <- score_burden(curr = 100L, prev = 80L, population = 65432)
#' burd_c <- class_burden(burd)
#' traj_score <- score_trajectory(curr = 100L, prev = 80L)
#' traj_pval  <- pval_trajectory(curr = 100L, prev = 80L)
#' traj_class_2020_06_10 <- class_trajectory(traj_score, traj_pval)
#'
#' comp_2020_06_10 <- confirmed_case_composite(traj_class_2020_06_10, burd_c)
confirmed_case_composite <- function(traj_class, burd_class) {
  out <- dplyr::case_when(
    burd_class == "Low" & traj_class < "Growing" ~ 1,
    burd_class == "Low" & traj_class == "Growing" ~ 2,
    burd_class == "Moderate" & traj_class < "Growing" ~ 2,
    burd_class == "Moderate" & traj_class == "Growing" ~ 3,
    burd_class == "Moderately High" & traj_class == "Shrinking" ~ 2,
    burd_class == "Moderately High" & traj_class > "Shrinking" ~ 3,
    burd_class == "High" & traj_class %in% c("Shrinking", "No statistically significant change", "Growing") ~ 3,
    TRUE ~ NA_real_
  )

  out <- factor(out,
                levels = 1:3,
                labels = c("Low", "Moderate", "High"),
                ordered = TRUE)

  out
}

#' Calculates upper control limits for the reverse cusum control chart based on a
#' 3\eqn{\sigma} significance level.
#'
#' @inheritParams score_trajectory
#' @param delta_t number of time periods back in time.
#'
#' @return the smallest count greater than curr that would produce a statistically
#'         significant \code{\link[stats]{poisson.test}}
#' @export
#'
#' @importFrom stats poisson.test
#' @importFrom stats pnorm
#'
#' @examples
#' #For 3 weeks into the past using 7-day binned counts
#' rev_cusum_ucl(curr = 100L, delta_t = 3)
rev_cusum_ucl <- function(curr, delta_t) {
  # requires: non-negative integer count, curr,  and positive integer weight, delta_t
  curr <- check_nonneg(curr, "curr")
  delta_t <- check_nonneg(delta_t, "delta_t")

  if (delta_t == 0L) {
    return(NA_real_)
  }

  # effects: returns the smallest count greater than curr that would provide significance at 3-sigma probability
  cum_lim <- delta_t * curr + 1
  p <- stats::poisson.test( c( curr , cum_lim ) , T = c( 1 , delta_t ) )$p.value
  while( p > stats::pnorm(-3) ) {
    cum_lim <- cum_lim + 1
    p <- stats::poisson.test( c( curr , cum_lim ) , T = c( 1 , delta_t ) )$p.value
  }

  out <- cum_lim/delta_t - curr

  out
}

#' Calculates lower control limits for the reverse cusum control chart based on a
#' 3\eqn{\sigma} significance level.
#'
#' @inheritParams rev_cusum_ucl
#'
#' @return the greatest count smaller than curr that would produce a statistically
#'         significant \code{\link[stats]{poisson.test}}
#' @export
#'
#' @importFrom stats poisson.test
#' @importFrom stats pnorm
#'
#' @examples
#' #For 3 weeks into the past using 7-day binned counts
#' rev_cusum_lcl(curr = 100L, delta_t = 3)
rev_cusum_lcl <- function( curr , delta_t = 1 ) {
  # requires: non-negative integer count, curr,  and positive integer weight, delta_t
  curr <- check_nonneg(curr, "curr")
  delta_t <- check_nonneg(delta_t, "delta_t")

  if (delta_t == 0L) {
    return(NA_real_)
  }

  # effects: returns the largest count less than curr that would provide significance at 3-sigma probability
  cum_lim <- NA


  if (curr == 0) {
      cum_lim <- 0
    } else {
      cum_lim <- delta_t * curr - 1L
      p <- stats::poisson.test( c( curr , cum_lim ) , T = c( 1 , delta_t ) ) $p.value
      while( p > stats::pnorm(-3) & cum_lim > 0) {
        cum_lim <- cum_lim - 1
        p <- stats::poisson.test( c( curr , cum_lim ) , T = c( 1 , delta_t ) )$p.value
      }
    }

  out <- cum_lim/delta_t - curr

  out
}

#' Process the shaped confirmed case data.frame into a Tableau ready format
#'
#' @param clean_case_df shaped case data produced by \code{\link{shape_case_data}}
#'
#' @return a Tableau ready data.framed with the following columns:
#' \describe{
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#' }
#'
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#'
#' @examples
#' library(dplyr)
#'
#' output <- pull_histTable() %>%
#'   shape_case_data() %>%
#'   process_confirmed_cases()
process_confirmed_cases <- function(clean_case_df) {
  dplyr::ungroup(clean_case_df) %>%
    dplyr::mutate(
      Burden = score_burden(curr = case_weekly_1,
                            prev = case_weekly_2,
                            pop = pop_2018),
      Burden_Class = class_burden(Burden),
      Trajectory = score_trajectory(curr = case_weekly_1,
                                    prev = case_weekly_2),
      Trajectory_P = pval_trajectory(curr = case_weekly_1,
                                     prev = case_weekly_2),
      Trajectory_Class = class_trajectory(traj = Trajectory,
                                          pval = Trajectory_P),
      Trajectory_FDR = fdr_trajectory(pval = Trajectory_P),
      Composite_Class = confirmed_case_composite(traj_class = Trajectory_Class,
                                                 burd_class = Burden_Class)

    ) %>%
    dplyr::mutate(
      Trajectory = signif(Trajectory, 2),
      Burden = signif(Burden, 2)
    ) %>%
    dplyr::select(
      Date = week_end_1,
      Region_ID = fips,
      Region = geo_name,
      Burden,
      Trajectory,
      Burden_Class,
      Trajectory_Class,
      Composite_Class,
      Trajectory_P,
      Trajectory_FDR
    )
}


