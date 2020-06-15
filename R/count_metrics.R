#' Trajectory Measure for Count Data
#'
#' Calculates the ratio of the most current to previous period
#' counts. Values greater than 1 indicate growing; less than one
#' indicate shrinking. \emph{NOTE: Counts must have the same period
#' (e.g. both summed over 7 consecutive days.)} In order to avoid
#' extreme values and dividing by zeros, the ratio includes unit offset
#' for pseudocounts and is capped at 5 (i.e. a 400\% increase).
#'
#' @param curr count for current period (must be non-negative integer)
#' @param prev count for previous period (must be non-negative integer)
#'
#' @return a numeric scalar
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
  min( ( curr + 1 ) / ( prev + 1 ) , 5 )
}

#' Classify Trajectory Measure for County Data
#'
#' This function tests to see if the count trajectory is statistically
#' distinct from zero with a Poisson test for the ratio of two counts
#' using \code{\link[stats]{posson.test}} at the two-sided p < .05 level.
#' If it is and the ratio as calculated by \link{score_trajectory} is less
#' than or equal to 0.9, the trajectory is classified as shrinking. If it is
#' and the ration is greather than or equal to 1.1, the trajectory is
#' classified as growing. If the test fails to reject the null hypothesis,
#' the trajectory is classified as not statistically significant.
#'
#' @inheritParams score_trajectory
#'
#' @return an ordered factor "Shrinking" < "Not Statistically Significant" < "Growing"
#' @export
#'
#' @importFrom stats poisson.test
#'
#' @examples
#' traj_class_2020_06_10 <- class_trajectory(curr = 100L, prev = 80L)
class_trajectory <- function(curr, prev) {
  # requires: pair of non-negative integers
  curr <- check_nonneg(curr, "curr")
  prev <- check_nonneg(prev, "prev")

  # effects: returns a 3-level ordinal classification (1 = shrinking, 2 = no call, 3 = growing )
  trajectory <- score_trajectory(curr, prev)
  is_significant <- stats::poisson.test(c(curr, prev))$p.value < 0.025

  out <- ifelse(trajectory <= 0.9 & is_significant, 1,
         ifelse(trajectory >=  1.1 & is_significant, 3, 2))

  out <- factor(out,
                levels = 1:3,
                labels = c("Shrinking", "Not Statistically Significant", "Growing"),
                ordered = TRUE)

  return(out)
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

  # effects: returns the smallest count greater than curr that would provide significance at 3-sigma probability
  out <- delta_t * curr + 1
  p <- stats::poisson.test( c( curr , out ) , T = c( 1 , delta_t ) )$p.value
  while( p > stats::pnorm(-3) ) {
    out <- out + 1
    p <- stats::poisson.test( c( curr , out ) , T = c( 1 , delta_t ) )$p.value
  }
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
  # effects: returns the largest count less than curr that would provide significance at 3-sigma probability
  out <- NA
  try(
    {
      out <- delta_t * curr - 1
      p <- stats::poisson.test( c( curr , out ) , T = c( 1 , delta_t ) ) $p.value
      while( p > stats::pnorm(-3) ) {
        out <- out - 1
        p <- stats::poisson.test( c( curr , out ) , T = c( 1 , delta_t ) )$p.value
      }
    }
  )
  out
}



