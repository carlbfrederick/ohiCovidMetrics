#parameter setup ----
curr_int <- 123L
gprev_int <- 50L
nprev_int <- 103L
sprev_int <- 175L
delta_t_u <- 2L
delta_t_l <- 3L
check_ucl <- rev_cusum_ucl(curr = curr_int, delta_t = delta_t_u)
check_lcl <- rev_cusum_lcl(curr = curr_int, delta_t = delta_t_l)


#score_trajectory ----
##. . correct calculation given unit count----
expect_equal(score_trajectory(curr_int, gprev_int), 100*((123L + 1L)/(50L + 1L) - 1L))
expect_equal(score_trajectory(curr_int, 10L), 500)

#class_trajectory ----
##. . correct call, type
### growing
expect_identical(class_trajectory(score_trajectory(curr_int, gprev_int),
                                  pval_trajectory(curr_int, gprev_int)),
                 ordered(3, levels = 1:3, labels = c("Shrinking", "No significant change", "Growing")))
### not statistically significant
expect_identical(class_trajectory(score_trajectory(curr_int, nprev_int),
                                  pval_trajectory(curr_int, nprev_int)),
                 ordered(2, levels = 1:3, labels = c("Shrinking", "No significant change", "Growing")))
### shrinking
expect_identical(class_trajectory(score_trajectory(curr_int, sprev_int),
                                  pval_trajectory(curr_int, sprev_int)),
                 ordered(1, levels = 1:3, labels = c("Shrinking", "No significant change", "Growing")))

#rev_cusum_ucl ----
##. .Test that upper limit really is smallest 3sigma integer

#rev_cusum_lcl ----
##. . Test that lower limit really is largest 3sigma integer



