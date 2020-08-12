#parameter setup ----
curr_int <- 123L
gprev_int <- 50L
nprev_int <- 103L
sprev_int <- 175L
pop <- 123456L
pop_mh <- 345612L
pop_m<- 1234567L
pop_l<- 12345678L
pop_c <- 34345L

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

#pval_trajectory ----
expect_equal(pval_trajectory(curr_int, gprev_int), poisson.test(c(123, 50))$p.value)
expect_equal(pval_trajectory(curr_int, nprev_int), poisson.test(c(123, 103))$p.value)
expect_equal(pval_trajectory(curr_int, sprev_int), poisson.test(c(123, 175))$p.value)

#fdr_trajectory ----
##from p.adjust example
set.seed(123)
x <- rnorm(50, mean = c(rep(0, 25), rep(3, 25)))
p <- 2*pnorm(sort(-abs(x)))
expect_equal(fdr_trajectory(p), p.adjust(p, method = "fdr"))

#score_burden ----
expect_equal(score_burden(curr_int, gprev_int, pop), 1e5 * (curr_int + gprev_int) / pop)

#class_burden ----
expect_identical(class_burden(score_burden(curr_int, gprev_int, pop_l)),
                 ordered(1, levels = 1:5, labels = c("Low", "Moderate", "Moderately High", "High", "Critical")))
expect_identical(class_burden(score_burden(curr_int, gprev_int, pop_m)),
                 ordered(2, levels = 1:5, labels = c("Low", "Moderate", "Moderately High", "High", "Critical")))
expect_identical(class_burden(score_burden(curr_int, gprev_int, pop_mh)),
                 ordered(3, levels = 1:5, labels = c("Low", "Moderate", "Moderately High", "High", "Critical")))
expect_identical(class_burden(score_burden(curr_int, gprev_int, pop)),
                 ordered(4, levels = 1:5, labels = c("Low", "Moderate", "Moderately High", "High", "Critical")))
expect_identical(class_burden(score_burden(curr_int, gprev_int, pop_c)),
                 ordered(5, levels = 1:5, labels = c("Low", "Moderate", "Moderately High", "High", "Critical")))

#confirmed_case_composite ----
##. . Low ----
###3 Shrinking and Low
expect_identical(
  confirmed_case_composite(
    class_trajectory(score_trajectory(curr_int, sprev_int), pval_trajectory(curr_int, sprev_int)),
    class_burden(score_burden(curr_int, gprev_int, pop_l))
  ),
  ordered(1, levels = 1:4, labels = c("Low", "Medium", "High", "Critical"))
)
### No sig change and Low
expect_identical(
  confirmed_case_composite(
    class_trajectory(score_trajectory(curr_int, nprev_int), pval_trajectory(curr_int, nprev_int)),
    class_burden(score_burden(curr_int, gprev_int, pop_l))
  ),
  ordered(1, levels = 1:4, labels = c("Low", "Medium", "High", "Critical"))
)
##. . Medium ----
### Growing and Low
expect_identical(
  confirmed_case_composite(
    class_trajectory(score_trajectory(curr_int, gprev_int), pval_trajectory(curr_int, gprev_int)),
    class_burden(score_burden(curr_int, gprev_int, pop_l))
  ),
  ordered(2, levels = 1:4, labels = c("Low", "Medium", "High", "Critical"))
)
### Shrinking and Moderate
expect_identical(
  confirmed_case_composite(
    class_trajectory(score_trajectory(curr_int, sprev_int), pval_trajectory(curr_int, sprev_int)),
    class_burden(score_burden(curr_int, gprev_int, pop_m))
  ),
  ordered(2, levels = 1:4, labels = c("Low", "Medium", "High", "Critical"))
)
### Shrinking and Moderately High
expect_identical(
  confirmed_case_composite(
    class_trajectory(score_trajectory(curr_int, sprev_int), pval_trajectory(curr_int, sprev_int)),
    class_burden(score_burden(curr_int, gprev_int, pop_mh))
  ),
  ordered(2, levels = 1:4, labels = c("Low", "Medium", "High", "Critical"))
)
##. . High ----
### Growing and Moderate
expect_identical(
  confirmed_case_composite(
    class_trajectory(score_trajectory(curr_int, gprev_int), pval_trajectory(curr_int, gprev_int)),
    class_burden(score_burden(curr_int, gprev_int, pop_m))
  ),
  ordered(3, levels = 1:4, labels = c("Low", "Medium", "High", "Critical"))
)
### No sig change and Moderately High
expect_identical(
  confirmed_case_composite(
    class_trajectory(score_trajectory(curr_int, nprev_int), pval_trajectory(curr_int, nprev_int)),
    class_burden(score_burden(curr_int, gprev_int, pop_mh))
  ),
  ordered(3, levels = 1:4, labels = c("Low", "Medium", "High", "Critical"))
)
### Growing and Moderately High
expect_identical(
  confirmed_case_composite(
    class_trajectory(score_trajectory(curr_int, gprev_int), pval_trajectory(curr_int, gprev_int)),
    class_burden(score_burden(curr_int, gprev_int, pop_mh))
  ),
  ordered(3, levels = 1:4, labels = c("Low", "Medium", "High", "Critical"))
)
### Shrinking and High
expect_identical(
  confirmed_case_composite(
    class_trajectory(score_trajectory(curr_int, sprev_int), pval_trajectory(curr_int, sprev_int)),
    class_burden(score_burden(curr_int, gprev_int, pop))
  ),
  ordered(3, levels = 1:4, labels = c("Low", "Medium", "High", "Critical"))
)
### No sig change and High
expect_identical(
  confirmed_case_composite(
    class_trajectory(score_trajectory(curr_int, nprev_int), pval_trajectory(curr_int, nprev_int)),
    class_burden(score_burden(curr_int, gprev_int, pop))
  ),
  ordered(3, levels = 1:4, labels = c("Low", "Medium", "High", "Critical"))
)
### Growing and High
expect_identical(
  confirmed_case_composite(
    class_trajectory(score_trajectory(curr_int, gprev_int), pval_trajectory(curr_int, gprev_int)),
    class_burden(score_burden(curr_int, gprev_int, pop))
  ),
  ordered(3, levels = 1:4, labels = c("Low", "Medium", "High", "Critical"))
)
##. . Critical ----
### Shrinking and Critical
expect_identical(
  confirmed_case_composite(
    class_trajectory(score_trajectory(curr_int, sprev_int), pval_trajectory(curr_int, sprev_int)),
    class_burden(score_burden(curr_int, gprev_int, pop_c))
  ),
  ordered(4, levels = 1:4, labels = c("Low", "Medium", "High", "Critical"))
)
### No sig change and Critical
expect_identical(
  confirmed_case_composite(
    class_trajectory(score_trajectory(curr_int, nprev_int), pval_trajectory(curr_int, nprev_int)),
    class_burden(score_burden(curr_int, gprev_int, pop_c))
  ),
  ordered(4, levels = 1:4, labels = c("Low", "Medium", "High", "Critical"))
)
### Growing and Critical
expect_identical(
  confirmed_case_composite(
    class_trajectory(score_trajectory(curr_int, gprev_int), pval_trajectory(curr_int, gprev_int)),
    class_burden(score_burden(curr_int, gprev_int, pop_c))
  ),
  ordered(4, levels = 1:4, labels = c("Low", "Medium", "High", "Critical"))
)


