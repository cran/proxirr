

# Test error evaluations
#
# alphas
expect_error(proxirr:::.gamma(alphas = 'hello'))
expect_error(proxirr:::.gamma(alphas = 45))
expect_error(proxirr:::.gamma(alphas = c('0.5',0.5,0.5)))
expect_error(proxirr:::.gamma(alphas = c(0.5,0.5,1.5)))
#
# na.rm
expect_error(proxirr:::.(alphas = c(0.5,0.5,0.5), na.rm = 'TRUE'))
expect_error(proxirr:::.(alphas = c(0.5,0.5,0.5), na.rm = 1))



# Test equalities
#
#
expect_equal(proxirr:::.gamma(alphas = c(0.25,0.25,0.25,0)), 0.578125)
expect_equal(proxirr:::.gamma(alphas = c(0.25,0.25,0.25)),   0.578125)
expect_equal(proxirr:::.gamma(alphas = c(0.5,0.5,0.5)),      0.875)
expect_equal(proxirr:::.gamma(alphas = c(0.75,0.75,0.75)),   0.984375)
#
expect_equal(proxirr:::.gamma(alphas = c(1,1,1,1)),            4)
expect_equal(proxirr:::.gamma(alphas = c(0,0,0,1)),            1)
expect_equal(proxirr:::.gamma(alphas = c(0.75,0.75,0.75,1)),   1.984375)
expect_equal(proxirr:::.gamma(alphas = c(0.75,0.75,0.75,1,1)), 2.984375)

#
#
expect_equal(proxirr:::.gamma(alphas = c(0.25,0.25,0.25,0,NA), na.rm = TRUE), 0.578125)
expect_equal(proxirr:::.gamma(alphas = c(0.25,0.25,0.25,NA),   na.rm = TRUE), 0.578125)
expect_equal(proxirr:::.gamma(alphas = c(0.5,0.5,0.5,NA),      na.rm = TRUE), 0.875)
expect_equal(proxirr:::.gamma(alphas = c(0.5,0.5,0.5,1,1,NA),  na.rm = TRUE), 2.875)
expect_equal(proxirr:::.gamma(alphas = c(0.75,0.75,0.75,1,NA), na.rm = TRUE), 1.984375)
