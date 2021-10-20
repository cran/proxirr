

# Test error evaluations
#
# alphas
expect_error(proxirr:::.beta(alphas = 'hello'))
expect_error(proxirr:::.beta(alphas = 45))
expect_error(proxirr:::.beta(alphas = c('0.5',0.5,0.5)))
expect_error(proxirr:::.beta(alphas = c(0.5,0.5,1.5)))
#
# na.rm
expect_error(proxirr:::.beta(alphas = c(0.5,0.5,0.5), na.rm = 'TRUE'))
expect_error(proxirr:::.beta(alphas = c(0.5,0.5,0.5), na.rm = 1))



# Test equalities
#
#
expect_equal(proxirr:::.beta(alphas = c(0.25,0.25,0.25,0)), 0.578125)
expect_equal(proxirr:::.beta(alphas = c(0.25,0.25,0.25)),   0.578125)
expect_equal(proxirr:::.beta(alphas = c(0.5,0.5,0.5)),      0.875)
expect_equal(proxirr:::.beta(alphas = c(0.75,0.75,0.75)),   0.984375)
#
expect_equal(proxirr:::.beta(alphas = c(1,1,1,1)), 1)
expect_equal(proxirr:::.beta(alphas = c(0,0,0,1)), 1)
#
#
expect_equal(proxirr:::.beta(alphas = c(0.25,0.25,0.25,0,NA), na.rm = TRUE), 0.578125)
expect_equal(proxirr:::.beta(alphas = c(0.25,0.25,0.25,NA),   na.rm = TRUE), 0.578125)
expect_equal(proxirr:::.beta(alphas = c(0.5,0.5,0.5,NA),      na.rm = TRUE), 0.875)
expect_equal(proxirr:::.beta(alphas = c(0.75,0.75,0.75,NA),   na.rm = TRUE), 0.984375)

