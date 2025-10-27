
# single alpha
#
# errors
expect_error(proxirr::alpha(10,100,NA))
expect_error(proxirr::alpha(10,100,NA, na.allow = FALSE))
#
# equalities
expect_equal(proxirr::alpha(25,100,50),0.5)
expect_equal(proxirr::alpha(25,100,NA, na.allow = TRUE),NA)

# vector alpha
#
# errors
expect_error(proxirr::alpha(c(1,25,49), c(100,100,100), c(50,50,NA), na.allow = FALSE))
expect_error(proxirr::alpha(c(1,25,49), c(100,100,100), c(50,50)))
expect_error(proxirr::alpha(c(1,25,49), c(100,100,100), 50))
#
# equalities
expect_equal(proxirr::alpha(c(1,25,49), c(100,100,100), c(50,50,50)), c(0.02,0.5,0.98))
expect_equal(proxirr::alpha(c(NA,25,49), c(100,100,100), c(50,50,50)), c(NA,0.5,0.98))
expect_equal(proxirr::alpha(c(1,25,49), c(100,NA,100), c(50,50,50)), c(0.02,NA,0.98))
expect_equal(proxirr::alpha(c(1,25,49), c(100,100,100), c(50,50,NA)), c(0.02,0.5,NA))

# data.frame alpha
#
test_df = data.frame(
  local = c(1,25,49),
  global = c(100,100,100),
  target = c(50,50,50),
  alpha = c(0.02,0.5,0.98)
)
# errors
expect_error(proxirr::alpha('local', 'global', 'target', df = test_df, alpha_col = 'alpha'))
#
# equalities
expect_equal(proxirr::alpha('local', 'global', 'target', df = test_df), c(0.02,0.5,0.98))
expect_equal(proxirr::alpha('local', 'global', 'target', df = test_df[,c(1,2,3)], alpha_col = 'alpha'), test_df)

