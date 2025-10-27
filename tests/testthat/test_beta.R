
# vector test
#
# test errors
expect_error(proxirr::beta(TRUE))
#
# test equalities
expect_equal(proxirr::beta(c(0.5,0.5,0.5)), 0.875)
expect_equal(proxirr::beta(c(0.5,0.5,NA), na.rm = TRUE), 0.75)

# data.frame test
#
# test errors
#
# test equalities
expect_equal(
  proxirr::beta(
    data.frame(
      local = c(25,25,25),
      global = c(100,100,100),
      target = c(50,50,50)
    ),
    'local', 'global', 'target'
    ),
  0.875)
expect_equal(
  proxirr::beta(
    data.frame(
      local = c(25,25,NA),
      global = c(100,100,100),
      target = c(50,50,50)
    ),
    'local', 'global', 'target', na.rm = TRUE
  ),
  0.75)






