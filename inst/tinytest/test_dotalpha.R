
# Test error evaluations
#
# local greater than global
expect_error(proxirr:::.alpha(local = 2, global = 1, target = 1), 'local value greater than global value')
# zero global
expect_error(proxirr:::.alpha(local = 0, global = 0, target = 1), 'global value must be a positive number')
# negative anything
expect_error(proxirr:::.alpha(local = -1, global = 1, target = 1), 'local value must be a non-negative number')
expect_error(proxirr:::.alpha(local = 1, global = -1, target = 1), 'global value must be a positive number')
expect_error(proxirr:::.alpha(local = 1, global = 1, target = -1), 'target value must be a non-negative number')
# triage
expect_error(proxirr:::.alpha(local = 1, global = 1, target = 1, triage = '1'), 'triage must be logical')
# NA
expect_error(proxirr:::.alpha(local = NA, global = 1, target = 1),  'local value must be a non-negative number')
expect_error(proxirr:::.alpha(local = 1, global = NA, target = 1),  'global value must be a positive number')
expect_error(proxirr:::.alpha(local = 1, global = 1, target = NA),  'target value must be a non-negative number')
# logical
expect_error(proxirr:::.alpha(local = TRUE, global = 1, target = 1))
expect_error(proxirr:::.alpha(local = FALSE, global = 1, target = 1))
expect_error(proxirr:::.alpha(local = 1, global = TRUE, target = 1))
expect_error(proxirr:::.alpha(local = 1, global = FALSE, target = 1))
expect_error(proxirr:::.alpha(local = 1, global = 1, target = TRUE))
expect_error(proxirr:::.alpha(local = 1, global = 1, target = FALSE))

# Test equalities
#
# NA
expect_equal(proxirr:::.alpha(local = NA, global = 1, target = 1, na.allow = TRUE), NA)
expect_equal(proxirr:::.alpha(local = 1, global = NA, target = 1, na.allow = TRUE), NA)
expect_equal(proxirr:::.alpha(local = 1, global = 1, target = NA, na.allow = TRUE), NA)
#
# triage
expect_equal(proxirr:::.alpha(local = 0,   global = 1, target = 2), 0)
expect_equal(proxirr:::.alpha(local = 0.1, global = 1, target = 2), 1)
expect_equal(proxirr:::.alpha(local = 0.1, global = 1, target = 2, triage = TRUE), 0)
expect_equal(proxirr:::.alpha(local = 1,   global = 1, target = 2, triage = TRUE), 0)
#
# values
expect_equal(proxirr:::.alpha(local = 0,   global = 100, target = 100), 0)
expect_equal(proxirr:::.alpha(local = 100, global = 100, target = 100), 1)
#
expect_equal(proxirr:::.alpha(local = 10,  global = 100, target = 50 ), 0.2)
expect_equal(proxirr:::.alpha(local = 25,  global = 100, target = 50 ), 0.5)
expect_equal(proxirr:::.alpha(local = 49,  global = 100, target = 50 ), 0.98)
expect_equal(proxirr:::.alpha(local = 50,  global = 100, target = 50 ), 1)
expect_equal(proxirr:::.alpha(local = 90,  global = 100, target = 50 ), 1)




