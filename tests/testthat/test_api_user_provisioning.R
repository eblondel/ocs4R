# test_api_user_provisioning.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for OCS manager User Provisioning API methods
#=======================
require(ocs4R, quietly = TRUE)
require(testthat)

context("api-user-provisioning")

test_that("User Provisioning API - getUsers",{
  users <- OCS$getUsers()
  expect_is(users, "character")
  expect_equal(length(users), 1L)
})

test_that("User Provisioning API - getUser",{
  user <- OCS$getUser("admin")
  expect_is(user, "list")
  expect_equal(length(user), 6L)
  user.df <- OCS$getUser("admin", TRUE)
  expect_is(user.df, "data.frame")
  expect_equal(ncol(user.df), 9L)
})

test_that("User Provisioning API - addUser",{
  added <- OCS$addUser("john.doe", password = "owncloud4john")
  expect_true(added)
  users <- OCS$getUsers()
  expect_equal(length(users), 2L)
  user <- OCS$getUser("john.doe")
  expect_is(user, "list")
  expect_true(user$enabled)
  expect_equal(user$quota$definition, "default")
  expect_equal(user$displayname, "john.doe")
  expect_null(user$email)
})

test_that("User Provisioning API - disableUser / enableUser",{
  disabled <- OCS$disableUser("john.doe")
  expect_true(disabled)
  user <- OCS$getUser("john.doe")
  expect_false(user$enabled)
  enabled <- OCS$enableUser("john.doe")
  expect_true(enabled)
  user <- OCS$getUser("john.doe")
  expect_true(user$enabled)
})

test_that("User Provisioning API - editUser",{
  edited <- OCS$editUser("john.doe", key = "display", value = "John Doe")
  expect_true(edited)
  john <- OCS$getUser("john.doe")
  expect_equal(john$displayname, "John Doe")
  edited2 <- OCS$editUserDisplayName("john.doe", displayName = "John Doe Jr.")
  expect_true(edited2)
  john <- OCS$getUser("john.doe")
  expect_equal(john$displayname, "John Doe Jr.")
})

test_that("User Provisioning API - deleteUser",{
  deleted <- OCS$deleteUser("john.doe")
  expect_true(deleted)
  users <- OCS$getUsers()
  expect_false("john.doe" %in% users)
})
  