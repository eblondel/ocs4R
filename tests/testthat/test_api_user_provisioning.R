# test_api_user_provisioning.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for OCS manager User Provisioning API methods
#=======================
require(ownCloud4R, quietly = TRUE)
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