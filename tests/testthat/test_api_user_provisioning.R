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
  user.df <- OCS$getUser("admin", TRUE)
  expect_is(user.df, "data.frame")
})

test_that("User Provisioning API - addUser",{
  added <- OCS$addUser("john.doe", password = "ocs4johnsecret")
  expect_true(added)
  users <- OCS$getUsers()
  expect_equal(length(users), 2L)
  user <- OCS$getUser("john.doe")
  expect_is(user, "list")
  expect_true(user$enabled)
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

test_that("User Provisioning API - getUserGroups",{
  admingroups <- OCS$getUserGroups("admin")
  expect_equal(admingroups, "admin")
})
  
test_that("User Provisioning API - getGroups",{
  groups <- OCS$getGroups()
  expect_equal(groups, "admin")
})

test_that("User Provisioning API - addGroup",{
  added <- OCS$addGroup("scientists")
  expect_true(added)
  groups <-  OCS$getGroups()
  expect_equal(groups, c("admin", "scientists"))
})

test_that("User Provisioning API - getGroup",{
  admin_group <- OCS$getGroup("admin")
  expect_equal(admin_group$id, "admin")
  expect_equal(admin_group$users, "admin")
  sc_group <- OCS$getGroup("scientists")
  expect_equal(sc_group$id, "scientists")
  expect_null(sc_group$users)
})

test_that("User Provisioning API - addToGroup",{
  added <- OCS$addToGroup("john.doe", "scientists")
  expect_true(added)
  expect_true("john.doe" %in% OCS$getGroup("scientists")$users)
})

test_that("User Provisioning API - removeFromGroup",{
  removed <- OCS$removeFromGroup("john.doe", "scientists")
  expect_true(removed)
  expect_false("john.doe" %in% OCS$getGroup("scientists")$users)
})

test_that("User Provisioning API - deleteUser",{
  deleted <- OCS$deleteUser("john.doe")
  expect_true(deleted)
  users <- OCS$getUsers()
  expect_false("john.doe" %in% users)
})

test_that("UserProvisioning API - deleteGroup",{
  deleted <- OCS$deleteGroup("scientists")
  expect_true(deleted)
  groups <- OCS$getGroups()
  expect_false("scientists" %in% groups)
})