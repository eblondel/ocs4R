# test_manager.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for OCS REST API manager
#=======================
require(ocs4R, quietly = TRUE)
require(testthat)

context("manager")

test_that("OCS API manager - version",{
  version <- OCS$getVersion()
  expect_equal(names(version)[1:5], c("major", "minor", "micro", "string", "edition"))
})

test_that("OCS API manager - capabilities",{
  caps <- OCS$getCapabilities()
  expect_true(all(c("core", "dav", "files", "files_sharing", "notifications") %in% names(caps)))
})