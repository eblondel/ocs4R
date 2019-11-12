# test_webdav.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for Webdav operations
#=======================
require(ocs4R, quietly = TRUE)
require(testthat)

context("webdav")

test_that("WEBDAV - list files",{
  files <- OCS$listFiles()
  expect_is(files, "data.frame")
  expect_true(nrow(files)>0)
  subfiles <- OCS$listFiles("Documents")
  expect_is(subfiles, "data.frame")
  expect_true(nrow(subfiles)>0)
})

test_that("WEBDAV - make collection",{
  myfolder_url <- OCS$makeCollection("myfolder")
  expect_is(myfolder_url, "character")
  expect_true(endsWith(myfolder_url, "myfolder"))
  files <- OCS$listFiles()
  expect_true("myfolder/" %in% files$name)
  subfiles <- OCS$listFiles("myfolder")
  expect_is(subfiles, "data.frame")
  expect_true(nrow(subfiles)==0)
})

test_that("WEBDAV - upload file",{
  nf <- file.create("test.txt")
  nfcon <- file("test.txt", "wb")
  writeChar("This is a test file", nfcon)
  nfname <- OCS$uploadFile("test.txt")
  expect_true("test.txt" %in% OCS$listFiles()$name)
})