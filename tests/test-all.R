library(testthat)
library(ocs4R)

#test environment
ocs_url <- "http://localhost:8080"
ocs_user <- "admin"
ocs_pwd <- "admin"

OCS <- ocsManager$new(
  url = "http://localhost:8080", 
  user = ocs_user, 
  pwd = ocs_pwd, 
  logger = "DEBUG"
)

if(is(OCS, "ocsManager")){
  cat(sprintf("OCS REST endpoint at '%s' configured with token. Running integration tests...\n", ocs_url))
  test_check("ownCloud4R")
}else{
  cat("OCS REST endpoint at '%s' not configured. Skipping integration tests...\n")
}