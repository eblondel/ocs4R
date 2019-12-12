#' ocsApiUserProvisioningManager
#' @docType class
#' @export
#' @keywords ocs manager userprovisioning api
#' @return Object of \code{\link{R6Class}} for modelling an ocsManager for Webdav API
#' @format \code{\link{R6Class}} object.
#' @section General Methods (inherited from 'ocsManager'):
#' \describe{
#'  \item{\code{new(url, user, pwd, logger)}}{
#'    This method is used to instantiate an ocsApiUserProvisioningManager. The user/pwd are
#'    mandatory in order to connect to 'ocs'. The logger can be either
#'    NULL, "INFO" (with minimum logs), or "DEBUG" (for complete curl 
#'    http calls logs)
#'  }
#'  \item{\code{connect()}}{
#'    A method to connect to 'ocs' and set version/capabilities
#'  }
#'  \item{\code{getVersion()}}{
#'    Get the 'ocs' server version
#'  }
#'  \item{\code{getCapabilities()}}{
#'    Get the 'ocs' server capabilities
#'  }
#' }
#'
#' @section User Provisioning methods:
#' \describe{
#'  \item{\code{addUser(userid, email, password, groups)}}{
#'    Adds a user given a \code{userid} (required). All other fields (email, password, groups) are
#'    optional for the user creation. Returns \code{TRUE} if the user is added, \code{FALSE} otherwise.
#'  }
#'  \item{\code{getUsers()}}{
#'    Get the list of users. This method returns a vector of class 'character' giving
#'    the user IDs available in the OCS cloud plateform.
#'  }
#'  \item{\code{getUser(userid, pretty)}}{
#'    Get the user details from its \code{userid}. If the argument \code{pretty} is set to TRUE,
#'    this will return an object of class \code{data.frame}, otherwise (by default) it returns 
#'    an object of class \code{list}.
#'  }
#'  \item{\code{editUser(userid, key, value)}}{
#'    Edits a user, identifier by a userid. The user property to be edited should be set using its
#'    key (eg displayname) and the value to be modified for this key. Returns \code{TRUE} if the user 
#'    is edited, \code{FALSE} otherwise.
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ocsApiUserProvisioningManager <-  R6Class("ocsApiUserProvisioningManager",
  inherit = ocsManager,
  public = list(
    initialize = function(url, user, pwd, logger = NULL){
      super$initialize(url, user, pwd, logger)
    },
    
    #OCS USER PROVISIONING API
    #-------------------------------------------------------------------------------------------
    
    #Instruction set for users
    #-------------------------------------------------------------------------------------------
    
    #addUser
    addUser = function(userid, email = NULL, password = NULL, groups = NULL){
      request <- "ocs/v1.php/cloud/users"
      post_req <- ocs4R::ocsRequest$new(
        type = "HTTP_POST", private$url, request,
        private$user, private$pwd, token = private$token, cookies = private$cookies,
        content = list(
          userid = userid,
          email = email,
          password = password,
          groups = groups
        ),
        logger = self$loggerType
      )
      post_req$execute()
      post_req_resp <- post_req$getResponse()
      added <- post_req_resp$ocs$meta$statuscode == 100
      return(added)
    },
    
    #getUsers
    getUsers = function(){
      get_users <- ocs4R::ocsRequest$new(
        type = "HTTP_GET", private$url, "ocs/v1.php/cloud/users",
        private$user, private$pwd, token = private$token, cookies = private$cookies,
        logger = self$loggerType
      )
      get_users$execute()
      get_users_resp <- get_users$getResponse()
      users <- unlist(get_users_resp$ocs$data$users)
      return(users)
    },
    
    #getUser
    getUser = function(userid, pretty = FALSE){
      get_user <- ocs4R::ocsRequest$new(
        type = "HTTP_GET", private$url, sprintf("ocs/v1.php/cloud/users/%s", userid),
        private$user, private$pwd, token = private$token, cookies = private$cookies,
        logger = self$loggerType
      )
      get_user$execute()
      get_user_resp <- get_user$getResponse()
      user <- get_user_resp$ocs$data
      if(pretty){
        user <- t(as.data.frame(unlist(user)))
        row.names(user) <- 1
      }
      return(user)
    },
    
    #editUser
    editUser = function(userid, key, value){
      request <- sprintf("ocs/v1.php/cloud/users/%s", userid)
      post_req <- ocsRequest$new(
        type = "HTTP_PUT", private$url, request,
        private$user, private$pwd, token = private$token, cookies = private$cookies,
        content = list(
          key = key,
          value = value
        ),
        logger = self$loggerType
      )
      post_req$execute()
      post_req_resp <- post_req$getResponse()
      edited <- post_req_resp$ocs$meta$statuscode == 100
      return(edited)
    },
    
    #enableUser
    enableUser = function(){
      stop("'enableUser' method not yet implemented")
    },
    
    #disableUser
    disableUser = function(){
      stop("'disableUser' method not yet implemented")
    },
    
    #deleteuser
    deleteuser = function(){
      stop("'deleteUser' method not yet implemented")
    },
    
    #getUserGroups
    getUserGroups = function(){
      stop("'getGroups' method not yet implemented")
    },
    
    #addToGroup
    addToGroup = function(){
      stop("'addToGroup' method not yet implemented")
    },
    
    #removeFromGroup
    removeFromGroup = function(){
      stop("'removeFromGroup' method not yet implemented")
    },
    
    #createSubadmin
    createSubadmin = function(){
      stop("'createSubadmin' method not yet implemented")
    },
    
    #removeSubadmin
    removeSubadmin = function(){
      stop("'removeSubadmin' method not yet implemented")
    },
    
    #getSubadminGroups
    getSubadminGroups = function(){
      stop("'getSubadminGroups' method not yet implemented")
    },
    
    #Instruction set for groups 
    #-------------------------------------------------------------------------------------------
    
    #getGroups
    getGroups = function(){
      stop("'getGroups' method not yet implemented")
    },
    
    #addGroup
    addGroup = function(){
      stop("'addGroup' method not yet implemented")
    },
    
    #getGroup
    getGroup = function(){
      stop("'getGroup' method not yet implemented")
    },
    
    #getSubadmins
    getSubadmins = function(){
      stop("'getSubadmins' method not yet implemented")
    },
    
    #deleteGroup
    deleteGroup = function(){
      stop("'deleteGroup' method not yet implemented")
    }
    
  )
)
