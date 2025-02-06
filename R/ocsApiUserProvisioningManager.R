#' ocsApiUserProvisioningManager
#' @docType class
#' @export
#' @keywords ocs manager userprovisioning api
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ocsManager for Webdav API
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ocsApiUserProvisioningManager <-  R6Class("ocsApiUserProvisioningManager",
  inherit = ocsManager,
  public = list(
    
    #'@description Initialize manager
    #'@param url url
    #'@param user user
    #'@param pwd pwd
    #'@param logger logger
    #'@param keyring_backend backend to use with \pkg{keyring}. Default is \code{NULL}
    initialize = function(url, user, pwd, logger = NULL,
                          keyring_backend = 'env'){
      super$initialize(url, user, pwd, logger, keyring_backend)
    },
    
    #OCS USER PROVISIONING API
    #-------------------------------------------------------------------------------------------
    
    #Instruction set for users
    #-------------------------------------------------------------------------------------------
    
    #'@description Adds a user given a \code{userid} (required). All other fields (email, password, groups) are
    #'    optional for the user creation. Returns \code{TRUE} if the user is added, \code{FALSE} otherwise.
    #'@param userid user ID
    #'@param email email
    #'@param password user password
    #'@param groups groups
    addUser = function(userid, email = NULL, password = NULL, groups = NULL){
      request <- "ocs/v1.php/cloud/users"
      post_req <- ocs4R::ocsRequest$new(
        type = "HTTP_POST", private$url, request,
        private$user, pwd = private$getPassword(), 
        token = private$getToken(), cookies = private$cookies,
        content = list(
          userid = userid,
          email = email,
          password = password,
          groups = groups
        ),
        contentType = NULL,
        logger = self$loggerType
      )
      post_req$execute()
      post_req_resp <- post_req$getResponse()
      added <- post_req_resp$ocs$meta$statuscode == 100
      return(added)
    },
    
    #'@description Get the list of users. This method returns a vector of class 'character' giving 
    #'the user IDs available in the OCS cloud plateform.
    getUsers = function(){
      get_users <- ocs4R::ocsRequest$new(
        type = "HTTP_GET", private$url, "ocs/v1.php/cloud/users",
        private$user, pwd = private$getPassword(),
        token = private$getToken(), cookies = private$cookies,
        logger = self$loggerType
      )
      get_users$execute()
      get_users_resp <- get_users$getResponse()
      users <- unlist(get_users_resp$ocs$data$users)
      return(users)
    },
    
    #'@description Get the user details from its \code{userid}. If the argument \code{pretty} is set to TRUE,
    #'    this will return an object of class \code{data.frame}, otherwise (by default) it returns 
    #'    an object of class \code{list}.
    #'@param userid user ID
    #'@param pretty pretty
    getUser = function(userid, pretty = FALSE){
      get_user <- ocs4R::ocsRequest$new(
        type = "HTTP_GET", private$url, sprintf("ocs/v1.php/cloud/users/%s", userid),
        private$user, pwd = private$getPassword(), 
        token = private$getToken(), cookies = private$cookies,
        logger = self$loggerType
      )
      get_user$execute()
      get_user_resp <- get_user$getResponse()
      user <- get_user_resp$ocs$data
      class(user$enabled) <- "logical"
      class(user$two_factor_auth_enabled) <- "logical"
      if(pretty){
        user <- as.data.frame(t(unlist(user)))
        row.names(user) <- 1
      }
      return(user)
    },
    
    #'@description Edits a user, identifier by a userid. The user property to be edited should be set using its
    #'    key (eg display) and the value to be modified for this key. Returns \code{TRUE} if the user 
    #'    is edited, \code{FALSE} otherwise.
    #'@param userid user ID
    #'@param key key
    #'@param value value
    editUser = function(userid, key, value){
      allowedKeys <- c("email", "quota", "display", "password")
      if(!key %in% allowedKeys){
        errMsg <- sprintf("Key should be among the following [%s]", paste(allowedKeys, collapse=","))
        self$ERROR(errMsg)
        stop(errMs)
      }
      request <- sprintf("ocs/v1.php/cloud/users/%s", userid)
      put_req <- ocsRequest$new(
        type = "HTTP_PUT", private$url, request,
        private$user, pwd = private$getPassword(), 
        token = private$getToken(), cookies = private$cookies,
        content = list(key = key, value = value),
        logger = self$loggerType
      )
      put_req$execute()
      put_req_resp <- put_req$getResponse()
      edited <- FALSE
      if(is(put_req_resp, "list")) if(!is.null(put_req_resp$key) && !is.null(put_req_resp$value)){
        if(put_req_resp$key == key && put_req_resp$value == value) edited <- TRUE
      }
      return(edited)
    },
    
    #'@description Edits a user display name.
    #'@param userid user ID
    #'@param displayName display name
    editUserDisplayName = function(userid, displayName){
      return(self$editUser(userid, key = "display", value = displayName))
    },
    
    #'@description Edits a user email
    #'@param userid user ID
    #'@param email email
    editUserEmail = function(userid, email){
      return(self$editUser(userid, key = "email", value = email))
    },
    
    #'@description Edits a user password
    #'@param userid user ID
    #'@param password password
    editUserPassword = function(userid, password){
      return(self$editUser(userid, key = "password", value = password))
    },
    
    #'@description Edits a user quota
    #'@param userid user ID
    #'@param quota quota
    editUserQuota = function(userid, quota){
      return(self$editUser(userid, key = "quota", value = quota))
    },
    
    #'@description Enables a user
    #'@param userid user ID
    #'@return \code{TRUE} if enabled, \code{FALSE} otherwise
    enableUser = function(userid){
      request <- sprintf("ocs/v1.php/cloud/users/%s/enable", userid)
      put_req <- ocsRequest$new(
        type = "HTTP_PUT", private$url, request,
        private$user, pwd = private$getPassword(), 
        token = private$getToken(), cookies = private$cookies,
        content = "",
        logger = self$loggerType
      )
      put_req$execute()
      return(TRUE)
    },
    
    #'@description Disables a user
    #'@param userid user ID
    #'@return \code{TRUE} if disabled, \code{FALSE} otherwise
    disableUser = function(userid){
      request <- sprintf("ocs/v1.php/cloud/users/%s/disable", userid)
      put_req <- ocsRequest$new(
        type = "HTTP_PUT", private$url, request,
        private$user, pwd = private$getPassword(), 
        token = private$getToken(), cookies = private$cookies,
        content = "",
        logger = self$loggerType
      )
      put_req$execute()
      return(TRUE)
    },
    
    #'@description Deletes a user
    #'@param userid user ID
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    deleteUser = function(userid){
      request <- sprintf("ocs/v1.php/cloud/users/%s", userid)
      delete_req <- ocsRequest$new(
        type = "HTTP_DELETE", private$url, request,
        private$user, pwd = private$getPassword(), 
        token = private$getToken(), cookies = private$cookies,
        logger = self$loggerType
      )
      delete_req$execute()
      return(TRUE)
    },
    
    #'@description Get user groups
    #'@param userid user ID
    #'@return the user groups
    getUserGroups = function(userid){
      get_usergroups <- ocs4R::ocsRequest$new(
        type = "HTTP_GET", private$url, sprintf("ocs/v1.php/cloud/users/%s/groups", userid),
        private$user, pwd = private$getPassword(), 
        token = private$getToken(), cookies = private$cookies,
        logger = self$loggerType
      )
      get_usergroups$execute()
      get_usergroups_resp <- get_usergroups$getResponse()
      usergroups <- unlist(get_usergroups_resp$ocs$data$groups)
      return(usergroups)
    },
    
    #'@description Adds a user to a group.
    #'@param userid user ID
    #'@param groupid group ID
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addToGroup = function(userid, groupid){
      request <- sprintf("ocs/v1.php/cloud/users/%s/groups", userid)
      post_req <- ocs4R::ocsRequest$new(
        type = "HTTP_POST", private$url, request,
        private$user, pwd = private$getPassword(), 
        token = private$getToken(), cookies = private$cookies,
        content = list(groupid = groupid),
        contentType = NULL,
        logger = self$loggerType
      )
      post_req$execute()
      post_req_resp <- post_req$getResponse()
      added <- post_req_resp$ocs$meta$statuscode == 100
      return(added)
    },
    
    #'@description Removes a user from a group.
    #'@param userid user ID
    #'@param groupid group ID
    #'@return \code{TRUE} if removed, \code{FALSE} otherwise
    removeFromGroup = function(userid, groupid){
      request <- sprintf("ocs/v1.php/cloud/users/%s/groups", userid)
      delete_req <- ocsRequest$new(
        type = "HTTP_DELETE", private$url, request,
        private$user, pwd = private$getPassword(), 
        token = private$getToken(), cookies = private$cookies,
        content = list(groupid = groupid),
        logger = self$loggerType
      )
      delete_req$execute()
      return(TRUE)
    },
    
    #'@description Creates a subadmin
    createSubadmin = function(){
      stop("'createSubadmin' method not yet implemented")
    },
    
    #'@description Removes a subadmin
    removeSubadmin = function(){
      stop("'removeSubadmin' method not yet implemented")
    },
    
    #'@description Get subadmin groups
    getSubadminGroups = function(){
      stop("'getSubadminGroups' method not yet implemented")
    },
    
    #Instruction set for groups 
    #-------------------------------------------------------------------------------------------
    
    #'@description Get the list of groups. This method returns a vector of class 
    #''character' giving the usergroups IDs
    #'@param search search
    #'@param limit limit
    #'@param offset offset
    getGroups = function(search = NULL, limit = NULL, offset = NULL){
      get_groups <- ocs4R::ocsRequest$new(
        type = "HTTP_GET", private$url, "ocs/v1.php/cloud/groups",
        private$user, pwd = private$getPassword(), 
        token = private$getToken(), cookies = private$cookies,
        namedParams = list(search = search, limit = limit, offset = offset),
        logger = self$loggerType
      )
      get_groups$execute()
      get_groups_resp <- get_groups$getResponse()
      groups <- unlist(get_groups_resp$ocs$data$groups)
      return(groups)
    },
    
    #'@description Adds a group
    #'@param groupid group ID
    #'@return \code{TRUE} if added, \code{FALSE}
    addGroup = function(groupid){
      request <- "ocs/v1.php/cloud/groups"
      post_req <- ocs4R::ocsRequest$new(
        type = "HTTP_POST", private$url, request,
        private$user, pwd = private$getPassword(), 
        token = private$getToken(), cookies = private$cookies,
        content = list(groupid = groupid),
        contentType = NULL,
        logger = self$loggerType
      )
      post_req$execute()
      post_req_resp <- post_req$getResponse()
      added <- post_req_resp$ocs$meta$statuscode == 100
      return(added)
    },
    
    #'@description Gets a group
    #'@param groupid group ID
    #'@return the group as \code{list} including the group ID and the list of users
    getGroup = function(groupid){
      get_group <- ocs4R::ocsRequest$new(
        type = "HTTP_GET", private$url, sprintf("ocs/v1.php/cloud/groups/%s", groupid),
        private$user, pwd = private$getPassword(), 
        token = private$getToken(), cookies = private$cookies,
        logger = self$loggerType
      )
      get_group$execute()
      get_group_resp <- get_group$getResponse()
      group <- list(id = groupid, users = unlist(get_group_resp$ocs$data$users))
      return(group)
    },
    
    #'@description Deletes a group
    #'@param groupid group ID
    #'@return \code{TRUE} if deleted, \code{FALSE}
    deleteGroup = function(groupid){
      request <- sprintf("ocs/v1.php/cloud/groups/%s", groupid)
      delete_req <- ocsRequest$new(
        type = "HTTP_DELETE", private$url, request,
        private$user, pwd = private$getPassword(), 
        token = private$getToken(), cookies = private$cookies,
        logger = self$loggerType
      )
      delete_req$execute()
      return(TRUE)
    },
    
    #'@description Get subadmins
    getSubadmins = function(){
      stop("'getSubadmins' method not yet implemented")
    }
    
  )
)
