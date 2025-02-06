#' ocsApiSharingManager
#' @docType class
#' @export
#' @keywords ocs manager sharing api
#' @return Object of \code{\link{R6Class}} for modelling an ocsManager for the Sharing API
#' @format \code{\link{R6Class}} object.
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ocsApiSharingManager <-  R6Class("ocsApiSharingManager",
  inherit = ocsManager,
  public = list(
    
    #'@description Initialize manager
    #'@param url url
    #'@param user user
    #'@param pwd pwd
    #'@param logger logger
    #'@param keyring_backend backend to use with \pkg{keyring}. Default is \code{NULL}
    initialize = function(url, user, pwd, logger = NULL,
                          keyring_backend = NULL){
      super$initialize(url, user, pwd, logger, keyring_backend)
    },
    
    #OCS SHARING API
    #-------------------------------------------------------------------------------------------
    
    #'@description Get list of shares as \code{list}. To return as \code{data.frame}, 
    #'set \code{pretty = TRUE}. The method accepts additional parameters. 
    #'@param path path
    #'@param reshares reshares
    #'@param shared_with_me list only those shared with me?
    #'@param state state
    #'@param subfiles subfiles
    #'@param pretty pretty
    #'@return the list of shares as \code{list} or \code{data.frame}
    getShares = function(path = NULL, reshares = FALSE, shared_with_me = NULL,
                         state = NULL, subfiles = FALSE, 
                         pretty = FALSE){
      
      private$checkSharingAPIAvailability()
      
      allowedStates <- c("accepted", "all", "declined", "pending", "rejected")
      if(!is.null(state)) if(!(state %in% allowedStates)){
        errMsg <- sprintf("'state' should be one value among [%s]", 
                          paste(allowedStates, collapse=","))
        self$ERROR(errMsg)
        stop(errMsg)
      }
      
      request <- "ocs/v1.php/apps/files_sharing/api/v1/shares"
      get_req <- ocsRequest$new(
        type = "HTTP_GET", private$url, request,
        private$user, pwd = private$getPassword(), 
        token = private$getToken(), cookies = private$cookies,
        namedParams = list(
          path = path,
          reshares = reshares,
          shared_with_me = shared_with_me,
          state = state,
          subfiles = subfiles
        ),
        logger = self$loggerType
      )
      get_req$execute()
      get_resp <- get_req$getResponse()
      names(get_resp)
      get_out <- get_resp$ocs$data
      if(!is.null(get_out)) if(pretty){
        get_out <- as.data.frame(do.call("rbind",lapply(get_out, unlist)))
      }
      return(get_out)
    },
    
    #'@description Creates a share for the path (file or folder), given a name. The \code{shareType} should be among
    #'    values 'user','group','publiclink' or 'federated'.The \code{shareWith} is required for \code{shareType} 
    #'    'user' and 'group' and corresponds to the username or groupname. The \code{permissions} can be set among
    #'    values 'read', 'update', 'create', 'delete', 'read-write', 'share', 'all'. By default the permissions will 
    #'    be the default permissions set by the ocs server (by default 'all').
    #'@param path path
    #'@param name name
    #'@param shareType the type of share
    #'@param shareWith a list of users to share with
    #'@param publicUpload public upload
    #'@param password to set to access the share
    #'@param permissions permissions
    #'@param expireDate expire date
    createShare = function(path, name, shareType, shareWith, publicUpload = NULL, password = NULL, 
                           permissions = NULL, expireDate = NULL){
      
      private$checkSharingAPIAvailability()
      
      if(!private$capabilities$files_sharing$can_share){
        errMsg <- "The file sharing is not enabled!"
        self$ERROR(errMsg)
        stop(errMsg)
      }
      
      allowedShareTypes <- c("user", "group", "publiclink", "federated")
      if(!(shareType %in% allowedShareTypes)){
        errMsg <- sprintf("Share Type '%s' is not among allowed share types [%s]", 
                          shareType, paste0(allowedShareTypes, collapse=","))
        self$ERROR(errMsg)
        stop(errMsg)
      }
      shareType <- switch(shareType,
        "user" = 0,
        "group" = 1,
        "publiclink" = 3,
        "federated" = 6
      )
      
      if(!is.null(permissions)){
        allowedPermissions <- c("read", "update", "create", "delete", "read/write", "share", "all")
        if(!(permissions %in% allowedPermissions)){
          errMsg <- sprintf("Permission '%s' is not among allowed permissions [%s]",
                            permissions, paste0(allowedPermissions, collapse=","))
          self$ERROR(errMsg)
          stop(errMsg)
        }
        permissions <- switch(permissions,
          "read" = 1,
          "update" = 2,
          "create" = 4,
          "delete" = 8,
          "read/write" = 15,
          "share" = 16,
          "all" = 31
        )
      }else{
        permissions <- private$capabilities$files_sharing$default_permissions
      }
      
      if(!is.null(expireDate)){
        if(!is(expireDate, "Date")){
          errMsg <- "The 'expireDate' should be an object of class 'Date'"
          self$ERROR(errMsg)
          stop(errMsg)
        }else{
          expireDate <- as(expireDate, "character")
        }
      }
      
      if(!is.null(password)) if(!private$capabilities$files_sharing$public$password$enforced){
        self$WARN("The argument 'password' is ignored because OFCS Sharing public API 'password' is not enforced")
        password <- NULL
      }
      if(!is.null(expireDate)) if(!private$capabilities$files_sharing$public$expire_date$enabled){
        self$WARN("The argument 'expireDate' is ignored because OCS Sharing public API 'expireDate' is not enabled")
        expireDate <- NULL
      }
      
      request <- "ocs/v1.php/apps/files_sharing/api/v1/shares"
      post_req <- ocsRequest$new(
        type = "HTTP_POST", private$url, request,
        private$user, pwd = private$getPassword(), 
        token = private$getToken(), cookies = private$cookies,
        namedParams = list(
          name = name,
          path = path,
          shareType = shareType,
          shareWith = shareWith,
          publicUpload = publicUpload,
          password = password,
          permissions = permissions,
          expireDate = expireDate
        ),
        logger = self$loggerType
      )
      post_req$execute()
      return(post_req$getResponse())
    },
    
    #'@description Shares a resource (file or folder) with a user given its username handled with argument \code{username}. 
    #'   The \code{permissions} can be set among values 'read', 'update', 'create', 'delete', 'read-write', 'share', 
    #'   'all'. By default the permissions will be the default permissions set by the ocs server (by default 'all').
    #'   Returns 
    #'@param path path
    #'@param name name
    #'@param username username
    #'@param group group
    #'@param permissions permissions
    #'@param pretty pretty
    #'@return the share properties as \code{list} (or as\code{data.frame} if \code{pretty} is set to TRUE).
    shareWithUser = function(path, name, username, permissions = NULL, pretty = FALSE){
      share_resp <- self$createShare(
        path = path,
        name = URLencode(name),
        shareType = "user",
        shareWith = username,
        permissions = permissions
      )
      share_out <- share_resp$ocs$data
      if(!is.null(share_out)) if(pretty){
        share_out <- as.data.frame(t(share_out))
      }
      return(share_out)
    },
    
    #'@description Shares a resource (file or folder) with a group given its name handled with argument \code{group}. 
    #'   The \code{permissions} can be set among values 'read', 'update', 'create', 'delete', 'read-write', 'share', 
    #'   'all'. By default the permissions will be the default permissions set by the ocs server (by default 'all').
    #'@param path path
    #'@param name name
    #'@param group group
    #'@param permissions permissions
    #'@param pretty pretty
    #'@return the share properties as \code{list} (or as\code{data.frame} if \code{pretty} is set to TRUE).
    shareWithGroup = function(path, name, group, permissions = NULL, pretty = FALSE){
      share_resp <- self$createShare(
        path = path,
        name = URLencode(name),
        shareType = "group",
        shareWith = group,
        permissions = permissions
      )
      share_out <- share_resp$ocs$data
      if(!is.null(share_out)) if(pretty){
        share_out <- as.data.frame(t(share_out))
      }
      return(share_out)
    },
    
    #'@description Shares a resource (file or folder) as public link. The function returns the public link generated by ocs.
    #'@param path path
    #'@param name name
    #'@param publicUpload public upload?
    #'@param password password
    #'@param permissions permissions
    #'@param expireDate expire date
    #'@param return the public sharing link
    shareAsPublicLink = function(path, name = NULL, publicUpload = FALSE, password = NULL, 
                                 permissions = NULL, expireDate = NULL){
      if(!private$capabilities$files_sharing$public$enabled){
        errMsg <- "The OCS Sharing public API is not enabled. Impossible to share as public link!"
        self$ERROR(errMsg)
        stop(errMsg)
      }
      if(is.null(name)) name <- private$capabilities$files_sharing$public$defaultPublicLinkShareName
      
      sharelink_resp <- self$createShare(
        path = path,
        name = URLencode(name),
        shareType = "publiclink",
        shareWith = NULL,
        publicUpload = publicUpload,
        password = password,
        permissions = permissions,
        expireDate = expireDate
      )
      self$INFO(sprintf("Public link for '%s': %s", path, sharelink_resp$ocs$data$url))
      return(sharelink_resp$ocs$data$url)
    }
  )
)
