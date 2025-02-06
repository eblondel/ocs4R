#' ocsManager
#' @docType class
#' @export
#' @keywords ocs manager
#' @return Object of \code{\link{R6Class}} for modelling an ocsManager
#' @format \code{\link{R6Class}} object.

#' @examples
#' \dontrun{
#'    #Not Run:
#'    #Connect to an OCS API
#'    OCS <- ocsManager$new(url = ocs_url, user = ocs_user, pwd = ocs_pwd, logger = "DEBUG")
#'    version <- OCS$getVersion()
#'    caps <- OCS$getCapabilities()
#'    
#'    #OCS User Provisioning API
#'    #-------------------------------------
#'    #users
#'    users <- OCS$getUsers() #get users
#'    user <- OCS$getUser("admin") #get a user
#'    user.df <- OCS$getUser("admin", TRUE) #the same user as data.frame
#'    added <- OCS$addUser("john.doe", password = "ocs4john") #add a user
#'    disabled <- OCS$disableUser("john.doe") #disable a user
#'    enabled <- OCS$enableUser("john.doe") #enable auser
#'    edited <- OCS$editUser("john.doe", key = "display", value = "John Doe") #edit user
#'    #edit some user field
#'    edited2 <- OCS$editUserDisplayName("john.doe", displayName = "John Doe Jr.") 
#'    deleted <- OCS$deleteUser("john.doe")
#'    
#'    #groups
#'    admingroups <- OCS$getUserGroups("admin")
#'    groups <- OCS$getGroups()
#'    added <- OCS$addGroup("scientists") #add a new group
#'    sc_group <- OCS$getGroup("scientists") #get group details
#'    added <- OCS$addToGroup("john.doe", "scientists") #add user to group
#'    removed <- OCS$removeFromGroup("john.doe", "scientists") #remove user from group
#'    deleted <- OCS$deleteGroup("scientists")
#'    
#'    #OCS Webdav API
#'    #-------------------------------------
#'    #list files
#'    files <- OCS$listFiles()
#'    subfiles <- OCS$listFiles("Documents")
#'    #make collection
#'    OCS$makeCollection("myfolder")
#'    subfiles <- OCS$listFiles("myfolder")
#'    #upload a file?
#'    filename <- "magic.txt"
#'    file.create(filename); writeLines("ocs4R is great", filename)
#'    #we upload the file in 'Documents' folder
#'    OCS$uploadFile(filename, "/Documents")
#'    #check if file is uploaded
#'    OCS$listFiles('Documents')
#'    
#'    #OCS Sharing API
#'    #-------------------------------------
#'    #let's add a user with User provisioning API
#'    added <- OCS$addUser("john.doe", password = "ocs4john") #add a user
#'    #let's share the previously uploaded file with John Doe
#'    OCS$shareWithUser("/Documents", filename, "john.doe")
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ocsManager <-  R6Class("ocsManager",
  inherit = ocs4RLogger,
  lock_objects = FALSE,
  private = list(
    url = NULL,
    user = NULL,
    pwd = NULL,
    token =  NULL,
    cookies = NULL,
    version = NULL,
    capabilities = NULL,
    
    keyring_backend = NULL,
    keyring_service = NULL,
    
    #getPassword
    getPassword = function(){
      if(!is.null(private$keyring_backend)){
        private$keyring_backend$get(service = private$keyring_service, username = paste0(private$user,"_pwd"))
      }else{
        private$pwd
      }
    },
    
    #getToken (if existing)
    getToken = function(){
      token <- NULL
      if(!is.null(private$keyring_service)){
        keyring_token <- suppressWarnings(try(private$keyring_backend$get(service = private$keyring_service, username = paste0(private$user, "_token")), silent = TRUE))
        if(!is(keyring_token, "try-error")) token <- keyring_token
      }else{
        token <-private$token
      }
      return(token)
    },
    
    #checkAPIAvailability
    checkAPIAvailability = function(name, element){
      if(!private$capabilities[[element]]$api_enabled){
        errMsg <- sprintf("This method requires the 'OCS' REST API %s to be enabled.", name)
        errMsg <- paste(errMsg, "Please get in touch with your administrator")
        self$ERROR(errMsg)
        stop(errMsg)
      }
    },
    
    #check
    checkSharingAPIAvailability = function(){
      private$checkAPIAvailability("OCS Sharing API", "files_sharing")
    }
  ),
  public = list(
    #'@field apis list of APIs
    apis = list(),
    
    #'@description This method is used to instantiate an ocsManager. The user/pwd are
    #'mandatory in order to connect to 'ocs'. The logger can be either NULL, "INFO" 
    #'(with minimum logs), or "DEBUG" (for complete curl http calls logs).
    #'
    #'The \code{keyring_backend} can be set to use a different backend for storing 
    #'the user password with \pkg{keyring} (Default value is NULL, meaning the password 
    #'is stored as private field).
    #'
    #'@param url url
    #'@param user user
    #'@param pwd pwd
    #'@param logger logger type 
    #'@param keyring_backend keyring back-end. Default is \code{NULL}
    initialize = function(url, user, pwd, logger = NULL,
                          keyring_backend = NULL){
      super$initialize(logger = logger)
      private$url = url
      private$user <- user
      if(!is.null(keyring_backend)){
        if(!keyring_backend %in% names(keyring:::known_backends)){
          errMsg <- sprintf("Backend '%s' is not a known keyring backend!", keyring_backend)
          self$ERROR(errMsg)
          stop(errMsg)
        }
        
        private$keyring_backend <- keyring:::known_backends[[keyring_backend]]$new()
        private$keyring_service <- paste0("ocs4R@", url)
        private$keyring_backend$set_with_value(private$keyring_service, username = paste0(user,"_pwd"), password = pwd)
      }else{
        private$pwd <- pwd
      }
      
      #try to connect
      if(!startsWith(self$getClassName(), "ocsApi")){
        
        #test connection
        self$connect()
        
        #inherit managers methods (experimenting)
        list_of_classes <- rev(ls("package:ocs4R"))
        supportedManagers <- list_of_classes[regexpr("ocsApi.+Manager", list_of_classes)>0]
        for(manager in supportedManagers){
          class <- eval(parse(text=manager))
          man <- class$new(url, user, pwd, logger, keyring_backend)
          api_name <- tolower(unlist(strsplit(unlist(strsplit(manager, "ocsApi"))[2],"Manager"))[1])
          self$apis[[api_name]] <- man
          list_of_methods <- rev(names(man))
          for(method in list_of_methods){
            methodObj <- man[[method]]
            if(!(method %in% names(self)) && class(methodObj) == "function"){1
              self[[method]] <- methodObj
              environment(self[[method]]) <- environment(self$connect)
            } 
          }
        }
      }
      invisible(self)
    },
    
    #'@description Method to connect to 'ocs' and set version/capabilities
    connect = function(){
      caps_req <- ocsRequest$new(
        type = "HTTP_GET", private$url, "ocs/v1.php/cloud/capabilities",
        private$user, pwd = private$getPassword(), 
        logger = self$loggerType
      )
      caps_req$execute()
      caps_resp <- caps_req$getResponse()
      
      req_cookies <- caps_resp$cookies
      cookies <- as.list(req_cookies$value)
      names(cookies) <- req_cookies$name
      if(length(cookies[names(cookies)=="XSRF-TOKEN"])>0){
        token <- cookies[names(cookies)=="XSRF-TOKEN"][[1]]
        if(!is.null(private$keyring_backend)){
          private$keyring_backend$set_with_value(private$keyring_service, username = paste0(private$user,"_token"), password = token) 
        }else{
          private$token = token
        }
      }
      cookies <- unlist(cookies[names(cookies)!="XSRF-TOKEN"])
      private$cookies <- paste0(sapply(names(cookies), function(cookiename){paste0(cookiename,"=",cookies[[cookiename]])}),collapse=";")
      
      keyring_token <- private$getToken()
      if(!is.null(keyring_token)){
        caps_req <- ocsRequest$new(
          type = "HTTP_GET", private$url, "ocs/v1.php/cloud/capabilities",
          private$user, token = keyring_token, cookies = private$cookies, 
          logger = self$loggerType
        )
        caps_req$execute()
        caps_resp <- caps_req$getResponse()
      }
      
      if(caps_resp$ocs$meta$status == "failure"){
        errMsg <- sprintf("Could not connect to ocs '%s': %s", private$url, caps_resp$ocs$meta$message)
        self$ERROR(errMsg)
        stop(errMsg)
      }
      if(caps_resp$ocs$meta$status == "ok"){
        self$INFO(sprintf("Successful connection to ocs '%s'!", private$url))
        private$version = caps_resp$ocs$data$version
        private$capabilities = caps_resp$ocs$data$capabilities
      }
    },
    
    #'@description  Get the 'ocs' server version
    getVersion = function(){
      return(private$version)
    },
    
    #'@description  Get the 'ocs' server capabilities
    getCapabilities = function(){
      return(private$capabilities)
    },
    
    #'@description Get the Webdav API manager
    #'@return an instance of \llink{ocsApiWebdavManager}
    getAPIWebdavManager = function(){
      return(self$apis[["webdav"]])
    },
    
    #'@description Get the Sharing API manager
    #'@return an instance of \llink{ocsApiSharingManager}
    getAPISharingManager = function(){
      return(self$apis[["sharing"]])
    },
    
    #'@description Get the User Provisioning API manager
    #'@return an instance of \llink{ocsApiUserProvisioningManager}
    getAPIUserProvisioningManager = function(){
      return(self$apis[["userprovisioning"]])
    }
    
  )
)
