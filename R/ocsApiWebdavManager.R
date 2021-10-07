#' ocsApiWebdavManager
#' @docType class
#' @export
#' @keywords ocs manager webdav api
#' @return Object of \code{\link{R6Class}} for modelling an ocsManager for Webdav API
#' @format \code{\link{R6Class}} object.
#' @section General Methods (inherited from 'ocsManager'):
#' \describe{
#'  \item{\code{new(url, user, pwd, logger, keyring_backend)}}{
#'    This method is used to instantiate an ocsApiWebdavManager. The user/pwd are
#'    mandatory in order to connect to 'ocs'. 
#'    
#'    The logger can be either NULL, "INFO" (with minimum logs), or "DEBUG" 
#'    (for complete curl http calls logs).
#'    
#'    The \code{keyring_backend} can be set to use a different backend for storing 
#'    the user password with \pkg{keyring} (Default value is 'env').
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
#' @section WebDAV methods:
#' \describe{
#'  \item{\code{getWebdavRoot()}}{
#'    Get the 'ocs' WebDAV root URL
#'  }
#'  \item{\code{listFiles(relPath)}}{
#'    WebDAV method to list folders/files given a relative path. The relative path is set
#'    to \code{"/"} by default, which corresponds to the root of the 'ocs' repository.
#'  }
#'  \item{\code{makeCollection(name, relPath)}}{
#'    WebDAV method to make a collection. By default \code{relPath} is set to \code{"/"} (root).
#'    The \code{name} is the name of the new collection to be created. The function is recursive
#'    in the sense that a \code{name} can be provided as relative path of a collection tree 
#'    (eg \code{newfolder1/newfolder2/newfolder3}), the function will create recursively as 
#'    many collections are handled in the name. 
#'  }
#'  \item{\code{uploadFile(filename, relPath, delete_if_existing)}}{
#'    WebDAV method to upload a file. By default \code{relPath} is set to \code{"/"} (root).
#'  }
#'  \item{\code{deleteFile(filename, relPath)}}{
#'    WebDAV method to delete a file. By default \code{relPath} is set to \code{"/"} (root).
#'  }
#'  \item{\code{getPublicFile(share_token)}}{
#'    Get details of a shared public file given its share token
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ocsApiWebdavManager <-  R6Class("ocsApiWebdavManager",
  inherit = ocsManager,
  public = list(
    initialize = function(url, user, pwd, logger = NULL,
                          keyring_backend = 'env'){
      super$initialize(url, user, pwd, logger, keyring_backend)
    },
    
    #OCS WEBDAV API
    #-------------------------------------------------------------------------------------------
    
    #getWebdavRoot
    getWebdavRoot = function(){
      return(private$capabilities$core[["webdav-root"]])
    },
    
    #listFiles
    listFiles = function(relPath = "/"){
      if(!startsWith(relPath, "/")) relPath <- paste0("/", relPath)
      request <- paste0(self$getWebdavRoot(), relPath)
      list_req <- ocsRequest$new(
        type = "WEBDAV_PROPFIND", private$url, request,
        private$user, pwd = private$keyring_backend$get(service = private$keyring_service, username = paste0(private$user,"_pwd")), 
        token = private$getToken(), cookies = private$cookies,
        logger = self$loggerType
      )
      list_req$execute()
      list_resp <- list_req$getResponse()
      return(list_resp)
    },
    
    #makeCollection
    makeCollection = function(name, relPath = "/"){
      col_names <- unlist(strsplit(name, "/"))
      if(!startsWith(relPath, "/")) relPath <- paste0("/", relPath)
      if(!endsWith(relPath, "/")) relPath <- paste0(relPath, "/")
      if(length(col_names)==1){
        request <- paste0(self$getWebdavRoot(), relPath, name)
        mkcol_req <- ocsRequest$new(
          type = "WEBDAV_MKCOL", private$url, request,
          private$user, pwd = private$keyring_backend$get(service = private$keyring_service, username = paste0(private$user,"_pwd")), 
          token = private$getToken(), cookies = private$cookies,
          logger = self$loggerType
        )
        mkcol_req$execute()
        mkcol_resp <- mkcol_req$getResponse()
        return(mkcol_resp)
      }else{
        self$INFO(sprintf("Nested collections detected in '%s'. Splitting name to make nested collections", name))
        for(i in 1:length(col_names)){
          newRelPath <- relPath
          if(i>1) newRelPath <- paste0(newRelPath, paste(col_names[1:(i-1)], collapse="/"), "/")
          print(col_names[i])
          print(newRelPath)
          if(!paste0(col_names[i],"/") %in% self$listFiles(relPath = newRelPath)$name) {
            self$makeCollection(col_names[i], newRelPath)
          }
        }
      }
    },
    
    #uploadFile
    uploadFile = function(filename, relPath = "/", delete_if_existing = FALSE){
      
      if(delete_if_existing){
        try(self$deleteFile(filename = filename, relPath = relPath))
      }
      
      if(!startsWith(relPath, "/")) relPath <- paste0("/", relPath)
      if(!endsWith(relPath, "/")) relPath <- paste0(relPath, "/")
      request <- paste0(self$getWebdavRoot(), relPath, basename(filename))
      self$INFO(sprintf("WEBDAV - Uploading file '%s' at '%s'", 
                        filename, paste(private$url, request, sep="/")))
      upload_req <- ocsRequest$new(
        type = "HTTP_PUT", private$url, request,
        private$user, pwd = private$keyring_backend$get(service = private$keyring_service, username = paste0(private$user,"_pwd")), 
        token = private$getToken(), cookies = private$cookies,
        filename = filename,
        logger = self$loggerType
      )
      upload_req$execute()
      if(upload_req$getStatus()==201){
        self$INFO(sprintf("Successfuly uploaded file '%s' at '%s'",
                          filename, paste(private$url, request, sep="/")))
      }else{
        errMsg <- sprintf("WEBDAV - Error while uploading '%s' at '%s'",
                          filename, paste(private$url, request, sep="/"))
        self$ERROR(errMsg)
        stop(errMsg)
      }
      upload_resp <- upload_req$getResponse()
      return(upload_resp)
    },
    
    #deleteFile
    deleteFile = function(filename, relPath = "/"){
      if(!startsWith(relPath, "/")) relPath <- paste0("/", relPath)
      if(!endsWith(relPath, "/")) relPath <- paste0(relPath, "/")
      request <- paste0(self$getWebdavRoot(), relPath, basename(filename))
      self$INFO(sprintf("WEBDAV - Delete file '%s' at '%s'", 
                        filename, paste(private$url, request, sep="/")))
      upload_req <- ocsRequest$new(
        type = "HTTP_DELETE", private$url, request,
        private$user, pwd = private$keyring_backend$get(service = private$keyring_service, username = paste0(private$user,"_pwd")), 
        token = private$getToken(), cookies = private$cookies,
        filename = filename,
        logger = self$loggerType
      )
      upload_req$execute()
      if(upload_req$getStatus()==204){
        self$INFO(sprintf("Successfuly deleted file '%s' at '%s'",
                          filename, paste(private$url, request, sep="/")))
      }else{
        errMsg <- sprintf("WEBDAV - Error while deleting '%s' at '%s'",
                          filename, paste(private$url, request, sep="/"))
        self$ERROR(errMsg)
        stop(errMsg)
      }
      upload_resp <- upload_req$getResponse()
      return(upload_resp)
    },
    
    #downloadFile
    downloadFile = function(relPath, filename, outdir = "."){
      request <- sprintf("remote.php/dav/files/%s/%s/%s", private$user, relPath, filename)
      file_req <- ocsRequest$new(
        type = "HTTP_GET", private$url, request, format = NULL, namedParams = list(),
        private$user, pwd = private$keyring_backend$get(service = private$keyring_service, username = paste0(private$user,"_pwd")), 
        token = private$getToken(), cookies = private$cookies,
        logger = self$loggerType
      )
      file_req$execute()
      file_resp <- file_req$getResponse()
      writeBin(object = file_resp, con = file.path(outdir, filename))
      return(file.path(outdir, filename))
    },
    
    #getPublicFile
    getPublicFile = function(share_token){
      request <- file.path("remote.php/dav/public-files", share_token)
      file_req <- ocsRequest$new(
        type = "WEBDAV_PROPFIND", private$url, request,
        anonymous = TRUE, logger = self$loggerType
      )
      file_req$execute()
      file_resp <- file_req$getResponse()
      return(file_resp)
    }
    
  )
)
