#' ocsApiWebdavManager
#' @docType class
#' @export
#' @keywords ocs manager webdav api
#' @return Object of \code{\link{R6Class}} for modelling an ocsManager for Webdav API
#' @format \code{\link{R6Class}} object.
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ocsApiWebdavManager <-  R6Class("ocsApiWebdavManager",
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
    
    #OCS WEBDAV API
    #-------------------------------------------------------------------------------------------
    
    #'@description Get the 'ocs' WebDAV root URL
    getWebdavRoot = function(){
      return(private$capabilities$core[["webdav-root"]])
    },
    
    #'@description WebDAV method to list folders/files given a relative path. The relative path is set
    #'    to \code{"/"} by default, which corresponds to the root of the 'ocs' repository.
    #'@param relPath relative path
    #'@return the list of files
    listFiles = function(relPath = "/"){
      if(!startsWith(relPath, "/")) relPath <- paste0("/", relPath)
      request <- paste0(self$getWebdavRoot(), relPath)
      list_req <- ocsRequest$new(
        type = "WEBDAV_PROPFIND", private$url, request,
        private$user, pwd = private$getPassword(), 
        token = private$getToken(), cookies = private$cookies,
        logger = self$loggerType
      )
      list_req$execute()
      list_resp <- list_req$getResponse()
      return(list_resp)
    },
    
    #'@description WebDAV method to make a collection. By default \code{relPath} is set to \code{"/"} (root).
    #'    The \code{name} is the name of the new collection to be created. The function is recursive
    #'    in the sense that a \code{name} can be provided as relative path of a collection tree 
    #'    (eg \code{newfolder1/newfolder2/newfolder3}), the function will create recursively as 
    #'    many collections are handled in the name. 
    #'@param name name
    #'@param relPath relative path
    makeCollection = function(name, relPath = "/"){
      col_names <- unlist(strsplit(name, "/"))
      if(!startsWith(relPath, "/")) relPath <- paste0("/", relPath)
      if(!endsWith(relPath, "/")) relPath <- paste0(relPath, "/")
      if(length(col_names)==1){
        request <- paste0(self$getWebdavRoot(), relPath, name)
        mkcol_req <- ocsRequest$new(
          type = "WEBDAV_MKCOL", private$url, request,
          private$user, pwd = private$getPassword(), 
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
    
    #'@description WebDAV method to upload a file. By default \code{relPath} is set to \code{"/"} (root).
    #'@param filename file name
    #'@param relPath relative path
    #'@param delete_if_existing delete if existing file? Default is \code{FALSE}
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
        private$user, pwd = private$getPassword(), 
        token = private$getToken(), cookies = private$cookies,
        filename = filename,
        logger = self$loggerType
      )
      upload_req$execute()
      if(upload_req$getStatus() %in% c(201, 204)){
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
    
    #'@description WebDAV method to delete a file. By default \code{relPath} is set to \code{"/"} (root).
    #'@param filename file name
    #'@param relPath relative path
    deleteFile = function(filename, relPath = "/"){
      if(!startsWith(relPath, "/")) relPath <- paste0("/", relPath)
      if(!endsWith(relPath, "/")) relPath <- paste0(relPath, "/")
      request <- paste0(self$getWebdavRoot(), relPath, basename(filename))
      self$INFO(sprintf("WEBDAV - Delete file '%s' at '%s'", 
                        filename, paste(private$url, request, sep="/")))
      upload_req <- ocsRequest$new(
        type = "HTTP_DELETE", private$url, request,
        private$user, pwd = private$getPassword(), 
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
    
    #'@description Downloads a file
    #'@param relPath relative path
    #'@param filename file name
    #'@param outdir the out directory where to download the file
    downloadFile = function(relPath, filename, outdir = "."){
      request <- sprintf("remote.php/dav/files/%s/%s/%s", private$user, relPath, filename)
      file_req <- ocsRequest$new(
        type = "HTTP_GET", private$url, request, format = NULL, namedParams = list(),
        private$user, pwd = private$getPassword(), 
        token = private$getToken(), cookies = private$cookies,
        logger = self$loggerType
      )
      file_req$execute()
      file_resp <- file_req$getResponse()
      writeBin(object = file_resp, con = file.path(outdir, filename))
      return(file.path(outdir, filename))
    },
    
    #'@description Get details of a shared public file given its share token
    #'@param share_token the share token
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
