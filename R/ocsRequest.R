#' ocsRequest
#'
#' @docType class
#' @export
#' @keywords ocs request
#' @return Object of \code{\link{R6Class}} for modelling a generic 'ocs' web-service request
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(type, url, request, user, pwd, logger)}}{
#'    This method is used to instantiate a object for doing an 'ocs' web-service request
#'  }
#'  \item{\code{getRequest()}}{
#'    Get the request payload
#'  }
#'  \item{\code{getRequestHeaders()}}{
#'    Get the request headers
#'  }
#'  \item{\code{getStatus()}}{
#'    Get the request status code
#'  }
#'  \item{\code{getResponse()}}{
#'    Get the request response
#'  }
#'  \item{\code{getException()}}{
#'    Get the exception (in case of request failure)
#'  }
#'  \item{\code{getResult()}}{
#'    Get the result \code{TRUE} if the request is successful, \code{FALSE} otherwise
#'  }
#' }
#' 
#' @note Abstract class used internally by \pkg{ocs4R}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ocsRequest <- R6Class("ocsRequest",
  inherit = ocs4RLogger,
  portable = TRUE,
  #private methods
  private = list(
    url = NA,
    type = NA,
    request = NA,
	  requestHeaders = NA,
    namedParams = list(),
    content = NULL,
    contentType = "text/plain",
    filename = NULL,
	  status = NA,
    response = NA,
    exception = NA,
    result = NA,

    auth_scheme = "Basic",
    auth = NA,
    token = NULL,
    cookies = NULL,
    
    getUserAgent = function(){
      return(paste("ocs4R", packageVersion("ocs4R"), sep="-"))
    },

    #HTTP_GET
    #---------------------------------------------------------------
    HTTP_GET = function(url, request = NULL, namedParams){
      req <- url
      if(!is.null(request)) req <- paste(url, request, sep = "/")
      if(!endsWith(req,"?")) req <- paste0(req, "?")
      namedParams <- namedParams[!sapply(namedParams, is.null)]
      paramNames <- names(namedParams)
      namedParams <- lapply(namedParams, function(namedParam){
        if(is.logical(namedParam)) namedParam <- tolower(as(namedParam, "character"))
        return(namedParam)
      })
      params <- paste(paramNames, namedParams, sep = "=", collapse = "&")
      req <- paste0(req, params)
      self$INFO(sprintf("HTTP/GET - Fetching %s", req))
      
      r <- NULL
      if(self$verbose.debug){
        r <- with_verbose(GET(req, handle = handle(''), add_headers(
          "User-Agent" = private$getUserAgent(),
          "Authorization" = private$auth,
          "X-XSRF-TOKEN" = private$token,
          "Set-Cookie" = private$cookies,
          "OCS-APIRequest" = "true"
        )))
      }else{
        r <- GET(req, handle = handle(''), add_headers(
          "User-Agent" = private$getUserAgent(),
          "Authorization" = private$auth,
          "X-XSRF-TOKEN" = private$token,
          "Set-Cookie" = private$cookies,
          "OCS-APIRequest" = "true"
        ))
      }
      
      responseContent <- NULL
      if(status_code(r)==200){
        self$INFO(sprintf("HTTP/GET - Successful request '%s'", req))
        responseContent <- httr::content(r, type = "application/json", encoding = "UTF-8")
        if(responseContent$ocs$meta$status == "failure"){
          errMsg <- sprintf("%s [status code = %s]", responseContent$ocs$meta$message, responseContent$ocs$meta$statuscode)
          self$ERROR(errMsg)
          stop(errMsg)
        }
      }
      if(status_code(r)==401){
        errMsg <- sprintf("HTTP/GET - Unauthorized request '%s' (insufficient privileges)", req)
        self$ERROR(errMsg)
        stop(errMsg)
      }
      
      
      response <- list(request = request, requestHeaders = headers(r), cookies = cookies(r),
                       status = status_code(r), response = responseContent)
      return(response)
    },
    
    #HTTP_POST
    #---------------------------------------------------------------
    HTTP_POST = function(url, request = NULL, namedParams = list(), content = "", contentType = "text/plain"){
      req <- url
      if(!is.null(request)) req <- paste(url, request, sep = "/")
      if(!endsWith(req,"?")) req <- paste0(req, "?")
      namedParams <- namedParams[!sapply(namedParams, is.null)]
      paramNames <- names(namedParams)
      namedParams <- lapply(namedParams, function(namedParam){
        if(is.logical(namedParam)) namedParam <- tolower(as(namedParam, "character"))
        return(namedParam)
      })
      params <- paste(paramNames, namedParams, sep = "=", collapse = "&")
      req <- paste0(req, params)
      
      self$INFO(sprintf("HTTP/POST - Sending request '%s'", req))
      
      #content
      body <- content
      
      r <- NULL
      if(self$verbose.debug){
        r <- with_verbose(POST(req, handle = handle(''), 
          add_headers(
            "User-Agent" = private$getUserAgent(),
            "Content-Type" = contentType,
            "Authorization" = private$auth,
            "X-XSRF-TOKEN" = private$token,
            "Set-Cookie" = private$cookies,
            "OCS-APIRequest" = "true"
          ),
          body = body
        ))
      }else{
        r <- POST(req, handle = handle(''), add_headers(
          "User-Agent" = private$getUserAgent(),
          "Content-Type" = contentType,
          "Authorization" = private$auth,
          "X-XSRF-TOKEN" = private$token,
          "Set-Cookie" = private$cookies,
          "OCS-APIRequest" = "true"), body = body)
      }
      
      responseContent <- NULL
      if(status_code(r)==200){
        self$INFO(sprintf("HTTP/POST - Successful request '%s'", req))
        responseContent <- httr::content(r, type = "application/json", encoding = "UTF-8")
        if(responseContent$ocs$meta$status == "failure"){
          errMsg <- sprintf("%s [status code = %s]", responseContent$ocs$meta$message, responseContent$ocs$meta$statuscode)
          self$ERROR(errMsg)
          stop(errMsg)
        }
      }
      if(status_code(r)==401){
        errMsg <- sprintf("HTTP/POST - Unauthorized request '%s' (insufficient privileges)", req)
        self$ERROR(errMsg)
        stop(errMsg)
      }
      
      response <- list(request = req, requestHeaders = headers(r), cookies = cookies(r),
                       status = status_code(r), response = responseContent)
      return(response)
    },
    
    #HTTP_PUT
    #---------------------------------------------------------------
    HTTP_PUT = function(url, request = NULL, namedParams = list(), content = NULL, contentType = "application/x-www-form-urlencoded", filename = NULL){
      req <- url
      if(!is.null(request)) req = paste(url, request, sep="/")
      if(!endsWith(req,"?")) req <- paste0(req, "?")
      namedParams <- namedParams[!sapply(namedParams, is.null)]
      paramNames <- names(namedParams)
      namedParams <- lapply(namedParams, function(namedParam){
        if(is.logical(namedParam)) namedParam <- tolower(as(namedParam, "character"))
        return(namedParam)
      })
      params <- paste(paramNames, namedParams, sep = "=", collapse = "&")
      req <- paste0(req, params)

      self$INFO(sprintf("HTTP/PUT - Putting content at '%s'", req))
      
      #content
      query <- NULL
      body <- NULL
      if(missing(content) | is.null(content)){
        if(missing(filename) | is.null(filename)){
          stop("The filename must be provided")
        }
        content <- filename
        body <- httr::upload_file(filename)
      }else{
        query <- content
      }
      
      r <- NULL
      if(self$verbose.debug){
        r <- with_verbose(PUT(req, handle = handle(''), add_headers(
          "User-Agent" = private$getUserAgent(),
          "Authorization" = private$auth,
          "X-XSRF-TOKEN" = private$token,
          "Set-Cookie" = private$cookies,
          "OCS-APIRequest" = "true",
          "Content-Type" = contentType), body = body, query = query
        ))
      }else{
        r <- PUT(req, handle = handle(''), add_headers(
          "User-Agent" = private$getUserAgent(),
          "Authorization" = private$auth,
          "X-XSRF-TOKEN" = private$token,
          "Set-Cookie" = private$cookies,
          "OCS-APIRequest" = "true",
          "Content-Type" = contentType), body = body)
      }

      if(status_code(r)==201){
        self$INFO(sprintf("HTTP/PUT - Content successfuly uploaded at '%s'", req))
      }
      if(status_code(r)==401){
        errMsg <- sprintf("HTTP/PUT - Unauthorized request '%s' (insufficient privileges)", req)
        self$ERROR(errMsg)
        stop(errMsg)
      }
      
      response <- list(request = req, requestHeaders = headers(r), cookies = cookies(r),
                       status = status_code(r), response = content)
      return(response)
    },
    
    #HTTP_DELETE
    #---------------------------------------------------------------
    HTTP_DELETE = function(url, request = NULL, contentType = "text/plain"){
      req <- url
      if(!is.null(request)) req = paste(url, request, sep="/")
      
      self$INFO(sprintf("HTTP/DELETE - Deleting content at '%s'", req))
      
      
      r <- NULL
      if(self$verbose.debug){
        r <- with_verbose(DELETE(req, handle = handle(''), add_headers(
          "User-Agent" = private$getUserAgent(),
          "Content-Type" = contentType,
          "Authorization" = private$auth,
          "X-XSRF-TOKEN" = private$token,
          "Set-Cookie" = private$cookies,
          "OCS-APIRequest" = "true"
          )
        ))
      }else{
        r <- DELETE(req, handle = handle(''), add_headers(
          "User-Agent" = private$getUserAgent(),
          "Content-Type" = contentType,
          "Authorization" = private$auth,
          "X-XSRF-TOKEN" = private$token,
          "Set-Cookie" = private$cookies,
          "OCS-APIRequest" = "true")
        )
      }
      
      if(status_code(r)==201){
        self$INFO(sprintf("HTTP/DELETE - Successful deletion at '%s'", req))
      }
      if(status_code(r)==401){
        errMsg <- sprintf("HTTP/DELETE - Unauthorized request '%s' (insufficient privileges)", req)
        self$ERROR(errMsg)
        stop(errMsg)
      }
      
      response <- list(request = req, requestHeaders = headers(r), cookies = cookies(r),
                       status = status_code(r), response = content)
      return(response)
    },
    
    #WEBDAV_PROPFIND
    WEBDAV_PROPFIND = function(url, request){
      req <- paste(url, request, sep = "/")
      
      self$INFO(sprintf("WEBDAV/PROPFIND - Listing files at '%s'", req))
      
      h <- new_handle()
      handle_setopt(h, customrequest = "PROPFIND")
      headers <- list("OCS-APIRequest" = "true", "Authorization" = private$auth)
      if(!is.null(private$token)) headers <- c(headers, "X-XSRF-TOKEN" = private$token)
      if(!is.null(private$cookies)) headers <- c(headers, "Set-Cookie" = private$cookies)
      handle_setheaders(h, .list = headers)
      response <- curl_fetch_memory(req, h)
      xml <- rawToChar(response$content)
      response <- xmlParse(xml, asText = TRUE)
      webdavNS <- c(d = "DAV:")
      base <- paste(paste("/", strsplit(req, "/")[[1]][-1:-3], sep="", collapse=""), "/", sep="")
      nodes <- getNodeSet(response, "//d:response")
      if(length(nodes)>0){
        self$INFO("WEBDAV/PROPFIND - Successful file listing!")
      }else{
        errMsg <- "WEBDAV/PROPFIND - Error while listing files"
        self$ERROR(errMsg)
        stop(errMsg)
      }
      
      result <- do.call("rbind", lapply(nodes, function(node){
        out_node <- data.frame(
          name = sub(base, "", URLdecode(xpathSApply(xmlDoc(node), "//d:href", namespaces = webdavNS, xmlValue))),
          resourceType = ifelse(length(xmlChildren(getNodeSet(xmlDoc(node), "//d:propstat/d:prop/d:resourcetype", namespaces = webdavNS)[[1]]))==0,"file","collection"),
          contentType = {
            ct <- xpathSApply(xmlDoc(node), "//d:propstat/d:prop/d:getcontenttype", namespaces = webdavNS, xmlValue)
            if(length(ct)==0) ct <- NA
            ct
          },
          size = {
            s = xpathSApply(xmlDoc(node), "//d:propstat/d:prop/d:getcontentlength", namespaces = webdavNS, xmlValue)
            s = as.numeric(s)
            s <- if(length(s)==0) NA else s/1048576
            s
          },
          quota = {
            q = xpathSApply(xmlDoc(node), "//d:propstat/d:prop/d:quota-used-bytes", namespaces = webdavNS, xmlValue)
            q = as.numeric(q)
            q <- if(length(q)==0) NA else q/1e6
            q
          },
          lastModified = {
            date = xpathSApply(xmlDoc(node), "//d:propstat/d:prop/d:getlastmodified", namespaces = webdavNS, xmlValue)
            date = gsub(" GMT", "", date)
            lctime <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
            date <- strptime(date, "%a, %d %b %Y %H:%M:%S")
            Sys.setlocale("LC_TIME", lctime)
            date
          },
          stringsAsFactors = FALSE
        )
        return(out_node)
      }))
      result <- result[result$name != "", ]
      
      response <- list(request = req, requestHeaders = NA,
                       status = NA, response = result)
      return(response)
    },
    
    #WEBDAV_MKCOL
    WEBDAV_MKCOL = function(url, request){
      response <- NULL
      req <- paste(url, request, sep = "/")
      self$INFO(sprintf("WEBDAV/MKCOL - Creating collection '%s' at '%s'", request, req))
      h <- new_handle()
      handle_setopt(h, customrequest = "MKCOL")
      headers <- list("OCS-APIRequest" = "true", "Authorization" = private$auth)
      if(!is.null(private$token)) headers <- c(headers, "X-XSRF-TOKEN" = private$token)
      if(!is.null(private$cookies)) headers <- c(headers, "Set-Cookie" = private$cookies)
      handle_setheaders(h, .list = headers)
      response <- curl_fetch_memory(req, h)
      if(response$status_code==201){
        self$INFO(sprintf("WEBDAV/MKCOL - Successfuly created collection '%s'", request))
        response <- list(request = req, requestHeaders = NA,
                         status = response$status_code, response = response$url)
      }else{
        errMsg <- sprintf("WEBDAV/MKCOL - Error while creating collection '%s' at '%s'", request, req)
        self$ERROR(errMsg)
        stop(errMsg)
      }
      return(response)
    }
  ),
  
  #public methods
  public = list(
    #initialize
    initialize = function(type, url, request,
                          user = NULL, pwd = NULL,
                          token = NULL, cookies = NULL,
                          namedParams = list(),
                          content = NULL, contentType = "text/plain", 
                          filename = NULL,
                          logger = NULL, ...) {
      super$initialize(logger = logger)
      private$type = type
      private$url = url
      private$request = request
      private$namedParams = namedParams
      private$namedParams$format = "json"
      private$content = content
      if(type == "HTTP_PUT") contentType = "application/x-www-form-urlencoded"
      private$contentType = contentType
      private$filename = filename
      
      #authentication schemes
      if(!is.null(user) && !is.null(pwd)){
        #Basic authentication (user/pwd) scheme
        private$auth_scheme <- "Basic"
        private$auth <- paste(private$auth_scheme, openssl::base64_encode(paste(user, pwd,sep=":")))
      }
      private$token <- token
      private$cookies <- cookies
    },
    
    #execute
    execute = function(){
      
      req <- switch(private$type,
        "HTTP_GET" = private$HTTP_GET(private$url, private$request, private$namedParams),
        "HTTP_POST" = private$HTTP_POST(private$url, private$request, private$namedParams, private$content, private$contentType),
        "HTTP_PUT" = private$HTTP_PUT(private$url, private$request, private$namedParams, private$content, private$contentType, private$filename),
        "HTTP_DELETE" = private$HTTP_DELETE(private$url, private$request, private$contentType),
        "WEBDAV_PROPFIND" = private$WEBDAV_PROPFIND(private$url, private$request),
        "WEBDAV_MKCOL" = private$WEBDAV_MKCOL(private$url, private$request)
      )
      
      private$request <- req$request
      private$requestHeaders <- req$requestHeaders
      private$status <- req$status
      private$response <- req$response
    },
    
    #getRequest
    getRequest = function(){
      return(private$request)
    },
    
    #getRequestHeaders
    getRequestHeaders = function(){
      return(private$requestHeaders)
    },
    
    #getStatus
    getStatus = function(){
      return(private$status)
    },
    
    #getResponse
    getResponse = function(){
      return(private$response)
    },
    
    #getException
    getException = function(){
      return(private$exception)
    },
    
    #getResult
    getResult = function(){
      return(private$result)
    },
    
    #setResult
    setResult = function(result){
      private$result = result
    }
    
  )
)