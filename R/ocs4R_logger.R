#' ocs4RLogger
#'
#' @docType class
#' @export
#' @keywords logger
#' @return Object of \code{\link{R6Class}} for modelling a simple logger
#' @format \code{\link{R6Class}} object.
#' 
#' @note Logger class used internally by ocs4R
#'
ocs4RLogger <-  R6Class("ocs4RLogger",
  portable = TRUE,
  public = list(
    #'@field verbose.info is info logger active?
    verbose.info = FALSE,
    #'@field verbose.debug is debug logger active?
    verbose.debug = FALSE,
    #'@field loggerType logger type
    loggerType = NULL,
    
    #'@description Initializes the logger
    #'@param logger the type of logger. Default is \code{NULL}, accepts \code{INFO} or \code{DEBUG}
    initialize = function(logger = NULL){
      
      #logger
      if(!missing(logger)){
        if(!is.null(logger)){
          self$loggerType <- toupper(logger)
          if(!(self$loggerType %in% c("INFO","DEBUG"))){
            stop(sprintf("Unknown logger type '%s", logger))
          }
          if(self$loggerType == "INFO"){
            self$verbose.info = TRUE
          }else if(self$loggerType == "DEBUG"){
            self$verbose.info = TRUE
            self$verbose.debug = TRUE
          }
        }
      }
    },
    
    #'@description Logger function
    #'@param type type of log
    #'@param text text
    logger = function(type, text){
      if(self$verbose.info){
        cat(sprintf("[ocs4R][%s] %s - %s \n", type, self$getClassName(), text))
      }
    },
    
    #'@description Logger to report information. Used internally
    #'@param text text
    INFO = function(text){self$logger("INFO", text)},
    
    #'@description Logger to report warnings. Used internally
    #'@param text text
    WARN = function(text){self$logger("WARN", text)},
    
    #'@description Logger to report errors Used internally
    #'@param text text
    ERROR = function(text){self$logger("ERROR", text)},
    
    #'@description Get class name
    #'@return the class name
    getClassName = function(){
      return(class(self)[1])
    },
    
    #'@description Get class
    #'@return the class
    getClass = function(){
      class <- eval(parse(text=self$getClassName()))
      return(class)
    }
    
  )
)