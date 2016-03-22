##data_type_conversion.R
##2016-03-14 david.montaner@gmail.com


##' Data types for LabKey XML files
##'
##' Describes data type to be used in the XML files describing lists and datasets.
##'
##' @param x vector of R classes as returned by `class`.
##'
##' @export
datatype <- function (x) {
    conv <- c (character = "varchar",
               factor    = "varchar",  ## revise this
               integer   = "integer", 
               double    = "double", 
               numeric   = "double",
               Date      = "timestamp",
               POSIXct   = "timestamp",
               POSIXt    = "timestamp",
               logical   = "boolean", 
               entityid  = "entityid"  ## revise this
               )
    if (!all (x %in% names (conv))) stop ('DATATYPE NOT KNOWN')
    x <- conv[x]
    return (x)
}    
## test
##datatype ("character") == "varchar"

##' rangeURI for LabKey XML files
##'
##' Describes rangeURI to be used in the XML files describing lists and datasets.
##'
##' @param x vector of R classes as returned by `class`
##' 
##' @export
rangeURI <- function (x) {
    conv <- c (character = "string",
               factor    = "string",   ## revise this
               integer   = "int", 
               double    = "double", 
               numeric   = "double", 
               Date      = "dateTime",
               POSIXct   = "dateTime",
               POSIXt    = "dateTime",
               logical   = "boolean", 
               entityid  = "entityid"  ## revise this
               )
    if (!all (x %in% names (conv))) stop ('URI TYPE NOT KNOWN')
    x <- conv[x]
    x <- paste0 ("http://www.w3.org/2001/XMLSchema#", x)
    return (x)
}
## test
##rangeURI ("character") == "http://www.w3.org/2001/XMLSchema#string"
