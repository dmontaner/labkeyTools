##xml_formatting_tools.R
##2016-03-14 david.montaner@gmail.com


##' Create XML headers for LabKey
##'
##' Creates the XML headers as in LabKey exported files.
##' 
##' @param prefix as in XML::saveXML; single quotes are needed (if " is in the header).
##' @param comment a comment to be included in the XML file
##'
lkt.prefix <- function (prefix = '<?xml version="1.0" encoding="UTF-8"?>\n', comment) {
    comment <- lkt.comment (comment)
    prefix = paste0 (prefix, comment)
    return (prefix)
}

lkt.comment <- function (comment) {
    if (missing (comment)) {
        comment <- paste ("<!-- Exported using labkeyTools R library", Sys.time (), "-->\n")
    } else {
        ## if (is.null (comment) | is.na (comment) | (comment == "")) {
        ##     comment <- ""
        ## }
        if (is.null (comment)) {
            comment <- ""
        } else {
            if (is.na (comment) | (comment == "")) {
                comment <- ""
            } else {
                comment <- paste0 (comment, "\n")
            }
        }
    }
    return (comment)
}

## ## test

## lkt.comment ()
## lkt.comment (NULL)
## lkt.comment (NA)
## lkt.comment ("")
## lkt.comment ("Normal Comment. Remember the XML comments")

## lkt.prefix ()
## lkt.prefix (comment = NULL)
## lkt.prefix (comment = NA)
## lkt.prefix (comment = "")
## lkt.prefix (comment = "Normal Comment. Remember the XML comments")

################################################################################


##' Save XML file for LabKey
##'
##' Utility function to standardize XML saving in the labkeyTools library.
##'
##' @param xml xml object to be saved. Created using XML::xmlNode
##' @param path path directory where the file should be saved. If missing current working directory is used.
##' @param outfile name of the file to be saved
##' @param comment comment a comment to be included in the XML file
##'
##' @import XML
##'
##' @export

lkt.save.xml <- function (xml, path, outfile, comment) {
    if (!missing (path)) outfile <- file.path (path, outfile)
    saveXML (xml, file = outfile, encoding = "UTF-8", prefix = lkt.prefix (comment = comment))
    invisible (xml)
}
