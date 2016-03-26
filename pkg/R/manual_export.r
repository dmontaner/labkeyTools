##manual_export.r
##2016-03-24 david.montaner@gmail.com

## MANUAL IMPORT REQUIRES nicenames...

## SOME NOTES FROM THE LABKEY WEB

## If dataset has more than one row per participant/visit,
## an additional key field must be provided.
## There can be at most one row in the dataset for each combination of
## participant, visit and key.

## Options (in the web form) are:

## - None: No additional key
##   ->-> ParticipaId/date must be unique.

## - Data Field: A user-managed key field
##   ->-> The user must define the column AND introduce the values.

## - Managed Field: A numeric or string field defined below
##   will be managed by the server to make each new entry unique.
##   Numbers will be assigned auto-incrementing integer values,
##   strings will be assigned globally unique identifiers (GUIDs).
##   ->-> The user must define the column BUT does not need to introduce the value

## When participant id is not the key,
## by default the next field is taken as default KEY
## it can be changed... but since to use this default.
## Thus insert the lktKey in the third column.

## ParticipantId : is always taken as text format
## date : is a date format


##' Initializes the metainfo of a data.frame
##'
##' @param x A data.frame to be exported to LabKey
##' @param file the file name to be saved
##' @param x.info A data.frame of the describing the "Import Fields". See `singleFileImporInit`.
##' @param pid.col name of the participant column ID (usually "ParticipantId").
##' @param auto.key.name Name for an automatic key column if this needs to be created.
##' @param verbose verbose mode
##' @param data.file name of the data file
##' @param info.file name of the file with the "Import Field" description.
##'
##' @export

lktSimpleExport <- function (x,
                             file,
                             x.info, 
                             pid.col = "ParticipantId",
                             auto.key.name = "lktKey",
                             data.file = paste0 (file, "_data.tsv"),
                             info.file = paste0 (file, "_field_definition.txt"),
                             verbose = TRUE
                             ) {

    ## date column name. May be could be a parameter of the function
    dat.col = "Date"

    if (!tolower (pid.col) %in% tolower (colnames (x))) stop (pid.col, " column is not found in your dataset")
    if (!tolower (dat.col) %in% tolower (colnames (x))) stop (dat.col, " column is not found in your dataset")
    
    ## test fieldInfo here may be

    ## build x.info
    if (missing (x.info)) {
        x.info <- singleFileImporInit (x = x,
                                       pid.col = pid.col, 
                                       auto.key.name = auto.key.name, 
                                       verbose = verbose)
    }

    ## reorder columns
    columnas <- colnames (x)
    x <- x[, c (pid.col, dat.col, setdiff (columnas, c(pid.col, dat.col)))]
    
    ## save data
    write.table (x,      file = data.file, quote = FALSE, sep = "\t", na = "", row.names = FALSE)
    ## save info 
    write.table (x.info, file = info.file, quote = FALSE, sep = "\t", na = "", row.names = FALSE)
}

################################################################################

##' Initializes the metainfo of a data.frame
##'
##' @param x A data.frame
##' @param pid.col name of the participant column ID (usually "ParticipantId")
##' @param auto.key.name Name for an automatic key column if this needs to be created.
##' @param verbose verbose mode
##' 
##' @export

singleFileImporInit <- function (x,
                                 pid.col = "ParticipantId",
                                 auto.key.name = "lktKey",
                                 verbose = TRUE
                                 ##hide.date = TRUE,               ## not sure if I can implement this
                                 ## managed.additional.key = TRUE, ## not worth to implement this
                                 ) {
    
    ## WARNING: This will delete all existing fields and data!
    ## If you only wish to rename, add or delete fields, use the domain editor.
    ## Paste tab-delimited text with the following column headers and one row for each field
    
    ## Property - Required. Field name. Must start with a character and include only characters and numbers
    ## Label - Optional. Name that users will see for the field
    ## RangeURI - Optional. Values: xsd:int, xsd:string, xsd:double, xsd:boolean, xsd:dateTime. Defaults to xsd:string
    ## Format - Optional. Format for a date or numeric field
    ## NotNull - Optional. Set to TRUE if this value is required
    ## Hidden - Optional. Set to TRUE if this field should not be shown in default grid views
    ## ???
    ## LookupSchema - Optional. If there is a lookup defined on this column, this is the target schema
    ## LookupQuery  - Optional. If there is a lookup defined on this column, this is the target query or table name
    ## Description  - Optional. Description of the field

    ## date column name. May be could be a parameter of the function
    dat.col = "Date"
    
    ## columns describing the fields
    out.cols <- c ("Property", "Label", "RangeURI", "Format", "NotNull", "Hidden",
                   "MvEnabled",
                   "LookupSchema", "LookupQuery", "Description")
    
    column <- colnames (x)
    clases <- sapply (lapply (x, class), "[", 1)
    
    pid <- which (tolower (column) == tolower (pid.col))
    dat <- which (tolower (column) == tolower (dat.col))
    
    column <- column[-c (pid, dat)]
    clases <- clases[-c (pid, dat)]
    
    if (any (duplicated (x[,c (pid, dat)]))) { ## needs extra key
        column <- c (auto.key.name, column)
        clases <- c ("integer",     clases)
        ## if (!managed.additional.key) {
        ##     x[,auto.key.name] <- 1:nrow (x)
        ## }
        if (verbose) {
            cat ('NOTE: participant/visit is not unique.', fill = TRUE)
            cat ('      An additional key is required.', fill = TRUE)
            cat ('      Use the option "Managed Field" when uploading your data', fill = TRUE)
        }
    }
    
    mat <- as.data.frame (matrix (NA, nrow = length (clases), ncol = length (out.cols)))
    colnames (mat) <- out.cols
    ##
    mat[,"Property"] <- column  ## may be nice names here ??? 
    mat[,"RangeURI"] <- rangeURI (clases)
    mat[,c ("NotNull", "Hidden", "MvEnabled")] <- FALSE
    
    return (mat)
}
