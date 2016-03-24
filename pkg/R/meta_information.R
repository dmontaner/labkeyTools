##meta_information.R
##2016-03-14 david.montaner@gmail.com


##' Lists Meta Information
##'
##' Creates a data.frame with information about each data.frame in the `lists` list
##'
##' In LabKey _lists_ all tables must have a key column.
##' This needs to exist in the tsv files created to upload.
##' Also reflected in the information of the lists.xml file.
##' Use key = NA and lookupDisplay = NA if they do not exist.
##'
##' "key" and "lookupDisplay" are reserved names for the columns.
##' 
##' @param lists a list of data.frames to be uploaded as "LabKey lists".
##' @param key vector of length as length (lists) indicating the column which is key in the table
##' @param lookupDisplay vector of length as length (lists) indicating the column which is to be displayed when the table is uses as look-up.
##' @param baseID first id to be used (minus one)
##' @param verbose verbose mode
##'
##' @export

metaInfoLists <- function (lists,
                           key = NA,
                           lookupDisplay = NA,
                           baseID = 100,
                           verbose = TRUE) {
    
    N <- length (lists)

    name  <- names (lists)
    title <- names (lists) ## <columnTitle> in the lists.xml file
    ##name <- lktNiceNames  (name)    ## explore this
    ##title <- lktNiceTitles (title)  ## explore this
    
    id <- baseID + 1:N
    
    linfo <- data.frame (name, title, id, stringsAsFactors = FALSE)
    
    ## tsv file for lists; defined from name
    linfo[,"tsv.file"] <- paste0 (name, ".tsv")
    
    ## some extra columns for LabKey lists
    ## use NA if they are not available
    linfo[,"key"]           <- key           ## Key column                     ## <pkColumnName> in the XML file; check the <isKeyField> in the columns attributes
    linfo[,"lookupDisplay"] <- lookupDisplay ## List Properties > Title Field  ## <titleColumn>  in the XML file
    linfo[,"description"]   <- NA            ## List Properties > Description  ## <description>  in the XML file
    
    return (linfo)
}



## Test Meta information in Lists
##
## Utility function to test lists and meta formats.
##
## @export

test.meta.info.lists <- function (lists, meta) {
    if (any (names (lists) != meta[,"name"])) {
        stop ("name field in `meta` does not match with names in `lists`")
    }
}

################################################################################


##' Datasets Meta Information
##'
##' Creates a data.frame with information about each data.frame in the `datasets` list.
##'
##' In LabKey _datasets_ all tables must have a
##' `date` column and a
##' `subjectColumnName` column.
##' By default subjectColumnName = "ParticipantId".
##'
##' @param datasets a list of data.frames to be uploaded as _LabKey datasets_.
##' @param nicetitles if TRUE the name of the table is parsed into the title field.
##' @param subjectColumnName ParticipantId column name.
##' @param baseID first id to be used (minus one)
##' @param verbose verbose mode
##' @param description description of each of the datasets
##'
##' @export

metaInfoDatasets <- function (datasets,
                              nicetitles = TRUE,
                              subjectColumnName = "ParticipantId",
                              baseID = 5000,
                              description = "Contains up to one row of %s data for each Participant/Visit combination.",
                              verbose = TRUE) {
    
    N <- length (datasets)
    
    name <- names (datasets)
    id <- baseID + 1:N
    
    dinfo <- data.frame (name, id, stringsAsFactors = FALSE)
    
    ## tsv file for studies; defined from id
    dinfo[,"tsv.file"] <- paste0 ("dataset", id, ".tsv")
    
    ## some extra columns for LabKey datasets
    dinfo[,"demographicData"]   <- FALSE        ## this could be parameterized
    dinfo[,"type"]              <- "Standard"
    dinfo[,"subjectColumnName"] <- subjectColumnName
    
    ## nice titles
    if (nicetitles) {
        dinfo[,"title"] <- lktNiceTitles (dinfo[,"name"])
    } else {
        dinfo[,"title"] <- lktNiceTitles (dinfo[,"name"])
    }
    
    ##dinfo[,"description"] <- NA           ## Edit Dataset Definition > Description  ## <description>  in the XML file
    ##dinfo[,"description"] <- sprintf (description, dinfo[,"name"])
    dinfo[,"description"]   <- sprintf (description, dinfo[,"title"])
    
    return (dinfo)
}


## Test Meta information in Datasets
##
## Utility function to test datasets and meta formats.
##
## @export

test.meta.info.datasets <- function (datasets, meta) {
    if (any (names (datasets) != meta[,"name"])) {
        stop ("name field in `meta` does not match with names in `datasets`")
    }
}
