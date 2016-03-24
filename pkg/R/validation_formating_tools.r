
################################################################################
### VALIDATION
################################################################################

## validateDataFrame <- function (df) {
## }
## validateDataFrameList <- function (datasets,
##                                    meta.datasets) {
##     ## if included meta.datasets is also validated
##     ## not sure is needed
## }

## formatDataFrameLK <- function (df) {
## }
## formatDataFrameList <- function (datasets) {
## }

## just.numeric <- sapply (df, is.numeric) & ! sapply (df, is.integer)
## just.integer <- apply (df[just.numeric] == floor (df[just.numeric]), 2, all, na.rm = TRUE)
## just.long    <- apply (df[just.numeric] > long, 2, any, na.rm = TRUE)
## just.short <- !just.long

## Numeric variables which are effectively integers are formatted as.integers.
## Those which are long integers are converted into text.
##

################################################################################

##' Format a data.frame for LabKey
##'
##' @param df A data.frame. An element from the `datasets` list.
##' @param long.int.as.char If TRUE, long integers will be uploaded as character variables.
##' @param long The threshold to consider an integer to be long long. 
##'
##' @export

formatDataFrameLK <- function (df, long.int.as.char = TRUE, long = 10^9) {

    ## COERCE CHARACTER TO NUMERIC
    for (i in 1:ncol (df)) {
        es.char <- is.character (df[,i])
        if (es.char) {
            suppressWarnings (nm <- as.numeric (df[,i]))
            if (all (is.na (df[,i]) == is.na (nm))) {
                df[,i] <- nm ## if no NAs have been created I assume that the conversion is done properly
            }
        }
    }
    
    ## INTEGER FORMATTING
    for (i in 1:ncol (df)) {
        ## numeric; not integer
        es.num <- is.numeric (df[,i]) & !is.integer (df[,i])
        if (es.num) {
            ## effective integer. No real part
            es.int <- all (df[,i] == floor (df[,i]), na.rm = TRUE)
            if (es.int) {
                ## long integer
                es.big <- any (df[,i] > long, na.rm = TRUE)
                if (es.big) {
                    if (long.int.as.char) {
                        df[,i] <-  as.character (df[,i])
                    }
                } else {
                    df[,i] <-  as.integer (df[,i])
                }
            }
        }
    }
    return (df)
}


##' Format a data.frame list for LabKey
##'
##' @param datasets A list of data.frames to be uploaded as "LabKey study".
##' @param long.int.as.char If TRUE, long integers will be uploaded as character variables.
##' @param long The threshold to consider an integer to be long long. 
##'
##' @export

formatDataFrameList <- function (datasets, long.int.as.char = TRUE, long = 10^9) {

    ### REVISAR QUE LA LISTA TIENE NOMBRES
    
    for (i in 1:length (datasets))
        datasets[[i]] <- formatDataFrameLK (datasets[[i]],
                                          long.int.as.char = long.int.as.char,
                                          long = long)
    return (datasets)
}


################################################################################

##' Validate a dataframe to be exported to LabKey
##'
##' Validate a data.frame list to be exported to LabKey
##' 
##' @param df A data.frame. An element from the `datasets` list.
##' @param name name
##' @param long.char Long character threshold.
##'
##' @export

validateDataFrame <- function (df, name = "", long.char = 3999
                                 ##key = "Managed Key"  ## not needed
                                 ) {
    
    ## format name to be nice in the messages
    if (name == "") {
        name <-  " "
    } else {
        name <- sprintf (" %s ", name)
    }
    
    ## STOPs #####################################
    
    ## Long Characters
    for (i in 1:ncol (df)) {
        if (is.character (df[,i])) {
            ## maximum character length
            mchar <- max (nchar (df[,i]))
            if (mchar > long.char) {
                stop ("Column ", colnames (df)[i], " in your data.frame", name, "has more than ", long.char, " characters. ",
                      "Labkey defaults require shorter text values.")
            }
        }
    }
    
    ## date
    if (!"date" %in% tolower (colnames (df))) {
        stop ("A 'date' variable must exist in your data.frame", name)
    }
    if (any (is.na (df[,"date"]))) {
        stop ("The variable 'date' in your data.frame", name, "contains NA values.")
    }
    
    ## ParticipantId
    ##if (!"ParticipantId" %in% tolower (colnames (df))) {
    if (!"ParticipantId" %in% colnames (df)) {
        stop ("A 'ParticipantId' variable must exist in your data.frame", name)
    }
    if (any (is.na (df[,"ParticipantId"]))) {
        stop ("The variable 'ParticipantId' in your data.frame", name, "contains NA values.")
    }
    if (any (duplicated (df[,"ParticipantId"]))) {
        cat ("The variable 'ParticipantId' in your data.frame", name,
             "contains duplicated values. A virtual key variable will be created.", fill = TRUE)
        ## if (! key %in% tolower (colnames (df))) {
        ##     stop ("The variable 'ParticipantId' in your data.frame", name, "contains duplicated values and no key variable ", key, "was found.")
        ##     ##cat ("The variable 'ParticipantId' in your data.frame", name, "contains duplicated values and no key variable ", key, "was found.", fill = TRUE)
        ## }
        ## if (any (duplicated (df[,key]))) {
        ##     stop ("The variable 'ParticipantId' in your data.frame", name, "contains duplicated values and also your key variable ", key)
        ##     ##cat ("The variable 'ParticipantId' in your data.frame", name, "contains duplicated values and also your key variable ", key, fill = TRUE)
        ## }
    }
    
    ## WARNINGs ##################################
    w.col <- character (0)
    
    ## Factors
    es.factor <- sapply (df, is.factor)
    if (any (es.factor)) {
        w.col <- c (w.col,
                     warning ("Some columns in your table", name, "are factors."))
    }
    
    ## Integers
    ## Long Integers
    ## Colnames
    ## Duplicated colnames

    ##     COLNAMES CON PUNTOS: VER SI ESTO ENTRA EN EL LABKEY
    
    return (w.col)
}



##' Validate a dataframes
##'
##' @param datasets A list of data.frames to be uploaded as "LabKey study".
##' @param meta.datasets a data.frame of meta information about the element of `datasets`.
##' 
##' @export

validateDataFrameList <- function (datasets,
                                   meta.datasets = NULL) {
    ## if included meta.datasets is also validated. not sure is needed
    if (is.null (names (datasets))) {
        stop ("The data.frames list must have names")
    } else {
        if (any (names (datasets) == "")) {
            stop ("All elements in the data.frames list must have names")
        }
    }
    
    w.col <- character (0) ## warning collection
    for (ta in names (datasets)) {
        w.col <- c (w.col,
                     validateDataFrame (datasets[[ta]], name = ta))  ## errors and warnings are due to the validateDataFrame function
    }
    if (length (w.col) == 0) {
        cat ('All checks OK', fill = TRUE)
    }
}
