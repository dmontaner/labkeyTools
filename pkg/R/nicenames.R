##nicenames.R
##2016-03-14 david.montaner@gmail.com


##' Nice names for tables and columns
##'
##' Create valid names for LabKey tables or column names
##' Ideally should be used in:
##' - the names of the list of data.frames
##' - the columns of the data frames
##' 
##' @param x a character vector with the names
##'
##' @export

lktNiceNames <- function (x) {
    x <- trimws (x)
    ##x <- tolower (x)
    x <- gsub (" +", "_", x)
    x <- gsub ("[^a-zA-Z0-9]", "_", x) ## any non (^) alphanumeric to _
    x <- gsub ("_+", "_", x)
    ## MAY BE DUPLICATED NAMES SHOULD BE CHECKED HERE
    return (x)
}


##' @import stringr
##' @export

lktNiceTitles <- function (x) {
    x <- lktNiceNames (x)
    x <- gsub ("_", " ", x)
    ## CONFIRM THAT I WANT THIS DEPENDENCY HERE  stringr::str_to_title
    ##x <- str_to_title (x) ## NO. I remove it because it messts up camel case
    return (x)
}
