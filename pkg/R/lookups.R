##lookups.R
##2016-03-17 david.montaner@gmail.com


##' Initialize lookup data.frame
##'
##' Takes a list of data frames and initialized a `lookup` data.frame.
##'
##' A `lookup` data.frame is intended to link columns form one table
##' (lists or data) to
##' the `labels` they should be displayed in LabKey. 
##'
##' The `lookup` data.frame should contain the following columns:
##' table: the table to be labeled
##' column: the column to be labeled.
##' mapSchema: the schema where labels should be found.
##' mapTable: the table where labels should be found.
##' mapColumn: the column where values should be found. Values in `column` should match values in this column.
##' mapDisplay; the column with the label to be displayed.
##'
##' @param x a list of data.frames
##' @param mapSchema the LabKey schema where label tables should be found.
##' Usually takes value "lists" or "study".
##'
##' @return A data.frame with fields :
##' table: the table to be labeled
##' column: the column to be labeled.
##' mapSchema: the schema where labels should be found.
##' mapTable: the table where labels should be found.
##' mapColumn: the column where values should be found. Values in `column` should match values in this column.
##' mapDisplay; the column with the label to be displayed.
##'
##' @examples
##' ## myDFlist <- list (warpbreaks = warpbreaks, women = women)
##' res  <- lookupInit (myDFlist)
##' res2 <- lookupInit (myDFlist, mapSchema = "study")
##' res
##' res2
##' sapply (res, class)
##'
##' @export

lookupInit  <- function (x, mapSchema = "lists") {
    if (is.null (names (x))) stop ("a named list is expected in x")

    if (!mapSchema %in% c ("lists", "study")) warning ('mapSchema is generally "lists" or"study"')
    
    l <- lapply (x, colnames)
    lon <- sapply (l, length)
    
    table  <- rep (names (l), times = lon)
    column <- unlist (l, use.names = FALSE)
    
    if (length (table) != length (column)) stop ("table and column have different lengths")
    
    res <- data.frame (table, column, mapSchema, mapTable = NA, mapColumn = NA, mapDisplay = NA, stringsAsFactors = FALSE)
    return (res)
}
