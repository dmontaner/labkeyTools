##testLabKeyDownloadedTable.r
##2016-04-01 david.montaner@gmail.com

##' Validate imported Dataset
##'
##' Sometimes data.frames created by "Rlabkey" functions "labkey.selectRows" or "getRows" are corrupted.
##' testLabKeyDownloadedTable tests if the downloaded dataset is corrupted or not. 
##'
##' "labkey.selectRows" or "getRows" need to be used with "showHidden = TRUE".
##' 
##' @param x A data.frame imported using "Rlabkey".
##' @param verbose verbose mode.
##' @param uidcol LabKey internal unique ID column.
##'
##' @examples
##' \dontrun{
##' validDataset (data)
##' sapply (dataList, validDataset, FALSE)
##' }
##' 
##' @export

validDataset <- function (x, verbose = TRUE, uidcol = "lsid") {
    
    if (!uidcol %in% colnames (x)) {
        stop ('\n', uidcol, ' column not found in the data.frame.\nUse "showHidden = TRUE" in "labkey.selectRows / getRows".')
    }
    
    g <- grepl ("^urn:", x[,uidcol])
    
    if (all (g)) {
        res <- TRUE
    } else {
        res <- FALSE
    }
    
    if (verbose) {
        if (res) {
            cat ("The data.frame looks FINE according to the", uidcol, "column", fill = TRUE)
        } else {
            cat ("The data.frame looks CORRUPTED according to the", uidcol, "column", fill = TRUE)
        }
    }

    return (res)
}
