##zip4labkey.R
##2016-03-14 david.montaner@gmail.com


##' Zip folders to be imported into LabKey.
##'
##' Compresses folders as required by LabKey import tools.
##' 
##' @param path path to the folder to be zipped.
##' @param keep if TRUE uncompressed files are kept, otherwise deleted.
##'
##' @export

zip4labkey <- function (path, keep = TRUE) {
    wd0 <- getwd ()
    setwd (path)
    wd1 <- getwd ()
    zip (zipfile = paste0 (wd1, ".zip"), files = dir ())
    setwd (wd0)
    if (!keep) {
        unlink (path, recursive = TRUE)
    }
}
