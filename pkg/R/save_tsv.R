##save_tsv.R
##2016-03-14 david.montaner@gmail.com


##' Create tsv files for LabKey lists
##'
##' @param lists a list of data.frames to be uploaded as LabKey _lists_.
##' @param meta a data.frame of meta information about the element of lists. See `metaInfoLists`.
##' @param path directory where the files should be saved. If missing current working directory is used.
##' @param nicenames should column names be "cleaned". See `lktNiceNames`.
##' @param auto.key.name name for the automatic key column if this needs to be created.
##'
##' @export

create.lists.tsv <- function (lists,
                              meta = metaInfoLists (lists),  ## needed to handle key availability
                              path,
                              nicenames = TRUE,
                              auto.key.name = "lktKey") {

    ## check meta
    test.meta.info.lists (lists = lists, meta = meta)
    
    ## path
    if (!missing (path)) {
        meta[,"tsv.file"] <- file.path (path, meta[,"tsv.file"])
    }

    ## process key columns
    empty.char <- which (meta[,"key"] == "")
    meta[empty.char, "key"] <- NA
        
    ## WRITE
    N <- nrow (meta)
    for (i in 1:N) {
        datos <- lists[[i]]
        if (is.na (meta[i, "key"])) {
            datos[,auto.key.name] <- 1:nrow (datos)
        }
        if (nicenames) {
            nombres <- lktNiceNames (colnames (datos)) ## SHOULD AGREE WITH create.lists.columns.node
        } else {
            nombres <- colnames (datos)
        }
        ## print (i)
        ## print (nombres)
        ## print (head (datos))
        write.table (datos, file = meta[i, "tsv.file"], 
                     quote = TRUE,  ## revise quotation
                     sep = "\t", na = "", row.names = FALSE,
                     col.names = nombres)
    }
}

################################################################################


##' Create tsv files for LabKey datasets
##'
##' When ParticipantId is not a "Key" a _virtual_ Key column is created.
##' Unlike in _LabKey lists_ in _LabKey datasets_ this column does not need to appear in the tsv files;
##' it is enough to declare it in the manifest_metadata.xml file
##'
##' @param datasets a list of data.frames to be uploaded as _LabKey datasets_.
##' @param meta a data.frame of meta information about the element of datasets. See `metaInfoDatasets`.
##' @param path directory where the files should be saved. If missing current working directory is used.
##' @param nicetitles should column names be "cleaned". See `lktNiceTitles`.
##'
##' @export

create.datasets.tsv <- function (datasets,
                                 meta = metaInfoDatasets (datasets),
                                 path,
                                 nicetitles = TRUE) {
    
    ## check meta
    test.meta.info.datasets (datasets = datasets, meta = meta)
    
    ## path
    if (!missing (path)) {
        meta[,"tsv.file"] <- file.path (path, meta[,"tsv.file"])
    }
    
    ## WRITE
    N <- nrow (meta)
    for (i in 1:N) {
        datos <- datasets[[i]]
        if (nicetitles) {
            nombres <- lktNiceTitles (colnames (datos)) ## SHOULD AGREE WITH create.datasets.columns.node
        } else {
            nombres <- colnames (datos)
        }
        write.table (datos, file = meta[i, "tsv.file"], 
                     quote = TRUE, ## revise quotation
                     sep = "\t", na = "", row.names = FALSE,
                     col.names = nombres)
    }
}
