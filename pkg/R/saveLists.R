##saveLists.R
##2016-03-14 david.montaner@gmail.com

##' Save lists to be exported to LabKey
##'
##'
##' To upload the saved list go to LabKey and then:
##' Admin > Manage Lists > IMPORT LIST ARCHIVE
##' 
##' You can copy all lists in a folder to another folder
##' or another server using export and import.
##' Export packages up all your lists into a list archive:
##' a .lists.zip file that conforms to the LabKey list export format.
##' The process is similar to study export/import/reload.
##' Information on the list serialization format is covered as part of Study Import/Export Files and Formats.
##'
##' Export
##' 
##' To export all the lists in a folder to a list archive:
##' 
##' In the folder that contains lists of interest, go to the Lists
##' web part and click Manage Lists.
##' Select Export List Archive.
##' All lists in the current folder are exported into a zip archive.
##'
##' Import
##' To import a list archive:
##' 
##' In the folder where you would like to import the list archive,
##' go to the Lists web part and select Manage Lists.
##' Select Import List Archive.
##' Browse to the .zip file that contains your list archive and select it.
##' Click Import List Archive.
##' The imported lists will then be displayed in the Lists web part.
##' Note: Existing lists will be replaced by lists in the archive
##' with the same name; this could result in data loss and cannot be undone.
##'
##' See here for list Import into LabKey
##'
##' https://help.labkey.org/wiki/home/Documentation/Archive/15.2/page.view?name=exportImportLists
##'
##' @export
saveLists <- function (lists,
                       path, ## compulsory here in the function; that is why I change the order
                       meta,
                       protect = TRUE, ## if TRUE gives an error if the path exist
                       format = TRUE, ##format columns of the datasets or lists. May be change this parameter name.
                       long.int.as.char = TRUE, long = 10^9, ##after format
                       nicenames = TRUE,  ### May be change to NICECOLNAMES
                       validate = TRUE,
                       key = NA,
                       lookupDisplay = NA,
                       baseID = 100,
                       auto.key.name = "lktKey",
                       zip = TRUE,
                       keep = !zip,
                       verbose = TRUE) {
    
    if (missing (meta)) {
        if (verbose) cat ("Creating Lists metainformation", fill = TRUE)
        meta <- metaInfoLists (lists = lists, key = key, lookupDisplay = lookupDisplay, baseID = baseID)
    }
    
    if (format) {
        if (verbose) cat ("Formatting Lists columns", fill = TRUE)
        lists <- formatDataFrameList (datasets = lists, long.int.as.char = long.int.as.char, long = long)
    }
    
    ## CREATE LIST FOLDER
    if (protect) {
        if (file.exists (path) & (length (dir (path)) > 0)) {
            stop ("your path\n", path, "\nis a non empty directory. Set protect = FALSE if you want to overwrite it. ")
        }
    }
    if (verbose) cat ("Creating Lists folder", fill = TRUE)
    create.lists.folder.structure (path = path)
    
    ## SAVE TSV
    if (verbose) cat ("Writing Lists tsv files", fill = TRUE)
    create.lists.tsv (lists = lists, meta = meta, path = path, nicenames = nicenames, auto.key.name = auto.key.name)

    ## SAVE SETTINGS.XML
    if (verbose) cat ("Writing Lists settings.xml file", fill = TRUE)
    create.lists.settings.xml (lists = lists, meta = meta, path = path)
    
    ## SAVE LISTS.XML
    if (verbose) cat ("Writing Lists lists.xml file", fill = TRUE)
    create.lists.lists.xml (lists = lists, meta = meta, path = path, nicenames = nicenames)
    
    ## ZIP
    if (zip) {
        if (verbose) cat ("Creating Lists zip archive", fill = TRUE)
        zip4labkey (path = path, keep = keep)
    }
}

################################################################################

##' Save Lookup Lists
##' 
##' A wrapper function to saveLists
##'
##' Assumes:
##' - keys are in the firs column
##' - values or labels are in the second column
##'
##'
##' @export

saveLookUpLists <- function (lists,
                             path,
                             key = 1,
                             lookupDisplay = 2,
                             ...) {
    
    saveLists (lists = lists, path = path, key = key, lookupDisplay = lookupDisplay, ...)
}
