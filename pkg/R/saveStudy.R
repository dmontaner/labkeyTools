##saveStudy.R
##2016-03-14 david.montaner@gmail.com

## Some functions to create folder.zip exported tables

## final function

## Lists 
## LabKey lists are lightweight tables that can be adapted to a range of needs.

## Set Up a New Study
## Create a new observational study from scratch.

## LabKey datasets come in three different types:

## - Demographic. Demographic datasets record participant characteristics such as birth gender,
## birth date, and enrollment date. This data represents the permanent characteristics of the participants and is
## __collected only once__ for a study. (From a database point of view, demographic datasets have one __primary key, the participantId.__
## Demographic datasets contain up to one row of data for each participant.)

## - Clinical. Clinical datasets record participant characteristics that vary over time in the study,
## such as physical exam data and lab test data.
## Typical data includes weight, blood pressure, or lymphocyte counts.
## This data is collected at __multiple times__ over the course of the study.
## (From a database point of view, clinical datasets __have two primary keys, the participantId and a time point__.
## Clinical datasets may contain up to one row of data per subject/time point pair.)

## - Assay/Specimen. These datasets record the assay and specimen data in the study.
## (From a database point of view, assay/specimen datasets have the same primary keys as Clinical data,
## plus an __optional third key__. Multiple rows per subject/time point are allowed.)





##' Save a study to be imported into LabKey
##'
##' To upload the study creating a new project
##' go to LabKey > Newproject 
##' 
##' https://www.labkey.org/home/Documentation/wiki-page.view?name=studySetupManual
##'
##' https://www.labkey.org/home/Documentation/wiki-page.view?name=setStudyProperties
##'
##' @param datasets A list of data.frames to be uploaded as "LabKey study".
##' @param folderLabel Folder label for LabKey.
##' @param folderTitle Folder title for LabKey.
##' @param meta.datasets A data.frame of meta information about the element of `datasets`.
##' See `metaInfoDatasets`.
##' @param lookup Lookup data.frame. See `lookupInit`
##' @param protect If TRUE it will not overwrite the `path` file if it already exists.
##' @param path Path to the directory where the file should be saved.
##' If missing current working directory is used.
##' @param zip If TRUE the files created in `path` will be compressed as needed to be uploaded to LabKey.x
##' @param keep if TRUE uncompressed files are kept, otherwise deleted.
##' @param timepointType "DATE" or "VISIT".
##' @param subjectColumnName usually "ParticipantId".
##' @param subjectNounSingular as LabKey parameter
##' @param subjectNounPlural   as LabKey parameter
##' @param securityType usually "BASIC_WRITE" or "BASIC_READ"
##' @param startdDate          as LabKey parameter
##' @param baseID First id (minus one) to be used to name the tables. See `metaInfoLists`.
##' @param format If TRUE the data.frames in `lists` are formatted for a more suitable representation in LabKey.
##' @param validate If TRUE some validation of the data.sets in `lists` are carried out.
##' @param auto.key.name Name for an automatic key column if this needs to be created.
##' @param description Text description for tables.
##' @param verbose verbose mode
##'
##' @import XML
##'
##' @export

saveStudy <- function (datasets,
                       folderLabel,
                       folderTitle = folderLabel,
                       meta.datasets,
                       lookup = NULL, 
                       protect = TRUE, ## if TRUE gives an error if the path exist
                       ##meta.datasets = metaInfoDatasets (datasets, baseID),
                       path = paste0 (folderLabel, ".folder"),
                       zip = TRUE,
                       keep = !zip,
                       timepointType = "DATE",
                       subjectColumnName   = "ParticipantId",
                       subjectNounSingular = "Participant", 
                       subjectNounPlural   = "Participants",
                       securityType = "BASIC_WRITE",
                       startdDate = format (Sys.Date(), "%Y-%m-%dZ"),
                       
                       baseID = 5000,
                       format = TRUE,
                       validate = TRUE,

                       auto.key.name = "lktKey",
                       description = "Contains up to one row of %s data for each Participant/Visit combination.",
                       verbose = TRUE) {
    
    ## Format Data Frame List ESTA POR REVISAR
    if (format) {
        if (verbose) cat ("Formatting datasets", fill = TRUE)
        datasets <- formatDataFrameList (datasets)
    }
    
    ## Validate Data Frame List ESTA POR REVISAR
    if (validate) {
        if (verbose) cat ("Validating datasets", fill = TRUE)
        validateDataFrameList (datasets)
    }
    
    ## Create or Validade List Metadata
    if (missing (meta.datasets)) {
        if (verbose) cat ("Creating Datasets metainformation", fill = TRUE)
        meta.datasets <- metaInfoDatasets (datasets, baseID)
    } ## else validate
    
    ## check meta
    test.meta.info.datasets (datasets = datasets, meta = meta.datasets)
    
    ## #################################
    
    ## CREATE STUDY FOLDER
    if (protect) {
        if (file.exists (path) & (length (dir (path)) > 0)) {
            stop ("your path\n", path, "\nis a non empty directory. Set protect = FALSE if you want to overwrite it. ")
        }
    }
    if (verbose) cat ("Creating Study folder", fill = TRUE)
    create.folder.structure (path)       ## is the one which cleans up when protect = FALSE
    create.study.folder.structure (path)
    create.datasets.folder.structure (path)
    
    ## #################################    
    
    ## Create File folder.xml
    if (verbose) cat ("Writing folder.xml file", fill = TRUE)
    create.folder.folder.xml (label = folderLabel,
                              title  = folderTitle,
                              path = path)

    ## Create File study.xml
    if (verbose) cat ("Writing study.xml file", fill = TRUE)
    create.study.study.xml (label = folderLabel, 
                            timepointType = timepointType, ##"DATE",
                            subjectNounSingular = subjectNounSingular, ##"Participant", 
                            subjectNounPlural = subjectNounPlural, ##"Participants",
                            subjectColumnName = subjectColumnName, ##"ParticipantId", 
                            startdDate = startdDate, ##format(Sys.Date(), "%Y-%m-%dZ"),
                            securityType = securityType, ##"BASIC_WRITE",
                            path = file.path (path, "study"))
    
    ## Create File .dataset
    if (verbose) cat ("Writing .dataset file", fill = TRUE)
    create.datasets.dataset.file (name = folderLabel,
                                  path = file.path (path, "study", "datasets"))
    
    ## Create File datasets_manifest.xml
    if (verbose) cat ("Writing manifest.xml file", fill = TRUE)
    create.datasets.manifest.xml (datasets = datasets,
                                  meta = meta.datasets,
                                  path = file.path (path, "study", "datasets"))
    
    ## Create File datasets_metadata.xml
    if (verbose) cat ("Writing metadata.xml file", fill = TRUE)
    create.datasets.metadata.xml (datasets = datasets,
                                  meta = meta.datasets,
                                  lookup = lookup,
                                  path = file.path (path, "study", "datasets"),
                                  auto.key.name = auto.key.name)
    
    ## #################################    

    ##Create datasets. tsv files
    if (verbose) cat ("Writing Lists tsv files", fill = TRUE)
    create.datasets.tsv (datasets = datasets,
                         meta = meta.datasets,
                         path = file.path (path, "study", "datasets"),
                         nicetitles = FALSE)   ### VER ESTO
    
    ## #################################

    ## ZIP
    if (zip) {
        if (verbose) cat ("Creating Study zip archive", fill = TRUE)
        zip4labkey (path = path, keep = keep)
    }
}
