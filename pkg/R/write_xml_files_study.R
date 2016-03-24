##write_xml_files_study.R
##2016-03-14 david.montaner@gmail.com


##' Create study.xml file for LabKey
##'
##'
##' @param label The folder label in LabKey.
##' @param path Path to the directory where the file should be saved.
##' If missing current working directory is used.
##' @param timepointType "DATE" or "VISIT".
##' @param subjectColumnName usually "ParticipantId".
##' @param subjectNounSingular as LabKey parameter
##' @param subjectNounPlural   as LabKey parameter
##' @param startdDate          as LabKey parameter
##' @param securityType usually "BASIC_WRITE" or "BASIC_READ"
##' @param file.dataset name of the .dataset file.
##' @param comment Comment to be inserted into the xml file. set to "", NULL or NA for no comment.
##' @param outfile Name of the output file.
##' 
##' 
##' @import XML
##' @export

create.study.study.xml <- function (label, ##folderLabel,
                                    path,
                                    timepointType = "DATE",  ## see how to develop the "VISIT" value
                                    subjectColumnName   = "ParticipantId",
                                    subjectNounSingular = "Participant",
                                    subjectNounPlural   = "Participants",
                                    startdDate = format (Sys.Date(), "%Y-%m-%dZ"),
                                    securityType = "BASIC_WRITE",  ## "BASIC_READ" "BASIC_WRITE"                                    
                                    file.dataset = paste0 (label, ".dataset"),
                                    comment,
                                    outfile = "study.xml") {

    if (!timepointType %in% c ("DATE", "VISIT")) {
        stop ("timepointType :", timepointType, "not implemented jet")
    }
    if (timepointType == "VISIT") stop ("timepointType :", timepointType, "not implemented jet")

    
    ## STUDY
    n.study <- xmlNode (name = "study",
                        attrs = c (archiveVersion = "15.2",
                                   label = label,
                                   timepointType       = timepointType,
                                   subjectNounSingular = subjectNounSingular,
                                   subjectNounPlural   = subjectNounPlural,
                                   subjectColumnName   = subjectColumnName,
                                   investigator = "",
                                   grant = "",
                                   species = "",
                                   alternateIdPrefix = "",
                                   alternateIdDigits = "6",
                                   defaultTimepointDuration = "1",
                                   startDate    = startdDate,
                                   securityType = securityType), 
                        namespaceDefinitions = c ("http://labkey.org/study/xml",
                                                  xsi = "http://www.w3.org/2001/XMLSchema-instance"))
    
    ## datasets
    n.definition <- xmlNode (name = "definition",
                             attrs = c (file = file.dataset))
    
    n.datasets <- xmlNode (name = "datasets",
                           attrs = c (dir = "datasets",
                                      file = "datasets_manifest.xml"),
                           n.definition)
    
    
    ## studyDescription
    n.description <- xmlNode (name = "description",
                              attrs = c ('xsi:nil' = "true"))
    
    n.studyDescription <- xmlNode (name = "studyDescription",
                                   attrs = c (rendererType = "TEXT_WITH_LINKS"),
                                   n.description)
    
    
    ## properties
    n.properties <- xmlNode (name = "properties",
                             attrs = c (dir = "properties"))
    
    
    ## include the 3 children
    ## n.study <- addChildren (n.study, kids = list (n.datasets,
    ##                                               n.studyDescription,
    ##                                               n.properties))
    n.study <- addChildren (n.study, n.datasets, n.studyDescription, n.properties)

    ## SAVE
    lkt.save.xml (xml = n.study, path = path, outfile = outfile, comment = comment)
}
