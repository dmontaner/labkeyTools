##write_xml_files_folder.R
##2016-03-14 david.montaner@gmail.com


##' Create a folder.xml file
##'
##' @param label Folder label as in LabKey.
##' @param title Folder title as in LabKey.
##' @param path Path to the directory where the file should be saved.
##' If missing current working directory is used.
##' @param comment Comment to be inserted into the xml file. set to "", NULL or NA for no comment.
##' @param outfile Name of the output file.
##'
##' @import XML
##'
##' @export

create.folder.folder.xml <- function (label, ##folderLabel,
                                      title = label, ##folderTitle = folderLabel,
                                      path,
                                      comment,
                                      outfile = "folder.xml") {
    
    n.study <- xmlNode (name = "study", attrs = c (dir = "study"))
    ##
    n.folder <- xmlNode (name = "folder", n.study,
                         attrs = c (archiveVersion = "15.2",
                                    label = label,
                                    type  = "normal",
                                    title = title,
                                    xmlns = "http://labkey.org/folder/xml"))
    
    ## SAVE
    lkt.save.xml (xml = n.folder, path = path, outfile = outfile, comment = comment)
}
