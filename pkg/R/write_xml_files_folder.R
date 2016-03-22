##write_xml_files_folder.R
##2016-03-14 david.montaner@gmail.com

##' @import XML
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

