##write_xml_files_lists.R
##2016-03-14 david.montaner@gmail.com


##' Create settings.xml file for LabKey lists
##'
##' This file types seem equivalent: 
##' - settings.xml files for _LabKey lists_
##' - datasets_manifest.xml files for _LabKey datasets_
##'
##' @param lists a list of data.frames to be uploaded as LabKey _lists_.
##' @param meta a data.frame of meta information about the element of lists
##' @param comment set to "", NULL or NA for not comment
##' @param path directory where the file should be saved. If missing current working directory is used.
##' @param outfile name of the output file.
##'
##' @import XML
##' @export

create.lists.settings.xml <- function (lists, ## is not really required
                                       meta = metaInfoLists (lists),  ## needed to parse `id`
                                       path,
                                       comment,
                                       outfile = "settings.xml") {
    
    ## check meta
    test.meta.info.lists (lists = lists, meta = meta)
    
    ## lists node
    n.lists <- xmlNode (name = "lists",
                        attrs = c (xmlns = "http://labkey.org/list/xml"))

    ## list nodes
    N <- nrow (meta)
    for (i in 1:N) {
        n.list <- xmlNode (name = "list",
                           attrs = c (name = meta[i, "name"],
                                      id   = meta[i, "id"],
                                      entireListIndex        = "true",
                                      entireListIndexSetting = "2"))
        
        n.lists <- addChildren (n.lists, n.list)
    }
    
    ## SAVE
    lkt.save.xml (xml = n.lists, path = path, outfile = outfile, comment = comment)
}

################################################################################


##' Create lists.xml file for LabKey lists
##'
##' This file types seem equivalent: 
##' - lists.xml files for _LabKey lists_
##' - datasets_metadata.xml files for _LabKey datasets_
##'
##' @param lists a list of data.frames to be uploaded as LabKey _lists_.
##' @param meta a data.frame of meta information about the element of lists
##' @param path directory where the file should be saved. If missing current working directory is used.
##' @param comment set to "", NULL or NA for not comment
##' @param nicenames should column names be "cleaned". See lktNiceNames.
##' 
##' @param outfile name of the output file.
##'
##' @import XML
##' @export

create.lists.lists.xml <- function (lists,
                                    meta = metaInfoLists (lists),  ## needed to parse `id`
                                    path,
                                    nicenames = TRUE,
                                    auto.key.name = "lktKey",
                                    comment,
                                    outfile = "lists.xml") {
    
    ## check meta
    test.meta.info.lists (lists = lists, meta = meta)
    
    ## tables node
    n.tables <- xmlNode (name = "tables", namespaceDefinitions = "http://labkey.org/data/xml")

    ## table nodes
    N <- nrow (meta)
    for (i in 1:N) {
        n.table <- xmlNode (name = "table",
                            attrs = c (tableName = meta[i, "name"],
                                       tableDbType = "TABLE"))
        
        ## <description>
        if (!is.na (meta[i, "description"])) {
            n.description  <- xmlNode (name = "description", meta[i, "description"])            
            n.table <- addChildren (n.table, n.description)
        }
        
        ## <titleColumn>
        if (!is.na (meta[i, "lookupDisplay"])) {
            n.titleColumn  <- xmlNode (name = "titleColumn",  colnames (lists[[i]][meta[i, "lookupDisplay"]])) ##works with number and name
            n.table <- addChildren (n.table, n.titleColumn)
        }
        
        ## columns
        n.columns <- create.lists.columns.node (lists[[i]], keycol = meta[i, "key"], nicenames = nicenames, auto.key.name = auto.key.name)  ## reorder this maybe after the <pkColumnName>
        n.table <- addChildren (n.table, n.columns)
        
        ## <pkColumnName> ## apparently this tag is always needed
        if (is.na (meta[i, "key"])) {
            n.pkColumnName <- xmlNode (name = "pkColumnName", auto.key.name)
        } else {
            n.pkColumnName <- xmlNode (name = "pkColumnName", colnames (lists[[i]][meta[i, "key"]])) ##works with number and name
        }
        n.table <- addChildren (n.table, n.pkColumnName)  ## apparently this tag is always needed
        
        ## add table
        n.tables <- addChildren (n.tables, n.table)
    }
    
    ## SAVE
    lkt.save.xml (xml = n.tables, path = path, outfile = outfile, comment = comment)
}

##' Create columns node
##'
##' To be used within the `create.lists.lists.xml` function.
##'
##' If keycol is NA a new key column will be created (see `auto.key.name`)
##'
##' @param df a data.frame. Element of lists.
##' @param keycol the number or name of the column in `df` which is considered to be a key.
##' @param auto.key.name name for the automatic key column if this needs to be created.
##'
##' @import XML
##' @export

create.lists.columns.node <- function (df, keycol = NA, nicenames = TRUE, auto.key.name = "lktKey") {
    
    if (is.na (keycol)) {
        df[,auto.key.name] <- 1:nrow (df) ## integer column
        keycol <- auto.key.name
        has.to.hidde <- TRUE     ## if the index is a "synthetic" one I hide it
    } else {
        keycol <- names (df[keycol]) ## make sure that keycol is the column name and not the column number
        has.to.hidde <- FALSE
    }
    
    n.columns <- xmlNode (name = "columns")
    
    ## add column
    for (co in colnames (df)) {

        if (nicenames) {
            coNice <- lktNiceNames (co)  ## SHOULD AGREE WITH create.lists.tsv
        } else {
            coNice <- co
        }
        
        n.column <- xmlNode (name = "column",
                             attrs = c (columnName = coNice),
                             xmlNode (name = "datatype",    datatype (class (df[,co])[1])), 
                             xmlNode (name = "columnTitle", co), 
                             xmlNode (name = "rangeURI",    rangeURI (class (df[,co])[1])))
        
        if (co == keycol) {
            n.column <- addChildren (n.column, xmlNode (name = "isKeyField", "true"))
            if (has.to.hidde) {
                n.column <- addChildren (n.column,
                                         xmlNode (name = "nullable", "false"),
                                         xmlNode (name = "isHidden", "true"),
                                         xmlNode (name = "isAutoInc","true"))
            }
        }
        n.columns <- addChildren (n.columns, n.column)
    }
    
    return (n.columns)
}
