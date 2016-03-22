##write_xml_files_datasets.R
##2016-03-14 david.montaner@gmail.com

##' Create .dataset file for LabKey datasets
##'
##' @import XML
##' @export

create.datasets.dataset.file <- function (name,
                                          outfile = paste0 (name, ".dataset"),
                                          path) {
    ## has to be in
    ## file.path (path, "study", "datasets", outfile)
    
    ## SAVE
    if (!missing (path)) outfile <- file.path (path, outfile)
    writeLines (paste ("## Exported using labkeyTools R library", Sys.time ()), con = outfile)
}

################################################################################


##' Create manifest.xml file for LabKey datasets
##'
##' This file types seem equivalent: 
##' - settings.xml files for _LabKey lists_
##' - datasets_manifest.xml files for _LabKey datasets_
##'
##' @param datasets a list of data.frames to be uploaded as _LabKey datasets_.
##' @param meta a data.frame of meta information about the element of lists
##' @param comment set to "", NULL or NA for not comment
##' @param path directory where the file should be saved. If missing current working directory is used.
##' @param outfile name of the output file.
##'
##' @import XML
##' @export

create.datasets.manifest.xml <- function (datasets,
                                          meta = metaInfoDatasets (datasets),
                                          path,
                                          comment,
                                          outfile = "datasets_manifest.xml") {
    ## check meta
    test.meta.info.datasets (datasets = datasets, meta = meta)
    
    ## demographicData to text in lower case (LabKey uses {true, false} Boolean values)
    meta[,"demographicData"] <- tolower (meta[,"demographicData"])
    
    n.datasets.0 <- xmlNode (name = "datasets")
    
    N <- nrow (meta)
    for (i in 1:N) {
        no <- xmlNode (name = "dataset",
                       attrs = c (name              = meta[i, "name"],
                                  id                = meta[i, "id"],
                                  demographicData   = meta[i, "demographicData"],
                                  type              = meta[i, "type"]),
                       xmlNode (name = "tags"))
        
        n.datasets.0 <- addChildren (n.datasets.0, no)
    }
    
    n.datasets <- xmlNode (name = "datasets",
                           attrs = c (metaDataFile = "datasets_metadata.xml"),
                           namespaceDefinitions = "http://labkey.org/study/xml",
                           n.datasets.0)
    
    ## SAVE
    lkt.save.xml (xml = n.datasets, path = path, outfile = outfile, comment = comment)
}

##################################################################################


##' Create datasets_metadata.xml file for LabKey datasets
##'
##' This file types seem equivalent: 
##' - lists.xml files for _LabKey lists_
##' - datasets_metadata.xml files for _LabKey datasets_
##'
##' Description uses 
##'
##' @param datasets a list of data.frames to be uploaded as _LabKey datasets_.
##' @param meta a data.frame of meta information about the element of datasets
##' @param lookup lookup data.frame. See `lookupInit`
##' @param path directory where the file should be saved. If missing current working directory is used.
##' @param comment set to "", NULL or NA for not comment
##' @param outfile name of the output file.
##'
##' @import XML
##' @export

create.datasets.metadata.xml <- function (datasets,
                                          meta = metaInfoDatasets (datasets),
                                          lookup = NULL,
                                          path,
                                          auto.key.name = "lktKey",
                                          comment,
                                          outfile = "datasets_metadata.xml") {    
    ## check meta
    test.meta.info.datasets (datasets = datasets, meta = meta)
    
    ## some checks for the look up may come here
    
    ## tables node
    n.tables <- xmlNode (name = "tables", namespaceDefinitions = "http://labkey.org/data/xml")
    
    ## table nodes
    N <- nrow (meta)
    for (i in 1:N) {
        
        n.title       <- xmlNode (name = "tableTitle",  meta[i, "name"])
        n.description <- xmlNode (name = "description", meta[i, "description"])

        ## lookup restricted to the current table.
        ## This will not be needed if columns would be unique even across tables.
        ## This works OK with lookup = NULL
        ## but if it is not NULL then the lookup data.frame should have information for all columns in the table.
        ## Some of this information can take value NA.
        lookup.t <- lookup[lookup$table == meta[i, "name"],]
        ##
        n.columns <- create.datasets.columns.node (datasets[[i]],
                                                   lookup = lookup.t,
                                                   subjectColumnName = meta[i, "subjectColumnName"],
                                                   auto.key.name = auto.key.name)
        
        n.table <- xmlNode (name = "table",
                            attrs = c (tableName = meta[i, "name"],
                                       tableDbType = "TABLE"),
                            n.description, n.columns, n.title)
        
        n.tables <- addChildren (n.tables, n.table)
    }
    
    ## SAVE
    lkt.save.xml (xml = n.tables, path = path, outfile = outfile, comment = comment)
}

########################################

##' Create column nodes for datasets
##' 
##' To be used form within create.datasets.metadata.xml.
##'
##' If subjectColumnName (ParticipantId) is __not unique__ (ie. not a Key)
##' a new "virtual" column is created to be the key in the table.
##'
##' "Virtual" means it is not in the csv files but is declared in the
##' datasets_metadata.xml file and is created in LabKey when data are imported.
##'
##' @param df a data.frame; an element from datasets
##' @param lookup lookup data.frame. See *lookupInit*
##' @param subjectColumnName ParticipantId column name.
##' @param auto.key.name name for the automatic key column if this needs to be created.
##'
##' @import XML
##' @export

create.datasets.columns.node <- function (df,
                                          lookup = NULL,
                                          subjectColumnName,
                                          auto.key.name = "lktKey") {
    ## columns node
    n.columns <- xmlNode (name = "columns")
    
    ## column nodes
    for (co in colnames (df)) {
        clase <- class (df[,co])[1] ## sometimes (with Dates) there are two classes
        n.column <- xmlNode (name = "column",
                             attrs = c (columnName = co),
                             xmlNode (name = "datatype",    datatype (clase)),
                             xmlNode (name = "columnTitle", lktNiceTitles (co)),
                             xmlNode (name = "rangeURI",    rangeURI (clase)))
        ## fk node
        if (!is.null (lookup)) {
            touse <- lookup[,"column"] == co
            n.fk <- fkNode (schema  = lookup[touse, "mapSchema"], 
                            table   = lookup[touse, "mapTable"],
                            column  = lookup[touse, "mapColumn"],
                            display = lookup[touse, "mapDisplay"])
            n.column <- addChildren (n.column, kids = n.fk)
        }
        ## update columns
        n.columns <- addChildren (n.columns, n.column)
    }
    
    ## Create an extra "virtual" key column if PatientId is not unique.
    
    needskey <- any (duplicated (df[,subjectColumnName]), na.rm = TRUE)
    if (needskey) {
        n.column <- xmlNode (name = "column",
                             attrs = c (columnName = auto.key.name),
                             xmlNode (name = "datatype",    "integer"), 
                             xmlNode (name = "columnTitle", auto.key.name),
                             xmlNode (name = "isHidden",    "true"),
                             xmlNode (name = "isKeyField",  "true"),
                             xmlNode (name = "isAutoInc",   "true"))
        n.columns <- addChildren (n.columns, kids = list (n.column))
    }
    
    return (n.columns)
}

########################################

##' Creates a fk node for lookups
##'
##' Creates a fk node for those columns which will have a lookup link in LabKey.
##' 
##' fkNode is intended to be used within `create.datasets.columns.node`
##' particularly in `addChildren` `kids` parameter.
##' That is why its output is wrapped in a list.
##' 
##' https://www.labkey.org/download/schema-docs/xml-schemas/schemas/tableInfo_xsd/schema-summary.html#r184
##' 
##' @param schema the LabKey schema where the lookup table should be found.
##' Usually "lists" or "study".
##' If empty, the target ("one" side) table is assumed to exist in the same schema as the "many" side table.
##' @param table The name of the target table of the relationship, the "one" side of the many-to-one relationship.
##' @param column The name of the target column in the target table of the fk relationship.
##' @param display The name of the column in the lookup's target that should be shown as the value.
##' If not specified, defaults to the lookup target's title column.
##'
##' @return A list containing the fk node or an `empty` list.
##'
##' @import XML
##' @export

## ## call would be
## fkNode (schema  = lookup[,"fkSchema"],
##         table   = lookup[,"fkTable"],
##         column  = lookup[,"fkColumn"],
##         display = lookup[,"fkDisplay"])

fkNode <- function (schema = "lists", table, column = NA, display = NA) {
    
    if (!is.na (table)) {
        n.fk <- xmlNode (name = "fk")        
        
        if (!is.na (schema))  n.fk <- addChildren (n.fk, xmlNode (name = "fkDbSchema",          schema))    
                              n.fk <- addChildren (n.fk, xmlNode (name = "fkTable",             table))   ## the only one mandatory
        if (!is.na (column))  n.fk <- addChildren (n.fk, xmlNode (name = "fkColumnName",        column))
        if (!is.na (display)) n.fk <- addChildren (n.fk, xmlNode (name = "fkDisplayColumnName", display))
        
        n.fk <- list (n.fk) ## wrap the node in a list so that it can be included in addChildren using the `kids` parameter.
    } else {
        n.fk <- list () ## empty list. Can be later be included as: addChildren (n.fk, kids = list ())
    }
    
    return (n.fk)
}

## fkNode ("uno", "dos")
## fkNode ("uno", "dos", "tres")
## fkNode ("uno", "dos", "tres", "cuatro")
## fkNode (table = "dos")
## fkNode (table = NA)

##   <datatype>integer</datatype>
##   <columnTitle>Sex</columnTitle>
##   <rangeURI>http://www.w3.org/2001/XMLSchema#int</rangeURI>
	
##   <fk>
##     <fkDbSchema>lists</fkDbSchema>
##     <fkTable>label_sex</fkTable>
##     <fkColumnName>key</fkColumnName>
##   </fk>

## </column>

## <column columnName="ethnic_category">
##   <datatype>varchar</datatype>
##   <columnTitle>Ethnic Category</columnTitle>
##   <rangeURI>http://www.w3.org/2001/XMLSchema#string</rangeURI>

##   <fk>
##     <fkDbSchema>lists</fkDbSchema>
##     <fkTable>label_ethnicithy</fkTable>
##     <fkColumnName>key</fkColumnName>
##   </fk>

## </column>
