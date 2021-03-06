% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/saveLists.R
\name{saveLists}
\alias{saveLists}
\title{Save lists to be exported to LabKey}
\usage{
saveLists(lists, path, meta, protect = TRUE, format = TRUE,
  long.int.as.char = TRUE, long = 10^9, nicenames = TRUE, key = NA,
  lookupDisplay = NA, baseID = 100, auto.key.name = "lktKey",
  zip = TRUE, keep = !zip, verbose = TRUE)
}
\arguments{
\item{lists}{A list of data.frames to be uploaded as `LabKey lists`.}

\item{path}{Path to the directory where the file should be saved.
If zip = TRUE this directory will be zipped.}

\item{meta}{A data.frame of meta information about the element of `lists`.
See `metaInfoLists`.}

\item{protect}{If TRUE it will not overwrite the `path` file if it already exists.
If protect = FALSE then, if the file exist it will be deleted and created again.}

\item{format}{If TRUE the data.frames in `lists` are formatted for a more suitable representation in LabKey.
See `formatDataFrameList`.}

\item{long.int.as.char}{If TRUE, long integers will be uploaded as character variables.}

\item{long}{The threshold to consider an integer long.}

\item{nicenames}{If TRUE the column names are processed to be compatible with LabKey standards.}

\item{key}{Key column in the table. See `metaInfoLists`.}

\item{lookupDisplay}{Display column in the table when used as a `lookup` table. See `metaInfoLists`.
baseID = 100,}

\item{baseID}{First id (minus one) to be used to name the tables. See `metaInfoLists`.}

\item{auto.key.name}{Name for an automatic key column if this needs to be created.}

\item{zip}{If TRUE the files created in `path` will be compressed as needed to be uploaded to LabKey.x}

\item{keep}{if TRUE uncompressed files are kept, otherwise deleted.}

\item{verbose}{verbose mode}
}
\description{
To upload the saved list go to LabKey and then:
Admin > Manage Lists > IMPORT LIST ARCHIVE
}
\details{
You can copy all lists in a folder to another folder
or another server using export and import.
Export packages up all your lists into a list archive:
a .lists.zip file that conforms to the LabKey list export format.
The process is similar to study export/import/reload.
Information on the list serialization format is covered as part of Study Import/Export Files and Formats.

Export

To export all the lists in a folder to a list archive:

In the folder that contains lists of interest, go to the Lists
web part and click Manage Lists.
Select Export List Archive.
All lists in the current folder are exported into a zip archive.

Import
To import a list archive:

In the folder where you would like to import the list archive,
go to the Lists web part and select Manage Lists.
Select Import List Archive.
Browse to the .zip file that contains your list archive and select it.
Click Import List Archive.
The imported lists will then be displayed in the Lists web part.
Note: Existing lists will be replaced by lists in the archive
with the same name; this could result in data loss and cannot be undone.

See here for list Import into LabKey

https://help.labkey.org/wiki/home/Documentation/Archive/15.2/page.view?name=exportImportLists

long.int.as.char TRUE, long integers will be uploaded as character variables.
Suitable for phone numbers, NHS numbers and some other ids.

`key`, `lookupDisplay` and `baseID` parameters are overwritten by the `meta` data.frame
}

