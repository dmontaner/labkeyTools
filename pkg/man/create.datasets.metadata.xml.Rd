% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_xml_files_datasets.R
\name{create.datasets.metadata.xml}
\alias{create.datasets.metadata.xml}
\title{Create datasets_metadata.xml file for LabKey datasets}
\usage{
create.datasets.metadata.xml(datasets, meta = metaInfoDatasets(datasets),
  lookup = NULL, path, auto.key.name = "lktKey", comment,
  outfile = "datasets_metadata.xml")
}
\arguments{
\item{datasets}{A list of data.frames to be uploaded as "LabKey study".}

\item{meta}{a data.frame of meta information about the element of `datasets`.
See `metaInfoDatasets`.}

\item{lookup}{Lookup data.frame. See `lookupInit`}

\item{path}{Path to the directory where the file should be saved.
If missing current working directory is used.}

\item{auto.key.name}{Name for an automatic key column if this needs to be created.}

\item{comment}{Comment to be inserted into the xml file. set to "", NULL or NA for no comment.}

\item{outfile}{name of the output file.}
}
\description{
Creates a `datasets_metadata.xml` file
describing each of the columns in the tables of a "LabKey study".
}
\details{
##' This file types seem equivalent (but not completely equal):

- `settings.xml`          files for "LabKey lists"

- `datasets_manifest.xml` files for "LabKey study datasets"
}

