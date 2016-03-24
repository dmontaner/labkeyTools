##create_folders.R
##2016-03-14 david.montaner@gmail.com


##' Create a list folder structures
##' 
##' Functions to create folder structures needed in LabKey
##' to Import From Local Zip Archives
##'
##' @param path the path to the folder to be created. If exists it will be overwritten.
##'
##' @export

## lists (alone) folder
create.lists.folder.structure <- function (path) {
    unlink     (path, recursive = TRUE)
    dir.create (path, recursive = TRUE)
}

##################################################


##' Create a folder structures
##'
##' Base for studies
##' 
##' @param path the path to the folder to be created. If exists it will be overwritten.
##'
##' @export

## main folder (in a study)
create.folder.structure <- function (path) {
    unlink     (path, recursive = TRUE)
    dir.create (path, recursive = TRUE)
}

##################################################

##' Create a study folder structures
##'
##' Base for studies
##' 
##' @param path the path to the folder to be created. If exists it will be overwritten.
##' @param subpath needed sub folder
##'
##' @export

## study
create.study.folder.structure <- function (path, subpath = "study") {
    if (missing (path)) {
        path <- subpath
    } else {
        path <- file.path (path, subpath)
    }
    unlink     (path, recursive = TRUE)
    dir.create (path, recursive = TRUE)
}

##################################################


##' Create a dataset folder structure
##'
##' Base for studies
##' 
##' @param path the path to the folder to be created. If exists it will be overwritten.
##' @param middle  needed sub folder
##' @param subpath needed sub folder
##'
##' @export

## dataset (goes within the study)
create.datasets.folder.structure <- function (path, middle = "study", subpath = "datasets") {
    if (missing (path)) {
        path <- file.path (middle, subpath)
    } else {
        path <- file.path (path, middle, subpath)
    }
    unlink     (path, recursive = TRUE)
    dir.create (path, recursive = TRUE)
}
