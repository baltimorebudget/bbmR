#' Create a folder for each BBMR analyst
#'
#' Useful for distributing files by analyst. Analyst names are from the .Renviron file.
#'
#' @param path A string, the path where the folders should be created
#' @param additional_folders A string, or vector of strings, containing the names of additional folders to be created
#'
#' @author Lillian Nguyen
#'
#' @import stringr
#' @export

create_analyst_dirs <- function(path, additional_folders = NULL) {

  if (missing(path)) {
    path <- getwd()
  }

  analysts <- c(unlist(strsplit(Sys.getenv("BBMR_ANALYSTS"), "|", fixed = TRUE)),
                additional_folders)

  if (stringr::str_trunc(path, 1, side = "left", ellipsis = "") == "/") {
    dirs <- paste0(path, analysts)
  } else {
    dirs <- paste0(path, "/", analysts)
  }

  dirs %>%
    sapply(dir.create, recursive = TRUE)
}
