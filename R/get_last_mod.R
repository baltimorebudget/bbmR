#' Get Name of Last Modified File
#'
#' @param path string, file path
#' @param pattern string, regex
#' @param ignore.case logical
#' @param recursive logical
#'
#' @return a string with the filename of the last modified file
#'
#' @author Lillian Nguyen
#'
#' @export

get_last_mod <- function(path, pattern, ignore.case = FALSE, recursive = FALSE) {

  # Exception handling ####
  if (missing(path) | missing(pattern)) {
    stop("Missing argument(s)")
  }

  # FUNCTION #####################################################################
  files <- list.files(path, pattern, full.names = TRUE,
                      ignore.case = ignore.case, recursive = recursive)

  files %>%
    file.mtime() %>%
    which.max() %>%
    files[.] %>%
    return()
}
