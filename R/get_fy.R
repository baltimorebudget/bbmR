#' Get Fiscal Year from date
#'
#' @param date lubridate object
#'
#' @return numeric
#'
#' @author Lillian Nguyen
#'
#' @import dplyr
#' @import lubridate
#' @export


get_fy <- function(date = NULL) {

  stopifnot("Argument should be class 'date'" = identical(class(date), "Date"))

  case_when(
    month(date) >= 7 ~ year(date) + 1,
    month(date) <= 6 ~ year(date)) %>%
    return()

}
