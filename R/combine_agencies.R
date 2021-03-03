#' Combine agency names and IDs
#'
#' Combine agency data where legacy systems differ from existing municipal organization
#'
#' @param df dataframe with agency information that needs to be combined; use
#' rename_cols() first
#' @return df with modified agency columns
#'
#' @seealso \code{\link{rename_colsl}}
#'
#' @author Lillian Nguyen
#'
#' @import dplyr
#' @export


combine_agencies <- function(df) {

  if (TRUE %in% grepl("Program ID", names(df))) {
    df %>% rename(`Service Name` = `Program Name`, `Service ID` = `Program ID`)
  }

  df %>%
    dplyr::mutate(`Agency ID` = case_when(
      `Agency ID` == "7700" ~ "2600",
      `Agency ID` %in% c("4353", "4353") ~ "4301",
      `Service ID` ==  "350" ~ "4301", # historically BERT but agency incorrect
      TRUE ~ `Agency ID`),
      `Agency Name` = case_when(
        `Agency Name` == "War Memorial Commission" ~ "General Services",
        `Agency Name` %in% c("M-R: Office of Neighborhoods",
                             "M-R: Office of CitiStat Operations") ~ "Mayoralty",
        `Service ID` ==  "350" ~ "Mayoralty",
        TRUE ~ `Agency Name`)) %>%
    return()
}
