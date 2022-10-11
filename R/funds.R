#' Order funds
#'
#' Converts a column of fund name strings to factor in BBMR standardized order
#'
#' @param df Dataframe with fund column to modify.
#' @param col String. Name of the column to with fund names to order, defaults
#'  to "Fund Name".
#' @param before A string, name of a fund to tack on at the beginning of the factor (usually "Total").
#' @param after A string, name of a fund to tack on at the end of the factor (usually "Total").
#'
#' @return A dataframe with col converted to factor.
#'
#' @author Lillian Nguyen
#'
#' @import magrittr
#' @import dplyr
#' @export

order_funds <- function(df, col = "Fund Name", before = NULL, after = NULL) {

  funds <- c(
    # operating
    "General", "Internal Service", "Convention Center Bond",
    "Conduit Enterprise", "Wastewater Utility",
    "Water Utility", "Stormwater Utility",
    "Loan and Guarantee Enterprise",
    "Parking Enterprise", "Parking Management",
    "Federal", "State",
    # should eventually remove "special"
    "Special", "Special Revenue", "Special Grant",
    # capital
    "General Obligation Bonds", "Revenue Bonds",
    "County Transportation Bonds",
    # other
    "Other")

  if (!is.null(before)) {
    funds <- c(before, funds)
  }

  if (!is.null(after)) {
    funds <- c(funds, after)
  }

  df %>%
    mutate(!!sym(col) := factor(!!sym(col), funds))

}

#' Categorize operating funds
#'
#' Appends a fund category column based on an existent fund name column.
#'
#' @param df A dataframe, has a fund name column.
#' @param fund_col A string, name of the column to with fund names to order, defaults
#'  to "Fund Category".
#' @param cat_col A string, name to assign to the column of fund categories to be appended. Defaults
#'  to "Fund Category".
#' @param after A string, name of a fund to tack on at the end of the factor (usually "Total").
#'
#' @return A dataframe with col converted to factor.
#'
#' @author Lillian Nguyen
#'
#' @import magrittr
#' @import dplyr
#' @export
#'
categorize_op_funds <- function(df, fund_col = "Fund Name",
                                 cat_col = "Fund Category", after = NULL) {

  fund_col <- sym(fund_col)
  cat_col <- sym(cat_col)

  df <- df %>%
    mutate(
      !!cat_col := case_when(
        !!fund_col %in% c("General", "Motor Vehicle") ~ "General Fund",
        !!fund_col == "Internal Service" ~ "Internal Service Fund",
        !!fund_col %in% c("Parking Management", "Convention Center Bond") ~ "Special Purpose Funds",
        !!fund_col %in% c("Federal", "State", "Special Grant", "Special Revenue") ~ "Grant Funds",
        !!fund_col %in% c("Parking Enterprise", "Conduit Enterprise", "Loan and Guarantee Enterprise",
                          "Wastewater Utility", "Water Utility", "Stormwater Utility") ~ "Enterprise Funds",
        TRUE ~ !!fund_col))

  funds <- c("General Fund", "Special Purpose Funds", "Grant Funds", "Enterprise Funds", "Internal Service Fund")

  if (!is.null(after)) {
    funds <- c(funds, after)
  }

  df %>%
    mutate(!!cat_col := factor(!!cat_col, funds))

}

