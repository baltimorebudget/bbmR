#' Order funds
#'
#' Converts a column of fund name strings to factor in BBMR standardized order
#'
#' @param df Dataframe with fund column to modify.
#' @param col String. Name of the column to with fund names to order, defaults
#'  to "Fund Name".
#' @param before String. Name of a fund to tack on at the beginning, usually totals.
#' @param after String. Name of a fund to tack on at the end, usually totals.
#'
#' @return A dataframe with col converted to factor.
#'
#' @author Lillian Nguyen
#'
#' @import magrittr
#' @import dplyr
#' @export

order_funds <- function(df, col = "Fund Name", before = NULL, after = NULL) {

  # params:
  #   - col: a string, column to order
  #   - before: a string, fund to tack on at the end, usually totals
  #   - after: a string, fund to tack on at the end, usually totals

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

  df <- df %>%
    mutate(!!sym(col) := factor(!!sym(col), funds))

  return(df)
}
