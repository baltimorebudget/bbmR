#' Renames commonly inconsistent column names in BBMR data
#'
#' Standardizes column names to Title Case when recognized,
#' and tolowercasewithout spaces when not recognized
#'
#' @param df dataframe with colnames to modify
#'
#' @return A vector of strings
#'
#' @author Sara Brumfield
#'
#' @import magrittr
#' @import dplyr
#' @export

rename_upper_to_title <- function(df){
  col_names_original <- colnames(df)
  col_names_title_case <- str_to_title(col_names_original)
  col_names_title_case <- gsub(pattern = "Id", x = col_names_title_case, replacement = "ID")
  col_names_title_case <- gsub(pattern = "Oso", x = col_names_title_case, replacement = "OSO")
  col_names_title_case <- gsub(pattern = "Si ", x = col_names_title_case, replacement = "SI ")
  colnames(df) <- col_names_title_case

  return(df)
}

#' Renames commonly inconsistent column names in BBMR data
#'
#' Standardizes column names to Title Case when recognized,
#' and tolowercasewithout spaces when not recognized
#'
#' @param df dataframe with colnames to modify
#'
#' @return A vector of strings
#'
#' @author Lillian Nguyen
#'
#' @import magrittr
#' @import dplyr
#' @export

rename_cols <- function(df) {

  if (missing(df))
    stop("Missing dataframe")

  x <- colnames(df) %>%
    # Converts % and $ to words to avoid losing that info in colnames
    gsub("%", "Percent", ., fixed = TRUE) %>%
    gsub("$", "Dollars", ., fixed = TRUE) %>%
    # Eliminates .x, .y,, numbers, punctuation and spaces, then coerces to lowercase
    gsub("\\.x$|\\.y$|[^a-zA-Z0-9]", "", .) %>%
    tolower() %>%
    dplyr::recode(
      # Line items
      "agencyid"  = "Agency ID", "agencyname" = "Agency Name",
      "serviceid" = "Service ID", "servicename" = "Service Name",
      "programid" = "Service ID", "programname" = "Service Name",
      "activityid" = "Activity ID", "activityname" = "Activity Name",
      "subactivityid" = "Subactivity ID", "subactivityname" = "Subactivity Name",
      "fundid" = "Fund ID", "fundname" = "Fund Name",
      "detailedfundid" = "Detailed Fund ID", "detailedfundname" = "Detailed Fund Name",
      "objectid" = "Object ID", "objectname" = "Object Name",
      "subobjectid" = "Subobject ID", "subobjectname" = "Subobject Name",
      "objectiveid" = "Objective ID", "objectivename" = "Objective Name",
      "justification" = "Justification",
      # Positions
      "classificationid" = "Classification ID", "classificationname" = "Classification Name",
      "unionid" = "Union ID", "unionname" = "Union Name",
      "specialindicatorid" = "Special Indicator ID", "specialindicatorname" = "Special Indicator Name",
      "jobnumber" = "Job Number", "entrydate" = "Entry Date", "funding" = "Funding",
      "projectedsalary" = "Projected Salary", "totalcost" = "Total Cost",
      # Expenditure
      "26digitacct" = "26-Digit Account", "15digitacct" = "15-Digit Account",
      "26digitaccountnumber" = "26-Digit Account", "15digitaccountnumber" = "15-Digit Account",
      "carryforwarda" = "Carry-Forward A", "carryforwardb" = "Carry-Forward B",
      "carryforwardtotal" = "Carry-Forward Total",
      "appropriationsadjustment" = "Appropriations Adjustment", "totalbudget" = "Total Budget",
      "ytdexp" = "YTD Exp", "totalencumbrance" = "Total Encumbrance",
      "currentaccrual" = "Current Accrual",
      "may" = "May" # toTitleCase doesn't catch this as a month name
    )

  x <- case_when(
    # cols starting with FY
    # historical report
    grepl("^fy{1}[0-9]{2}surdef", x) ~
      paste0("FY", str_extract(x, "[0-9]{2}"), " Sur/Def"),
    grepl("^fy{1}[0-9]{2}totalbudget", x) ~
      paste0("FY", str_extract(x, "[0-9]{2}"), " Total Budget"),
    grepl("^fy{1}[0-9]{2}bapsactual", x) ~
      paste0("FY", str_extract(x, "[0-9]{2}"), " BAPS Actual"),
    grepl("^fy{1}[0-9]{2}netactual", x) ~
      paste0("FY", str_extract(x, "[0-9]{2}"), " Net Actual"),
    # expenditure report
    grepl("^fy{1}[0-9]{2}ytdactual", x) ~
      paste0("FY", str_extract(x, "[0-9]{2}"), " YTD Actual"),
    # line item report
    grepl("^fy{1}[0-9]{2}finrec", x) ~
      paste0("FY", str_extract(x, "[0-9]{2}"), " FinRec"),
    grepl("^fy{1}[0-9]{2}boe", x) ~
      paste0("FY", str_extract(x, "[0-9]{2}"), " BOE"),
    grepl("^fy{1}[0-9]{2}cou", x) ~
      paste0("FY", str_extract(x, "[0-9]{2}"), " COU"),
    grepl("^fy{1}[0-9]{2}cls|tls", x) ~
      paste0("FY", str_extract(x, "[0-9]{2}"), " ",
             toupper(str_replace(x, "fy{1}[0-9]{2}", ""))),
    grepl("^fy{1}[0-9]{2}[^cls|tls|boe|finrec]", x) ~
      paste0("FY", str_extract(x, "[0-9]{2}"), " ",
             tools::toTitleCase(str_replace(x, "fy{1}[0-9]{2}", ""))),
    # months (expenditure report)
    x %in% tolower(c(month.abb, month.name)) ~ tools::toTitleCase(x),
    TRUE ~ x) %>%
    return() }
