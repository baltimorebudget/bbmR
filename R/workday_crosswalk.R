#' Crosswalks Workday and BPFS values
#'
#' Joins datasets on common fields
#'
#' @param data, dataframe, default to data$revenue for data prep
#' @param coa_file, file path
#' @param coa_sheet sheet name for chart of accounts file
#'
#' @return dataframe
#'
#' @author Sara Brumfield

##need 4 pieces to xwalk data:
##1. revenue's chart of accounts file,
##2. BPFS's ACCT_MAP_26_15 table to derive a compatible program id # for Workday's FDM Program-Activity,
##3. Workday's FDM cross walk file for Fund <- Fund, Cost Center <- Pgm-Activity, Grant/SP <- Detailed Fund
##4. William's custom OSO mapping file to get Spend Category
##5. subactivity?

##26-digit: 4001(fund)-442200(detailed fund)-1110(program)-842200(activity)-601001(payroll natural)
##15-digit: 4001(fund)-111(program)-001(activity)-00(subactivity)-1(object)-01(subobject)

##revenue functions ===========
##for revenue budget book data set only
revenue_workday_xwalk <- function(data = data$revenue, coa_file = "G:/Analyst Folders/Sara Brumfield/_ref/Chart_of_Accounts 2022-09-06.xlsx",
                     coa_sheet = "All Revenue Categories (Non-Gr)") {
  coa <- import(coa_file, which = coa_sheet) %>%
  select("Short", "BBMR - Workday Description - Revenue Description - Revenue Category")

  coa["BBMR - Workday Description - Revenue Description - Revenue Category"][coa["BBMR - Workday Description - Revenue Description - Revenue Category"]=="Identified Gaps in SI#"] <- NA
  coa <- coa %>% unique()
  rev <- data %>% left_join(coa, by = c("Revenue Account" = "Short"))
  return(rev)

}


#' @param fiscal_year the column to check numbers on, e.g., Fiscal 2023 Budget
#' @return dataframe, numbers check result as text
#'
#' @author Sara Brumfield
#'
rev_projections_workday_xwalk <- function(fiscal_year = 2023) {
 input <- import("G:/Analyst Folders/Sara Brumfield/_ref/Chart_of_Accounts 2022-09-06.xlsx",
                 which = "All Revenue Categories (Non-Gr)")

 ##not working
 col = sym(paste0("Fiscal ", 2023, " Budget"))
 start_revenue <- sum(input$`Fiscal 2023 Budget`, na.rm = TRUE)

 data <- input %>%
   rename(`Revenue Category` = `BBMR RC #'s`,
          `Description` = `BBMR - Workday Description - Revenue Description - Revenue Category`) %>%
   select(`Short`, `Revenue Category`, `Description`)

 check <- input %>%
   rename(`Revenue Category` = `BBMR RC #'s`,
          `Description` = `BBMR - Workday Description - Revenue Description - Revenue Category`) %>%
   select(`Short`, `Revenue Category`, `Description`, `Fiscal 2023 Budget`) %>%
   group_by(`Short`, `Revenue Category`, `Description`) %>%
   summarise(Total = sum(`Fiscal 2023 Budget`, na.rm =TRUE))

 end_revenue <- sum(check$Total, na.rm = TRUE)
 message <- if (start_revenue == end_revenue) {"Totals match."} else {
   "Totals do not match."}

 message(message)

 return(data)
}

##expenditure functions ===================
##for planning year line item file format only

#' @param exp_file, BPFS-formatted excel planning year file
#' @param bpfs_table, "PLANNINGYEAR24" set as default
#'
#' @return dataframe of result, dataframe of missing cost centers
#'
#' @author Sara Brumfield
#'
exp_lines_workday_xwalk <- function(exp_file, bpfs_table = "PLANNINGYEAR24") {

  ##read in current phase line item and make the 15-digit account #
  input <- import(exp_file, which = "Details") %>%
  mutate(`15-Digit Acct #` = paste0(`Fund ID`, "-", `Program ID`, "-", str_pad(`Activity ID`, width = 3, pad = 0), "-", str_pad(`Subactivity ID`, width = 2, pad = 0), "-", `Object ID`, "-", substring(as.character(`Subobject ID`), 2)))

  ##read in FDM
  workday_values <- workday_get_values()

  ##join sets so input has 26 and 15 digit acct #'s for program-activity to cost center; join order is important
  df <- left_join(input, workday_values$acct_26, by = c("Program ID" = "PROGRAM_ID",
                                                        "Activity ID" = "ACTIVITY_ID",
                                                        "Subactivity ID" = "SUBACTIVITY_ID",
                                                        "Object ID" = "OBJECT_ID",
                                                        "Subobject ID" = "SUBOBJECT_ID",
                                                        "Fund ID" = "FUND_ID",
                                                        "DetailedFund ID" = "DETAILED_FUND_ID")) %>%
    left_join(workday_values$fund, by = c("Fund ID" = "BPFS Fund")) %>%
    left_join(workday_values$spend_cat, by = c("Object ID", "Subobject ID" = "BPFS SubObject ID")) %>%
    left_join(workday_values$acct_26, by = c("15-Digit Acct #" = "DIGIT_ACCOUNT_15")) %>%
    mutate(`Program Activity` = extract_program_activity(col = DIGIT_ACCOUNT_26.x)) %>%
    left_join(workday_values$cc, by = c("Program Activity" = "Pgm-Activity")) %>%
    left_join(workday_values$grant, by = c("DetailedFund ID" = "Grant")) %>%
    distinct

  check <- df %>% filter(is.na(`CCA ID`))

  message(dim(check)[1], " cost centers missing from dataset.")
  message(paste0("$", sum(check$Total, na.rm = TRUE)), " unassigned to a cost center.")

  return(list(data = df, missing = check))

  ##or make excel formulas
  #  data <- join_26 %>%
  #     mutate(
  #       `Cost Center` = "=VLOOKUP(MID([@DIGIT_ACCOUNT_26],13,11),'[Baltimore FDM Crosswalk.xlsx]Pgm-Activity'!$1:$1048576,19,FALSE)",
  #       `Grant` = paste0("=IF(OR(LEFT([@Fund],1)=", "4", ",LEFT([@Fund],1)=", "5", ",LEFT([@Fund],1)=", "7", ",LEFT([@Fund],4)=", "2089", "),VLOOKUP(VALUE(MID([@DIGIT_ACCOUNT_26],6,6)),'[Baltimore FDM Crosswalk.xlsx]ProjectGrant'!$1:$1048576,11,FALSE),\"\")"),
  #       `Special Purpose` = paste0("=IF(LEFT([@Fund],1)=", "6", ",VLOOKUP(VALUE(MID([@DIGIT_ACCOUNT_26],6,6)),'[Baltimore FDM Crosswalk.xlsx]ProjectGrant'!$1:$1048576,19,FALSE),\"\")"),
  #       `Spend Category` = "=VLOOKUP(VALUE(RIGHT([@DIGIT_ACCOUNT_26],6)), '[Baltimore FDM Crosswalk.xlsx]Natural'!$1:$1048576,20,FALSE)",
  #       Debit = "IF [@FY22 Adopted] > 0, [@FY22 Adopted], \"\")",
  #       Credit = "IF [@FY22 Adopted] < 0, [@FY22 Adopted], \"\")",
  #       Memo = "IF [@Justification] = \"\", \"\", [@Justification])",
  #       Fund = "=VLOOKUP(VALUE(LEFT([AD2]@DIGIT_ACCOUNT_15], 4)), '[Baltimore FDM Crosswalk.xlsx]Fund'!$1:$1048576, 10, FALSE)"
  #     )
  #
  # #adjust cell type for Excel
  # df <- apply_formula_class(df = data, cols = c("Debit", "Credit", "Memo", "Fund", "Special Purpose", "Grant", "Cost Center", "Spend Category"))

}

#' @param actuals_file, BAPS excel file of monthly expenditure data
#'
#' @return dataframe of result, data frame of missing cost centers
#'
#' @author Sara Brumfield
#'
#'
##projection year actuals from BAPS system (not Workday!)
##cannot map to cost center without a subactivity!!!!
exp_actuals_workday_xwalk <- function(actuals_file = "G:/Fiscal Years/Fiscal 2022/Projections Year/2. Monthly Expenditure Data/Month 12_June Projections/Expenditure 2022-06_Run7.xlsx") {

  input <- import(actuals_file, which = "CurrentYearExpendituresActLevel")

  ##read in FDM
  workday_values <- workday_get_values()

  df <- left_join(input, workday_values$acct_26, by = c(
    "Program ID" = "PROGRAM_ID",
    "Activity ID" = "ACTIVITY_ID",
    "Object ID" = "OBJECT_ID",
    "Subobject ID" = "SUBOBJECT_ID",
    "Fund ID" = "FUND_ID")) %>%
    left_join(workday_values$fund, by = c("Fund ID" = "BPFS Fund")) %>%
    left_join(workday_values$spend_cat, by = c("Object ID", "Subobject ID" = "BPFS SubObject ID")) %>%
    ##create program-activity key to map to x_cc
    mutate(`Program Activity` = extract_program_activity(col = DIGIT_ACCOUNT_26)) %>%
    ##join on cost center
    left_join(workday_values$cc, by = c("Program Activity" = "Pgm-Activity")) %>%
    #remove duplicates created by joins
    distinct()

  check <- df %>% filter(is.na(`CCA ID`))

  message(dim(check)[1], " cost centers missing from dataset.")
  message(paste0("$", sum(check$Total, na.rm = TRUE)), " unassigned to a cost center.")

  return(list(data = df, missing = check))
}
