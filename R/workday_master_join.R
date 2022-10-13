#' Creates master dataframe from joined values from Workday, BPFS and past appropriation files
#' Matches Workday fund = fund, spend category = OSO, cost center = program + activity, grant = detailed fund, ledger = payroll natural
#' @return dataframes of joined values from Workday, BPFS and past appropriation files
#'
#' @author Sara Brumfield
#'

make_master_join <- function() {

  ##read in FDM
  workday_values <- workday_get_values()

  ##read in appropriation values
  approp_values <- get_appropriation_files()

  ##join workday and appropriation values
  ##make master join as its own function
  match_values <- workday_values$acct_26 %>% full_join(approp_values, by = c("PROGRAM_ID" = "ProgramID",
                                                                             "ACTIVITY_ID" = "ActivityID",
                                                                             "SUBACTIVITY_ID" = "SubactivityID",
                                                                             "FUND_ID" = "FundID",
                                                                             "DETAILED_FUND_ID" = "DetailedFundID",
                                                                             "OBJECT_ID" = "ObjectID",
                                                                             "SUBOBJECT_ID" = "SubobjectID"), keep = TRUE) %>%
    distinct() %>%
    #consolidate values across columns from difference sources
    mutate(`Activity ID` = case_when(is.na(`ActivityID`) ~ `ACTIVITY_ID`,
                                     is.na(ACTIVITY_ID) ~ `ActivityID`)) %>%
    left_join(workday_values$fund, by = c("FUND_ID" = "BPFS Fund"), keep = TRUE) %>%
    distinct() %>%
    left_join(workday_values$spend_cat, by = c("OBJECT_ID"= "Object ID", "SUBOBJECT_ID" = "BPFS SubObject ID"), keep = TRUE) %>%
    distinct() %>%
    ##create program-activity key to map to x_cc
    mutate(`Program Activity` = extract_program_activity(col = DIGIT_ACCOUNT_26)) %>%
    ##join on cost center
    left_join(workday_values$cc, by = c("Program Activity" = "Pgm-Activity"), keep = TRUE) %>%
    #remove duplicates created by joins
    distinct() %>%


}
