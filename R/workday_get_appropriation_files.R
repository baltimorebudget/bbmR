#' Retrieves Workday values from past appropriationfiles
#' Matches Workday fund = fund, spend category = OSO, cost center = program + activity, grant = detailed fund, ledger = payroll natural
#' @return list of dataframes
#'
#' @author Sara Brumfield

get_appropriation_files <- function() {


  fy21_appropriation <- import("G:/Fiscal Years/Fiscal 2021/Projections Year/1. July 1 Prepwork/Fiscal 2021 Appropriation File.xlsx", which = "FY21 Appropriation File")
  fy22_appropriation <- import("G:/Fiscal Years/Fiscal 2022/Projections Year/1. July 1 Prepwork/Appropriation File/Fiscal 2022 Adopted Appropriation File With Positions and Carry Forwards.xlsx", which = "FY22 Adopted Appropriation File")
  fy23_appropriation <- import("G:/Fiscal Years/Fiscal 2023/Projections Year/1. July 1 Prepwork/Appropriation File/Fiscal 2023 Appropriation File_Deduped PINs.xlsx", which = "FY23 Appropriation File")

  data <-fy23_appropriation %>% full_join(fy22_appropriation, by = c("26-Digit Account Number", "15-Digit Account Number")) %>%
    full_join(fy21_appropriation, by = c("26-Digit Account Number" = "26 Digit Account Number", "15-Digit Account Number" = "15 Digit Account Number")) %>%
    mutate(`AgencyID` = case_when(TRUE ~ `Agency ID`,
                                  is.na(`Agency ID`) ~ `Agency ID.x`,
                                   is.na(`Agency ID.x`) ~ `Agency ID.y`),
           `ProgramID` = case_when(TRUE ~ `Program ID`,
                                   is.na(`Program ID`) ~ `Program ID.x`,
                                  is.na(`Program ID.x`) ~ `Program ID.y`),
           `ActivityID` = case_when(TRUE ~ `Activity ID`,
                                    is.na(`Activity ID`) ~ `Activity ID.x`,
                                   is.na(`Activity ID.x`) ~ `Activity ID.y`),
           `SubactivityID` = case_when(TRUE ~ `Subactivity ID`,
                                       is.na(`Subactivity ID`) ~ `Subactivity ID.x`,
                                       is.na(`Subactivity ID.x`) ~ `Subactivity ID.y`),
           `FundID` = case_when(TRUE ~ `Fund ID`,
                                is.na(`Fund ID`) ~ `Fund ID.x`,
                                is.na(`Fund ID.x`) ~ `Fund ID.y`),
           `DetailedFundID` = case_when(TRUE ~ `DetailedFund ID`,
                                        is.na(`DetailedFund ID`) ~ `DetailedFund ID.x`,
                                        is.na(`DetailedFund ID.x`) ~ `DetailedFund ID.y`),
           `ObjectID` = case_when(TRUE ~ `Object ID`,
                                  is.na(`Object ID`) ~ `Object ID.x`,
                                  is.na(`Object ID.x`) ~ `Object ID.y`),
           `SubobjectID` = case_when(TRUE ~ `Subobject ID`,
                                     is.na(`Subobject ID`) ~ `Subobject ID.x`,
                                    is.na(`Subobject ID.x`) ~ `Subobject ID.y`),
           ##do the same for Workday columns with missing values
           `WorkdayFundID` = case_when(TRUE ~ `Workday Fund ID.x`,
                                  is.na(`Workday Fund ID.x`) ~ `Workday Fund ID.y`),
           `WorkdayCCID` = case_when(TRUE ~ `Workday Cost Center ID`,
                                       is.na(`Workday Cost Center ID`) ~ `Workday Cost Center ID (Phase II)`),
           `WorkdayCCName` = case_when(TRUE ~ `Workday Cost Center Name.x`,
                                       is.na(`Workday Cost Center Name.x`) ~ `Workday Cost Center Name.y`),
           `WorkdaySCID` = case_when(TRUE ~ `Workday Spend Category ID.x`,
                                     is.na(`Workday Spend Category ID.x`) ~ `Workday Spend Category ID.y`),
           `WorkdaySCName` = case_when(TRUE ~ `Workday Spend Category Name.x`,
                                       is.na(`Workday Spend Category Name.x`) ~ `Workday Spend Category Name.y`),
           `WorkdayGrantID` = case_when(TRUE ~ `Workday Grant ID - New`,
                                     is.na(`Workday Grant ID - New`) ~ `Workday Grant or Special Purpose ID (Phase II)`),
           `WorkdaySPID` = case_when(TRUE ~ `Workday Special Purpose ID`,
                                        is.na(`Workday Special Purpose ID`) ~ `Workday Grant or Special Purpose ID (Phase II)`),
           `WorkdayGrantSPName` = case_when(TRUE ~ `Workday Grant or Special Purpose Name.x`,
                                       is.na(`Workday Grant or Special Purpose Name.x`) ~ `Workday Grant or Special Purpose Name.y`)) %>%
    select(`26-Digit Account Number`, `15-Digit Account Number`, AgencyID:WorkdayGrantSPName)

  return(data)
}
