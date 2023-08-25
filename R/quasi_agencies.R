
##assign quasi agencies to main agency for memo generation

assign_quasi_agency <- function(df) {

  df$`Agency Name`[df$`Program ID` == "493" & df$`Activity ID` == 1] <- "Baltimore Symphony Orchestra"
  df$`Program ID`[df$`Program ID` == "493" & df$`Activity ID` == 1] <- "493c"
  df$`Agency Name`[df$`Program ID` == "493" & df$`Activity ID` %in% c(10,11)] <- "Walters Art Museum"
  df$`Program ID`[df$`Program ID` == "493" & df$`Activity ID` %in% c(10,11)] <- "493b"
  df$`Agency Name`[df$`Program ID` == "493" & df$`Activity ID` %in% c(14,15)] <- "Baltimore Museum of Art"
  df$`Program ID`[df$`Program ID` == "493" & df$`Activity ID` %in% c(14,15)] <- "493a"
  df$`Agency Name`[df$`Program ID` == "493" & df$`Activity ID` == 42] <- "Maryland Zoo"
  df$`Program ID`[df$`Program ID` == "493" & df$`Activity ID` == 42] <- "493d"
  df$`Agency Name`[df$`Program ID` == "824"] <- "Baltimore Office of Promotion and the Arts"
  df$`Agency Name`[df$`Program ID` == "820"] <- "Visit Baltimore"
  df$`Agency Name`[df$`Program ID` == "385" & df$`Activity ID` %in% c(6,8)] <- "Legal Aid"
  df$`Agency Name`[df$`Program ID` == "385" & df$`Activity ID` == 12] <- "Family League"
  df$`Agency Name`[df$`Program ID` == "446" & df$`Activity ID` %in% c(4,20)] <- "BCCC"
  df$`Agency Name`[df$`Program ID` == "446" & df$`Activity ID` %in% c(13,14,17)] <- "Family League"
  df$`Agency Name`[df$`Program ID` == "590" & df$`Activity ID` == 32] <- "Baltimore Heritage Area"
  df$`Program ID`[df$`Program ID` == "590" & df$`Activity ID` == 32] <- "590c"
  df$`Agency Name`[df$`Program ID` == "590" & df$`Activity ID` == 38] <- "Lexington Market"
  df$`Program ID`[df$`Program ID` == "590" & df$`Activity ID` == 38] <- "590b"
  df$`Agency Name`[df$`Program ID` == "590" & df$`Activity ID` == 44] <- "Baltimore Public Markets"
  df$`Program ID`[df$`Program ID` == "590" & df$`Activity ID` == 44] <- "590a"

  return(df)
}

make_quasi_ids <- function(df) {

  data <- df %>%
    group_by(`Agency Name`) %>%
    mutate(IDs = paste(`Cost Center`)) %>% select(IDs)

  return(data)
}

make_quasi_names <- function(df) {

  data <- df %>%
    extract2("Agency Name") %>%
    unique()

  return(data)

}

subset_quasi_data <- function(agency_name) {

  agency = agency_name
  id = quasi_ids %>% filter(`Agency Name` == agency) %>% extract2("IDs")

  data <- list(
    line.item = expend %>%
                left_join(cc_xwalk, by = c("Program ID" = "Program_ID", "Activity ID" = "Activity_ID")) %>%
                rename(`Cost Center ID` = `Cost_Center_ID`, `Cost Center Name` = `Cost_Center_Name`),
    positions = pos,
    # all_pos = all_pos %>% mutate(`ID` = paste(`Program ID`, `Activity ID`)),
    analyst = quasis) %>%
    map(filter, `Cost Center ID` %in% id) %>%
    map(ungroup)

  data$analyst %<>% extract2("Analyst") %>% unique()
  data$agency <- agency

  return(data)
}

make_quasi_files <- function(list) {
  if(file.exists(paste0(getwd(), "/outputs/FY", params$fy, " ", toupper(params$phase), "/")) == FALSE) {
    print("Creating folder.")

    dir.create(paste0(getwd(), "/outputs/FY", params$fy, " ", toupper(params$phase), "/"), showWarnings = FALSE)

  } else {print("Folder already created.")}

  for (n in names(list)) {

    agency_name <- list[[n]]$agency
    analyst <- list[[n]]$analyst
    file_path <- paste0("outputs/FY", params$fy, " ", toupper(params$phase), "/FY", params$fy, " ", toupper(params$phase), " ", Sys.Date(), " ", agency_name, " Line Items and Positions.xlsx")

    expend <- list[[n]]$line.item
    positions <- list[[n]]$positions
    # all_positions <- list[[n]]$all_pos

    wb<- createWorkbook()
    addWorksheet(wb, "Line Items")
    addWorksheet(wb, "Positions")
    # addWorksheet(wb, "All Positions")
    writeDataTable(wb, 1, x = expend)
    writeDataTable(wb, 2, x = positions)
    # writeDataTable(wb, 3, x = all_positions)

    saveWorkbook(wb, file_path, overwrite = TRUE)

    message(agency_name, " file exported.")
  }}
