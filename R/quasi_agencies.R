
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
  df$`Agency Name`[df$`Program ID` == "385"] <- "Legal Aid"
  df$`Agency Name`[df$`Program ID` == "446"] <- "Family League"
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
    extract2("ID") %>%
    unique()

  results = c()

  for (x in data) {
    if (grepl("446", x) | grepl("385", x) | grepl("824", x) | grepl("820", x)) {
      x = str_trunc(x, width = 8, side = "right", ellipsis = "")
    } else {
      x = x
    }
    results = c(results, x)
  }
  results <- results %>% unique()
  return(results)
}

subset_quasi_data <- function(quasi_id) {

  if (grepl("446", quasi_id) | grepl("385", quasi_id) | grepl("824", quasi_id) | grepl("820", quasi_id)) {

    data <- list(
      line.item = expend %>% mutate(`ID` = paste(`Agency ID`, `Program ID`)),
      positions = pos %>% mutate(`ID` = paste(`Agency ID`, `Program ID`)),
      all_pos = all_pos %>% mutate(`ID` = paste(`Agency ID`, `Program ID`)),
      analyst = quasis %>% mutate(ID = str_trunc(ID, width = 8, side = "right", ellipsis = "")),
      agency = quasis %>% mutate(ID = str_trunc(ID, width = 8, side = "right", ellipsis = ""))) %>%
      map(filter, `ID` == quasi_id) %>%
      map(ungroup)

    data$analyst %<>% extract2("Analyst") %>% unique()
    data$agency %<>% extract2("Agency Name") %>% unique()

  } else {
    data <- list(
      line.item = expend %>% mutate(`ID` = paste(`Agency ID`, `Program ID`, `Activity ID`)),
      positions = pos %>% mutate(`ID` = paste(`Agency ID`, `Program ID`, `Activity ID`)),
      all_pos = all_pos %>% mutate(`ID` = paste(`Agency ID`, `Program ID`, `Activity ID`)),
      analyst = quasis,
      agency = quasis) %>%
      map(filter, `ID` == quasi_id) %>%
      map(ungroup)

    data$analyst %<>% extract2("Analyst") %>% unique()
    data$agency %<>% extract2("Agency Name") %>% unique()
  }

  return(data)
}

make_quasi_files <- function(list) {
  for (n in names(list)) {

    agency_name <- list[[n]]$agency
    analyst <- list[[n]]$analyst
    file_path <- paste0("outputs/FY", params$fy, " ", toupper(params$phase), "/FY", params$fy, " ", toupper(params$phase), " ", Sys.Date(), " ", agency_name, " Line Items and Positions.xlsx")

    expend <- list[[n]]$line.item
    positions <- list[[n]]$positions
    all_positions <- list[[n]]$all_pos

    wb<- createWorkbook()
    addWorksheet(wb, "Line Items")
    addWorksheet(wb, "Positions")
    addWorksheet(wb, "All Positions")
    writeDataTable(wb, 1, x = expend)
    writeDataTable(wb, 2, x = positions)
    writeDataTable(wb, 3, x = all_positions)

    saveWorkbook(wb, file_path, overwrite = TRUE)

    message(agency_name, " file exported.")
  }}
