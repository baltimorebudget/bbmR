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
      analyst = quasis,
      agency = quasis) %>%
      map(filter, `ID` == str_trunc(quasi_id, width = 8, side = "right", ellipsis = 2)) %>%
      map(ungroup)

    data$analyst %<>% extract2("Analyst") %>% unique()
    data$agency %<>% extract2("Agency Name") %>% unique()

  } else {
    data <- list(
      line.item = expend %>% mutate(`ID` = paste(`Agency ID`, `Program ID`, `Activity ID`)),
      positions = pos %>% mutate(`ID` = paste(`Agency ID`, `Program ID`, `Activity ID`)),
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
    file_path <- paste0("outputs/FY", params$fy, " ", toupper(params$phase), "/", analyst, "/FY", params$fy, " ", toupper(params$phase), " ", agency_name, " Line Items and Positions.xlsx")

    expend <- list[[n]]$line.item
    positions <- list[[n]]$positions

    wb<- createWorkbook()
    addWorksheet(wb, "Line Items")
    addWorksheet(wb, "Positions")
    writeDataTable(wb, 1, x = expend)
    writeDataTable(wb, 2, x = positions)

    saveWorkbook(wb, file_path, overwrite = TRUE)

    message(agency_name, " file exported.")
  }}
