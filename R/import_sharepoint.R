
get_sharepoint_files <- function(path = "C:/Users/sara.brumfield2/OneDrive - City Of Baltimore/FY2024 Planning/03-TLS-BBMR Review/Agency Analysis Tools/",
                                 pattern = paste0("^FY24 Change Table*")) {
  files <- list.files(path = path,
                      pattern = pattern,
                      full.names = TRUE, recursive = TRUE)
  return(files)
}

extract_tech_table <- function(df) {
  table <- df[which(df$`Tollgate Recommendations`=="Technical Adjustments"):which(df$`Tollgate Recommendations`=="Total")[1],]
  return(table)
}

extract_savings_table <- function(df) {
  table <- df[which(df$`Tollgate Recommendations`=="Savings Ideas"):which(df$`Tollgate Recommendations`=="Total")[2],]
  return(table)
}

import_tech_tables <- function(files, path = "((?<=C:/Users/sara.brumfield2/OneDrive - City Of Baltimore/FY2024 Planning/03-TLS-BBMR Review/Agency Analysis Tools/FY24 Change Table ).+(?=.xlsx))") {
  for (file in files) {
    agency = str_extract(file, path)
    sheets = na.omit(str_extract(excel_sheets(file), "\\d{3}"))
    df = data.frame()
    for (s in sheets) {
      z = read_excel(file, s) %>% 
        select(`Service Summary`, `...2`, `Tollgate Recommendations`:`...18`) %>%
        extract_tech_table() %>%
        mutate(ID = s,
               Agency = agency)
      # x = read_excel(file, s) %>%
      #   mutate(ID = s,
      #          Agency = agency)
      print(paste0(s, " added from ", file))
      df = rbind(df, z)
    }
    return(df)
  } 
}

import_savings_tables <- function(files, path = "((?<=C:/Users/sara.brumfield2/OneDrive - City Of Baltimore/FY2024 Planning/03-TLS-BBMR Review/Agency Analysis Tools/FY24 Change Table ).+(?=.xlsx))") {
  for (file in files) {
    agency = str_extract(file, path)
    sheets = na.omit(str_extract(excel_sheets(file), "\\d{3}"))
    df = data.frame()
    for (s in sheets) {
      z = read_excel(file, s) %>% 
        select(`Service Summary`, `...2`, `Tollgate Recommendations`:`...18`) %>%
        extract_savings_table() %>%
        mutate(ID = s,
               Agency = agency)
      # x = read_excel(file, s) %>%
      #   mutate(ID = s,
      #          Agency = agency)
      print(paste0(s, " added from ", file))
      df = rbind(df, z)
    }
    return(df)
  } 
}

import_change_tables <- function(files, path = "((?<=C:/Users/sara.brumfield2/OneDrive - City Of Baltimore/FY2024 Planning/03-TLS-BBMR Review/Agency Analysis Tools/FY24 Change Table ).+(?=.xlsx))") {
  for (file in files) {
    agency = str_extract(file, path)
    sheets = na.omit(str_extract(excel_sheets(file), "\\d{3}"))
    df = data.frame()
    for (s in sheets) {
      x = read_excel(file, s) %>%
        mutate(ID = s,
               Agency = agency)
      print(paste0(s, " added from ", file))
      df = rbind(df, x)
    }
    return(df)
  } 
}

consolidate_tables <- function(df = x, tab_name = a, type, file_name = file_path, save= TRUE) {
  
  num_style <- createStyle(numFmt = "#,##0;(#,##0)")
  
  header_style <- createStyle(fgFill = "white", border = "TopBottomLeftRight",
                              borderColour = "black", textDecoration = "bold", fontColour = "darkblue",
                              wrapText = TRUE)
  
  style <- createStyle(fgFill = "darkblue", border = "TopBottomLeftRight",
                       borderColour = "black", textDecoration = "bold", fontColour = "white",
                       wrapText = TRUE)
  
  style2 <- createStyle(textDecoration = c("bold", "italic"),
                        wrapText = TRUE)
  
  style_rows <- which(x$`Change Table (GF Only)`=="Adjustments") + 1
  
  style_rows2 <- which(x$`Change Table (GF Only)` %in% c("FY2023 Adopted", "CLS Adjustments", "Request Adjustments", 
                                                         "TLS Adjustments", "FinRec Adjustments", "BoE Adjustments",
                                                         "Council Adjustments", "FY2024 Budget")) + 1
  
  num_rows <- which(x$`Amount`!="Amount") + 1
  
  excel <- switch(type,
                  "new" = openxlsx::createWorkbook(),
                  "existing" = openxlsx::loadWorkbook(file_name)) %T>%
    openxlsx::addWorksheet(tab_name) %T>%
    openxlsx::writeDataTable(tab_name, x = df) %T>%
    addStyle(tab_name, style = style, rows = style_rows, cols = 1:4, gridExpand = TRUE, stack = FALSE) %T>%
    addStyle(tab_name, style = header_style, rows = 1, cols = 1:4, gridExpand = TRUE, stack = FALSE) %T>%
    addStyle(tab_name, style = style2, rows = style_rows2, cols = 1:4, gridExpand = TRUE, stack = FALSE) %T>%
    addStyle(tab_name, style = num_style, rows = num_rows, cols = 3, gridExpand = TRUE, stack = FALSE) %T>%
    setColWidths(tab_name, cols = 1, widths = 10) %T>%
    setColWidths(tab_name, cols = 2, widths = 45) %T>%
    setColWidths(tab_name, cols = 3:4, widths = 17)
  
  saveWorkbook(excel, file_name, overwrite = TRUE)
  
  message(a, " change tables exported.")
  # 
  # if (save == TRUE) {
  #   openxlsx::saveWorkbook(excel, file_name, overwrite = TRUE)
  #   base::message(tab_name, ' tab created in the file saved as ', file_name)
  # } else {
  #   base::message(tab_name, ' not saved. Use openxlsx::saveWorkbook().')
  #   return(excel)
  # }
}