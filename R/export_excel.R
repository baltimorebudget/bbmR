#' Exports formatted Excel file
#'
#' Exports Excel sheets with table formatting, headers, auto-column width, etc. Currently does not auto-size columns with dates correctly.
#'
#' @param df A data frame, will be exported as an Excel table
#' @param tab_name A string, the name to give to the exported sheet
#' @param file_name A string, the name of the file to be created
#' @param type "new" or "existing", whether to export the sheet as a new stand-alone file (default), or as an addition to an existing file
#' @param col_width optional (defaults to 'auto'), either 'auto' for autosized columns or a vector of numbers which must be equal to the number of columns
#' @param tab_color optional (defaults to none), hex color or color from colors()
#' @param table_name optional (defaults to Table3, Table4, etc.), unique table name in workbook; useful for writing Excel formulas that reference sheets on different tabs
#' @param show_tab optional (defaults to TRUE), whether the sheet exported is shown or hidden
#' @param save optional (defaults to TRUE), whether to save the final Excel workbook; not saving the workbook allows for additional, more complex openxlsx changes,
# like conditional formatting, to be added before manually saving the tab
#'
#' @return A new .xlsx file or a new sheet in an existing .xlsx file
#'
#' @seealso \code{\link{export_excel_tabs}}
#'
#' @examples
#' export_excel(iris, "Iris Data","iris.xlsx", "new")
#'
#' # if using option to resize columns
#' export_excel(iris, "Iris Data","iris.xlsx", "new", rep(15, 5))
#'
#' @author Jeremy Pesner, Lillian Nguyen
#'
#' @import magrittr
#' @import openxlsx
#' @export

export_excel <- function(
  df, tab_name, file_name, type = "new", col_width = 'auto',
  tab_color = NULL, table_name = NULL, show_tab = TRUE, save = TRUE) {

  if (missing(df) | missing(tab_name) | missing(file_name)) {
    stop("Missing argument(s)")}
  if (!type %in% c("new", "existing")){
    stop('Please specify if you are exporting a worksheet to a \"new\"
         or \"existing\" Excel file')}
  if (!grepl("\\.xlsx", file_name)){
    stop('Please ensure that the file_name includes the .xlsx extension.')}
  tab_name %<>% as.character
  if (nchar(tab_name) > 31){
    stop("Please shorten the tab name to 31 characters or fewer.")}

  Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe") # corrects Rtools in wrong env error

  options("openxlsx.orientation" = "landscape",
          "openxlsx.datetimeFormat" = "yyyy-mm-dd")
  excel <- switch(type,
                  "new" = openxlsx::createWorkbook(),
                  "existing" = openxlsx::loadWorkbook(file_name)) %T>%
    openxlsx::addWorksheet(
      tab_name, tabColour = tab_color,
      header = c(gsub('\\..*', '', file_name), "&[Tab]", as.character(Sys.Date())),
      footer = c(NA, "&[Page]", NA), visible = show_tab) %T>%
    openxlsx::writeDataTable(
      tab_name, df,
      tableStyle = "TableStyleLight1", tableName = table_name) %T>%
    openxlsx::setColWidths(tab_name, 1:ncol(df), widths = col_width) %T>%
    openxlsx::freezePane(tab_name, 1, firstRow = TRUE) %T>%
    openxlsx::pageSetup(tab_name, printTitleRows = 1) # repeat first row when printing

  if (save == TRUE) {
    openxlsx::saveWorkbook(excel, file_name, overwrite = TRUE)
    base::message(tab_name, ' tab created in the file saved as ', file_name)
  } else {
    base::message(tab_name, ' not saved. Use openxlsx::saveWorkbook().')
    return(excel)
  }
}
