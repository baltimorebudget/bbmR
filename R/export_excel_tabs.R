#' Splits dataframe into Excel sheets by column
#'
#' Takes one dataframe and exports it as multiple Excel sheets in the same file, with table formatting, headers, auto-column width, etc., based on the categories in a column. Currently does not auto-size columns with dates correctly. This function can be used first, and then ExportExcel(), with the "existing" argument,  used second in order to add a separate sheet.
#'
#' @param df A data frame, will be exported as an Excel table
#' @param tab_name a string, the name of the column which should be used to separate the df into tabs
#' @param file_name a string, the name of the file to be created
#' @param drop_col optional (defaults to TRUE) logical, if FALSE then the column specified in the tab_name argument remains is not dropped
#' @param col_width optional (defaults to 'auto'), either 'auto' for autosized columns or a vector of numbers which must be equal to the number of columns
#'
#' @return A new .xlsx file with records from a single dataframe placed on multiple tabs
#'
#' @seealso \code{\link{export_excel}}
#'
#' @examples
#' export_excel_tabs(iris, "Species", "iris.xlsx")
#'
#' @author Jeremy Pesner, Lillian Nguyen
#'
#' @import magrittr
#' @import openxlsx
#' @export

export_excel_tabs <- function(df, tab_name, file_name, drop_col = TRUE, col_width = 'auto') {

  # Exception handling ####
  if (missing(df) | missing(tab_name) | missing(file_name)) {
    stop("Missing argument(s)")}
  if (!grepl("\\.xlsx", file_name)) {
    stop('Please ensure that the file_name includes the .xlsx extension.')}
  if (!tab_name %in% colnames(df)) {
    stop('Please specify a column name that is in the dataframe.')}

  names <- df %>% extract2(tab_name) %>% unique %>% as.character %>% sort()

  if ((nchar(names) > 31) %>% any()) {
    stop("Please shorten all values in your 'tab name' column to <31 characters.")}

  # FUNCTION #####################################################################

  Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")  # corrects Rtools in wrong env error

  options("openxlsx.orientation" = "landscape",
          "openxlsx.datetimeFormat" = "mm/dd/yyyy")

  excel <- openxlsx::createWorkbook()
  title <- gsub('\\..*', '', file_name)
  today <- as.character(Sys.Date())

  # Filter the dataframe by each value in the tab_name col and export to Excel
  for (i in names) {
    tab.data <- df[df[[tab_name]] == i, ]

    if (drop_col == TRUE) {
      tab.data[[tab_name]] <- NULL
    }

    excel %T>%
      openxlsx::addWorksheet(i, header = c(title, "&[Tab]", today),
                             footer = c(NA, "&[Page] of &[Pages]", NA)) %T>%
      openxlsx::writeDataTable(i, tab.data, tableStyle = "TableStyleLight1") %T>%
      openxlsx::setColWidths(i, 1:ncol(tab.data), widths = col_width) %T>%
      openxlsx::pageSetup(i, printTitleRows = 1) # repeat first row when printing
  }

  openxlsx::saveWorkbook(excel, file_name, overwrite = TRUE)
  base::message('Data separated into ', length(names), ' tabs by ', tab_name, ' and saved as ', file_name)

}
