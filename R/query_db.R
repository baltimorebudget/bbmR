#' Query Finance BPFS db
#'
#' Queries from the production database
#'
#' @param schema, a character with "planningyearXX"
#' @param table, table name
#'
#' @return A connection, still needs to be collected
#'
#' @author Lillian Nguyen
#'
#' @import DBI
#' @import dbplyr
#' @import odbc
#' @export


query_db <- function(schema, table) {

  if (missing(schema)) {
    stop("Specify schema (typically in 'planningyearXX' format")
  }

  if (missing(table)) {
    stop("Specify table")
  }

  DBI::dbConnect(odbc::odbc(),
            Driver = "SQL Server",
            Server = Sys.getenv("DB_BPFS_SERVER"),
            Database = "Finance_BPFS",
            UID = Sys.getenv("DB_BPFS_USER"),
            PWD = Sys.getenv("DB_BPFS_PW")) %>%
    tbl(dbplyr::in_schema(schema, table)) %>%
    return()

}
