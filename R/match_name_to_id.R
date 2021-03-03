#' Match Name to ID
#'
#' Query BPFS database for latest Detailed Fund, Service,
#' Activity, Fund, Agency, Object, and/or Subobject names.
#' Useful for pulling the most recent names when working with historical data.
#'
#' @param df dataframe to match
#' @param cols a vector of strings of the column names to be matched
#' @param incl.higher logical, should the returned dataframe include the level of detail above certain columns (ex: should Fund ID be included with a Detailed Fund match?)
#' @param fy latest fiscal year, for database schema
#'
#' @return a dataframe with Name columns added
#'
#' @author Lillian Nguyen
#'
#' @import tidyverse
#' @export

match_name_to_id <- function(df, cols, incl.higher, fy = 22) {

  if (!hasArg(cols)) {
    cols <- c("Detailed Fund", "Service", "Activity",
              "Fund", "Agency", "Object", "Subobject")
  }

  l <- list()

  for (i in c("Fund", "Agency", "Object", "Subobject")) {
    if (i %in% cols) {
      l[[i]] <- query_db(paste0("planningyear", fy), i) %>%
        select(!!paste(i, "ID") := ID, !!paste(i, "Name") := NAME)
    }
  }

  for (i in c("Detailed Fund", "Service", "Activity", "Subactivity")) {
    if (i %in% cols) {
      higher <- switch(i, "Detailed Fund" = "Fund", "Service" = "Agency",
                       "Activity" = "Program", "Subactivity" = "Activity")

      l[[i]] <- query_db(
        paste0("planningyear", fy),
        switch(i, "Detailed Fund" = "detailed_fund", "Service" = "program",
               "Activity" = "activity", "Subactivity" = "subactivity"))

      if (incl.higher == TRUE) {
        if (i == "Subactivity") {
          l[[i]] <- l[[i]] %>%
            select(`Subactivity ID` = ID, `Subactivity Name` = NAME,
                   `Activity ID` = `ACTIVITY_ID`, `Service ID` = `PROGRAM_ID`)
        } else {
          l[[i]] <- l[[i]] %>%
            select(!!paste(i, "ID") := ID, !!paste(i, "Name") := NAME,
                   !!paste(ifelse(higher == "Program", "Service", higher), "ID") :=
                     paste0(toupper(higher), "_ID"))
        }

      } else {
        l[[i]] <- l[[i]] %>%
          select(!!paste(i, "ID") := ID, !!paste(i, "Name") := NAME)
      }
    }
  }

  l <- map(l, collect)

  if ("Activity" %in% cols) {
    l$Activity %<>%  mutate(`Activity ID` = str_pad(`Activity ID`, 3, "left", "0"))
  }

  for (j in cols) {
    df %<>%
      left_join(l[[j]])
  }

  # reorder in order of cols as listed in argument
  df %<>%
    select(one_of(paste(rep(cols, each = 2), c("ID", "Name"))),
           everything())

  return(df)

}
