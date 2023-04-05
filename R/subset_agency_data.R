subset_agency_data <- function(agency_id) {

  data <- list(
    line.item = expend,
    positions = pos,
    all_pos = all_pos,
    analyst = analysts,
    agency = analysts) %>%
    map(filter, `Agency ID` == agency_id) %>%
    map(ungroup)

  data$analyst %<>% extract2("Analyst")
  data$agency %<>% extract2("Agency Name")

  return(data)
}
