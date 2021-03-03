#' Connect to Formstack API
#'
#' Retrieves authorization info from .Renviron file
#'
#' @author Lillian Nguyen
#'
#' @import httr
#' @export


connect_fs_api <- function() {
  list(
    key = Sys.getenv("FS_API_KEY"),
    endpoint = httr::oauth_endpoint(
      authorize = Sys.getenv("FS_OAUTH_AUTHORIZE"),
      access = "https://www.formstack.com/api/v2/oauth2/token"),
    app = httr::oauth_app(
      "R",
      key = Sys.getenv("FS_OAUTH_KEY"),
      secret = Sys.getenv("FS_OAUTH_SECRET"),
      redirect_uri = Sys.getenv("FS_OAUTH_REDIR"))
  )
}


#' Get Formstack submissions
#'
#' Use this for finer-grained control of Formstack submission retrieval. This
#' function is the backbone of extract_fs_submissions().
#'
#' @param api, A list containing the API connection to Formstack,
#' generated from  connect_fs_api()
#' @param form_id numeric, Formstack ID for form
#' @param query list, conforming to Formstack API
#' @param per_page Numeric, defaults to the max of 100. The number of
#' submissions to retrieve per page.
#'
#' @return A list with payload and submission data
#'
#' @author Lillian Nguyen
#'
#' @import httr
#' @importFrom assertthat assert_that are_equal
#' @export

get_fs_submissions <- function(api, form_id, query) {
  # query
  httr::GET(paste0("https://www.formstack.com/api/v2/form/", form_id, "/submission.json"),
      add_headers(Authorization = paste("Bearer", api$key, sep = " ")),
      query = c(list(id = form_id,
                     data = 1,
                     per_page = 100), query),
      encode = "json") %>%
    httr::content()
}

#' Extract Formstack submissions
#'
#' Pulls and extracts all data from a Formstack form, regardless of length. Only
#' keeps submission data; removes all payload data.
#'
#' @param api, A list containing the API connection to Formstack,
#' generated from  connect_fs_api()
#' @param form_id numeric, Formstack ID for form
#' @param approved_only logical, TRUE to filter for only approved submissions
#'
#' @return A df with submission data
#'
#' @author Lillian Nguyen
#'
#' @import httr
#' @import jsonlite
#' @export

extract_fs_submissions <- function(api, form_id, approved_only = FALSE) {

  payload <-
    get_fs_submissions(
      api = api,
      form_id = form_id,
      query = list(per_page = 100))

  # Then pull data from each of those pages into a master df
  x <- 1:payload$pages

  data <- map(x, function(x) {
    raw <- get_fs_submissions(
      api = api,
      form_id = form_id,
      query = list(page = x,
                   per_page = 100,
                   data = 1)) %>%
      extract2("submissions")

    time <- raw %>%
      map(extract2, "timestamp") %>%
      unlist()

    list(
      submissions = raw %>% # get only submission data; remove all payload data
        map(extract2, "data") %>%
        set_names(time),
      approvals = raw %>%
        map(extract2, "approval_status"))

  })


  submissions <- sapply(data, "[[", "submissions") %>%
    # submissions are organized in lists by page;
    # remove this list since we don't care what page each submission was on
    unlist(recursive = FALSE)

  approvals <- sapply(data, "[[", "approvals") %>%
    unlist()

  df <- map(1:length(submissions), function(x) {

    submissions[[x]] %>%
      toJSON() %>%
      fromJSON() %>%
      map(`[`, c("label", "value")) %>%
      bind_rows() %>%
      spread(label, value) %>%
      mutate(Timestamp = ymd_hms(names(submissions[x])))
  })

  df <- df %>%
    bind_rows() %>%
    mutate(`Approval Status` = approvals)

  assert_that(are_equal(payload$total, nrow(df)))

  if (approved_only) {
    df <- df %>%
      filter(`Approval Status` == "Approved")
  }

  return(df)
}
