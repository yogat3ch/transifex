organizations_list <- function(filter_slug = NULL, page = NULL) {
  query <- compact_concat(
    `filter[slug]` = filter_slug,
    `page[cursor]` = page
  )
  req <- req_init() |>
    httr2::req_url_path("organizations") |>
    httr2::req_url_query(!!!query)
  resp <- req_do(req)
  return(resp)
}
