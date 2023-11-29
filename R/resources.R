resources_list <- function(filter_slug = NULL, filter_name = NULL, filter_project = "o:virga-labs:p:dmdu", page = NULL) {
  query <- purrr::compact(list(
    `filter[slug]` = filter_slug,
    `filter[name]` = filter_name,
    `filter[project]` = prefix_o(filter_project),
    `page[cursor]` = page
  ))
  req <- req_init() |>
    httr2::req_url_path("resources") |>
    httr2::req_url_query(!!!query)
  resp <- req_do(req)
  return(resp)
}
