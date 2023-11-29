projects_list <- function(filter_organization = "virga-labs", filter_slug = NULL, filter_name = NULL, page = NULL) {
  query <- purrr::compact(list(
    `filter[slug]` = filter_slug,
    `filter[name]` = filter_name,
    `filter[organization]` = prefix_o(filter_organization),
    `page[cursor]` = page
  ))
  req <- req_init() |>
    httr2::req_url_path("projects") |>
    httr2::req_url_query(!!!query)
  resp <- req_do(req)
  return(resp)

}
