get_resource_strings_collection <- function(resource = "o:virga-labs:p:dmdu:r:local_dev", filter_gte = NULL, filter_lte = NULL, filter_key = NULL, tag_all = NULL, tag_any = NULL, tag_query = NULL, limit = 150L, page = NULL) {
  query <- purrr::compact(list(
    `filter[resource]` = prefix_o(resource),
    `filter[strings_data_modified][gte]` = filter_gte,
    `filter[strings_data_modified][lte]` = filter_lte,
    `filter[key]` = filter_key,
    `filter[tags][any]` = tag_any,
    `filter[tags][all]` = tag_all,
    `filter[tags][query]` = tag_query,
    limit = limit,
    `page[cursor]` = page
  ))

  req <- req_init() |>
    httr2::req_url_path("resource_strings") |>
    httr2::req_url_query(!!!query)

  resp <- req_do(req)

}
