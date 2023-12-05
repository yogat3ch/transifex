get_resource_strings_collection <- function(resource = filter_string(organization = "virga-labs", project = "dmdu", resource = "local_dev"), filter_gte = NULL, filter_lte = NULL, filter_key = NULL, tag_all = NULL, tag_any = NULL, tag_query = NULL, limit = 150L, page = NULL) {
  query <- compact_concat(
    `filter[resource]` = resource,
    `filter[strings_data_modified][gte]` = filter_gte,
    `filter[strings_data_modified][lte]` = filter_lte,
    `filter[key]` = filter_key,
    `filter[tags][any]` = tag_any,
    `filter[tags][all]` = tag_all,
    `filter[tags][query]` = tag_query,
    limit = limit,
    `page[cursor]` = page
  )

  req <- req_init() |>
    httr2::req_url_path("resource_strings") |>
    httr2::req_url_query(!!!query)

  resp <- req_do(req)

}

get_resource_translations_collection <- function(resource = filter_string(organization = "virga-labs", project = "dmdu", resource = "local_dev"), language = filter_string(language = "es"), filter_gt = NULL, filter_lt = NULL, filter_key = NULL, tag_all = NULL, tag_any = "rob_vul_metric", tag_query = NULL, limit = 150L, page = NULL) {
  query <- compact_concat(
    `filter[resource]` = resource,
    `filter[language]` = language,
    `filter[date_translated][gt]` = filter_gt,
    `filter[date_translated][gt]` = filter_lt,
    `filter[resource_string][key]` = filter_key,
    `filter[resource_string][tags][any]` = tag_any,
    `filter[resource_string][tags][all]` = tag_all,
    `filter[resource_string][tags][query]` = tag_query,
    limit = limit,
    `page[cursor]` = page
  )

  req <- req_init() |>
    httr2::req_url_path("resource_translations") |>
    httr2::req_url_query(!!!query)

  req_do(req)

}
