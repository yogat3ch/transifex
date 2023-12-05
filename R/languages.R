list_languages <- function(filter_code = "es", filter_code_any = NULL) {
  query <- compact_concat(
    `filter[code]` = filter_code,
    `filter[code][any]` = filter_code_any
  ))

  req <- req_init() |>
    httr2::req_url_path("languages") |>
    httr2::req_url_query(!!!query)

  req_do(req)
}
