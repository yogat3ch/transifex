resource_collection_to_shiny.i18n_json <- function(resource_strings_collection, resource_translations_collection) {
  purrr::map2(resource_strings_collection$data, resource_translations_collection$data, \(.x, .y) {
    rlang::list2(
      !!resource_pluck$language(.x) := resource_pluck$string(.x),
      !!resource_pluck$language(.y) := resource_pluck$string(.y)
      )
  })
}
resource_strings <- list(
  language_location = c("relationships", "language", "data", "id"),
  string_location = c("attributes", "strings", "other")
)
resource_pluck <- list(
  language = function(.x) {
    prefix$language(purrr::pluck(.x, !!!resource_strings$language_location), remove = TRUE)
  },
  string = function(.x) {
    purrr::pluck(.x, !!!resource_strings$string_location)
  }
)


#' Construct a shiny.i18n compatible JSON file
#'
#' @param tag_any \code{chr} Tags, combined via ANY
#' @param tag_all \code{chr} Tags, combined via ALL
#' @param tag_query \code{chr} See \href{Get Resource Strings Collection}{https://developers.transifex.com/reference/get_resource-strings} parameters for documentation
#' @param file \code{chr} path of file to write json
#'
#' @return \code{list} JSON object as list
#' @export
#'
#' @examples
#' \dontrun {
#' (json <- shiny.i18n_json(tag_any = "rob_vul_metric"))
#'  shiny.i18n_json(tag_any = "rob_vul_metric", file = "test.json")
#' }
shiny.i18n_json <- function(resource = filter_string(organization = "virga-labs", project = "dmdu", resource = "local_dev"), tag_any = NULL, tag_all = NULL, tag_query = NULL, file = NULL) {
  json <- list(
    cultural_date_format = "%Y-%m-%d",
    languages = c("en", "es")
  )
  resource_strings <- get_resource_strings_collection(resource = resource, tag_any = tag_any, tag_all = tag_all, tag_query = tag_query, limit = 1000L)
  resource_translations <- get_resource_translations_collection(resource = resource, tag_any = tag_any, tag_all = tag_all, tag_query = tag_query, limit = 1000L)
  json$translation <- resource_collection_to_shiny.i18n_json(resource_strings, resource_translations)
  if (!is.null(file))
    jsonlite::write_json(json, auto_unbox = TRUE, pretty = TRUE, path = file)
  json
}
