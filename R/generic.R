token_get <- function () {
  Sys.getenv("TRANSIFEX_API_SECRET")
}

base_url <- "https://rest.api.transifex.com"

req_init <- function(verbose = use_debug()) {
  req <- httr2::request(base_url) |>
    httr2::req_retry(max_tries = 3, max_seconds = 30, backoff = \(x) if (x == 2) 1 else 0) |>
    httr2::req_throttle(500 / 60) |>
    httr2::req_headers(`accept` = "application/vnd.api+json") |>
    req_authorize() |>
    httr2::req_error(is_error = \(.x) FALSE)
  if (verbose)
    req <- httr2::req_verbose(req)
  return(req)
}

req_do <- function(req) {
  req <- httr2::req_url(req, utils::URLdecode(req$url))
  resp <- httr2::req_perform(req)
  out <- if (httr2::resp_is_error(resp)) {
    body <- httr2::resp_body_json(resp)
    rlang::error_cnd(
      class = glue::glue("httr2_error_{resp$status}"),
      !!!body,
      resp_raw = httr2::resp_raw(resp),
      req_raw = req,
      use_cli_format = TRUE
    )
  } else
    httr2::resp_body_json(resp)

  return(out)
}

prefix_o <- function(x) {
  if (grepl("^o\\:", x))
    x
  else
    paste0("o:", x)
}

#' Authorize an API Request
#' @description
#' TRANSIFEX_API_SECRET must be set up as an Environment variable for this function to function properly
#'
#' @inheritParams auth_check
#'
#' @return \code{\link[httr2]{request}}
#' @export
#'

req_authorize <- function(req) {
  httr2::req_auth_bearer_token(req, token_get())
}

use_debug <- function() {
  getOption("use_debug", FALSE)
}

#' Verify the connection with the Transifex API
#' @description
#' TRANSIFEX_API_SECRET must be set up as an Environment variable for this function to return `TRUE`
#' @param req \code{\link[httr2]{request}}
#' @param verbose \code{lgl} whether to include verbose output in console. See \code{\link[httr2]{req_verbose}}
#'
#' @return \code{lgl}
#' @export
#'
#' @examples
#' \dontrun{
#' # Assumes TRANSIFEX_API_SECRET env var is set-up with API Key
#'   if (auth_check())
#'     print("Transifex API Key is connected")
#' }
auth_check <- function(req) {
  req <- req_init() |>
    httr2::req_url_path("organizations")
  resp <- httr2::req_perform(req)
  return(httr2::resp_status(resp) == 200L)
}

