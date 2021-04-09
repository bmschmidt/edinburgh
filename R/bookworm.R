#' @description Get a data.frame from a remote Bookworm server
#'
#' @details This handles the dispatching, and some mild error-correcting,
#' on sending a query to a bookworm server and retrieving the results. There are two ways
#' to run the query: either by specifying the "query" object the most directly, or
#' by filling in the methods one at a time. The latter is easier to understand, but the other
#' is sometimes easier when dispatching calls programatically.
#'
#' @title Run a bookworm query
#' @param host The domain where the Bookworm lives.
#' @param port The port that the API lives on; usually 80.
#' @param database The name of the bookworm to use at the host.
#' @param method The Bookworm API method to be used. Only "return_tsv" works particularly
#' well at the moment. In post 2018 Bookworms, use "data".
#' @param format The format type. tsv, json, feather. In post-2018 bookworms, use "tsv" here.
#' @param protocol "https" or "http". Overridden if included in host.
#' @param search_limits The search constraints, expressed as a list (see below).
#' @param compare_limits The limits for a comparison. Takes the same format as search_limits. If NULL, none is passed and default behavior
#' is applied. (Usually the best choice for non-experts.)
#' @param groups A list of groupings to be applied.
#' @param counttype A list of summary statistics to calculate.
#' @param query The Bookworm query to be searched for, expressed as a list. Most of the above methods
#' (query, groups, counttype, aesthetic, etc.)
#' will update fields in the query, but it can also be called directly.
#' If the query conflicts with any of them (has a different groups element, eg),
#' currently the query object is the default.
#' @param ... Any additional named arguments will be passed to search_limits.
#' @return A data_frame consisting of the results of the call.
#' @author Benjamin Schmidt
#' @examples
#'
#' library(dplyr)
#' totals <- bookworm(
#'   host = "bookworm.htrc.illinois.edu",
#'   groups = "date_year", "counttype" = "TotalTexts",
#'   database = "Bookworm2016", method = "return_tsv"
#' )
#'
#' plot(totals[totals$date_year %in% 1700:2000, ], type = "l", main = "Total works in the Hathi Trust Public Domain corpus")
#'
#' totals <- bookworm(
#'   host = "bookworm.htrc.illinois.edu",
#'   groups = "date_year", "counttype" = c("WordCount", "TextCount"),
#'   database = "Bookworm2016", method="return_tsv"
#' )
#'
#' totals <- totals[totals$date_year %in% 1700:2000, ]
#' plot(totals$date_year, totals$WordCount / totals$TextCount, type = "l", main = "Average length of works in the Hathi Trust Public Domain corpus")
#'
#' results <- bookworm(
#'   host = "bookworm.htrc.illinois.edu", search_limits = list("word" = "evolution"),
#'   groups = "date_year", "counttype" = "WordsPerMillion",
#'   database = "Bookworm2016", method = "return_tsv"
#' )
#'
#' plot(results[results$date_year %in% 1700:2000, ], main = "Usage of 'evolution' in the Hathi Trust Public Domain corpus")
bookworm <- function(
                     host = NULL,
                     port = 80,
                     database = NULL,
                     method = "data",
                     format = "tsv",
                     counttype = c("WordCount"),
                     protocol = "https", # OR: http
                     compare_limits = NULL,
                     groups = NULL,
                     search_limits = RJSONIO::emptyNamedList,
                     query = list(),
                     ...) {
  for (term in c("groups", "search_limits", "compare_limits", "counttype")) {
    if (!is.null(get(term))) {
      query[[term]] <- get(term)
    }
  }
  for (term in c("method", "database", "format")) {
    if (!is.null(term)) {
      if (!is.null(get(term))) {
        query[[term]] <- jsonlite::unbox(get(term))
      }
    }
  }
  if (is.null(host)) stop("You must specify the hostname for the bookworm")
  if (substr(host, 1, 4) == "http") {protocol = ""} else {
    protocol = paste0(protocol, "://")
  }

  for (needed in c("database")) {
    if (is.null(query[[needed]])) stop("You must specify a ", needed, " to run a query")
  }

  additional_lims <- list(...)
  for (limit in names(additional_lims)) {
    additional_lims[[limit]] <- as.list(additional_lims[[limit]])
    query[["search_limits"]][[limit]] <- additional_lims[[limit]]
  }

  json <- gsub("\n", " ", jsonlite::toJSON(query))

  message(json)

  json <- URLencode(json, reserved = TRUE)

  if (port != 80) {
    host = paste(host, port, sep = ":")
  }

  destination <- paste(protocol, host, "/cgi-bin/dbbindings.py?query=", json, sep = "")
  message(destination)
  if (method == "return_tsv") {
    data <- readr::read_delim(destination, quote = "", na = c(""), quoted_na = FALSE, delim = "\t")
  }
  if (format == "tsv") {
    data <- readr::read_delim(destination, quote = "", na = c(""), quoted_na = FALSE, delim = "\t")
  }

  return(data)
}



