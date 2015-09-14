##' \description{Get a data.frame from a remote Bookworm server}
##'
##' \details{This handles the dispatching, and some mild error-correcting,
##' on sending a query to a bookworm server and retrieving the results. There are two ways
##' to run the query: either by specifying the "query" object the most directly, or
##' by filling in the methods one at a time. The latter is easier to understand, but the other
##' is sometimes easier when dispatching calls programatically.}
##'
##' @title Run a bookworm query
##' @param host The domain where the Bookworm lives.
##' @param port The port that the API lives on; usually 80.
##' @param database The name of the bookworm to use at the host.
##' @param method The Bookworm API method to be used. Only "return_tsv" works particularly
##' well at the moment.
##' @param search_limits The search constraints, expressed as a list (see below).
##' @param compare_limits The limits for a comparison. Takes the same format as search_limits. If NULL, none is passed and default behavior
##' is applied. (Usually the best choice for non-experts.)
##' @param groups A list of groupings to be applied.
##' @param counttype A list of summary statistics to calculate.
##' @param query The Bookworm query to be searched for, expressed as a list. Most of the above methods
##' (query, groups, counttype, aesthetic, etc.)
##' will update fields in the query, but it can also be called directly.
##' If the query conflicts with any of them (has a different groups element, eg),
##' currently the query object is the default.
##'
##' @return A data.frame consisting of the results of the call.
##' @author Benjamin Schmidt
##' @examples
##'
##' totals = bookworm(host="bookworm.htrc.illinois.edu", search_limits=list(),
##' groups="date_year","counttype"=list("TotalTexts"),
##' database="hathipd")
##'
##' plot(totals[totals$date_year %in% 1700:2000,],type='l',main="Total works in the Hathi Trust Public Domain corpus")
##'
##' totals = bookworm(host="bookworm.htrc.illinois.edu", search_limits=list(),
##' groups="date_year","counttype"=list("WordCount","TextCount"),
##' database="hathipd")
##' totals=totals[totals$date_year %in% 1700:2000,]
##' plot(totals$date_year,totals$WordCount/totals$TextCount,type='l',main="Average length of works in the Hathi Trust Public Domain corpus")
##'
##' results = bookworm(host="bookworm.htrc.illinois.edu", search_limits=list("word" = list("evolution")),
##' groups="date_year","counttype"=list("WordsPerMillion"),
##' database="hathipd")
##'
##' plot(results[results$date_year %in% 1700:2000,],main="Usage of 'evolution' in the Hathi Trust Public Domain corpus")


bookworm = function(
  host="localhost",
  port=80,
  database="federalist",
  method="return_tsv",
  counttype=list("WordCount"),
  compare_limits = NULL,
  groups = list(),
  search_limits = list(),
  query=list()
) {
  for (term in c("method","database","groups","search_limits","compare_limits","counttype")) {
    if(is.null(query[[term]])) {
      if (!is.null(get(term))) {
        query[[term]] = get(term)
      }
    }

  }
  if (length(query[['search_limits']]) == 0) {query[['search_limits']]=emptyNamedList}
  if (length(query)==0) {
    query[['database']] = database
  }
  json = toJSON(query)
  json = URLencode(json)
  destination = paste(host,":",port,"/cgi-bin/dbbindings.py?query=",json,sep="")

  if (method!="return_tsv") {
    data = scan(textConnection(getURL(destination)),what='raw',quote=NULL)
    data = paste(data,collapse=" ")
    if (try(assign("data",fromJSON(data[1])),silent=T)==FALSE) {
      stop("Error: unable to parse JSON results.\n",paste(data,collapse="\n"))
      warning(destination)
    }
  }
  if (method=="return_tsv") {
    text = getURL(destination)
    data = read.table(text=text,
      header=T,
      sep="\t",
      stringsAsFactors=FALSE,
      blank.lines.skip=T,
      check.names=F, # Bookworm allows column names to start with numbers.
      encoding="UTF-8",
      flush=T,
      quote='',
      fill=T,
      comment.char='')
    if(ncol(data)==1 & method=="return_tsv") {
      data = data[grep("^[<>]",data[,1],invert=T),]
      return(paste(as.character(data),collapse="\n"))
    }
    data[,ncol(data)] = as.numeric(as.character(data[,ncol(data)]))
  }
  return(data)
}
