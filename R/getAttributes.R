
#' getAttributes
#' @description Allows users to filter and retrieve data from a particular mart.
#' @param database RSQLite object imported previously to R
#' @param mart Dataset name, written inside ""
#' @param filters A character vector with restrictions of the query
#' @param value A character vector with the corresponding restriction values
#' @param cond Conditional when more than one filter is used. Can be "and", "or","not"
#' #' @examples
#' regulon <- importDB("~/Documents/regulondb_sqlite3.db")
#' getAttr(
#' database = regulon,
#' mart = "GENE",
#' filters = c("strand", "celullar_location"),
#' value = c("forward","cytosol" ),
#' cond = "and")
#' @export
getAttributes <- function(database, mart, filters, value, cond ){
  query_cmd <- "SELECT * FROM "
  for (i in 1:length(filters)) {
    if (i == 1) {
      query_cmd <- paste0(query_cmd,mart," WHERE ",filters[i]," = '",value[i],"' ")
    } else {
      query_cmd <- paste0(query_cmd,cond," ",filters[i]," = '",value[i],"' ")
    }
  }
  print(query_cmd)
  res <- DBI::dbSendQuery(database, query_cmd)
  attributes <- DBI::dbFetch(res)
  attributes
}
