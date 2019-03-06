#' listDatasets
#'
#' @description Prints all marts contained in the SQL object
#' @param database RSQLite object imported previously to R
#' @import dplyr
#' @examples
#' regulon <- importDB("~/Documentsregulondb_sqlite3.db")
#' listDatasets(regulon)
#' @export
listDatasets <- function(database){
  dbListTables(database)
}
