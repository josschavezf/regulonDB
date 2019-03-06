#' listAttributes
#' @description Prints the column names of the mart
#' @param database RSQLite object imported previously to R
#' @param mart Dataset name, written inside ""
#' @examples
#' regulon <- importDB("~/Documents/regulondb_sqlite3.db")
#' listAttributes(regulon, "TF")
#' @export
listAttributes <- function(database, mart) {
  tryCatch(
    {
      regulonAttributes = DBI::dbListFields(database, mart)
      regulonAttributes
    },
    error=function(cond) {
      message("Error: Database or mart does not exist")
    }
  )
}
