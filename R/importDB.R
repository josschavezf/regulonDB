#' importDB
#' @description Import an SQLite database from a local path
#' @import RSQLite
#' @import DBI
#' @param file local path of the SQL file, written inside ""
#' @examples regulon <- importDB("~/Documentsregulondb_sqlite3.db")
#' @export
importDB <- function(file) {
  myDataset <- DBI::dbConnect(RSQLite::SQLite(), file)
  myDataset
}
