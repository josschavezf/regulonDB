#' getRegulatedGenesByTF
#' @description Shows all the genes regulated by a Transcription Factor indicated by the user
#' @param database RSQLite object imported previously from an SQLite database file to R
#' @param mart Dataset table into the database, use the syntax "dataset"
#' @examples ## import the SQLite database
#' ## path_sqlite3 <- file.choose()
#' regulon <- DBI::dbConnect(RSQLite::SQLite(), path_sqlite3)
#' listDatasets(regulon)
#' listAttributes(regulon, "TF")
#' getAttr(
#' database = regulon,
#' mart = "GENE",
#' filters = c("operon_id", "operon_name"),
#' value = c("ECK125235983","cmoM-mukFEB" ),
#' cond = "and")
#' GetRegulatedGenesByTF(regulon, "Ara")
#'  dbDisconnect(regulon)
#' @export
getRegulatedGenesByTF <- function(database,tf){
  genes_regulon <- tbl(database, "GENE")
  tf_table <- genes_regulon %>%
    select(name, gene_tf)
  ## TO-DO Check if the LIKE operator is the right syntax for extracting concrete TF info (should "=" be used instead)
  result <- tf_table %>% filter( dplyr::sql( paste0("gene_tf LIKE '%",tf,"%' ") )) %>% select("name")
  tibble_result <- collect(select(result, name))[,1]
  as.matrix(tibble_result)

}
