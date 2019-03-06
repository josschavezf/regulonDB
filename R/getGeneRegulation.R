#' getGeneRegulation
#' @description This function gives the regulators of input genes and whether their effect is possitive or negative
#' @param database RSQLite object imported previously to R
#' @param genes genes of interest
#' @param format onerow, multirow, table
#' @import tidyr
#' @examples
#' regulon <- importDB("~/Documents/regulondb_sqlite3.db")
#' getRegulatedGenesByTF(regulon, "Ara")
#'  dbDisconnect(regulon)
#' @export
getGeneRegulation <- function( database, genes, format ) {

  ## Create base stringfor the query request
  query_cmd <- "SELECT * FROM GENE WHERE"

  ## loop to dynamically construct the final query request
  for (i in 1:length(genes)) {
    if (i == 1) {
      query_cmd <- paste0(query_cmd," name = '",genes[i],"'")
    } else {
      query_cmd <- paste0(query_cmd," or"," name = '",genes[i],"'")
    }
  }

  ## Create the query instruction
  query_cmd <- dbSendQuery(database, query_cmd)
  ## request the query, keep only columns name and gene_tf, and store it in a temporary df
  tempdf <- dbFetch(query_cmd)[,c("name","gene_tf")]
  ## change header names
  colnames(tempdf) <- c("genes","regulators")

  ## create a third temporal column to store useful regulator ID data (this wont be showed in the user output)
  tempdf$regulator_id <- ""

  ## create another empty df to populate with one line per regulator, for concantenating transient results
  tempdf3 <- tempdf[0,]

  ## Vectorize each cell in the regulators column
  ## operate in a for loop
  for (j in 1:nrow(tempdf)) {
    ## replace tabs by spaces
    tempvec0 <- gsub("\t", " ", tempdf[j,"regulators"])
    ## split every term in from the original regulator values
    tempvec0 <- unlist(strsplit(tempvec0, split = c(" ")))

    ## Eliminate the ECKnnnnnnn values, to keep only regulator Names
    tempvec1 <- tempvec0[!grepl("ECK", x = tempvec0)]

    ## find the ECKnnnnnnn values, to keep only regulator ECK id's
    tempvec2 <- tempvec0[grepl("ECK", x = tempvec0)]

    ## create an empty df to transiently populate with one line per regulator
    tempdf2 <- tempdf[0,]

    ## For every element in the regulators vector, print one line of the original tempdf in the second tempdf
    ## n keeps count of the vector of regulators, and i keeps track of the regulated gene beign evaluated
    for (n in 1:length(tempvec1)) {
      ## populate gene names
      tempdf2[n,"genes"] <- tempdf[j,"genes"]
      ## populate one by one, the regulator factors detected for the gene
      tempdf2[n,"regulators"] <- tempvec1[n]
      ## populate one by one, the ID for regulator factors detected for the gene
      tempdf2[n,"regulator_id"] <- tempvec2[n]
    }

    tempdf3 <- rbind(tempdf3,tempdf2)

  }

  ## Recover effect data for every gene:regulator pair
  ## Said data is in NETWORK table

  ##Create empty column to store effect
  tempdf3$effect <- ""

  for (k in 1:nrow(tempdf3)) {
    ## check if regulator value is NA and assign NA to effect function
    if ( is.na(tempdf3[k,"regulators"])) {
      tempdf3[k,"effect"] <- NA
      ## else, extract the effect value for the gene-TF pair
    } else {
      ## Create full string for the query request
      ##Query should ask for the gene and its regulator
      query_cmd <- paste0("SELECT * FROM NETWORK WHERE regulator_id = '",
                          tempdf3[k,"regulator_id"],
                          "' and regulated_name = '",
                          tempdf3[k,"genes"],
                          "' and network_type = 'TF-GENE'")
      ## Create the query instruction
      query_cmd <- dbSendQuery(database, query_cmd)
      ## request the query, keep only columns effect
      tempdf3[k,"effect"] <- dbFetch(query_cmd)[,c("effect")]
    }
  }

  ## translate string values of effect to +,-,+- codes
  tempdf3[,"effect"] <- gsub("dual","+-",tempdf3[,"effect"])
  tempdf3[,"effect"] <- gsub("activator","+",tempdf3[,"effect"])
  tempdf3[,"effect"] <- gsub("repressor","+-",tempdf3[,"effect"])

  ## remove the regulator_id not originally requested by the design (but used to query the SQLite db)
  tempdf3 <- tempdf3[,c("genes","regulators","effect")]

  ###
  ## transform results to one row per gene results

  ## start an empty temp df to store transient data
  tempdf4 <- tempdf3[0,c("genes","regulators")]

  ## Define a vector with every unique gene in the data set
  unique_genes <- unique(tempdf3[,"genes"])

  ## Start a for loop to iteratively generate the data row for every gene
  for (l in 1:length(unique_genes)) {

    ## create an internal df to perform concatenations more readably
    tempdf <- tempdf3[ tempdf3$genes == unique_genes[l],]

    ## Store data per row, per gene
    tempdf4[l,"genes"] <- unique_genes[l]
    ## concatenate columns and paste collapse into a single string, comma separated
    tempdf4[l,"regulators"] <- paste0(tempdf$regulators,"(",tempdf$effect,")", collapse = ",")

  }

  ## Pass the multi row format to table format
  tempdf5 <- spread(tempdf3, regulators, effect)

  ## Up to this points, two data objects have been generated:
  ## table 3 is the multirow format requested
  ## table 4 is the onerow format requested
  ## table 5 is the table format requested
  ## depending on the requested format, one of these will be printed
  if ( format == "multirow" ) {
    tempdf3
  } else if (format == "onerow" ){
    tempdf4
  } else if (format == "table" ){
    tempdf5
  }
}
