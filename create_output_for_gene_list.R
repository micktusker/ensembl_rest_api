source('./ensembl_rest_api_functions.R')
source('./file_functions.R')
options(stringsAsFactors = FALSE)

# Returns the first column in a given file. Use this to turn
#  a single column of gene names into a vector.
getGeneNamesFromFileAsVector <- function(gene.names.file) {
  return(getColumnAsVector(gene.names.file))
}

# Create and return a two-column data frame of column name mapped to
#  Ensembl gene ID.
getGeneNamesEnsemblIDsAsDataFrame <- function(gene.names) {
  gene.names.ensembl.ids.dataframe <- data.frame()
  for (gene.name in gene.names) {
    ensembl.gene.ids <- getEnsemblGeneIDsForName(gene.name)
    for (ensembl.gene.id in ensembl.gene.ids) {
      print(c(gene.name, ensembl.gene.id))
      row <- c(gene.name, ensembl.gene.id)
      gene.names.ensembl.ids.dataframe <- rbind(row, gene.names.ensembl.ids.dataframe)
    }
  }
  return(gene.names.ensembl.ids.dataframe)
}

# Create a large data frame where all the values from the Ensembl REST API Gene
#  JSON appear as columns.
makeDataFrameForGeneList <- function(gene.names) {
  known.id.apoe <- 'ENSG00000130203'
  gene.attributes <- getEnsemblGeneAtrributeNames(known.id.apoe)
  gene.details.dataframe <- data.frame()
  for (gene.name in gene.names) {
    ensembl.gene.ids <- getEnsemblGeneIDsForName(gene.name)
    #print(gene.name)
    for (ensembl.gene.id in ensembl.gene.ids) {
      print(ensembl.gene.id)
      attribute.getter <- getEnsemblGeneAtrribute(ensembl.gene.id)
      attrribute.values <- rep(NA, length(gene.attributes))
      for (i in 1:length(gene.attributes)) {
        print(gene.attributes[i])
        attribute.value <- attribute.getter(gene.attributes[i])
        if(is.null(attribute.value)) {
          attrribute.values[i] <- '##'
        } else {
          attrribute.values[i] <- attribute.getter(gene.attributes[i])
        }
      }
      attrribute.values[length(attrribute.values) + 1] <- gene.name
      gene.details.dataframe <- rbind(attrribute.values, gene.details.dataframe)      
    }
    Sys.sleep(1)
  }
  df.names <- gene.attributes
  df.names[length(df.names) + 1] <- 'given.gene.name'
  names(gene.details.dataframe) <- df.names #gene.attributes
  return(gene.details.dataframe)
}

