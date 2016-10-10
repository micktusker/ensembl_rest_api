library(jsonlite)
# Use the Ensembl REST API to return details for genes.

# Given a gene name, return a data frame with the corresponding Ensembl gene IDs.
# The default species is human.
# The "try" captures the error thrown aby a gene name that is not found in Ensembl.
getEnsemblGeneDataFrameForName <- function(gene.name, species = 'homo_sapiens') {
  url.tmpl <- 'https://rest.ensembl.org/xrefs/symbol/%s/%s?content-type=application/json'
  url <- sprintf(url.tmpl, species, toupper(gene.name))
  result <- try(fromJSON(url))
  if (class(result) == 'data.frame') {
    return(result)
  } else {
    empty.data.frame <- data.frame()
    empty.data.frame <- rbind(c('gene', sprintf('No_Ensembl_ID_Found: %s', gene.name)), empty.data.frame)
    names(empty.data.frame) <- c("type", "id")
    return(empty.data.frame)
  }
}

# Return a vector of Ensembl gene IDs for the given gene name and species (defuault = human).
getEnsemblGeneIDsForName <- function(gene.name, species = 'homo_sapiens') {
  gene.result <- getEnsemblGeneDataFrameForName(gene.name)
  return(as.vector(gene.result$id))
}

# Return a list with all the Ensembl details for a given Ensembl gene ID.
getEnsemblDetailsForGeneID <- function(ensembl.gene.id) {
  url.tmpl <- 'https://rest.ensembl.org/lookup/id/%s?content-type=application/json'
  url <- sprintf(url.tmpl, ensembl.gene.id)
  return(fromJSON(url))
}

# Return a vector containing the names of all the values stored in the list returned by
#  getEnsemblDetailsForGeneID. 
getEnsemblGeneAtrributeNames <- function(ensembl.gene.id) {
  return(names(getEnsemblDetailsForGeneID(ensembl.gene.id)))
}

# Returns a function.
# Creates a closure around the value returned by getEnsemblDetailsForGeneID().
# The inner function that is returned can be used to return single item vectors
# for a given attribute name.
# The closure makes the code more efficient because there is only a single call to the REST API
#  and its return list value is stored in the closure so that required values can be accessed as
#  needed vy calling the inner function.
getEnsemblGeneAtrribute  <- function(ensembl.gene.id) {
  ensemb.gene.details <- getEnsemblDetailsForGeneID(ensembl.gene.id)
  getAttribute <- function(attribute.name) {
    attribute.value <- ensemb.gene.details[[attribute.name]]
    names(attribute.value) <- NULL
    return(attribute.value)
  }
  return(getAttribute)
}