# Convenience function to return the contents of text files as a data frame.
getFileAsDataFrame <- function(path.to.file, delimiter = '\t', first.row.as.header = TRUE) {
  file.as.dataframe <- read.table(path.to.file, header = first.row.as.header, sep = delimiter, stringsAsFactors = FALSE)
  return(file.as.dataframe)
}

# Return a specified column number (defaults to first column) from a text file.
getColumnAsVector <- function(path.to.file, column.number = 1) {
  file.as.data.frame <- getFileAsDataFrame(path.to.file)
  column <- file.as.data.frame[,column.number]
  return(as.vector(column))
}