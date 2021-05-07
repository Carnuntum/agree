

linesOfCode <- function(fileTypeChar) {
  
  if(fileTypeChar == 'all') {
    l <- unlist(str_split(dir(), '\n'))
    readList <- l[grepl('\\.', l)]
  } else {
    readList <- dir()[endsWith(dir(), fileTypeChar)]
  }
  
  lines <- 0
  spaces <- 0
  
  suppressWarnings(
    for(i in seq_along(readList)) {
      temp <- readLines(file(description = readList[i]))
      
      sp <- length(temp[temp == ''])
      lns <- length(temp) - sp
      
      lines <- lines + lns
      spaces <- spaces + sp
    }
  )
  
  return(list('lines' = lines,
              'empty' = spaces))
}


