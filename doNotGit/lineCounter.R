

linesOfCode <- function(fileTypeChar) {
  
  if(fileTypeChar == 'all') {
    l <- unlist(stringr::str_split(dir(), '\n'))
    readList <- l[grepl('\\.', l)]
  } else {
    readList <- dir()[endsWith(dir(), fileTypeChar)]
  }
  
  lines <- 0
  spaces <- 0
  comments <- 0
  
  suppressWarnings(
    for(i in seq_along(readList)) {
      temp <- readLines(file(description = readList[i]))
      
      #sp <- length(temp[temp == ''])
      
      sp <- length(temp[grepl('^\\s*$', temp)]) 
      com <- length(temp[stringr::str_detect(temp, '(^#|^\\s+(#))')])
      
      lns <- length(temp) - sp - com
      
      lines <- lines + lns
      spaces <- spaces + sp
      comments <- comments + com
    }
  )
  
  return(list('lines' = lines,
              'spaces' = spaces,
              'comments' = comments,
              'linesAndComments' = lines + comments,
              'all' = lines + spaces + comments))
}


globalLineChanger <- function(fileTypeChar, pattern,  replacement) {
  require(stringr)
  
  readList <- dir()[endsWith(dir(), fileTypeChar)]
  
  dir.create('tempEntries')
  
  suppressWarnings(
    for(entry in readList) {
      
      print(paste0('changing file ', entry))
      print(paste0('replacing: ', pattern, ' with: ', replacement))
      
      temp <- readLines(file(description = entry))
      
      temp <- str_replace_all(temp, pattern = pattern, replacement = replacement)
      
      writeLines(temp, con = paste0('tempEntries/', '_test_', entry))
    }
  )
}

