library(tidyverse)

sizeReport <- function(path, patt = ".*", dironly = FALSE, level = Inf) {
  
  files <- data.frame(name = character(), size = numeric())
  
  
  walkDir <- function(path, level) {
    
    filesInDir <- list.files(path, recursive = FALSE)
    
    for (file in filesInDir) {
      
      fullPath <- file.path(path, file)
      
      if (dironly && !dir.exists(fullPath)) {
        next
      }
      
      if (dir.exists(fullPath) && level > 0) {
        walkDir(fullPath, level - 1)
      } else {
        
        if (!dir.exists(fullPath) && grepl(patt, file)) {
          files <<- rbind(files, data.frame(name = fullPath, size = file.size(fullPath)))
        }
      }
    }
  }
  
  
  walkDir(path, level)
  
  
  return(files)
}
x <- sizeReport(path = getwd()) %>% 
  mutate(size1 = ceiling(size/(1024*1024))) %>% 
  filter(size1 >= 100)

y <- x %>% 
  mutate(name = gsub(getwd(), "", name)) %>% 
  select(name)

write.table(y, "gitignoreauto.txt", row.names = F, col.names = F, quote = F)
