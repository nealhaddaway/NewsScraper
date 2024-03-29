library(xml2)
library(gdata)
library(tidyr)
library(svMisc)

# Downloaded URLs using pattern bulk downloader DownThemAll chrome extension

# Load in html files from local source
dir <- list.files(path="./googleresults/NewScientist", all.files=TRUE, 
                  full.names=TRUE)
source <- dir[endsWith(dir, '.html')]

output <- data.frame(source = NULL, page = NULL, urls = NULL, row = NULL)

# extract source name and page number
for (i in 1:length(source)){
  source_name <- source[i]
  source_num <- unlist(regmatches(source_name, gregexpr("[[:digit:]]+", source_name)))
  if(identical(source_num, character(0))){
    source_num <- 0
  }
  
  # read in each html file one by one
  html <- xml2::read_html(source[i])
  
  # identify hrefs by <a> attribute
  links <- html %>% 
    rvest::html_elements("a") %>% 
    html_attr('href') 
  links <- unique(links[startsWith(links, 'https://www.newscientist.com')])
  links <- links[!is.na(links)]
  order <- seq(1,length(links))
  
  # create output
  output <- rbind(output, data.frame(source = source_name, 
                                     page = as.numeric(source_num), 
                                     urls = links, 
                                     row = order))
  
  # progress bar
  progress(i, length(source))
  
  # erase html
  rm(html)
}

# order outputs and create 'order' column
output <- output[order(output$page),]
output$order <- seq(1:nrow(output))

write.csv(output, 'NewScientist.csv', row.names = FALSE)

