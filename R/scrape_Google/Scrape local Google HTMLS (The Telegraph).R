library(xml2)
library(gdata)
library(tidyr)
library(svMisc)

# Downloaded URLs using pattern bulk downloader DownThemAll chrome extension

# Load in html files from local source
dir <- list.files(path="./googleresults/TheTelegraph", all.files=TRUE, 
                  full.names=TRUE)
farming <- list.files(path="./googleresults/TheTelegraph/farming", all.files=TRUE, 
                      full.names=TRUE) %>%
  .[endsWith(., '.html')]
agriculture <- list.files(path="./googleresults/TheTelegraph/agriculture", all.files=TRUE, 
                          full.names=TRUE) %>%
  .[endsWith(., '.html')]
crops <- list.files(path="./googleresults/TheTelegraph/crops", all.files=TRUE, 
                    full.names=TRUE) %>%
  .[endsWith(., '.html')]
livestock <- list.files(path="./googleresults/TheTelegraph/livestock", all.files=TRUE, 
                        full.names=TRUE) %>%
  .[endsWith(., '.html')]
foodsystem <- list.files(path="./googleresults/TheTelegraph/foodsystem", all.files=TRUE, 
                         full.names=TRUE) %>%
  .[endsWith(., '.html')]
source <- c(farming, agriculture, crops, livestock, foodsystem)

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
  links <- unique(links[startsWith(links, 'https://www.telegraph.co.uk')])
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

#filter for unique results
output <- output %>% 
  distinct(urls, .keep_all = TRUE)

write.csv(output, 'googleresults/TheTelegraph/TheTelegraph_googleresults.csv', row.names = FALSE)

