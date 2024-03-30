# Try to download news article text and images
library(RCurl)
library(rvest)
library(tidyverse)
library(svMisc)

input <- read.csv("googleresults/TheTelegraph/TheTelegraph_googleresults.csv")

output <- data.frame(page=NULL, 
                     title=NULL, 
                     url=NULL, 
                     text=NULL, 
                     lead_img=NULL, 
                     lead_caption=NULL,
                     n_photos=NULL)

# loop through 1000 search results, scraping titles, text, image urls and captions
for(i in 1:length(input$urls)){
  
  # extract html code
  html <- RCurl::getURL(input$urls[i], followlocation=TRUE)
  html <- minimal_html(html)
  
  # find paragraph and heading texts
  text <- html %>% 
    html_elements("p, h1, h2, h3, h4") %>%
    html_text2() %>% 
    gsub("â€™", "'", .) %>% 
    gsub("Â", "", .) %>%
    gsub("â€œ", '"', .) %>%
    gsub("â€“", '-', .) %>%
    gsub("â€", '"', .)
  text <- paste(text, collapse='\n\n')
  if(identical(text, character(0))){text<-'[No main text]'}
  
  # find title
  title <- html %>% 
    html_elements("h1") %>%
    html_text2() %>% 
    gsub("â€™", "'", .) %>% 
    gsub("Â", "", .) %>%
    gsub("â€œ", '"', .) %>%
    gsub("â€“", '-', .) %>%
    gsub("â€", '"', .)
  if(identical(title, character(0))){title<-'[No title]'}
  
  # find lead image
  lead_img <- html %>% 
    html_elements("main") %>%
    html_elements("figure") %>%
    html_elements("img") %>%
    html_attr("src") %>%
    sub("width=120", "width=1000", .)
  lead_img <- lead_img[grepl('*.jpg|.png|.jpeg', lead_img)] #save only jpg or png files
  #lead_img2 <- lead_img[!grepl('*badge.png', lead_img)] #remove Guardian badge
  #lead_img2 <- lead_img[!grepl('*uploads*', lead_img)] #remove folders from uploads section (headshots)
  captions <- html %>% 
    #html_elements("figure") %>%
    html_elements("figcaption") %>%
    html_text2() %>%
    unique()
  n_photos <- length(captions)
  if(identical(lead_img, character(0))){lead_img<-'[No lead image]'} else {
    lead_img <- paste0('https://www.telegraph.co.uk', lead_img[1])
  }
  
  # find lead image caption
  lead_caption <- html %>% 
    html_elements("figcaption") %>%
    html_text2() %>%
    unique()
  if(identical(lead_caption, character(0))){lead_caption<-'[No lead caption]'}
  lead_caption <- lead_caption[1]
  
  # make a df of page, title, text and images/captions
  output_1 <- data.frame(page=i, 
                         title=title, 
                         url=input$urls[i], 
                         text=text, 
                         lead_img=lead_img, 
                         lead_caption=lead_caption,
                         n_photos=n_photos)
  
  # append to output
  output <- rbind(output, output_1)
  
  #progress bar
  progress(i, length(input$urls))
}

# deduplicate rows of df
output <- unique(output)

# filter text for search terms
output$subject_check <- grepl('farm|agricultur|crop|livestock|food', output$text)
output$research_check <- grepl('study|research|university|institute|studied|researchers', output$text)

subject_output <- subset(output, subject_check==TRUE)
research_output <- subset(subject_output, research_check==TRUE)

# save output
write.csv(output, 'results/TheTelegraph/TheTelegraphOutput.csv', row.names = FALSE)
write.csv(subject_output, 'results/TheTelegraph/TheTelegraphOutput_subject.csv', row.names = FALSE)
write.csv(research_output, 'results/TheTelegraph/TheTelegraphOutput_subject&research.csv', row.names = FALSE)

# save images
# save all images
for (i in 1:length(output$lead_img)) {
  if(output$lead_img[i]=='[No lead image]'){
    print(paste0('No image to download for record #', i))
  } else {
    tryCatch({
      download.file(output$lead_img[i], 
                            destfile=paste0("lead_images/TheTelegraph/output/TheTelegraph_page-", i,".jpg"), 
                            quiet = TRUE)
      print(paste0('Image #', i, ' downloaded successfully'))},
             error = function(msg){
               print(paste0('Cannot open URL ', i))
             },
             warning = function(msg){
               print(paste0('Cannot open URL ', i))
             })
  }
}

# save research images
for (i in 1:length(research_output$lead_img)) {
  if(research_output$lead_img[i]=='[No lead image]'){
    print(paste0('No image to download for record #', i))
  } else {
    tryCatch({
      download.file(research_output$lead_img[i], 
                            destfile=paste0("lead_images/TheTelegraph/research_output/TheTelegraph_page-", i,".jpg"), 
                            quiet = TRUE)
      print(paste0('Image #', i, ' downloaded successfully'))},
             error = function(msg){
               print(paste0('Cannot open URL ', i))
             },
             warning = function(msg){
               print(paste0('Cannot open URL ', i))
             })
    
  }
}

# save farming images
for (i in 1:length(subject_output$lead_img)) {
  if(subject_output$lead_img[i]=='[No lead image]'){
    print(paste0('No image to download for record #', i))
  } else {
    tryCatch({
      download.file(subject_output$lead_img[i], 
                            destfile=paste0("lead_images/TheTelegraph/subject_output/TheTelegraph_page-", i,".jpg"), 
                            quiet = TRUE)
      print(paste0('Image #', i, ' downloaded successfully'))},
             error = function(msg){
               print(paste0('Cannot open URL ', i))
             },
             warning = function(msg){
               print(paste0('Cannot open URL ', i))
             })
    
  }
}
