library(tidyverse)
library(legislatoR)
library(rvest)

deu_political <- get_political(legislature = "deu")

deu_core <- get_core(legislature = "deu")

deu_portraits <- get_portrait(legislature = "deu")


img_folder <- "data/politicians_images/"
dir.create(img_folder)


scrape_politician_images <- function(image_url, pageid) {
  download.file(image_url, destfile = paste0(img_folder, pageid, ".jpg"), mode = "wb", quiet = TRUE)
}

for (i in 1:length(deu_portraits$pageid)) {
  if (!file.exists(paste0(img_folder, deu_portraits$pageid[i], ".jpg"))) {  
    # skip article when we run into an error   
    tryCatch( 
      scrape_politician_images(deu_portraits$image_url[i], deu_portraits$pageid[i]),
      error = function(e) e
    )
    # don't kill their server --> be polite!  
    Sys.sleep(runif(1, 0, 1))
  }
}
