library(tidyverse)
library(janitor)
d <- read_csv("resume_code/decra_23.csv") %>% clean_names()
d <- d %>% mutate(fellow = gsub("Dr ","",fellow),
                  fellow = gsub("Associate Professor ", "", fellow),
                  fellow = gsub("Assistant Professor ", "", fellow))
d$scholar_id <- as.character(NA)
d$first_name <- as.character(NA)
d$surname <- as.character(NA)
d$scopus_id <- as.character(NA)
d$pubs <- as.numeric(NA)
d$citations <- as.numeric(NA)
d$h_index <- as.numeric(NA)
library(rscopus)


for(i in 1:dim(d)[1]){
  #i <- 1
  d$first_name[i]  <-  str_split(d$fellow[i], " ")[[1]][1]
  d$surname[i] <- str_split(d$fellow[i], " ")[[1]][2]
  person <- rscopus::get_complete_author_info(d$surname[i],
                                              d$first_name[i],
                                              affil_name =  d$administering_organisation[i])
  if(length(as.numeric(person$content$`search-results`$entry[[1]]$`document-count`)) > 0){
    d$pubs[i] <- as.numeric(person$content$`search-results`$entry[[1]]$`document-count`)
    d$scopus_id[i] <- person$content$`search-results`$entry[[1]]$`dc:identifier`
    d$scopus_id[i] <- gsub("AUTHOR_ID:","",d$scopus_id[i])
    p2 <- rscopus::author_retrieval_id(d$scopus_id[i], view = "METRICS")
    d$citations[i] <- as.numeric(p2$content$`author-retrieval-response`[[1]]$coredata$`citation-count`)
    d$h_index[i] <- as.numeric(p2$content$`author-retrieval-response`[[1]]$`h-index`)
  }
  #   scholar_id[i] <- get_scholar_id(d$surname[i],
  #                                   d$first_name[i])
  # d$h_index[i] <- predict_h_index(d$scholar_id[i])
  # d$profile[i] <- get_profile(d$scholar_id[i])
}
saveRDS(d, "decra_scopus_ids.RDS")

d %>% select(fellow, pubs, citations, scopus_id)
d %>% filter(surname == "Noetel") %>% select(scopus_id)
author_retrieval("57190857713", view = "METRICS")
