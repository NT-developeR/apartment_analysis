rm(list = ls())
###
library(rvest)
library(dplyr)
page <- "http://www.bn.ru/zap_fl.phtml?kkv1=1&kkv2=5&price1=&price2=" 

#http://www.bn.ru/zap_bd.phtml?kkv1=0&kkv2=0&price1=&price2  новостройки
#http://www.bn.ru/zap_fl.phtml?kkv1=2&kkv2=2&price1=&price2=  двухкомнатные
  
data_all <- read_html(page)  #взять теги отсюда))))
tags_lists <- read.table(file.path(getwd(), "flat_bn_tags"), skip = 1, header = TRUE, sep = ";", stringsAsFactors = FALSE)
#tags_lists <- tags_lists[ -c(3,8), ] #неправильно читаются данные - длина массива по 31 значению

pages <- data_all %>% #TBD при таком раскладе пропукаем первую страницу - попробовать подобрать другой тег
  html_nodes(".sr_pages a") %>%
  html_attr("href") %>% unique() 
#%>% append(page)

#[1] "/zap_bd.phtml?kkv1=1&kkv2=2&price1=&price2=&start=50"  "/zap_bd.phtml?kkv1=1&kkv2=2&price1=&price2=&start=100"
#[3] "/zap_bd.phtml?kkv1=1&kkv2=2&price1=&price2=&start=150" "/zap_bd.phtml?kkv1=1&kkv2=2&price1=&price2=&start=950"
#

#первый подход меняем ссылки за счет работы с текстом

splited <- strsplit(pages, "=") 
pages_path <- splited[1]
pages_path <- unlist (pages_path) 
pages_path <- pages_path[1:5] %>%  paste(collapse = "=" )

items_per_page <- do.call("cbind", splited) %>% tbl_df() %>% slice(6) %>% as.numeric()
pages_numbers <- seq(from = items_per_page[1], to = items_per_page[length(items_per_page)], by = items_per_page[2] - items_per_page[1])
 
relevant_pages <- lapply(pages_numbers, function(io){
  reslt <- paste(c(pages_path, io), collapse = "=") 
})

relevant_pages <- do.call("rbind", relevant_pages) %>% as.list()


data_session <- html_session(page)

data_collection <- function(data_all){
  res <- lapply(tags_lists$css_tag, function(v){
    re <- assign(tags_lists$description[which(tags_lists$css_tag == v)], data_all %>% html_nodes(v) %>% html_text()) 
    if(any(is.na(re))) {
      print("smth is wrong during the parsing")
    }
    return(re)
  })
  
  #names(res) <- tags_lists$description
  results <- do.call("rbind", res)
  print( paste("Page parsing had been finished. Amount of parsed data:" ,length(res[[1]])))
  return(results)
} #end of data collection function

page_lister <- lapply(relevant_pages, function(l){
  
  jump_to(data_session, l) %>% data_collection()
}
) #end of lapply 

str(page_lister)
flats <- do.call("cbind", page_lister) %>%  t() %>%
  as.data.frame( stringsAsFactors = FALSE  ) 

names(flats) <- gsub(" ", "", tags_lists$description)

#приводим в порядок структуру файла

flats[ , c("number_of_rooms", "total_square", "living_square", "cost") ]  <- apply(flats[ , c("number_of_rooms", "total_square", "living_square", "cost") ], 2, as.numeric)

hist(flats$cost, breaks = seq(from = 1000, to = 10000, by = 1000))
