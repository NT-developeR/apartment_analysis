  rm(list =  ls())
  
  library(rvest)
  library(dplyr)
  page <- "http://www.bn.ru/zap_rm.phtml?kkv1=&kkv2=&vkomn1=5&vkomn2=12&price1=&price2=&so1=&so2=&sg1=15&sg2=22&sk1=&sk2=&sjem1=&sjem2=&sosed1=&sosed2=&region1=1&region6=6&region44=44&region47=47&r4=1&ags=&search_ags=1&sorttype=0&sortordtype=0&text=" 
  #всего комнат от 5 до 12, жил.площ. от 15 до 22 кв.м, в Адм., Вас., Пет., Цен. районах
  data_all <- read_html(page)  #взять теги отсюда))))
  tags_lists <- read.table(file.path(getwd(), "room_bn_tags"),  header = TRUE, sep = ";", stringsAsFactors = FALSE)
  

  split_and_numeric <- function(rooms){
    try <-  apply(rooms, 2,  function(i){
      if ( all(grepl("/", i)  == TRUE) ) {
        splitted <- strsplit(i, "/") 
        split <- do.call("rbind", splitted)
        res <- apply(split, 2, as.numeric)
      }
    })
  } #end of split and numeric function
  
  
room_page <- function(data_all){
  rooms <-  do.call("cbind", 
                    lapply(tags_lists$css_tag, function(v){
                      assign(tags_lists$description[which(tags_lists$css_tag == v)], data_all %>% html_nodes(v) %>% html_text()) 
                    })
  )
  
  colnames(rooms) <- gsub(" ", "", tags_lists[,2])
  rooms <- as.data.frame(rooms, stringsAsFactors = FALSE)
  

  results <- do.call("cbind", split_and_numeric(rooms))
  colnames(results) <- c("relevant_level", "total_levels", "total rooms", "neighbors", "total neigbors")
  #подумать об автоматизации комбинации изменных и неизмененных колонок -> 
  results_rooms <- data.frame(rooms[ , -c(2, 5)], results )
  results_rooms[ ,c("living_square", "price")] <- apply(results_rooms[ ,c("living_square", "price")], 2, function(i)
    as.numeric(as.character(i)))
  
  return(results_rooms)
} #end of room page function
####организуем листание страничек######

pages <- data_all %>% #TBD при таком раскладе пропукаем первую страницу - попробовать подобрать другой тег
  html_nodes(".sr_pages a") %>%
  html_attr("href") %>% unique() %>% append(page)

data_session <- html_session(page)

page_lister <- lapply(pages, function(l){
  
  jump_to(data_session, l) %>% room_page()
}
) #end of lapply 

rooms <- do.call("rbind", page_lister) 
hist(rooms$price, breaks = seq( from = 500, to = 4000, by= 100))

#______________________________думать об отслеживании динамики______________________________