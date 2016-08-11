rm(list = ls())

library(rvest)
library(dplyr)

page <- "http://www.bn.ru/newbuild/photo/search/?region8=8&region31=31&kkv1=&kkv2=&text=" # новостройки московский и ломоносовский район страница 1
data <- read_html(page) 

####загрузка со страницы новостроек в районе ####
price <- html_nodes(data, ".obj_bg tr:nth-child(1) td:nth-child(3) b") 
cost <- as.numeric(gsub("[^0-9]", "", unlist(price)))
room_number <- as.numeric(html_text(html_nodes(data, "tr:nth-child(5) strong")))
adress <- html_text(html_nodes(data, "b a"))[1:length(cost)] # послед
sailing_type <- html_text(html_nodes(data,".obj_bg td:nth-child(1) strong"))
building_type <- html_text(html_nodes(data, ".obj_bg tr:nth-child(3) a"))
###Какие данные со страницы района могут быть полезны?
# добавить продавца
#  ???

#собираем ссылки на детальную информацию об объектах
adress_details <- data %>% 
  html_nodes("b a") %>%
  html_attr("href") 

### загрузка детальной информации - с вкладки об объекте ####
data_session <- html_session(page)
#
# readability <- vector(mode = "numeric",length=0)

details_parsing <- function(v) { #парсим инфу с деталями об объекте
 
  nodes_list <- c("tr:nth-child(8) .str", "tr:nth-child(9) .str", "tr:nth-child(7) .str", "tr:nth-child(10) .str")
  #меняют структуру сайта??))) две разных структуры node_list. отсюда вырастает задача определения css selection по тексту контента
  #ждем пока устаканят структуру сайта))
  x <- v
  
  parced_data <- sapply(nodes_list, function(a) 
    html_nodes(x, a) %>% html_text()
  )

  if( length(unlist(parced_data)) == 0 ) {
    #readability <- append(readability, 0, after = length(readability))
    message ("data could not be parsed")
  } else {
  stages <- strsplit(parced_data[3], "/") %>% unlist() %>% as.numeric(as.character())
  
  detiled_info <- data.frame(
                    "total_square" = as.numeric(as.character(parced_data[1])),
                    "living_square" = as.numeric(as.character(parced_data[2])),#общая и жилая площадь
                    "living_level" = stages[1],#этаж расположения
                    "total_level" = stages[2],  #и общая этажность
                    "readiness" = parced_data[4], row.names = NULL)
 # readability <- append(readability, 1, after = length(readability))
  message("detailed information about the building is parsed")
      # if( exists("detiled_info") ) {
      #   detiled_info <- rbind(detiled_info, parsed)
      #   message("data is added to the dataframe")
      # }else{
      #   detiled_info <- parsed
      #   message("dataframe is started")
      # }
  return(detiled_info)
  }
  #return(list("readability" = readability, "detailed_info"= detiled_info))
  
}#end of details_parsing


  #предусмотреть защиту от ошибок и освоить функцию TryCatch

  building_details_collection <- function ( ){
    try_list <-  sapply(adress_details, function(v)
        jump_to(data_session, v) %>% details_parsing()
        )
    
    # на выходе имеем: 
    # * если даже данных не было - они не опущены (как планировалось): -> лучше оставить нули чтобы была связка с  данными с прошлой вкладки
    # * датафреймы загнаны в листы: нужно объединить 
    
    
    try_list_two <- unlist(try_list, recursive = FALSE)
    #из датафреймов уйти в листы и уже работать с ними через unlist
    return(try_list)
  } #end of building_details_collection()
    
    

  results <- building_details_collection()





