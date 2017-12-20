# install.packages('rvest')

library(rvest)
library(htmltidy)
library(stringr)
library(dplyr)

cov_opendata <- read_html("http://data.vancouver.ca/datacatalogue/index.htm")

cov_table_rows <-cov_opendata %>% 
  xml_nodes(".content table tr") 

cov_header <- cov_table_rows[1] %>% 
  html_nodes("th") %>% 
  html_text()
cov_header

cov_body <- cov_table_rows[-1]
cov_list <- lapply(cov_body,FUN = html_nodes,css="td")
cov_list_data <- cov_list[sapply(cov_list,length) == 9] #Rows with data have 9 columns


cov_df <- cov_list_data %>% 
  lapply(as.character) %>% 
  unlist() %>% 
  matrix(ncol = 9,byrow=T) %>% 
  data.frame(stringsAsFactors = F) %>% 
  `colnames<-`(cov_header)

View(cov_df)

extract_object <- function(xml_vec, object) {
  
  extract_ <- function(xml,obj) {
    xml = xml %>% 
      str_replace_all("<br>","") %>%  # remove offending breaks. xml2 doesn't recognize unclosed breaks
      str_replace_all("<img.*?>","") %>% # remove img. the ? looks for the shortest regex
      as_xml_document()
    if (object == "href") {
      result = xml %>% 
        html_node("a") %>% 
        html_attr("href") %>% 
        as.character()
    } else {
      result = xml %>% 
        html_text()
    }
    result
  }
  
  xml_vec %>% 
    as.character() %>% 
    sapply(FUN = extract_,obj = object)  
}

cov_df_clean <- cov_df %>% 
  mutate(data_name = extract_object(`Name & Information about Data`,"text"),
         data_href = extract_object(`Name & Information about Data`,"href"),
         CSV = extract_object(CSV,"href"),
         XLS = extract_object(XLS,"href"),
         DWG = extract_object(DWG,"href"),
         KML = extract_object(KML,"href"),
         SHP = extract_object(SHP,"href"),
         ECW = extract_object(ECW,"href"),
         `Other Formats` = extract_object(`Other Formats`,"href"),
         `Google Maps` = extract_object(`Google Maps`,"href")
         ) %>% 
  select(data_name,
         data_href,
         everything(),
         -`Name & Information about Data`)

#fix http links, exclude links that alraedy have ftp http or https
baselink = "http://data.vancouver.ca/datacatalogue"

