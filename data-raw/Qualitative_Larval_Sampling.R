require(wql)
require(LTMRdata)
require(readr)
require(dplyr)
require(lubridate)
require(tidyr)
require(stringr)
require(utils)
require(rvest)
require(XML)

# downloading data because the dataset is too huge to keep on file
# start pipeline to edi
# relational tables

# Find the newest revision
# IF you want to pull a specific version of a package, which is a number
version <- NA
link <- ifelse(is.na(version), "https://pasta.lternet.edu/package/data/eml/edi/2127/newest",
               paste0("https://pasta.lternet.edu/package/data/eml/edi/2127/1/", version))

tableLinks <- read.delim(link, header = F) %>%
  .[[1]]%>%
  paste0("https://pasta.lternet.edu/package/data/eml/edi/2127/1/",.)

tableNames <- lapply(tableLinks, function(x) {
  entityName <- read_html(gsub("data", "name", x)) %>%
    html_text()

  data.frame(id = gsub(".*\\/", "", x),
             name = entityName,
             url = x)
}) %>%
  bind_rows()



QLS <-read.csv(tableNames %>%
                 dplyr::filter(grepl("Larval fish data", name)) %>%
                 pull(url),
               header=F
               ,skip=1
               ,sep=","
               ,quot='"'
               , col.names=c(
                 "date_time",
                 "building",
                 "common_name",
                 "scientific_name",
                 "count",
                 "comments"    ), check.names=TRUE)%>%
  dplyr::rename(Date=date_time,Facility=building,Taxa=scientific_name,Count=count,Comments=comments)%>%
  mutate(Source="QLS",
         Latitude = 37.825612769565474,
         Longitude =-121.59584120116043)%>%
  select(Source,Latitude,Longitude,Date,Taxa,Count,Comments)

usethis::use_data(QLS, overwrite=TRUE, compress="xz")
