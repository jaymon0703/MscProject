require(jsonlite)
require(rjsonpath)
require(data.table)
require(purrr)
require(dplyr)
require(reshape)

# dev
# document <- fromJSON(txt="test_data/{0a4dfb20-0ce5-4d55-9bd9-20122b79baf5}.json")
# idx_bike <- which(document$RIDES$sport=="Bike")
# document$RIDES$METRICS
# metrics <- fromJSON(txt="test_data/{0a4dfb20-0ce5-4d55-9bd9-20122b79baf5}.json")$RIDES$METRICS
#

# Get data into a single list
# Test with n files in a sub folder called test_data
setwd("/home/jasen/Personal-Work/GitHub/MscProject")
filenames <- list.files(path = "test_data")
idx_bike <- list()
for(f in 1:length(filenames)){
  idx_bike[[f]] <- which(fromJSON(txt=paste0("test_data/",filenames[f]))$RIDES$sport == "Bike")
}

ls_metrics <- list()
for(f in 1:length(filenames)){
  ls_metrics[[f]] <- fromJSON(txt=paste0("test_data/",filenames[f]))$RIDES$METRICS[idx_bike[[f]],]
  ls_metrics[[f]][["date"]] <- fromJSON(txt=paste0("test_data/",filenames[f]))$RIDES$date[idx_bike[[f]]]
  ls_metrics[[f]][["id"]] <-  rep(fromJSON(txt=paste0("test_data/",filenames[f]))$ATHLETE$id, length(ls_metrics[[f]][["date"]]))
  ls_metrics[[f]][["sport"]] <- fromJSON(txt=paste0("test_data/",filenames[f]))$RIDES$sport[idx_bike[[f]]]
  ls_metrics[[f]][["yob"]] <-  rep(fromJSON(txt=paste0("test_data/",filenames[f]))$ATHLETE$yob, length(ls_metrics[[f]][["date"]]))
  ls_metrics[[f]][["gender"]] <-  rep(fromJSON(txt=paste0("test_data/",filenames[f]))$ATHLETE$gender, length(ls_metrics[[f]][["date"]]))
}

# dt_metrics <- as.data.table(sapply(dt_metrics, unlist))
# merged.data.frame <- Reduce(function(...) merge(..., all=T), ls_metrics)
# i <- c("total_distance","average_speed")
# merged_df <- ls_metrics %>% reduce(full_join, by = "i")
# merged_df <- reshape::merge_all(ls_metrics, by=i)
# 
# 
# x <- data.frame(i = c("a","b","c"), j = 1:3, stringsAsFactors=FALSE)
# y <- data.frame(i = c("b","c","d"), k = 4:6, stringsAsFactors=FALSE)
# z <- data.frame(i = c("c","d","a"), l = 7:9, stringsAsFactors=FALSE)
# list(x, y, z) %>% reduce(left_join, by = "i")

