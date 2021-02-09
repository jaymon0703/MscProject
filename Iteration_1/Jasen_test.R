require(jsonlite)
require(rjsonpath)
require(data.table)

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
ls_metrics <- list()
for(f in 1:length(filenames)){
  ls_metrics[[f]] <- fromJSON(txt=paste0("test_data/",filenames[f]))$RIDES$METRICS
  ls_metrics[[f]][["date"]] <- fromJSON(txt=paste0("test_data/",filenames[f]))$RIDES$date
  ls_metrics[[f]][["id"]] <-  rep(fromJSON(txt=paste0("test_data/",filenames[f]))$ATHLETE$id, length(ls_metrics[[f]][["date"]]))
  ls_metrics[[f]][["sport"]] <- fromJSON(txt=paste0("test_data/",filenames[f]))$RIDES$sport
  ls_metrics[[f]][["yob"]] <-  rep(fromJSON(txt=paste0("test_data/",filenames[f]))$ATHLETE$yob, length(ls_metrics[[f]][["date"]]))
  ls_metrics[[f]][["gender"]] <-  rep(fromJSON(txt=paste0("test_data/",filenames[f]))$ATHLETE$gender, length(ls_metrics[[f]][["date"]]))
}

# dt_metrics <- as.data.table(sapply(ls_metrics, unlist))
