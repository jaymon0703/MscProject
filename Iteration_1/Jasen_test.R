require(jsonlite)
require(data.table)
t1 <- Sys.time()
# Get data into a single list
# Test with n files in a sub folder called test_data
setwd("/home/jasen/Personal-Work/GitHub/MscProject")
filenames <- list.files(path = "test_data")
idx_bike <- list() # if we want to filter for "Bike" observations
# for(f in 1:length(filenames)){
#   idx_bike[[f]] <- which(fromJSON(txt=paste0("test_data/",filenames[f]))$RIDES$sport == "Bike")
# }
# Build all the dataframes in a list called ls_metrics
ls_metrics <- list()
for(f in 1:length(filenames)){
  ls_metrics[[f]] <- fromJSON(txt=paste0("test_data/",filenames[f]))$RIDES$METRICS#[idx_bike[[f]],]
  ls_metrics[[f]][["date"]] <- fromJSON(txt=paste0("test_data/",filenames[f]))$RIDES$date#[idx_bike[[f]]]
  ls_metrics[[f]][["id"]] <-  rep(fromJSON(txt=paste0("test_data/",filenames[f]))$ATHLETE$id, length(ls_metrics[[f]][["date"]]))
  ls_metrics[[f]][["sport"]] <- fromJSON(txt=paste0("test_data/",filenames[f]))$RIDES$sport#[idx_bike[[f]]]
  ls_metrics[[f]][["yob"]] <-  rep(fromJSON(txt=paste0("test_data/",filenames[f]))$ATHLETE$yob, length(ls_metrics[[f]][["date"]]))
  ls_metrics[[f]][["gender"]] <-  rep(fromJSON(txt=paste0("test_data/",filenames[f]))$ATHLETE$gender, length(ls_metrics[[f]][["date"]]))
}
# Merge into a data.table
dt_merged <- rbindlist(ls_metrics, fill = TRUE)
# Reorder columns such that "id", "date", "sport", "yob" and "gender" are the first columns
# setcolorder(dt_merged, c(colnames(idx_cols),colnames(dt_merged[-idx_cols]))) # this works, but only by reference...saves space but difficult to view
idx_metadata_cols <- which(colnames(dt_merged) %in% c("date","id","sport","yob","gender"))
idx_metric_cols <- which(!colnames(dt_merged) %in% c("date","id","sport","yob","gender"))
dt_merged <- dt_merged[,c(idx_metadata_cols,idx_metric_cols), with=FALSE]
bike <- dt_merged[sport=="Bike"]
t2 <- Sys.time()
print(t2-t1)
