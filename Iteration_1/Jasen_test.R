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

idx_list <- which(sapply(dt_merged, class) == "list")
idx_list

class(type.convert(dt_merged[,11]))

dt_merged <- dt_merged[,c('date','id','ride_count','workout_time','time_riding','athlete_weight','total_work','average_power','nonzero_power',
                          'max_power','cp_setting','coggan_np','coggan_if','coggan_tss','coggam_variability_index','coggan_tssperhour',
                          'time_in_zone_P1','1s_critical_power','5s_critical_power','10s_critical_power','15s_critical_power','20s_critical_power',
                          '30s_critical_power','1m_critical_power','2m_critical_power','3m_critical_power','5m_critical_power','8m_critical_power',
                          '10m_critical_power','20m_critical_power','30m_critical_power','60m_critical_power','time_in_zone_L1','time_in_zone_L2',
                          'time_in_zone_L3','time_in_zone_L4','time_in_zone_L5','time_in_zone_L6','time_in_zone_L7','1s_peak_wpk','5s_peak_wpk',
                          '10s_peak_wpk','15s_peak_wpk','20s_peak_wpk','30s_peak_wpk','1m_peak_wpk','5m_peak_wpk','10m_peak_wpk','20m_peak_wpk',
                          '30m_peak_wpk','60m_peak_wpk')]
coln <- colnames(dt_merged)
coln_len <- length(coln)
# only include rows that have an average power value
dt_merged <- dt_merged[!sapply(dt_merged$average_power, is.null)]
# only include where rides are longer than 60min
dt_merged <- dt_merged[!sapply(dt_merged$'60m_critical_power', is.null)]
dt_merged <- dt_merged[!sapply(dt_merged$'60m_critical_power', is.na)]
# number of rows in datafram
dt_merged_nrow <- nrow(dt_merged)

# Get index of list columns
idx_list <- which(sapply(dt_merged, class)=="list")
# Get first element of each list column

# Tried several variants of data.table syntax in the for loop but it just does not work...
# so we will just use base R syntax and 14 lines for each list column that needs subsetting and type converting
# Leaving the for loop here so its there for the record if you wanna have another go...
# for(i in idx_list){
#   cols <- names(dt_merged)[i]
#   # dt_merged[, .SD, .SDcols=cols] <- unlist(lapply(dt_merged[, .SD, .SDcols=cols], `[[`, 1))
#   # dt_merged[, .SD, .SDcols=cols] <- unlist(sapply(dt_merged[, cols, with=FALSE], `[[`, 1))
#   dt_merged[, i, with=FALSE] <- unlist(sapply(dt_merged[, cols, with=FALSE], `[[`, 1))
# }

dt_merged$average_power <- as.numeric(unlist(lapply(dt_merged$average_power, `[[`, 1)))
dt_merged$nonzero_power <- as.numeric(unlist(lapply(dt_merged$nonzero_power, `[[`, 1)))
dt_merged$coggan_np <- as.numeric(unlist(lapply(dt_merged$coggan_np, `[[`, 1)))
dt_merged$coggan_if <- as.numeric(unlist(lapply(dt_merged$coggan_if, `[[`, 1)))
dt_merged$coggam_variability_index <- as.numeric(unlist(lapply(dt_merged$coggam_variability_index, `[[`, 1)))
# remove rows with "NULL" values
zero_idx <- which(dt_merged$coggan_tssperhour=="NULL")
dt_merged$coggan_tssperhour[zero_idx] <- 0
dt_merged$coggan_tssperhour <- as.numeric(unlist(lapply(dt_merged$coggan_tssperhour, `[[`, 1)))
# class(dt_merged$coggan_tssperhour)
#
dt_merged$average_power <- as.numeric(unlist(lapply(dt_merged$average_power, `[[`, 1)))
dt_merged$nonzero_power <- as.numeric(unlist(lapply(dt_merged$nonzero_power, `[[`, 1)))
# remove rows with "NULL" values
zero_idx <- which(dt_merged$time_in_zone_P1=="NULL")
dt_merged$time_in_zone_P1[zero_idx] <- 0
dt_merged$time_in_zone_P1 <- as.numeric(unlist(lapply(dt_merged$time_in_zone_P1, `[[`, 1)))
# remove rows with "NULL" values
zero_idx <- which(dt_merged$time_in_zone_L1=="NULL")
dt_merged$time_in_zone_L1[zero_idx] <- 0
dt_merged$time_in_zone_L1 <- as.numeric(unlist(lapply(dt_merged$time_in_zone_L1, `[[`, 1)))
# remove rows with "NULL" values
zero_idx <- which(dt_merged$time_in_zone_L2=="NULL")
dt_merged$time_in_zone_L2[zero_idx] <- 0
dt_merged$time_in_zone_L2 <- as.numeric(unlist(lapply(dt_merged$time_in_zone_L2, `[[`, 1)))
# remove rows with "NULL" values
zero_idx <- which(dt_merged$time_in_zone_L3=="NULL")
dt_merged$time_in_zone_L3[zero_idx] <- 0
dt_merged$time_in_zone_L3 <- as.numeric(unlist(lapply(dt_merged$time_in_zone_L3, `[[`, 1)))
# remove rows with "NULL" values
zero_idx <- which(dt_merged$time_in_zone_L4=="NULL")
dt_merged$time_in_zone_L4[zero_idx] <- 0
dt_merged$time_in_zone_L4 <- as.numeric(unlist(lapply(dt_merged$time_in_zone_L4, `[[`, 1)))
# remove rows with "NULL" values
zero_idx <- which(dt_merged$time_in_zone_L5=="NULL")
dt_merged$time_in_zone_L5[zero_idx] <- 0
dt_merged$time_in_zone_L5 <- as.numeric(unlist(lapply(dt_merged$time_in_zone_L5, `[[`, 1)))
# remove rows with "NULL" values
zero_idx <- which(dt_merged$time_in_zone_L6=="NULL")
dt_merged$time_in_zone_L6[zero_idx] <- 0
dt_merged$time_in_zone_L6 <- as.numeric(unlist(lapply(dt_merged$time_in_zone_L6, `[[`, 1)))
# remove rows with "NULL" values
zero_idx <- which(dt_merged$time_in_zone_L7=="NULL")
dt_merged$time_in_zone_L7[zero_idx] <- 0
dt_merged$time_in_zone_L7 <- as.numeric(unlist(lapply(dt_merged$time_in_zone_L7, `[[`, 1)))

idx_list <- which(sapply(dt_merged, class)=="list") # this is empty, implying no more list types

# change column types
dt_merged$ride_count <- as.numeric(dt_merged$ride_count)
dt_merged$workout_time <- as.numeric(dt_merged$workout_time)
dt_merged$time_riding <- as.numeric(dt_merged$time_riding)
dt_merged$athlete_weight <- as.numeric(dt_merged$athlete_weight)
dt_merged$total_work <- as.numeric(dt_merged$total_work)
dt_merged$max_power <- as.numeric(dt_merged$max_power)
dt_merged$cp_setting <- as.numeric(dt_merged$cp_setting)
dt_merged$coggan_tss <- as.numeric(dt_merged$coggan_tss)
dt_merged$'1s_critical_power' <- as.numeric(dt_merged$'1s_critical_power')
dt_merged$'5s_critical_power' <- as.numeric(dt_merged$'5s_critical_power')
dt_merged$'10s_critical_power' <- as.numeric(dt_merged$'10s_critical_power')
dt_merged$'15s_critical_power' <- as.numeric(dt_merged$'15s_critical_power')
dt_merged$'20s_critical_power' <- as.numeric(dt_merged$'20s_critical_power')
dt_merged$'30s_critical_power' <- as.numeric(dt_merged$'30s_critical_power')
dt_merged$'1m_critical_power' <- as.numeric(dt_merged$'1m_critical_power')
dt_merged$'2m_critical_power' <- as.numeric(dt_merged$'2m_critical_power')
dt_merged$'3m_critical_power' <- as.numeric(dt_merged$'3m_critical_power')
dt_merged$'5m_critical_power' <- as.numeric(dt_merged$'5m_critical_power')
dt_merged$'8m_critical_power' <- as.numeric(dt_merged$'8m_critical_power')
dt_merged$'10m_critical_power' <- as.numeric(dt_merged$'10m_critical_power')
dt_merged$'20m_critical_power' <- as.numeric(dt_merged$'20m_critical_power')
dt_merged$'30m_critical_power' <- as.numeric(dt_merged$'30m_critical_power')
dt_merged$'60m_critical_power' <- as.numeric(dt_merged$'60m_critical_power')
dt_merged$time_in_zone_L1 <- as.numeric(dt_merged$time_in_zone_L1)
dt_merged$time_in_zone_L2 <- as.numeric(dt_merged$time_in_zone_L2)
dt_merged$time_in_zone_L3 <- as.numeric(dt_merged$time_in_zone_L3)
dt_merged$time_in_zone_L4 <- as.numeric(dt_merged$time_in_zone_L4)
dt_merged$time_in_zone_L5 <- as.numeric(dt_merged$time_in_zone_L5)
dt_merged$time_in_zone_L6 <- as.numeric(dt_merged$time_in_zone_L6)
dt_merged$time_in_zone_L7 <- as.numeric(dt_merged$time_in_zone_L7)
dt_merged$'1s_peak_wpk' <- as.numeric(dt_merged$'1s_peak_wpk')
dt_merged$'5s_peak_wpk' <- as.numeric(dt_merged$'5s_peak_wpk')
dt_merged$'10s_peak_wpk' <- as.numeric(dt_merged$'10s_peak_wpk')
dt_merged$'15s_peak_wpk' <- as.numeric(dt_merged$'15s_peak_wpk')
dt_merged$'20s_peak_wpk' <- as.numeric(dt_merged$'20s_peak_wpk')
dt_merged$'30s_peak_wpk' <- as.numeric(dt_merged$'30s_peak_wpk')
dt_merged$'1m_peak_wpk' <- as.numeric(dt_merged$'1m_peak_wpk')
dt_merged$'5m_peak_wpk' <- as.numeric(dt_merged$'5m_peak_wpk')
dt_merged$'10m_peak_wpk' <- as.numeric(dt_merged$'10m_peak_wpk')
dt_merged$'20m_peak_wpk' <- as.numeric(dt_merged$'20m_peak_wpk')
dt_merged$'30m_peak_wpk' <- as.numeric(dt_merged$'30m_peak_wpk')
dt_merged$'60m_peak_wpk' <- as.numeric(dt_merged$'60m_peak_wpk')
#class(dt_merged$coggan_tssperhour)
# rename columns that start with a number for BigQuery upload
setnames(dt_merged, old=c('1s_critical_power'), new=c('critical_power_1s'))
setnames(dt_merged, old=c('5s_critical_power'), new=c('critical_power_5s'))
setnames(dt_merged, old=c('10s_critical_power'), new=c('critical_power_10s'))
setnames(dt_merged, old=c('15s_critical_power'), new=c('critical_power_15s'))
setnames(dt_merged, old=c('20s_critical_power'), new=c('critical_power_20s'))
setnames(dt_merged, old=c('30s_critical_power'), new=c('critical_power_30s'))
setnames(dt_merged, old=c('1m_critical_power'), new=c('critical_power_1m'))
setnames(dt_merged, old=c('2m_critical_power'), new=c('critical_power_2m'))
setnames(dt_merged, old=c('3m_critical_power'), new=c('critical_power_3m'))
setnames(dt_merged, old=c('5m_critical_power'), new=c('critical_power_5m'))
setnames(dt_merged, old=c('8m_critical_power'), new=c('critical_power_8m'))
setnames(dt_merged, old=c('10m_critical_power'), new=c('critical_power_10m'))
setnames(dt_merged, old=c('20m_critical_power'), new=c('critical_power_20m'))
setnames(dt_merged, old=c('30m_critical_power'), new=c('critical_power_30m'))
setnames(dt_merged, old=c('60m_critical_power'), new=c('critical_power_60m'))
setnames(dt_merged, old=c('1s_peak_wpk'), new=c('peak_wpk_1s'))
setnames(dt_merged, old=c('5s_peak_wpk'), new=c('peak_wpk_5s'))
setnames(dt_merged, old=c('10s_peak_wpk'), new=c('peak_wpk_10s'))
setnames(dt_merged, old=c('15s_peak_wpk'), new=c('peak_wpk_15s'))
setnames(dt_merged, old=c('20s_peak_wpk'), new=c('peak_wpk_20s'))
setnames(dt_merged, old=c('30s_peak_wpk'), new=c('peak_wpk_30s'))
setnames(dt_merged, old=c('1m_peak_wpk'), new=c('peak_wpk_1m'))
setnames(dt_merged, old=c('5m_peak_wpk'), new=c('peak_wpk_5m'))
setnames(dt_merged, old=c('10m_peak_wpk'), new=c('peak_wpk_10m'))
setnames(dt_merged, old=c('20m_peak_wpk'), new=c('peak_wpk_20m'))
setnames(dt_merged, old=c('30m_peak_wpk'), new=c('peak_wpk_30m'))
setnames(dt_merged, old=c('60m_peak_wpk'), new=c('peak_wpk_60m'))
t2 <- Sys.time()
print('Time to clean data')
print(t2-t1)

