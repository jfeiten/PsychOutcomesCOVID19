dt_root <- read.csv("data/data_t0.tsv", sep = "\t", header = TRUE, encoding = "UTF-8", na.strings = "")
features_df <- read.csv("data/labels_features_t0.tsv", sep = "\t", header = TRUE, encoding = "UTF-8")

pMiss <- function(x){sum(is.na(x))/length(x)*100}
aMiss <- function(x){sum(is.na(x))}

p_rows <- apply(dt_root, 1, pMiss)

# Exclusao de linhas com 90% de missing
dt <- dt_root[p_rows <= 90, ]

p_cols <- apply(dt, 2, pMiss)
names(p_cols) <- features_df$Feature_code
p_cols


# Correcao dos dados das perguntas de sofrimento mental
dt_coded <- dt
colnames(dt_coded) <- names(p_cols)

sf_data <- dt_coded[, grep("sf", colnames(dt_coded))]
str(sf_data)

sf_data <- map_df(sf_data, function(x){levels(x) <- c(0, 1); x <- as.numeric(as.character(x))})
map(sf_data, table)

sf_data_list <- list()

sf_data_list[[1]] <- sf_data[, 1:20]
sf_data_list[[2]] <- sf_data[, 21:40]
sf_data_list[[3]] <- sf_data[, 41:60]
sf_data_list[[4]] <- sf_data[, 61:80]

MakeDatasets <- function(x, sf_list){
  df <- data.frame(sf_list[[1]][, x],
                   sf_list[[2]][, x],
                   sf_list[[3]][, x],
                   sf_list[[4]][, x])
  
  df
}

sf_datasets <- map(c(1:20), sf_list = sf_data_list, MakeDatasets)
names(sf_datasets) <- paste0("sf", 1:20)

MergeColumns <- function(df){
  
  SumFeatures <- function(x, ...){
    l <- length(x[is.na(x)]) 
    if (l == length(x)) y <- NA else y <- sum(x[!is.na(x)])
    return(y)
  }
  
  y <- apply(df, 1, SumFeatures)
  return(y)
}


sf_new_dataset <- map_df(sf_datasets, MergeColumns)
sf_new_colnames <- as.character(features_df$Feature_label[grepl("sfb", features_df$Feature_code)])
sf_new_colnames
colnames(sf_new_dataset) <- sf_new_colnames
View(sf_new_dataset)

summary(sf_new_dataset)

other_vars <- grepl("v", features_df$Feature_code)
other_vars

dt_new <- data.frame(dt[, other_vars])
colnames(dt_new) <- features_df$Feature_label[other_vars]

dt_new <- data.frame(dt_new, sf_new_dataset)
View(dt_new)
d3heatmap::d3heatmap(sf_new_dataset[complete.cases(sf_new_dataset), ], colors = "Reds")

library(lubridate)
timestamp <- mdy_hms(dt_new$Timestamp)
time_since <- timestamp - timestamp[1]
ndays <- seconds_to_period(time_since)
ndays <- day(ndays)
tb <- as.data.frame(table(ndays))
tb

dt_new <- data.frame(ndays, dt_new)
dt_new$Timestamp[which(ndays == 57)][1]

date_record <- lubridate::date(timestamp)
dt_new$date_record <- date_record

library(dplyr)
dt_new_by_date <- dt_new %>% group_by(date_record) %>% tally()

library(ggplot2)
p <- ggplot(data=dt_new_by_date %>% filter(date_record >= "2020-06-12"), 
            aes(x=date_record, y=n)) +
  geom_bar(stat="identity") + coord_flip()

p

save.image(file = "session/analysis_17062020")
