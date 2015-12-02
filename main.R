library(readr)
library(data.table)
library(wmf)
library(ggplot2)
library(BCDA)
options(scipen=500)

main <- function(){
  
  # Get
  retrieve_data <- function(){
    
    files <- list.files("/a/mw-log/archive/CirrusSearchUserTesting", pattern = "CirrusSearchUserTesting\\.log-201511", full.names = TRUE)
    dates <- as.numeric(substr(files,71,78))
    files <- files[dates %in% seq(20151121, 20151128, 1)]
    
    results <- lapply(files, function(x){
      file <- tempfile()
      system(paste("gunzip -c ", x, ">", file))
      
      data <- readr::read_tsv(file,
                              col_names = c("date", "group", "queries", "results", "source", "time_taken", "ip",
                                            "user_agent", "query_metadata"),
                              col_types = "ccciciccc")
      file.remove(file)
      data <- as.data.table(data[grepl(x = data$query_metadata, pattern = "full_text", fixed = TRUE), ])
      data <- data[!data$user_agent == "",]
      data$non_zero <- ifelse(data$results < 1, FALSE, TRUE)
      data$more_than_3 <- ifelse(data$results < 3, FALSE, TRUE)
      data$langdetect <- grepl(x = data$query_metadata, pattern = "langdetect")
      data$date <- as.Date(substring(data$date, 0, 10))
      data <- data[,j=list(events=.N), by = c("date","group", "source", "non_zero", "more_than_3", "langdetect")]
      gc()
      return(data)
    })
    return(do.call("rbind", results))
  }
  
  data <- retrieve_data()
  
  # Filter and do exploratory analysis
  data <- data[data$date %in% seq(as.Date("2015-11-21"), as.Date("2015-11-27"), "day"),]
  data$group[data$group == "multilang-it-n-a"] <- "Control"
  data$group[data$group == "multilang-it-n-b"] <- "<1"
  data$group[data$group == "multilang-it-n-c"] <- "<3"
  
  per_group <- data[, j = list(events = sum(events)), by = c("date", "group")]
  ggsave(file = "events_by_group_summary.png",
         plot = ggplot(per_group, aes(date, events, group = group, type = group, colour = group)) + geom_line() +
           theme_fivethirtynine() + labs(title = "Events by day and group, second language switching test",
                                         x = "Date", y = "Events") + expand_limits(y = 0))
  
  per_platform <- data[, j = list(events = sum(events)), by = c("source", "group")]
  ggsave(plot = ggplot(per_platform, aes(source, events, group = group, type = group, fill = group)) + 
           geom_bar(stat = "identity", position = "dodge") + theme_fivethirtynine() +
           labs(title = "Distribution of events per group and source, Second language switching test",
                x = "Source", y = "Events"),
         file = "events_by_source_summary.png")
  
  
}
