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
  data$group[data$group == "multilang-lt-n-a"] <- "Control"
  data$group[data$group == "multilang-lt-n-b"] <- "<1"
  data$group[data$group == "multilang-lt-n-c"] <- "<3"
  
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
  
  # Function for performing necessary analysis
  analyse_ab_data <- function(dataset, groups, field){
    
    output <- list()
    
    # High-level data
    high_level <- dataset[dataset$group %in% groups, j = sum(events), by = c(field, "group")]
    high_level <- high_level[order(high_level[,1, with = FALSE], high_level$group, decreasing = T),]
    
    # Generate count table
    count_table <- matrix(data = high_level$V1, nrow = 2, byrow = FALSE,
                          dimnames=list("Group" = unique(high_level$group),
                                        "Outcome" = c("Results", "Zero results")))
    
    # Write out prop_diff_tail and relative_risk
    output$high_level_prop_diff <- ci_prop_diff_tail(count_table)
    output$high_level_risk <- ci_relative_risk(count_table)
    
    # Split by web and API, if applicable
    api_data <- dataset[dataset$source == "api",]
    if(nrow(api_data)){
      api_data <- api_data[api_data$group %in% groups, j = sum(events), by = c(field, "group")]
      api_data <- api_data[order(api_data[,1, with = FALSE], api_data$group, decreasing = T),]
      count_table <- matrix(data = api_data$V1, nrow = 2, byrow = FALSE,
                            dimnames=list("Group" = unique(api_data$group),
                                          "Outcome" = c("Results", "Zero results")))
      output$api_prop_diff <- ci_prop_diff_tail(count_table)
      output$api_risk <- ci_prop_diff_tail(count_table)
    }
    
    web_data <- dataset[dataset$source == "web",]
    if(nrow(web_data)){
      web_data <- web_data[web_data$group %in% groups, j = sum(events), by = c(field, "group")]
      web_data <- web_data[order(web_data[,1, with = FALSE], web_data$group, decreasing = T),]
      count_table <- matrix(data = web_data$V1, nrow = 2, byrow = FALSE,
                            dimnames=list("Group" = unique(web_data$group),
                                          "Outcome" = c("Results", "Zero results")))
      output$web_prop_diff <- ci_prop_diff_tail(count_table)
      output$web_risk <- ci_prop_diff_tail(count_table)
    }
    
    return(output)
  }
  
  # basic analysis of the <1 data
  non_zero_basic_results <- analyse_ab_data(data, groups = c("Control", "<1"), field = "non_zero")
  
#   $high_level_prop_diff
#   [1] -0.02949650 -0.02884083
#   
#   $high_level_risk
#   [1] 0.964146 1.061427
#   
#   $api_prop_diff
#   [1] -0.0342201 -0.0334806
#   
#   $api_risk
#   [1] -0.03422089 -0.03348276
#   
#   $web_prop_diff
#   [1] -0.01259161 -0.01114163
#   
#   $web_risk
#   [1] -0.01259145 -0.01114222
#   

  # basic analysis of the <1 data that hits the langdetect parameter
  langdetect_data <- data[data$langdetect == TRUE,]
  non_zero_langdetect_results <- analyse_ab_data(langdetect_data, groups = c("Control", "<1"), field = "non_zero")
  
#   $high_level_prop_diff
#   [1] 0.224984 0.227874
#   
#   $high_level_risk
#   [1] 3.541259 3.614576
#   
#   $web_prop_diff
#   [1] 0.2249853 0.2278746
#   
#   $web_risk
#   [1] 0.2249846 0.2278656
  
  # Okay, basic analysis of <3 data
  lessthan_basic_results <- analyse_ab_data(data, groups = c("Control", "<3"), field = "more_than_3")
  
#   $high_level_prop_diff
#   [1] 0.01170693 0.01244354
#   
#   $high_level_risk
#   [1] 1.016063 1.017077
#   
#   $api_prop_diff
#   [1] 0.0239356 0.0247683
#   
#   $api_risk
#   [1] 0.02393818 0.02476878
#   
#   $web_prop_diff
#   [1] -0.03305317 -0.03145465
#   
#   $web_risk
#   [1] -0.03304728 -0.03145821
  
  # Langdetect only
  lessthan_langdetect_results <- analyse_ab_data(langdetect_data, groups = c("Control", "<3"), field = "more_than_3")
  
#   $high_level_prop_diff
#   [1] -0.10158554 -0.09299385
#   
#   $high_level_risk
#   [1] 0.03258774 0.03211152
#   
#   $web_prop_diff
#   [1] -0.10158554 -0.09303557
#   
#   $web_risk
#   [1] -0.10158554 -0.09302172
  
  # Write out to be safe
  save(list = ls(pattern = "_results"), file = "results.RData")
}
