library(tidyverse)
library(lubridate)
library(openxlsx)
library(readxl)
library(rpivotTable)
library(cleaninginspectoR)
library(rio)
library(koboquest)
# Log issues function
issue_log <- function(data, question, issue = NULL, action = NULL){
  cleaning_log <- data.frame("uuid" = as.character(),
                             "region" = as.character(),
                             "district" = as.character(),
                             "today" = as.character(),
                             "enumerator" = as.character(),
                             "question" = as.character(),
                             "issue" = as.character(),
                             "old_value" = as.character(),
                             "new_value" = as.character(),
                             "Reason" = as.character(),
                             "action" = as.character(),
                             "ki_contact"=as.character(),
                             stringsAsFactors = F)
  
  if(nrow(data) > 0){
    for (a in 1:nrow(data)) {
      cleaning_log <- cleaning_log %>% 
        add_row(
          tibble_row(
            uuid = as.character(data[a, "uuid"]),
            today = as.character(data[a, "today"]),
            region = as.character(data[a, "localisation_region_label"]),
            district = as.character(data[a, "district_name"]),
            enumerator = as.character(data[a, "enum_name"]),
            question = as.character(question),
            issue = as.character(issue),
            old_value = as.character(data[a, question]),
            action = as.character(action),
            ki_contact=as.character(data[a,"ki_contact"])
          )
        )
    }
  }
  return(cleaning_log)
  
} 

# Survey time check function
time_check <- function(df, time_min, time_max){
  df <- df %>% mutate(interview_duration = difftime(as.POSIXct(ymd_hms(end)), as.POSIXct(ymd_hms(start)), units = "mins"),
                      CHECK_interview_duration = case_when(
                        interview_duration < time_min ~ "Too short",
                        interview_duration > time_max ~ "Too long",
                        TRUE ~ "Okay"
                      )
  )
  return(df)
}

# Anomyze dataset
anonymise_dataset <- function(data, variables_to_remove=c()){
  
  data_anon <- data[, !(names(data) %in% variables_to_remove)]
  
  data_anon
}

# FOs distribution
scc_locations <- function(data, coverage){
  clogs <- data <- data %>% 
    filter(region %in% coverage)
}

# cleaning log locations
scc_clog <- function(data, coverage){
  clogs  <- data %>% 
    filter(region %in% coverage)
}

"%!in%" <- Negate("%in%")


# other_options <- df %>% 
#   dplyr::select(ends_with("_other"), "region", "district", "today", "uuid") %>% 
#   pivot_longer(ends_with("_other"),
#                names_to = "question",
#                values_to = "old_value") %>% 
#   filter(!is.na(old_value)) %>% 
#   mutate(issue = "please translate other options and recode them in the respective choice if possible",
#          new_value = "",
#          Reason = "",
#          action = "c") %>% 
#   select("uuid", "region", "district", "today", "question", "issue",
#          "old_value", "new_value","Reason", "action")
# renaming column heads to merge with the clogs
# names(other_options)[names(other_options) == "localisation_region_label"] <- 'region'
# names(other_options)[names(other_options) == "localisation_district_label"] <- 'district'


# other_options_sm <- other_options[which(map_lgl(gsub("_other$","",other_options$question),~questionnaire$question_is_select_multiple(.x))),]
# 
# rmap <- function (.x, .f, ...) {
#   if(is.null(dim(.x))) stop("dim(X) must have a positive length")
#   .x <- t(.x) %>% as.data.frame(.,stringsAsFactors=F)
#   purrr::map(.x=.x,.f=.f,...)
# }
# 
# other_options_sm <- do.call("rbind",
#                             rmap(other_options_sm,~{
#                               question_id = .x[[5]]
#                               question_1 = gsub("_other",".other",question_id)
#                               question_2 = gsub("_other",".[ENTER_OPTION_NAME]",question_id)
#                               
#                               return(data.frame(uuid = rep(.x[[1]],3),
#                                                 region = rep(.x[[2]],3),
#                                                 district = rep(.x[[3]],3),
#                                                 today = rep(.x[[4]],3),
#                                                 question = c(.x[[5]],question_1,question_2),
#                                                 issue = rep(.x[[6]],3),
#                                                 old_value = c(.x[[7]],1,0),
#                                                 new_value = c(.x[[8]],0,1),
#                                                 Reason
#                                                 action = rep(.x[[9]],3),stringsAsFactors = F
#                               ))
#                             })) 
# 
# other_options <-  rbind(other_options_sm,
#                         other_options[which(!map_lgl(gsub("_other$","",other_options$question),~questionnaire$question_is_select_multiple(.x))),])


# function to detect outliers
detect.outliers <- function(df, method="sd", n.sd=3){
  res <- data.frame()
  for (col in colnames(df)[colnames(df)!=c("enum_name","ki_contact")]){
    #define columns for the cleaning log
    
    df.temp <- data.frame(uuid=df$uuid,region=df$region,district=df$district, today=df$today,	enumerator=df$enumerator,
                          new_value = " ", Reason=" ",ki_contact=df$ki_contact,action="c", value=as.numeric(df[[col]])) %>% filter(!is.na(value))
    if (method=="sd-linear"){
      df.temp <- df.temp %>%
        mutate(is.outlier.high=value > mean(value, na.rm=T) + n.sd*sd(value, na.rm=T),
               is.outlier.low=value < mean(value, na.rm=T) - n.sd*sd(value, na.rm=T))
    } else if (method=="sd-log"){
      df.temp <- df.temp %>%
        mutate(col.log=log(value),
               is.outlier.high=col.log > mean(col.log, na.rm=T) + n.sd*sd(col.log, na.rm=T),
               is.outlier.low=col.log < mean(col.log, na.rm=T) - n.sd*sd(col.log, na.rm=T))
    } else if (method=="iqr-linear") {
      df.temp <- df.temp %>%
        mutate(is.outlier.high=value > quantile(value, 0.75) + 1.5*IQR(value),
               is.outlier.low=value < quantile(value, 0.25) - 1.5*IQR(value))
    } else if (method=="iqr-log") {
      df.temp <- df.temp %>%
        mutate(col.log=log(value),
               is.outlier.high=col.log > quantile(col.log, 0.75) + 1.5*IQR(col.log),
               is.outlier.low=col.log < quantile(col.log, 0.25) - 1.5*IQR(col.log))
    } else stop("Method unknown")
    df.temp <- df.temp %>% 
      pivot_longer(c("is.outlier.high", "is.outlier.low"), 
                   names_to="issue", values_to="is.outlier") %>% 
      filter(is.outlier) %>% 
      mutate(question=col, old_value=value,
             issue=ifelse(issue=="is.outlier.high", "High outlier", "Low outlier")) %>% 
      select(uuid, region,district,today,enumerator,question, old_value,new_value,Reason, issue,action,ki_contact)
    res <- rbind(res, df.temp)
  }
  return(res)
}

