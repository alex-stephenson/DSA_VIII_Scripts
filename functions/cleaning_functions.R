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


#------------------------------------------------------------------------------------------------------------------------------------------
# check interview duration

check_int_duration <- function(df_raw, x_uuid="_uuid", audit_dir_path = "", today = "today"){
  # validation and error handling
  if (!today %in% names(df_raw))
    stop(paste0("Column ", today, " doesn't exist in the data set."))
  if (!"start" %in% names(df_raw))
    stop("Column start doesn't exist in the data set.")
  if (!"end" %in% names(df_raw))
    stop("Column end doesn't exist in the data set.")
  if (nrow(df_raw) < 0 || !is.data.frame(df_raw)) 
    stop("df_raw is either empty or it's not type data.frame.")
  
  # if the audit directory is not passed by user, should ignore looking for audit files and it's calculation
  if (!audit_dir_path == "") {
    uuid <- dir(audit_dir_path)
    if (length(uuid) > 0) {
      # Audit Checks
      calculate_duration <- function(audit_dir_path, uuid){
        df <- read.csv(paste0(audit_dir_path, uuid, "/audit.csv"))
        if (all(c("start", "end") %in% names(df))) {
          if (nrow(df) > 0) {
            # df <- df %>% filter(df$event != "group questions")
            df <- df %>% filter(node != "") %>% 
              # filter(event != "group questions") %>% 
              mutate(
                is_duplicate = start == lag(start),
                time = (end - start) / 60000
              ) %>% filter (!is_duplicate) 
            # %>% filter(time < 10.0)
            duration_minutes <- round(sum(df$time))
            # duration_minutes <- round(sum(df$end - df$start)/60000, 1)
            df_duration_calcualted <- data.frame(uuid = uuid, duration_minutes = duration_minutes)
            return(df_duration_calcualted)  
          }
        }
      }
      
      duration_df <- sapply(uuid, calculate_duration, audit_dir_path = audit_dir_path)
      
      if (all(class(duration_df) == "list")) {
        duration_df <- do.call(rbind, duration_df)
      } else if("matrix" %in% class(duration_df)){
        duration_df <- t(duration_df)
        rownames(duration_df) <- 1:nrow(duration_df)
        duration_df <- as.data.frame(duration_df)
        
        duration_df$uuid <- as.character(duration_df$uuid)
        duration_df$duration_minutes <- as.character(duration_df$duration_minutes)
      }
      
    }
  }
  
  #time check based on start end time
  uuid <- df_raw[x_uuid] %>% unlist() %>% unname()
  duration_minutes  <-  round(as.POSIXct(ymd_hms(df_raw$end)) - as.POSIXct(ymd_hms(df_raw$start))) %>% as.numeric()
  df_no_audit_files <- data.frame(uuid, duration_minutes)
  
  if ("duration_df" %in% ls()) {
    df_no_audit_files <- df_no_audit_files[!df_no_audit_files$uuid %in% duration_df$uuid, ]
    # Join Audit checks and main data set
    duration_df_all <- rbind(duration_df, df_no_audit_files)   
  } else {
    duration_df_all <- df_no_audit_files
  }
  
  names(duration_df_all)[names(duration_df_all) == "uuid"] <- x_uuid
  
  # Merging both audit checks
  data_duration_calculated <- df_raw %>%
    left_join(duration_df_all) %>% 
    dplyr:: rename(interview_duration = duration_minutes)
  
  return(data_duration_calculated)
}
