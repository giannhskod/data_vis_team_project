###################################################################
# DEPENDENCIES and GLOBALS                                      ###
###################################################################

# Install required packages if not already installed
chooseCRANmirror(graphics = TRUE, ind = c(1, 2, 3, 4, 5))
knitr::opts_chunk$set(echo = TRUE)

list.of.packages = c(
  "dplyr",
  "dbplyr",
  "ggplot2",
  "kableExtra",
  "readr",
  "RSQLite",
  "gganimate",
  "tidyr",
  "sf",
  "shiny",
  "stringr",
  "data.table",
  "hrbrthemes",
  "alluvial"
)
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

if (length(new.packages)) {
  install.packages(new.packages, dependencies = TRUE)
}

suppressMessages(library("data.table"))
suppressMessages(library("DBI"))
suppressMessages(library("dplyr"))
suppressMessages(library("dbplyr"))
suppressMessages(library("ggplot2"))
suppressMessages(library("hrbrthemes"))
suppressMessages(library("tidyr"))
suppressMessages(library("kableExtra"))
suppressMessages(library("lubridate"))
suppressMessages(library("readr"))
suppressMessages(library("DBI"))
suppressMessages(library("gganimate"))
suppressMessages(library("sf"))
suppressMessages(library("shiny"))
suppressMessages(library("stringr"))
suppressMessages(library("hrbrthemes"))

theme = theme_ipsum()
data_src_dir = paste(getwd(), "/data_src/", sep = "")

#' Function for ingesting a .csv file from the projectes data source folder.
#'
#' @param: .csv file path
#' @return: data.frame
ingest_one_csv = function(csv_file_name, enc = "UTF-8") {
  df = read.csv(paste(data_src_dir, csv_file_name, sep = ""), encoding = enc)
  return(df)
}

#' Function for ingesting a .rds file.
#'
#' @param: .rds file path
#' @return: data.frame
ingest_one_rds = function(file_path) {
  df = readRDS(file_path)
  
  return(df)
}

#' Function for ingesting and concatenating .csv files in a directory.
#'
#' @param: .csv files directory
#' @return: data.frame
ingest_all_csv = function(path) {
  df = list.files(path = path,
                  ,
                  pattern = ".csv",
                  full.names = TRUE) %>%
    lapply(read.csv) %>%
    bind_rows
  return(df)
}

#' Function for ingesting and concatenating .rds files in a directory.
#'
#' @param: .rds files directory
#' @return: data.frame
ingest_all_rds = function(path) {
  file_list = list.files(path = path, pattern = ".rds")
  df = unlist(lapply(file_list, readRDS))
  
  return(df)
}

#' Insert / append to a SQLLite table.
#'
#' @param: dataframe
#' @param: SQLLite table name
#' @param: mode
#' @return: dataframe
df_to_sqlite = function(df,
                        db.name = "db.sqlite",
                        table.name,
                        mode = "append") {
  db = dbConnect(RSQLite::SQLite(), db.name)
  
  if (mode == "append") {
    dbWriteTable(db, table.name, df, append = TRUE)
  }
  
  if (mode == "overwrite") {
    dbWriteTable(db, table.name, df, overwrite = TRUE)
  }
  
  dbDisconnect(db)
  
  return(db)
}

#' Retrieves a SQLite query in a data.frame format.
#'
#' @param: database name
#' @param: SQL query
#' @return: data.frame
sqlite_to_df = function(db.name = "db.sqlite", query) {
  df = NULL
  
  # Connect and fetch.
  out = tryCatch({
    # Connect to db.
    db = dbConnect(RSQLite::SQLite(), db.name)
    
    # Get result set.
    result_set = dbSendQuery(db, query)
    
    # To data.frame.
    df = fetch(result_set)
    
    return(df)
  },
  error = function(e) {
    message("Exception: ")
    message(e)
    
    return(NULL)
  },
  warning = function(e) {
    message("Warning: ")
    message(e)
    
    return(NULL)
  })
  
  return(out)
}