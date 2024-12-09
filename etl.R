library(dplyr)
library(data.table)
library(duckdb)
library(dbplyr)
library(purrr)
library(vetiver)


init_db <- function(do=FALSE) {
  if (do==TRUE) {
    duck_dir <- './input/insurance.duckdb'
    drv <- duckdb(duck_dir)
    
    file_names <- list('train'='./input/train.csv',
                       'test' ='./input/test.csv',
                       'sample' = './input/sample_submission.csv')
    con <- dbConnect(drv)
    
    file_names |> iwalk(\(file_name, table_name) duckdb::duckdb_read_csv(con,table_name, file_name)) 
    dbDisconnect(con)
    print('transform csv files into duckdb')
  }
  
}



get_duck_con <- function() {
  duck_dir <- './input/insurance.duckdb'
  drv <- duckdb(duck_dir,read_only=TRUE)
  con <- dbConnect(drv)
  
}

get_train <- function(){
  file_name <- './input/train.csv'
  con <- get_duck_con()
  ddb <-
    dbReadTable(con,'train')|> 
    mutate(`Policy.Start.Date`=ymd_hms(`Policy.Start.Date`))
  
  dbDisconnect(con)
  return(ddb)
}

get_sample <- function(){
  file_name <- './input/sample_submission.csv'
  con <- get_duck_con()
  ddb <- dbReadTable(con,'sample')
  dbDisconnect(con)
  return(ddb)
}

get_test <- function(){
  file_name <- './input/test.csv'
  con <- get_duck_con()
  ddb <- dbReadTable(con,'test')
  dbDisconnect(con)
  return(ddb)
  
}
