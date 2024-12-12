library(dplyr)
library(data.table)
library(duckdb)
library(dbplyr)
library(purrr)
library(vetiver)
library(lubridate)

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
  return(con)
}

get_train <- function(){
  con <- get_duck_con()
  ddb <-
    tbl(con,'train') |>
    mutate(Policy.Start.Date = as_datetime(Policy.Start.Date))
    #dbReadTable(con,'train_t')|> 
  
  #dbDisconnect(con)
  return(ddb)
}

get_test <- function(){
  con <- get_duck_con()
  ddb <- 
    tbl(con,'test')|>
    mutate(Policy.Start.Date=as_datetime(Policy.Start.Date))
  
   # dbReadTable(con,'test') |>
  #dbDisconnect(con)
  return(ddb)
  
}

get_sample <- function(){
  file_name <- './input/sample_submission.csv'
  con <- get_duck_con()
  ddb <- 
    tbl(con,'sample')
   # dbReadTable(con,'sample')
  #dbDisconnect(con)
  return(ddb)
}

get_colname_by_type <- function(type='numeric'){
  sample_df <- get_train() |> head(100) |>collect()
  
  sample_df_filted <-
    switch(type,
           'numeric'=sample_df|> select(where(is.numeric)),
           'character'=sample_df|> select(where(is.character)),
           'datetime'=sample_df|> select(where(is.POSIXt))
           )
  colnames <- sample_df_filted |> colnames()
  return(colnames)
}
