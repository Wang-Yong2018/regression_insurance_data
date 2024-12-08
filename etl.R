library(dplyr)
library(data.table)
library(duckdb)
library(dbplyr)
library(purrr)
library(vetiver)
con <-dbConnect(duckdb())
init_db <- function() {
  file_names <- list('train'='./input/train.csv',
                    'test' ='./input/test.csv',
                    'sample' = './input/sample_submission.csv')
  
 file_names |> iwalk(\(file_name, table_name) duckdb::duckdb_read_csv(con,table_name, file_name)) 
}

get_train <- function(){
  file_name <- './input/train.csv'
  ddb <- dbReadTable(con,'train')
  return(ddb)
}

get_sample <- function(){
  file_name <- './input/sample_submission.csv'
  ddb <- dbReadTable(con,'sample')
  return(ddb)
}

get_test <- function(){
  file_name <- './input/test.csv'
  ddb <- dbReadTable(con,'test')
  return(ddb)
  
}


get_fit_wf <- function(rcp,data,name,wf=lm_wf){
  fit_mod <- wf |>
    add_recipe(rcp)|>
    fit({{data}}) 
  
  result_df <- 
    fit_mod |> 
    augment(new_data = {{data}}) |>
    select(truth,.pred)
  
  rmsel_value <- 
    result_df |>
    rmsel(truth, .pred)|>
    
    pull(.estimate)
  rmse_value <- 
    result_df |>
    rmse(truth, .pred) |>
    pull(.estimate)
  
  rsq_value <- 
    result_df |> 
    rsq(truth, .pred) |>
    pull(.estimate)
  
  vetival_mod <-vetiver_model(model=fit_mod, 
                              model_name = paste0(name) ,
                              metadata = list(metrics=list(rmsel=rmsel_value,
                                              rsq=rsq_value,
                                              rmse=rmse_value)))
        
  return(vetival_mod)
}