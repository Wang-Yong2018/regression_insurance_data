library(dplyr)
library(data.table)
library(duckdb)
library(dbplyr)
library(purrr)
library(vetiver)
library(lubridate)
library(moments)
library(memoise)

cm <- cachem::cache_mem(max_size = 500*2**20)



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

get_train_old <- function(){
  con <- get_duck_con()
  ddb <-
    tbl(con,'train') |>
    mutate(Policy.Start.Date = as_datetime(Policy.Start.Date),
           year = year(Policy.Start.Date),
           month= month(Policy.Start.Date),
           wday =wday(Policy.Start.Date),
           second=second(Policy.Start.Date),
           minute = minute(Policy.Start.Date),
           hour=hour(Policy.Start.Date),
           week=week(Policy.Start.Date))
    #dbReadTable(con,'train_t')|> 
  
  #dbDisconnect(con)
  return(ddb)
}

get_train <- function(){
  con <- get_duck_con()
  ddb <-
    tbl(con,'train') |>
    mutate(
      microsecond = as.numeric(substring(Policy.Start.Date,20,29)),
      Policy.Start.Date = as_datetime(Policy.Start.Date),
      year = year(Policy.Start.Date),
      month= month(Policy.Start.Date),
      wday =wday(Policy.Start.Date),
      #second=second(Policy.Start.Date),
      #minute = minute(Policy.Start.Date),
     #hour=hour(Policy.Start.Date),
      week=week(Policy.Start.Date))
  #dbReadTable(con,'train_t')|> 
  
  #dbDisconnect(con)
  return(ddb)
}
get_test <- function(){
  con <- get_duck_con()
  ddb <- 
    tbl(con,'test')|>
    mutate(
      microsecond = as.numeric(substring(Policy.Start.Date,20,29)),
      Policy.Start.Date = as_datetime(Policy.Start.Date),
      year = year(Policy.Start.Date),
      month= month(Policy.Start.Date),
      wday =wday(Policy.Start.Date),
      #second=second(Policy.Start.Date),
      #minute = minute(Policy.Start.Date),
      #hour=hour(Policy.Start.Date),
      week=week(Policy.Start.Date))
  
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



get_cut_pct <- function(df) {
  
  income_pct <- df |> pull(Annual.Income) |> quantile(
    x = _,
    probs = seq(0, 1, 0.05),
    na.rm = TRUE,
    include.lowest = T
  )
  age_pct <- df|> pull(Age) |> quantile(
    x = _,
    probs = seq(0, 1, 0.1),
    na.rm = TRUE,
    include.lowest = T
  )
  microsecond_pct <- df|>pull(microsecond)|> quantile(
    x=_, 
    probs=seq(0,1,0.2),
    na.rm = TRUE,
    include.lowest = T
  )
  date_pct <- df|>pull(Policy.Start.Date)|> quantile(
    x=_, 
    probs=seq(0,1,0.05),
    na.rm = TRUE,
    include.lowest = T
  )
  Credit_pct <-df |>pull(Credit.Score)|> quantile(
    x=_, 
    probs=seq(0,1,0.1),
    na.rm = TRUE,
    include.lowest = T
  )
  Health_pct <- df|>pull(Health.Score)|> quantile(
    x=_, 
    probs=seq(0,1,0.1),
    na.rm = TRUE,
    include.lowest = T
  )
  
  result<-list('income_pct'=income_pct,
               'age_pct'=age_pct,
               'microsecond_pct'=microsecond_pct,
               'date_pct'=date_pct,
               'Credit_pct'=Credit_pct,
               'Health_pct'=Health_pct)
  return(result) 
}

internal_get_grp_feature<- function(df){
  grp_col <-c('Gender','Marital.Status',
              'Number.of.Dependents', 'Education.Level',
              'Occupation','Location', 'Policy.Type',
              'Smoking.Status',
              'Exercise.Frequency',
              'Property.Type' )
  result_pct <- df|>get_cut_pct() 
  tmp_df <- df|>
    collect()|>
    mutate( Annual.Income_cut = cut(Annual.Income,  breaks = result_pct$income_pct, include.lowest=TRUE),
            Credit.Score_cut = cut(Credit.Score,  breaks = result_pct$Credit_pct,  include.lowest=TRUE),
            Health.Score_cut = cut(Health.Score,  breaks = result_pct$Health_pct,  include.lowest=TRUE),
            Age_cut = cut(Age,  breaks = result_pct$age_pct,  include.lowest=TRUE)
            
    )
  tmp_dt <- tmp_df|>as.data.table()
  tmp_dt[, missing_count := rowSums(is.na(.SD))]
  missing_cnt_df <- tmp_dt|>select(id,missing_count)|>as_tibble()
  rm(tmp_dt)
  
  grp_df <-
    tmp_df |>
    group_by(across(all_of(grp_col)),.drop=TRUE ) |>
    summarise(
      cnt=n(),
      mean_income=mean(Annual.Income,na.rm=TRUE),
      mean_age=mean(Age,na.rm=TRUE),
      #min_premium = mean(Premium.Amount),
      #max_premium = max(Premium.Amount),
      #sd_premium =  coalesce(sd(Premium.Amount),0),
      ids=list(id),
      .groups = 'drop') |>
    ungroup()|>
    select(-c(grp_col))|>
    unnest_longer(col=ids,values_to='id',indices_to='id_seq')|>
    arrange(id)
  
  grp_df <- grp_df |>
    left_join(missing_cnt_df, by=c('id'))
  
  return(grp_df)
}
get_grp_feature <- memoise(internal_get_grp_feature, cache = cm)


get_prepared_df <- function(df){
  
} 


internal_get_weekly_premium <- function(){

  weekly_df <-
    get_train()|>
    mutate(date=Policy.Start.Date, year=year(date),month=month(date),week=week(date),wday=wday(date))|>
    select(date, year, week,wday, Premium.Amount)|>
    collect() |>
    group_by(year,week,wday) |>
    summarize(
      avg=mean(Premium.Amount,na.rm=TRUE),
      max=max(Premium.Amount,na.rm=TRUE),
      min=min(Premium.Amount,na.rm=TRUE),
      sd=sd(Premium.Amount,na.rm=TRUE),
      median=median(Premium.Amount,na.rm=TRUE),
      skewness=skewness(Premium.Amount,na.rm=TRUE),
      kurtosis=kurtosis(Premium.Amount,na.rm=TRUE),
      weekly_cnt=n(),
      first=first(Premium.Amount,na.rm=TRUE),
      last=last(Premium.Amount,na.rm=TRUE),
      .groups='drop')
 return(weekly_df) 
}
get_weekly_premium <- memoise(internal_get_weekly_premium, cache = cm)


get_enrich_weekly_premium<-function(df){
  weekly_premium_df <- get_weekly_premium()
  weekly_id_df <- 
    df |>   
    mutate(date=Policy.Start.Date, year=year(date),month=month(date),week=week(date),wday=wday(date))|>
    select(id, date, year, week,wday) |>
    collect()|>
    left_join(weekly_premium_df,by=c('year','week','wday'))|>
    select(-c('year','week','wday','date'))
  return(weekly_id_df)
}

get_enrich_df <- function(input_df){
  df <- input_df
  
  result_pct <- df|> get_cut_pct()
  grp_col <- df |>get_grp_feature()
  
  weekly_premium_df <- df|>get_enrich_weekly_premium()
  
  enriched_df <-
    df |> 
    rename(date = `Policy.Start.Date`) |>
    collect()|>
    left_join(grp_col,by='id')|>
    left_join(weekly_premium_df,by='id')
  
  if ('Premium.Amount' %in% names(enriched_df)) {
    enriched_df <- enriched_df |>
      rename(truth=Premium.Amount)
  }
  
 return(enriched_df ) 
}

get_rcp <-function(df){
  
}
  