library(vetiver)
library(pins)

get_model_board <- function(){
  board_path <- './vetiver/'
  model_board <- board_folder(path = board_path,
                              versioned = TRUE)
  
}

keep_model <- function(mod=NULL) {
  if (is.null(mod)) {
    stop('pls input a fitted mod')
  }
  get_model_board()|>
     vetiver_pin_write(mod) 
}

get_mods_log_residual<- function(mod_name_pattern = NULL) {
  model_board <- get_model_board()
  model_names <-
    model_board |>
    pin_list()|>
    set_names()
  
  if (!is.null(mod_name_pattern)){
    model_names <-
      model_names |>
      keep(\(name) grepl(mod_name_pattern,name))
  }
  
  metric_result <- 
    model_names |> 
    map(\(name)  
      model_board |> pin_meta(name) |>  pluck('user','residuals') |>  as_tibble()|>rename(value=log1p_residual) ) |>
    list_rbind( names_to = "name") #  per column list_cbind,   per vector list_c
  
}


show_model_permformance <- function(metric_name='rmsel'){
  model_board <- get_model_board()
  model_names <-
    model_board |>
    pin_list() |>
    set_names()
  
  metric_result <- 
    model_names |> 
    map(\(name)  
      model_board |>
          pin_meta(name) %>% 
          pluck('user','metrics') %>%
          as_tibble()
    ) %>%
    list_rbind( names_to = "id") #  per column list_cbind,   per vector list_c
  
  # metric_result %>% filter(.metric==metric_name) %>% arrange(.estimate)
  metric_result |>
    select(id,metric_name) |>
    rename(.estimate = metric_name) 
  
}

get_mod <- function(name){
  model_board <- get_model_board()
  vetiver_mod <- 
    model_board|>
    pin_read(name={{name}})
  return(vetiver_mod)
}
# 
# library(vetiver)
# library(pins)
# 
# keep_model <- function(mod=NULL) {
#   if (is.null(mod)) {
#     stop('pls input a fitted mod')
#   }
#   
#   model_board <- board_folder(path = './vetiver',versioned = TRUE)
#   model_board |>
#     vetiver_pin_write(mod) 
#   
# }
# 
# 
# show_model_permformance <- function(metric_name='rmsel'){
#   model_names <-model_board%>% pin_list() %>% set_names()
#   
#   metric_result <- 
#     model_names %>% 
#     map(\(name)  
#         model_board %>%  
#           pin_meta(name) %>% 
#           pluck('user','metrics') %>%
#           as_tibble()
#     ) %>%
#     list_rbind( names_to = "id") #  per column list_cbind,   per vector list_c
#   
#   # metric_result %>% filter(.metric==metric_name) %>% arrange(.estimate)
#   metric_result %>% select(id,.estimate=rmsel) #%>% arrange(.estimate)
#   
# }
