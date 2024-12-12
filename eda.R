source('etl.R')
nominal_col <- get_colname_by_type('character')
income_pct <- get_train() |> pull(Annual.Income) |>quantile(x=_,probs=seq(0,1,0.2),na.rm=TRUE,include.lowest=T)
age_pct <- get_train() |> pull(Age) |>quantile(x=_,probs=seq(0,1,0.2),na.rm=TRUE,include.lowest=T)
grp_price_df <-
  get_train() |>
  rename(date = Policy.Start.Date,
         price= Premium.Amount)|>
  mutate(income_tag=cut(Annual.Income,breaks=income_pct)) |> 
  mutate(age_tag=cut(Age,breaks=age_pct)) |> 
  group_by(across(all_of(c(nominal_col,
                           'income_tag','age_tag', 'Number.of.Dependents',
                           'Vehicle.Age',	'Insurance.Duration'))))|>
  window_order(date)|>
  summarize(
    min=min(price),
    max=max(price),
    mean=mean(price),
    sd= sd(price),
    cnt= n(),
    list_price=list(price)
  )|>
  collect()
 
