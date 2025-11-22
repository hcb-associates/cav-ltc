test <- ltc_long |>
  filter(substr(practice_code, 1, 1) == "W") |> 
  filter(lhb == 'Cardiff and Vale University Health Board ') |> 
  summarise(median = median(prevalence, na.rm = TRUE)
            , .by = c(ltc , cluster)
            ) |> 
  filter(ltc %in% c('Chronic.obstructive.pulmonary.disease', 'Secondary.prevention.of.coronary.heart.disease', 'Hypertension', 'Diabetes.mellitus..patients.aged.17..', 'Atrial.fibrillation', 'Heart.failure'))

  
  ltc_long |> distinct(lhb)
  
  
  
test |> filter(cluster == 'City & Cardiff South ')
  
  
  
  test2 <- final_df |>
    filter(substr(practice_code, 1, 1) == "W") |>
    filter(lhb == 'Cardiff and Vale University Health Board ') |>
    summarise(median_prev = median(prevalence, na.rm = TRUE)
              ,median_model = median(model_prev, na.rm = TRUE)
              , .by = c(ltc , cluster)
    )
  
  

  
  test2 |> filter(cluster == 'City & Cardiff South ')
  
  
  
  test3 <- growth_cluster_df |>
    # filter(substr(practice_code, 1, 1) == "W") |>
    # filter(lhb == 'Cardiff and Vale University Health Board ') |>
    summarise(median_prev = median(prevalence, na.rm = TRUE)
              ,median_model = median(model_prev, na.rm = TRUE)
              , .by = c(ltc , cluster)
    )
  
  
  
  test3 |> filter(cluster == 'Cardiff South West ')
  
  
  growth_cluster_df_summary |> filter(cluster == 'City & Cardiff South ')
  
  
  growth_cluster_df_summary |> write_csv('growth_cluster_df_summary_original.csv')
  