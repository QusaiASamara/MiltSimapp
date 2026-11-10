
pk_summary <- function(data_stat,treatment_duration,range_type) {
  
  data_tot <- data_stat %>%
    mutate(TIME = TIME/24, AUC = AUC/24, TEC90 = TEC90/24, Tmax = Tmax/24)
  
  data_child <- data_stat %>%
    mutate(TIME = TIME/24, AUC = AUC/24, TEC90 = TEC90/24, Tmax = Tmax/24) %>%
    filter(AGE < 12)
  
  data_adult <- data_stat %>%
    mutate(TIME = TIME/24, AUC = AUC/24, TEC90 = TEC90/24, Tmax = Tmax/24) %>%
    filter(AGE >= 12)
  
  n_children <- n_distinct(data_child$ID)
  n_adults <- n_distinct(data_adult$ID)
  n_total <- n_distinct(data_tot$ID)

  calculate_metrics <- function(data, treatment_duration) {
    
  # Summarize max data
  Cmax <- data %>%
    group_by(ID) %>%
    summarise(
      C_max = max(Cmax, na.rm = TRUE)) %>% # ug/ml
    ungroup() %>%
    summarise(
      Median = round(median(C_max, na.rm = TRUE),2),
      IQR_low = round(quantile(C_max,ifelse(range_type=="perc_95",0.05, 0.25), na.rm = TRUE),2),
      IQR_upp = round(quantile(C_max, ifelse(range_type=="perc_95",0.95, 0.75), na.rm = TRUE),2)) %>% 
    mutate(Metric = "Cmax (mg/L)") %>%
    as.data.frame()

  
  Tmax <- data %>%
    group_by(ID) %>%
    summarise(
      T_max = max(Tmax, na.rm = TRUE)) %>% 
    ungroup() %>%
    summarise(
      Median = round(median(T_max, na.rm = TRUE),2),
      IQR_low = round(quantile(T_max, ifelse(range_type=="perc_95",0.05, 0.25), na.rm = TRUE),2),
      IQR_upp = round(quantile(T_max, ifelse(range_type=="perc_95",0.95, 0.75), na.rm = TRUE),2)) %>% 
    mutate(Metric = "Tmax (days)")
  
  # Summarize AUC0_D14
  AUC0_D14 <- data %>%
    group_by(TIME) %>%
    filter(TIME == 14) %>%
    summarise(
      Median = round(median(AUC, na.rm = TRUE),2),  # mg*day/L
      IQR_low = round(quantile(AUC, ifelse(range_type=="perc_95",0.05, 0.25), na.rm = TRUE),2),
      IQR_upp = round(quantile(AUC, ifelse(range_type=="perc_95",0.95, 0.75), na.rm = TRUE),2)) %>%
    mutate(Metric = "AUC0_D14 (mg*day/L)") 
  
  # Summarize AUC0_Dlast
  AUC0_Dlast <- data %>%
    group_by(TIME) %>%
    filter(TIME == treatment_duration) %>%
    summarise(
      Median = round(median(AUC, na.rm = TRUE),2),  # mg*day/L
      IQR_low = round(quantile(AUC, ifelse(range_type=="perc_95",0.05, 0.25), na.rm = TRUE),2),
      IQR_upp = round(quantile(AUC, ifelse(range_type=="perc_95",0.95, 0.75), na.rm = TRUE),2)) %>%
    mutate(Metric = "AUC0_EOT (mg*day/L)")

  
  TOEC_90_Dlast <- data %>%
    group_by(ID) %>%
    filter(TIME == max(TIME)) %>%
    ungroup()%>%
    summarise(
      Median = round(median(TEC90, na.rm = TRUE),2),  # mg*day/L
      IQR_low = round(quantile(TEC90, ifelse(range_type=="perc_95",0.05, 0.25), na.rm = TRUE),2),
      IQR_upp = round(quantile(TEC90, ifelse(range_type=="perc_95",0.95, 0.75), na.rm = TRUE),2)) %>%
    mutate(Metric = "T>EC90 (days)")
  
    
    TEC90 <- data %>%
      group_by(ID) %>%
      filter(CONC_CENT >= 10.6) %>%  
      summarize(tec90 = min(TIME)) %>%
      summarise(
        Median = round(median(tec90, na.rm = TRUE),2),  # mg*day/L
        IQR_low = round(quantile(tec90, ifelse(range_type=="perc_95",0.05, 0.25), na.rm = TRUE),2),
        IQR_upp = round(quantile(tec90, ifelse(range_type=="perc_95",0.95, 0.75), na.rm = TRUE),2)) %>%
      mutate(Metric = "TEC90 (days)")
    
  
  
  # Combine all summaries into one data frame
  combined_data <- bind_rows(Cmax,Tmax, AUC0_D14, AUC0_Dlast, TOEC_90_Dlast, TEC90) %>% 
    mutate(
      Value = paste0(Median, " (", IQR_low, " - ", IQR_upp, ")")
    ) %>%
    dplyr::select(Metric, Value)
  
  # Print the combined data frame
  return(combined_data)
  }
  child_summary <- calculate_metrics(data_child, treatment_duration)
  adult_summary <- calculate_metrics(data_adult, treatment_duration)
  total_summary <- calculate_metrics(data_tot, treatment_duration)
  
  # Merge summaries into a single table
  result_table <- child_summary %>%
    rename( children = Value) %>%
    left_join(adult_summary %>% rename(adults = Value), by = "Metric") %>%
    left_join(total_summary %>% rename(total = Value), by = "Metric")
  
  colnames(result_table) <- c(
    "Parameters",
    paste0("Children (", n_children, ")"),
    paste0("Adults (", n_adults, ")"),
    paste0("Total (", n_total, ")")
  )
  
  return(result_table)
}


  