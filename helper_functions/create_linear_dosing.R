  
create_lin_dataset <- function(data, model,weight, seed,  use_loading_dose = FALSE, 
                               fixed_load_dose = NULL, 
                               load_freq = NULL, 
                               load_interval = NULL,
                               main_freq,
                               main_interval,
                               weight_bands = NULL,
                               TSWITCH_val,
                               upper,lower,
                               end, delta, mode, Utility_score_mode, dosage ) {
  
  id_count <- data$ID
  
  ##Adherence
  
  THETA0 <- 3.5293918      # baseline adherence
  THETA1 <- -0.3368967   # pill burden effect
  
  # If loading dose is enabled, create both loading and maintenance doses
  if (use_loading_dose && !is.null(load_freq) && !is.null(load_interval)) {
    loading_dose_addl <- load_freq - 1
    loading_dose_ii <- load_interval
    maintenance_dose_addl <- main_freq - 1
    maintenance_dose_ii <- main_interval
    maintenance_dose_start <- (loading_dose_ii * load_freq)
    treatment_duration <- ((load_freq * load_interval)/24) + ((main_freq * main_interval)/24)
    load_dose <- ev(amt = fixed_load_dose, II = loading_dose_ii, ADDL = loading_dose_addl)
    maintenance_dose <- ev(time = maintenance_dose_start, amt = 0, cmt = 1,
                           II = maintenance_dose_ii, ADDL = maintenance_dose_addl)
    
    dose <- c(load_dose, maintenance_dose)
  } else {
    # Only create maintenance dose
    maintenance_dose_addl <- main_freq - 1
    maintenance_dose_ii <- main_interval
    treatment_duration <- (main_freq * main_interval)/24
    dose <- ev(amt = 0, cmt = 1, II = maintenance_dose_ii, 
               ADDL = maintenance_dose_addl, time = 0)
  }
  
  # Replicate the event for multiple IDs
  multiple_dose_data <- ev_rep(dose, ID = id_count) %>% rename(EVID= evid, TIME=time, 
                                                                 CMT=cmt, AMT = amt)
  
  Sim_data <- data %>% 
    merge(multiple_dose_data, by = "ID", all.x = TRUE)
  
  
  # Create weight band ranges if weight bands are provided
  if (!is.null(weight_bands)) {
    weight_band_cases <- sapply(weight_bands, function(band) {
      sprintf("EVID == 1 & TIME == 0 & FLOORED_WT >= %s & FLOORED_WT < %s ~ %s",
              band$min, band$max, band$dose)
    })
    weight_band_expression <- paste(weight_band_cases, collapse = ",\n")
  }
  
  if (!is.null(weight_bands)) {
    NM_lin <- Sim_data %>%
      mutate(
        FLOORED_WT = floor(WT),
        AMT = eval(parse(text = sprintf("case_when(\n%s,\nTRUE ~ 0\n)", weight_band_expression))),
        AMT = ifelse(EVID == 1 & TIME > 0 , case_when(
          WT >= 45 ~ 150,
          WT >= 30 & WT < 45 ~ 100,
          WT < 30 ~ plyr::round_any(WT * 2.5, 10)), AMT))
  } else if (!is.null(fixed_load_dose)) {
    NM_lin <- Sim_data %>%
      mutate(
        FLOORED_WT = floor(WT),
        AMT = ifelse(EVID == 1 & TIME > 0, case_when(
          WT >= 45 ~ 150,
          WT >= 30 & WT < 45 ~ 100,
          WT < 30 ~ plyr::round_any(WT * 2.5, 10)), AMT))
  } else {
    NM_lin <- Sim_data %>%
      mutate(
        FLOORED_WT = floor(WT),
        AMT = ifelse(EVID == 1, case_when(
          WT >= 45 ~ 150,
          WT >= 30 & WT < 45 ~ 100,
          WT < 30 ~ plyr::round_any(WT * 2.5, 10)), 0))
    }
    
    
    NM_lin <- NM_lin %>%
      filter(FLOORED_WT < weight) %>%
      mutate(
        PILLS = ceiling(calculate_pillcount(AMT, dosage)),
        LOGIT_ADH = THETA0 + THETA1 * PILLS,
        ADH = if (Utility_score_mode == "dosage_form") {
          exp(LOGIT_ADH) / (1 + exp(LOGIT_ADH))
        } else {
          rep(1, length(LOGIT_ADH))
        }
      ) %>%
      dplyr::select(ID, TIME, EVID, CMT, II, ADDL, AMT, PILLS, ADH, AGE, SEX, HT, WT, FFM) %>% 
      arrange(ID, TIME)
    
  set.seed(seed)
  
  Conventional_model <- model %>%
    data_set(NM_lin) %>%
    carry.out(a.u.g) %>%
    obsaug %>%
    param(TSWITCH= (treatment_duration + TSWITCH_val)*24)%>%
    mrgsim(delta = delta, end = end * 24) %>%
    as.data.frame()
  
  # conventional_pk <- Conventional_model %>%
  #   group_by(TIME) %>%
  #   summarise(
  #     median = median(CONC_CENT, na.rm = TRUE),
  #     p5 = quantile(CONC_CENT, 0.05, na.rm = TRUE),
  #     p95 = quantile(CONC_CENT, 0.95, na.rm = TRUE)) %>%
  #   mutate(
  #     TIME = TIME / 24,
  #     TYPE = "Conventional")
    
  conventional_pk_stat <- pk_summary(Conventional_model, treatment_duration)
  
  if(use_loading_dose == TRUE) {
    conventional_model <- Conventional_model %>% group_by(ID) %>%
      filter(TIME > 0) %>%
      mutate( AMT = max(AMT))
  } else {
    conventional_model <- Conventional_model %>% group_by(ID) %>%
      mutate( AMT = max(AMT))
  }
    
  conventional  <- create_bin(conventional_model, Time = treatment_duration, weight, mode)
    
    
  AUC_TOEC90_conventional <- AUC_TOEC90_summary(conventional, upper, lower)
    
  
  # DOSE_summary <- AUC_TOEC90_conventional %>%
  #   group_by(across(c(starts_with("WT_BAND"), starts_with("DOSE")))) %>%
  #   mutate(COUNT_BIN_dose = sum(COUNT_BIN)) %>%
  #   distinct(across(c(starts_with("WT_BAND"), starts_with("DOSE"))), COUNT_BIN_dose, .keep_all = TRUE) %>%
  #   group_by(across(c(starts_with("WT_BAND")))) %>%
  #   filter(COUNT_BIN_dose == max(COUNT_BIN_dose)) %>%
  #   select(starts_with("WT_BAND"), starts_with("DOSE")) 
  #   
  return(list(AUC_TOEC90_conventional = AUC_TOEC90_conventional,
              pk_model_output = Conventional_model,
              stat_data = conventional_pk_stat,
              sumplot = conventional,
              treatment_duration = treatment_duration,
              sens_df = NM_lin
              # DOSE_summary = DOSE_summary
              ))
    
}
