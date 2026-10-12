
create_allometric_WB_dosing <- function(data, model, weight, seed, use_loading_dose = FALSE, 
                                        fixed_load_dose = NULL, 
                                        load_freq = NULL, 
                                        load_interval = NULL,
                                        main_freq,
                                        main_interval,
                                        weight_bands = NULL,
                                        TSWITCH_val,
                                        upper,lower,
                                        end,delta,mode, Utility_score_mode, dosage) {
  
  
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
  
  
  
  # Step 4: Process Sim_data
  All_WB_data <- Sim_data %>% 
    mutate(BIN = floor(WT),
           FLOORED_WT = floor(WT)) %>% 
    filter(BIN < weight) %>%
    arrange(BIN, WT, AGE) %>%
    mutate(BINNED_WT = factor(case_when(BIN < 6 ~ " < 06 kg",
                                        BIN >=6 & BIN < 10 ~ paste0("0", BIN, " kg"),
                                        BIN >= 10 & BIN < 60 ~ paste0(BIN, " kg"),
                                        BIN >= 60 ~ "60 kg +" ))) %>% 
    arrange(ID) %>%
    group_by(BINNED_WT) 
      
  if (!is.null(weight_bands)) {
    All_WB_data <- All_WB_data %>%
      mutate(
        AMT = eval(parse(text = sprintf("case_when(\n%s,\nTRUE ~ 0\n)", weight_band_expression))),
        AMT = ifelse(EVID == 1 & TIME > 0, case_when(
              BIN < 6 ~ 20,
              BIN >= 6 & BIN < 10 ~ 30,
              BIN >= 10 & BIN < 15 ~ 50,
              BIN >= 15 & BIN < 20 ~ 60,
              BIN >= 20 & BIN < 25 ~ 70,
              BIN >= 25  & BIN < 30 ~ 80,
              BIN >= 30 & BIN < 45 ~ 100,
              BIN >= 45  ~ 150
            ), AMT))
  } else if (!is.null(fixed_load_dose)) {
    All_WB_data <- All_WB_data %>%
      mutate(
        AMT = ifelse(EVID == 1 & TIME > 0, case_when(
          BIN < 6 ~ 20,
          BIN >= 6 & BIN < 10 ~ 30,
          BIN >= 10 & BIN < 15 ~ 50,
          BIN >= 15 & BIN < 20 ~ 60,
          BIN >= 20 & BIN < 25 ~ 70,
          BIN >= 25  & BIN < 30 ~ 80,
          BIN >= 30 & BIN < 45 ~ 100,
          BIN >= 45  ~ 150), AMT))
  } else {
    All_WB_data <- All_WB_data %>%
      mutate(
        AMT = ifelse(EVID == 1, case_when(
          BIN < 6 ~ 20,
          BIN >= 6 & BIN < 10 ~ 30,
          BIN >= 10 & BIN < 15 ~ 50,
          BIN >= 15 & BIN < 20 ~ 60,
          BIN >= 20 & BIN < 25 ~ 70,
          BIN >= 25  & BIN < 30 ~ 80,
          BIN >= 30 & BIN < 45 ~ 100,
          BIN >= 45  ~ 150), 0))
  }
  All_WB_data <- All_WB_data %>%
    mutate(
      PILLS = ceiling(calculate_pillcount(AMT, dosage)),
      LOGIT_ADH = THETA0 + THETA1 * PILLS,
      ADH = if (Utility_score_mode == "dosage_form") {
        exp(LOGIT_ADH) / (1 + exp(LOGIT_ADH))
      } else {
        rep(1, length(LOGIT_ADH))
      }
    ) %>%
    ungroup() %>%
    dplyr::select(ID, TIME, EVID, CMT, II, ADDL, AMT, PILLS, ADH, AGE, SEX, HT, WT, FFM) %>% 
    arrange(ID, TIME)
  
  
  set.seed(seed)
  
  Allometric_WB_model <- model %>% 
    data_set(All_WB_data) %>%
    carry.out(a.u.g) %>%
    obsaug %>%
    param(TSWITCH= (treatment_duration + TSWITCH_val)*24)%>%
    mrgsim(delta = delta, end = end * 24) %>%
    as.data.frame()
  
# allom_WB_pk <- Allometric_WB_model %>%
  #   group_by(TIME) %>%
  #   summarise(
  #     median = median(CONC_CENT, na.rm = TRUE),
  #     p5 = quantile(CONC_CENT, 0.05, na.rm = TRUE),
  #     p95 = quantile(CONC_CENT, 0.95, na.rm = TRUE)) %>%
  #   mutate(
  #     TIME = TIME / 24,
  #     TYPE = "Allometric_WB")
  
  Allo_WB_pk_stat <- pk_summary(Allometric_WB_model, treatment_duration)
  
  if(use_loading_dose == TRUE) {
    Allom_WB_model <- Allometric_WB_model %>% group_by(ID) %>%
      filter(TIME > 0) %>%
      mutate( AMT = max(AMT))
  } else {
    
    Allom_WB_model <- Allometric_WB_model %>% group_by(ID) %>%
      mutate(AMT = max(AMT))
  }

  
  Allometric_WB <- create_bin(Allom_WB_model, Time = treatment_duration, weight,mode)
  
  AUC_TOEC90_Allometric_WB <- AUC_TOEC90_summary(Allometric_WB, upper, lower)
  
  AUC_TOEC90_Allo_WB <- AUC_TOEC90_Allometric_WB %>% 
    group_by(FLAG_Allometric_WB, WT_BAND_Allom_WB_model) %>%
    mutate(SUM_BIN_WB = COUNT_UPPER_Allometric_WB + COUNT_LOWER_Allometric_WB,
           SUM_WT_BAND_WB = sum(SUM_BIN_WB)) %>%
    ungroup() %>%
    group_by(BINNED_WT) %>%
    filter(SUM_WT_BAND_WB == min(SUM_WT_BAND_WB)) %>%
    filter(row_number() == 1)
  
  DOSE_summary <- AUC_TOEC90_Allo_WB %>%
    group_by(across(c(starts_with("WT_BAND"), starts_with("DOSE")))) %>%
    mutate(COUNT_BIN_dose = sum(COUNT_BIN)) %>%
    distinct(across(c(starts_with("WT_BAND"), starts_with("DOSE"))), COUNT_BIN_dose, .keep_all = TRUE) %>%
    group_by(across(c(starts_with("WT_BAND")))) %>%
    filter(COUNT_BIN_dose == max(COUNT_BIN_dose)) %>%
    select(starts_with("WT_BAND"), starts_with("DOSE")) 
  
  
  return(list(AUC_TOEC90_Allo_WB = AUC_TOEC90_Allo_WB,
              pk_model_output = Allometric_WB_model,
              stat_data = Allo_WB_pk_stat,
              sumplot = Allometric_WB,
              DOSE_summary = DOSE_summary,
              treatment_duration = treatment_duration,
              sens_df = All_WB_data ))
}
