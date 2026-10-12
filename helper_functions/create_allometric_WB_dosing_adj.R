
create_costum_allometric_WB_dosing <- function(data, model, custom_doses, weight, seed, 
                                               use_loading_dose = FALSE, 
                                               fixed_load_dose = NULL, 
                                               load_freq = NULL, 
                                               load_interval = NULL,
                                               main_freq,
                                               main_interval,
                                               weight_bands_load = NULL,
                                               TSWITCH_val,
                                               upper_limit,lower_limit,
                                               end, delta,mode,Utility_score_mode,dosage) {
  
  # Get the number of flags from custom_doses
  num_flags <- custom_doses$num_flags
  weight_bands <- custom_doses$weight_bands
  doses <- custom_doses$doses
  
  ##Adherence
  
  THETA0 <- 3.5293918      # baseline adherence
  THETA1 <- -0.3368967   # pill burden effect
  
  # Generate dose matrix directly from custom_doses
  dose_matrix <- do.call(rbind, lapply(seq_along(doses), function(i) {
    data.frame(
      Computed_Age_Cat = i,
      Flag = 1:num_flags,
      Dose = doses[[i]]
    )
  }))
  
  # Parse weight bands into numeric ranges
  weight_band_ranges <- lapply(weight_bands, function(band) {
    # Extract numeric ranges from the weight band
    range_vals <- gsub("[^0-9.-]", "", band)
    range_vals <- strsplit(range_vals, "-")[[1]] %>% as.numeric()
    return(range_vals)
  })
  
  # Rest of your existing code remains the same...
  id_count <- data$ID
  
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
  
  weight_band_expression <- NULL
  
  # Create weight band ranges if weight bands are provided
  if (!is.null(weight_bands_load)) {
    weight_band_cases <- sapply(weight_bands_load, function(band) {
      sprintf("EVID == 1 & TIME == 0 & FLOORED_WT >= %s & FLOORED_WT < %s ~ %s",
              band$min, band$max, band$dose)
    })
    weight_band_expression <- paste(weight_band_cases, collapse = ",\n")
  }
  
  
  # Step 1: Compute BIN and BINNED_WT
  Bin_data <- Sim_data %>%
    group_by(ID) %>%
    mutate(BIN = floor(WT)) %>%
    arrange(BIN, WT, AGE) %>%
    mutate(BINNED_WT = factor(case_when(
      BIN < 6 ~ " < 06 kg",
      BIN >= 6 & BIN < 10 ~ paste0("0", BIN, " kg"),
      BIN >= 10 & BIN < 60 ~ paste0(BIN, " kg"),
      BIN >= 60 ~ "60 kg +"
    ))) %>%
    ungroup()
  
  # Create FLAG structure for each BINNED_WT
  unique_bins <- unique(Bin_data$BINNED_WT)
  Bin_data <- data.frame(
    BINNED_WT = rep(unique_bins, each = num_flags),
    FLAG = rep(1:num_flags, length(unique_bins))
  )
  
  # Calculate the maximum number of IDs for FLAG adjustment
  NIDs <- max(unique(subset(data, WT < weight)$ID))
  
  # Step 2: Process Allometric Dosing
  All_WB_data <- Sim_data %>%
    mutate(BIN = floor(WT),
           FLOORED_WT = floor(WT)) %>%
    arrange(BIN, WT, AGE) %>%
    mutate(
      BINNED_WT = factor(case_when(
        BIN < 6 ~ " < 06 kg",
        BIN >= 6 & BIN < 10 ~ paste0("0", BIN, " kg"),
        BIN >= 10 & BIN < 60 ~ paste0(BIN, " kg"),
        BIN >= 60 ~ "60 kg +"
      ))) %>%
    ungroup() %>%
    left_join(Bin_data, by = "BINNED_WT", relationship = "many-to-many") %>%
    filter(BIN < weight) %>%
    arrange(FLAG, ID) %>%
    mutate(ID = case_when(
      FLAG == num_flags ~ ID + (num_flags - 1) * NIDs,
      FLAG > 1 ~ ID + (FLAG - 1) * NIDs,
      TRUE ~ ID
    )) %>%
    group_by(BINNED_WT) %>%
    mutate(AGE_CAT = {
      case_when_expr <- purrr::map2(weight_band_ranges, seq_along(weight_band_ranges), function(range, idx) {
        if (!is.na(range[1]) && !is.na(range[2])) {
          paste0("BIN >= ", range[1], " & BIN < ", range[2], " ~ ", idx)
        } else {
          NULL
        }
      }) %>% 
        purrr::compact() %>% 
        paste(collapse = ", ")
      eval(parse(text = paste0("case_when(", case_when_expr, ", TRUE ~ NA_real_)")))
    }) %>%
    left_join(dose_matrix, by = c("AGE_CAT" = "Computed_Age_Cat", "FLAG" = "Flag")) # Join doses
  
  
  if (!is.null(weight_bands_load)) {
  All_WB_data <- All_WB_data %>%
    mutate(
      AMT = eval(parse(text = sprintf("case_when(\n%s,\nTRUE ~ 0\n)", weight_band_expression))),
      AMT = ifelse(EVID == 1 & TIME > 0, Dose, AMT))
  
  } else if (!is.null(fixed_load_dose)) {
    All_WB_data <- All_WB_data %>%
      mutate(
        AMT = ifelse(EVID == 1 & TIME > 0 , Dose, AMT))
  } else { 
    All_WB_data <- All_WB_data %>%
      mutate(
        AMT = ifelse(EVID == 1, Dose, 0))
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
    dplyr::select(ID, TIME, EVID, CMT, II, ADDL, AMT, PILLS, ADH, AGE, SEX, HT, WT, FFM, FLAG) %>% 
    arrange(ID, TIME)
  
  
  set.seed(seed)
  
  Allometric_WB_custom_model <- model %>% 
    data_set(All_WB_data) %>%
    carry.out(a.u.g) %>%
    obsaug %>%
    param(TSWITCH= (treatment_duration + TSWITCH_val)*24)%>%
    mrgsim(delta = delta, end = end * 24) %>%
    as.data.frame()
  
  #allom_WB_pk <- Allometric_WB_custom_model %>%
   # group_by(TIME) %>%
    #summarise(
     # median = median(CONC_CENT, na.rm = TRUE),
      #p5 = quantile(CONC_CENT, 0.05, na.rm = TRUE),
      #p95 = quantile(CONC_CENT, 0.95, na.rm = TRUE)) %>%
    #mutate(
      #TIME = TIME / 24,
      #TYPE = "Allometric_WB_custom")
  
  allo_cust_WB_pk_stat <- pk_summary(Allometric_WB_custom_model, treatment_duration)
  
  if(use_loading_dose == TRUE) {
    Allom_WB_custom_model <- Allometric_WB_custom_model %>% group_by(ID) %>%
      filter(TIME > 0) %>%
      mutate( AMT = max(AMT))
  } else {
    Allom_WB_custom_model <- Allometric_WB_custom_model %>% group_by(ID) %>%
      mutate( AMT = max(AMT))
  }
  
  
  
  Allometric_WB_custom <- create_bin(Allom_WB_custom_model, Time = treatment_duration, weight,mode)
  
  Allometric_WB_custom <- Allometric_WB_custom %>% 
    mutate(
      UPPER = ifelse(AUC > upper_limit, 1, 0), 
      LOWER_TOEC90 = ifelse(TEC90 < lower_limit, 1, 0),
      WT_BAND_Allometric_WB_custom = {
        case_when_expr <- purrr::map2(weight_band_ranges, seq_along(weight_band_ranges), function(range, idx) {
          if (!is.na(range[1]) && !is.na(range[2])) {
            paste0("BIN >= ", range[1], " & BIN < ", range[2], " ~ '", range[1], " to ", range[2], " kg'")
          } else {
            NULL
          }
        }) %>% 
          purrr::compact() %>% 
          paste(collapse = ", ")
        eval(parse(text = paste0("case_when(", case_when_expr, ", TRUE ~ NA_character_)")))
      }) %>% select(-WT_BAND_Allom_WB_custom_model)
  
  AUC_TOEC90_Allometric_WB_custom <- AUC_TOEC90_summary(Allometric_WB_custom, upper_limit, lower_limit)
  
  AUC_TOEC90_Allo_WB_custom <- AUC_TOEC90_Allometric_WB_custom %>%
    group_by(FLAG_Allometric_WB_custom, WT_BAND_Allometric_WB_custom) %>%
    mutate(SUM_BIN_custom = COUNT_UPPER_Allometric_WB_custom + COUNT_LOWER_Allometric_WB_custom,
           SUM_WT_BAND_custom = sum(SUM_BIN_custom)) %>%
    ungroup() %>%
    group_by(BINNED_WT) %>%
    filter(SUM_WT_BAND_custom == min(SUM_WT_BAND_custom)) %>%
    filter(row_number() == 1)
  
  DOSE_summary <- AUC_TOEC90_Allo_WB_custom %>%
    group_by(WT_BAND_Allometric_WB_custom,DOSE_Allometric_WB_custom) %>%
    mutate(COUNT_BIN_dose = sum(COUNT_BIN)) %>%
    distinct(across(c(starts_with("WT_BAND"), starts_with("DOSE"))), COUNT_BIN_dose, .keep_all = TRUE) %>%
    group_by(across(c(starts_with("WT_BAND")))) %>%
    filter(COUNT_BIN_dose == max(COUNT_BIN_dose)) %>%
    select(starts_with("WT_BAND_Allometric_WB_custom"), starts_with("DOSE")) 
  
  return(list(AUC_TOEC90_Allo_WB_custom = AUC_TOEC90_Allo_WB_custom, 
              pk_model_output = Allometric_WB_custom_model,
              stat_data = allo_cust_WB_pk_stat,
              sumplot = Allometric_WB_custom,
              DOSE_summary = DOSE_summary,
              treatment_duration = treatment_duration,
              sens_df = All_WB_data))
}
