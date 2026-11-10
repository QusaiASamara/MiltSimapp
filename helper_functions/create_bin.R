get_mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}

create_bin <- function(data,Time, weight, mode) {

  data_name <- deparse(substitute(data))
  
  data <- data %>%
    group_by(ID) %>%
    mutate(TIME = TIME/24,
           TEC90= TEC90[TIME == max(TIME)]) %>%
    filter(TIME == ifelse (mode =="rep", max(TIME),Time)) %>%
    mutate(
      TEC90 = TEC90 / 24,
      T_EC90 = T_EC90 / 24,
      hazard = h/1.84,
      AUC = ifelse(mode=="rep", AUC_inf/24, AUC/ 24),
      BIN = floor(WT) ## floor or round??
    ) %>%
    filter(BIN < weight) %>%
    ungroup() %>%
    arrange(BIN, WT, AGE) %>%
    mutate(
      BINNED_WT = factor(case_when(
        BIN < 6 ~ "< 6",
        BIN >= 6 & BIN < 10 ~ paste0("0", BIN),
        BIN >= 10 & BIN < 30 ~ paste0(BIN),
        BIN >= 30 & BIN < 60 ~ paste0(BIN),
        BIN >= 70 ~ "> 60"
      ))) %>%
    group_by(BINNED_WT, FLAG) %>%
    mutate(DOSE_G = round(get_mode(AMT), 1)) %>%
    ungroup()  %>%
    group_by(BIN) %>%
    mutate(COUNT_BIN_bin = n()) %>%
    ungroup() %>%
    {
      if (!startsWith(data_name, "Allometric_WB_custom")) {
        mutate(.,!!paste0("WT_BAND_", data_name) := case_when(
          BIN < 6 ~ "< 6 kg", 
          BIN >= 6 & BIN < 10 ~ "06 to 09 kg", 
          BIN >= 10 & BIN < 15 ~ "10 to 14 kg", 
          BIN >= 15 & BIN < 20 ~ "15 to 19 kg", 
          BIN >= 20 & BIN < 25 ~ "20 to 24 kg", 
          BIN >= 25 & BIN < 30 ~ "25 to 29 kg", 
          BIN >= 30 & BIN < 45 ~ "30 to 44 kg", 
          BIN >= 45 ~ "45+ kg"
        ))
      } else {
        .
      }
    } %>% 
    arrange(ID)
  
  return(data)
}
