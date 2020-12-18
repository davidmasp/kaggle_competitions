

specs_data = cols(
  sensor_1 = col_double(),
  sensor_2 = col_double(),
  sensor_3 = col_double(),
  sensor_4 = col_double(),
  sensor_5 = col_double(),
  sensor_6 = col_double(),
  sensor_7 = col_double(),
  sensor_8 = col_double(),
  sensor_9 = col_double(),
  sensor_10 = col_double()
)


read_sensor_file <- function(fname){
  dat <- vroom::vroom(fname,col_types = specs_data,progress = FALSE)
  
  sID = stringr::str_extract(fname, "[:digit:]+(?=.csv)") %>% 
    as.integer()
  
  dat$segment_id = sID
  dat$minute = 1:nrow(dat)
  return(dat)
}

read_sensor_file2 <- function(fname){
  dat <- vroom::vroom(fname,col_types = specs_data,progress = FALSE, id = "path")
  
  sID = stringr::str_extract(dat$path, "[:digit:]+(?=.csv)") %>% 
    as.integer()
  
  dat$segment_id = sID
  return(dat)
}



build_features <- function(data_frame){
  #browser()
  data_frame %>% tidyr::pivot_longer(names_to = "sensor",
                                  values_to = "value",
                                  cols = tidyselect::starts_with("sensor_")) %>%
    dplyr::group_by(sensor) %>% 
    dplyr::mutate(mean_sensor = mean(value,na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(value_inputed = ifelse(is.na(value), mean_sensor, value)) %>% 
    dplyr::select(-mean_sensor) %>% 
    dplyr::group_by(segment_id,sensor) %>%
    dplyr::summarise(
      mean = mean(value_inputed, na.rm = TRUE),
      isNA = any(is.na(value)),
      q75 = quantile(value_inputed,prob = .75, na.rm = TRUE),
      q25 = quantile(value_inputed, prob = .25, na.rm = TRUE),
      q90 = quantile(value_inputed,prob = .9, na.rm = TRUE),
      q10 = quantile(value_inputed,prob = .1, na.rm = TRUE)
    ) %>% 
    tidyr::pivot_wider(names_from = "sensor",
                       values_from = c("mean",
                                       "q75",
                                       "q25",
                                       "isNA",
                                       "q90",
                                       "q10")) -> dat_proc
  
  return(dat_proc)
}
