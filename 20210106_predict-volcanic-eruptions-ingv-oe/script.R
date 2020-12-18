

# imports -----------------------------------------------------------------

library(magrittr)
library(randomForest)
library(readr)
library(progress)
source("utils.R")

# params ------------------------------------------------------------------

files = fs::dir_ls("data/train/")
files_test = fs::dir_ls("data/test/")

dbug = T
if (dbug){
  files = sample(files,size = 100)
}

# data --------------------------------------------------------------------

pb <- progress_bar$new(total = length(files),
                       format = ":percent [:bar] eta: :eta | elapsed: :elapsed",
                       clear = FALSE,
                       width= cli::console_width())

purrr::map_df(files,function(x){
  pb$tick()
  read_sensor_file(x)
}) -> dat_all

dat_proc <- build_features(dat_all)

# train data --------------------------------------------------------------
train_data = vroom::vroom(file = "data/train.csv")
dplyr::left_join(x = dat_proc,train_data) -> dat_full

dat_full %<>% dplyr::ungroup() %>%  dplyr::select(-segment_id)

tmp_res <- randomForest(time_to_eruption ~ .,
                         data = dat_full,
                         mtry = 6,
                         importance = TRUE,
                         na.action = na.omit)

tmp_res
importance_df = importance(tmp_res)
importance_df[,1] %>% barplot
mask = importance_df[,1] > 0
importance_df[ mask,1] %>% names(.) -> vars_in

dat_full %>% dplyr::select(all_of(vars_in),time_to_eruption) -> dat_sel

tmp_res2 <- randomForest(time_to_eruption ~ .,
                        data = dat_sel,
                        mtry = 5,
                        importance = TRUE,
                        na.action = na.omit)

# caret -------------------------------------------------------------------

# library(doParallel)
# library(caret)
# cl <- makePSOCKcluster(5)
# registerDoParallel(cl)
# 
# ## All subsequent models are then run in parallel
# model <- train(y ~ ., data = training, method = "rf")
# 
# ## When you are done:
# stopCluster(cl)

# output ------------------------------------------------------------------


spl_factor = sample(c(letters,LETTERS),size = length(files_test), replace = T)

files_test_list <- split(files_test,spl_factor)

pb <- progress_bar$new(total = length(files_test_list),
                       format = ":percent [:bar] eta: :eta | elapsed: :elapsed",
                       clear = FALSE,
                       width= cli::console_width())


purrr::map_df(files_test_list,function(x){
    pb$tick()
    read_sensor_file2(x) -> dat_result
    dat_result = dplyr::select(dat_result,-path)
    dat_result_proc <- build_features(dat_result)
    dat_result_sel <- dplyr::select(dat_result_proc,
                                  all_of(vars_in),
                                  segment_id )
    predict(tmp_res2,dat_result_sel) -> predicted_value
    data.frame(segment_id = dat_result_sel$segment_id,
               time_to_eruption = predicted_value)
}) -> dat_output


readr::write_csv(dat_output,"submission.csv")
