library(caret)
library(tidyr)
library(tidyverse)
library(dplyr)

set.seed(1)

#### Relist data names into manageable lists for model input ####

as_inTrain.full_relist_rds_names <- name_relist_fn(input_rds = "as_inTrain.full_names.rds",
                                                   data_class = "full")
as_inTest.full_relist_rds_names <- name_relist_fn(input_rds = "as_inTest.full_names.rds",
                                                  data_class = "full")
as_inTrain.cut_relist_rds_names <- name_relist_fn(input_rds = "as_inTrain.cut_names.rds",
                                                  data_class = "cut")
as_inTest.cut_relist_rds_names <- name_relist_fn(input_rds = "as_inTest.cut_names.rds",
                                                 data_class = "cut")
as_inTrain.split_relist_rds_names <- name_relist_fn(input_rds = "as_inTrain.split_names.rds",
                                                    data_class = "split")
as_inTest.split_relist_rds_names <- name_relist_fn(input_rds = "as_inTest.split_names.rds",
                                                   data_class = "split")

ps_inTrain.full_relist_rds_names <- name_relist_fn(input_rds = "ps_inTrain.full_names.rds",
                                                   data_class = "full")
ps_inTest.full_relist_rds_names <- name_relist_fn(input_rds = "ps_inTest.full_names.rds",
                                                  data_class = "full")
ps_inTrain.cut_relist_rds_names <- name_relist_fn(input_rds = "ps_inTrain.cut_names.rds",
                                                  data_class = "cut")
ps_inTest.cut_relist_rds_names <- name_relist_fn(input_rds = "ps_inTest.cut_names.rds",
                                                 data_class = "cut")
ps_inTrain.split_relist_rds_names <- name_relist_fn(input_rds = "ps_inTrain.split_names.rds",
                                                    data_class = "split")
ps_inTest.split_relist_rds_names <- name_relist_fn(input_rds = "ps_inTest.split_names.rds",
                                                   data_class = "split")

snv_inTrain.full_relist_rds_names <- name_relist_fn(input_rds = "snv_inTrain.full_names.rds",
                                                    data_class = "full")
snv_inTest.full_relist_rds_names <- name_relist_fn(input_rds = "snv_inTest.full_names.rds",
                                                   data_class = "full")
snv_inTrain.cut_relist_rds_names <- name_relist_fn(input_rds = "snv_inTrain.cut_names.rds",
                                                   data_class = "cut")
snv_inTest.cut_relist_rds_names <- name_relist_fn(input_rds = "snv_inTest.cut_names.rds",
                                                  data_class = "cut")
snv_inTrain.split_relist_rds_names <- name_relist_fn(input_rds = "snv_inTrain.split_names.rds",
                                                     data_class = "split")
snv_inTest.split_relist_rds_names <- name_relist_fn(input_rds = "snv_inTest.split_names.rds",
                                                    data_class = "split")


#### Set model tuning ####

fitControl <- trainControl(method = "repeatedcv",
                           number = 10, # 10-fold CV
                           repeats = 3, # repeated three times
                           classProbs = TRUE, # computes class probs of holdout samples during resampling
                           savePredictions = TRUE, # save prediction values
                           returnResamp = "all", # saves all of the resampled performance measures
                           allowParallel = TRUE # allows parallel processing if available
                           )

#### RF Model Training ####

##### Initial Seed = 1 RF Training #####

rf_train_fn(input_names_rds = "inTrain.1_as_full_bin60.rds",
            load_rdata = "as_inTrain.full_data.RData",
            control = fitControl)

rf_train_fn(input_names_rds = "inTrain.1_ps_full_bin60.rds",
            load_rdata = "ps_inTrain.full_data.RData",
            control = fitControl)

rf_train_fn(input_names_rds = "inTrain.1_snv_full_bin60.rds",
            load_rdata = "snv_inTrain.full_data.RData",
            control = fitControl)

rf_train_fn(input_names_rds = "inTrain.1_as_cut_bin60.rds",
            load_rdata = "as_inTrain.cut_data.RData",
            control = fitControl)

rf_train_fn(input_names_rds = "inTrain.1_ps_cut_bin60.rds",
            load_rdata = "ps_inTrain.cut_data.RData",
            control = fitControl)

rf_train_fn(input_names_rds = "inTrain.1_snv_cut_bin60.rds",
            load_rdata = "snv_inTrain.cut_data.RData",
            control = fitControl)

rf_train_fn(input_names_rds = "inTrain.1_as_split_bin60.rds",
            load_rdata = "as_inTrain.split_data.RData",
            control = fitControl) ##

rf_train_fn(input_names_rds = "inTrain.1_ps_split_bin60.rds",
            load_rdata = "ps_inTrain.split_data.RData",
            control = fitControl)

rf_train_fn(input_names_rds = "inTrain.1_snv_split_bin60.rds",
            load_rdata = "snv_inTrain.split_data.RData",
            control = fitControl)

##### Follow up seed = 2,3 RF Training #####

rf_train_fn(input_names_rds = c("inTrain.2_as_full_bin60.rds","inTrain.3_as_full_bin60.rds"),
            load_rdata = "as_inTrain.full_data.RData",
            control = fitControl)

rf_train_fn(input_names_rds = c("inTrain.2_ps_full_bin60.rds","inTrain.3_ps_full_bin60.rds"),
            load_rdata = "ps_inTrain.full_data.RData",
            control = fitControl)

rf_train_fn(input_names_rds = c("inTrain.2_snv_full_bin60.rds","inTrain.3_snv_full_bin60.rds"),
            load_rdata = "snv_inTrain.full_data.RData",
            control = fitControl)

rf_train_fn(input_names_rds = c("inTrain.2_as_cut_bin60.rds","inTrain.3_as_cut_bin60.rds"),
            load_rdata = "as_inTrain.cut_data.RData",
            control = fitControl)

rf_train_fn(input_names_rds = c("inTrain.2_ps_cut_bin60.rds","inTrain.3_ps_cut_bin60.rds"),
            load_rdata = "ps_inTrain.cut_data.RData",
            control = fitControl)

rf_train_fn(input_names_rds = c("inTrain.2_snv_cut_bin60.rds","inTrain.3_snv_cut_bin60.rds"),
            load_rdata = "snv_inTrain.cut_data.RData",
            control = fitControl)

rf_train_fn(input_names_rds = c("inTrain.2_as_split_bin60.rds","inTrain.3_as_split_bin60.rds"),
            load_rdata = "as_inTrain.split_data.RData",
            control = fitControl)

rf_train_fn(input_names_rds = c("inTrain.2_ps_split_bin60.rds","inTrain.3_ps_split_bin60.rds"),
            load_rdata = "ps_inTrain.split_data.RData",
            control = fitControl)

rf_train_fn(input_names_rds = c("inTrain.2_snv_split_bin60.rds","inTrain.3_snv_split_bin60.rds"),
            load_rdata = "snv_inTrain.split_data.RData",
            control = fitControl)

# 
# Compare resampling between moedl 5.8.2
# # rmse(
# test_data$cnt, 
# predict(rf_ranger, test_data)$predictions
# )
# 
# SVM https://rpubs.com/uky994/593668

#### PLS-DA Model Training ####

##### Initial Seed = 1 PLS-DA Training #####

plsda_train_fn(input_names_rds = "inTrain.1_as_full_bin60.rds",
               load_rdata = "as_inTrain.full_data.RData",
               control = fitControl)

plsda_train_fn(input_names_rds = "inTrain.1_ps_full_bin60.rds",
               load_rdata = "ps_inTrain.full_data.RData",
               control = fitControl)

plsda_train_fn(input_names_rds = "inTrain.1_snv_full_bin60.rds",
               load_rdata = "snv_inTrain.full_data.RData",
               control = fitControl)

plsda_train_fn(input_names_rds = "inTrain.1_as_cut_bin60.rds",
               load_rdata = "as_inTrain.cut_data.RData",
               control = fitControl)

plsda_train_fn(input_names_rds = "inTrain.1_ps_cut_bin60.rds",
               load_rdata = "ps_inTrain.cut_data.RData",
               control = fitControl)

plsda_train_fn(input_names_rds = "inTrain.1_snv_cut_bin60.rds",
               load_rdata = "snv_inTrain.cut_data.RData",
               control = fitControl)

plsda_train_fn(input_names_rds = "inTrain.1_as_split_bin60.rds",
               load_rdata = "as_inTrain.split_data.RData",
               control = fitControl) ##

plsda_train_fn(input_names_rds = "inTrain.1_ps_split_bin60.rds",
               load_rdata = "ps_inTrain.split_data.RData",
               control = fitControl)

plsda_train_fn(input_names_rds = "inTrain.1_snv_split_bin60.rds",
               load_rdata = "snv_inTrain.split_data.RData",
               control = fitControl)

##### Follow up seed = 2,3 PLS-DA Training #####

plsda_train_fn(input_names_rds = c("inTrain.2_as_full_bin60.rds","inTrain.3_as_full_bin60.rds"),
            load_rdata = "as_inTrain.full_data.RData",
            control = fitControl)

plsda_train_fn(input_names_rds = c("inTrain.2_ps_full_bin60.rds","inTrain.3_ps_full_bin60.rds"),
            load_rdata = "ps_inTrain.full_data.RData",
            control = fitControl)

plsda_train_fn(input_names_rds = c("inTrain.2_snv_full_bin60.rds","inTrain.3_snv_full_bin60.rds"),
            load_rdata = "snv_inTrain.full_data.RData",
            control = fitControl)

plsda_train_fn(input_names_rds = c("inTrain.2_as_cut_bin60.rds","inTrain.3_as_cut_bin60.rds"),
            load_rdata = "as_inTrain.cut_data.RData",
            control = fitControl)

plsda_train_fn(input_names_rds = c("inTrain.2_ps_cut_bin60.rds","inTrain.3_ps_cut_bin60.rds"),
            load_rdata = "ps_inTrain.cut_data.RData",
            control = fitControl)

plsda_train_fn(input_names_rds = c("inTrain.2_snv_cut_bin60.rds","inTrain.3_snv_cut_bin60.rds"),
            load_rdata = "snv_inTrain.cut_data.RData",
            control = fitControl)

plsda_train_fn(input_names_rds = c("inTrain.2_as_split_bin60.rds","inTrain.3_as_split_bin60.rds"),
            load_rdata = "as_inTrain.split_data.RData",
            control = fitControl)

plsda_train_fn(input_names_rds = c("inTrain.2_ps_split_bin60.rds","inTrain.3_ps_split_bin60.rds"),
            load_rdata = "ps_inTrain.split_data.RData",
            control = fitControl)

plsda_train_fn(input_names_rds = c("inTrain.2_snv_split_bin60.rds","inTrain.3_snv_split_bin60.rds"),
            load_rdata = "snv_inTrain.split_data.RData",
            control = fitControl)


#### SVM Kernel Evaluation ####

svm_as_full_bin60_eval <- evaluate_svm_triple_fn(input_names_rds = "inTrain.1_as_full_bin60.rds",
                                                 load_rdata = "as_inTrain.full_data.RData",
                                                 control = fitControl)

svm_ps_full_bin60_eval <- evaluate_svm_triple_fn(input_names_rds = "inTrain.1_ps_full_bin60.rds",
                                                 load_rdata = "ps_inTrain.full_data.RData",
                                                 control = fitControl)

svm_ps_full_bin60_eval <- evaluate_svm_triple_fn(input_names_rds = "inTrain.1_snv_full_bin60.rds",
                                                 load_rdata = "snv_inTrain.full_data.RData",
                                                 control = fitControl)

svm_as_cut_bin60_eval <- evaluate_svm_triple_fn(input_names_rds = "inTrain.1_as_cut_bin60.rds",
                                                load_rdata = "as_inTrain.cut_data.RData",
                                                control = fitControl)

svm_ps_cut_bin60_eval <- evaluate_svm_triple_fn(input_names_rds = "inTrain.1_ps_cut_bin60.rds",
                                                load_rdata = "ps_inTrain.cut_data.RData",
                                                control = fitControl)

svm_snv_cut_bin60_eval <- evaluate_svm_triple_fn(input_names_rds = "inTrain.1_snv_cut_bin60.rds",
                                                 load_rdata = "snv_inTrain.cut_data.RData",
                                                 control = fitControl)

svm_as_split_bin60_eval <- evaluate_svm_triple_fn(input_names_rds = "inTrain.1_as_split_bin60.rds",
                                                  load_rdata = "as_inTrain.split_data.RData",
                                                  control = fitControl)

svm_ps_split_bin60_eval <- evaluate_svm_triple_fn(input_names_rds = "inTrain.1_ps_split_bin60.rds",
                                                  load_rdata = "ps_inTrain.split_data.RData",
                                                  control = fitControl)

svm_snv_split_bin60_eval <- evaluate_svm_triple_fn(input_names_rds = "inTrain.1_snv_split_bin60.rds",
                                                   load_rdata = "snv_inTrain.split_data.RData",
                                                   control = fitControl)