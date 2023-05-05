set.seed(1)

# Removes empty values used because of uneven vector lengths
names_list <- read.csv("names_list.csv", header = TRUE)
names_list <- map(names_list, ~ discard(.x, . == ""))
names_list <- map(names_list, compact)
names_list$t1_60_classID <- ifelse(names_list$T1_60_ID == "OOE", "EVOO", "Other")
names_list$t2_60_classID <- ifelse(names_list$T2_60_ID == "OOE", "EVOO", "Other")
names_list$t2_400_classID <- ifelse(names_list$T2_400_ID == "OOE", "EVOO", "Other")

save(names_list, file = "names_list.RData")

#### Icoshift Data ####
# T1 60MHz ICO Data Importing
t1_60_ico_bin.raw <- read.csv("T1_60_ICO_BIN.csv", header = FALSE)
t1_60_ico_nat.raw <- read.csv("T1_60_ICO_NAT.csv", header = FALSE)
t1_60_ico_zf.raw <- read.csv("T1_60_ICO_ZF.csv", header = FALSE)

# T2 60MHz ICO Data Importing
t2_60_ico_bin.raw <- read.csv("T2_60_ICO_BIN.csv", header = FALSE)
t2_60_ico_nat.raw <- read.csv("T2_60_ICO_NAT.csv", header = FALSE)
t2_60_ico_zf.raw <- read.csv("T2_60_ICO_ZF.csv", header = FALSE)

# T2 400MHz ICO Data Importing
t2_400_ico_bin.raw <- read.csv("T2_400_ICO_BIN.csv", header = FALSE)
t2_400_ico_pbin.raw <- read.csv("T2_400_ICO_pBIN.csv", header = FALSE)
t2_400_ico_nat.raw <- read.csv("T2_400_ICO_NAT.csv", header = FALSE)
t2_400_ico_zf.raw <- read.csv("T2_400_ICO_ZF.csv", header = FALSE)

#### Manual Data ####
# T1 60MHz MAN Data Importing
t1_60_man_bin.dl <- read.csv("T1_60_MAN_BIN.csv", header = FALSE)
t1_60_man_nat.dl <- read.csv("T1_60_MAN_NAT.csv", header = FALSE)
t1_60_man_zf.dl <- read.csv("T1_60_MAN_ZF.csv", header = FALSE)

# T2 60MHz MAN Data Importing
t2_60_man_bin.dl <- read.csv("T2_60_MAN_BIN.csv", header = FALSE)
t2_60_man_nat.dl <- read.csv("T2_60_MAN_NAT.csv", header = FALSE)
t2_60_man_zf.dl <- read.csv("T2_60_MAN_ZF.csv", header = FALSE)

# T2 400MHz MAN Data Importing
t2_400_man_bin.dl <- read.csv("T2_400_MAN_BIN.csv", header = FALSE)
t2_400_man_pbin.dl <- read.csv("T2_400_MAN_pBIN.csv", header = FALSE)
t2_400_man_nat.dl <- read.csv("T2_400_MAN_NAT.csv", header = FALSE)
t2_400_man_zf.dl <- read.csv("T2_400_MAN_ZF.csv", header = FALSE)

#### Formatting MAN Data ####

# T1_60_MAN
t1_60_man_bin <- format_MAN_data(t1_60_man_bin.dl)
t1_60_man_nat <- format_MAN_data(t1_60_man_nat.dl)
t1_60_man_zf <- format_MAN_data(t1_60_man_zf.dl)

# T2_60_MAN
t2_60_man_bin <- format_MAN_data(t2_60_man_bin.dl)
t2_60_man_nat <- format_MAN_data(t2_60_man_nat.dl)
t2_60_man_zf <- format_MAN_data(t2_60_man_zf.dl)

# T2_400_MAN
t2_400_man_bin <- format_MAN_data(t2_400_man_bin.dl)
t2_400_man_pbin <- format_MAN_data(t2_400_man_pbin.dl)
t2_400_man_nat <- format_MAN_data(t2_400_man_nat.dl)
t2_400_man_zf <- format_MAN_data(t2_400_man_zf.dl)

#### Formatting ICO data ####

# Loading ppm values
ico_ppm_list.raw <- as.list(read.csv("ico_ppm_list.csv", header = TRUE))
ico_ppm_list <- lapply(ico_ppm_list.raw, function(x) {
  x <- x[!is.na(x)] # Remove NA values
  return(x)
})

# T1_60_ICO
t1_60_ico_bin <- format_ICO_data(t1_60_ico_bin.raw)
t1_60_ico_nat <- format_ICO_data(t1_60_ico_nat.raw)
t1_60_ico_zf <- format_ICO_data(t1_60_ico_zf.raw)

# T2_60_ICO
t2_60_ico_bin <- format_ICO_data(t2_60_ico_bin.raw)
t2_60_ico_nat <- format_ICO_data(t2_60_ico_nat.raw)
t2_60_ico_zf <- format_ICO_data(t2_60_ico_zf.raw)

# T2_400_ICO
t2_400_ico_bin <- format_ICO_data(t2_400_ico_bin.raw)
t2_400_ico_pbin <- format_ICO_data(t2_400_ico_pbin.raw)
t2_400_ico_nat <- format_ICO_data(t2_400_ico_nat.raw)
t2_400_ico_zf <- format_ICO_data(t2_400_ico_zf.raw)

#### Save reformatted data ####

save(t1_60_man_bin,t1_60_man_nat,t1_60_man_zf,t2_60_man_bin,t2_60_man_nat,t2_60_man_zf,
     t2_400_man_bin,t2_400_man_pbin,t2_400_man_nat,t2_400_man_zf,t1_60_ico_bin,t1_60_ico_nat,
     t1_60_ico_zf,t2_60_ico_bin,t2_60_ico_nat,t2_60_ico_zf,t2_400_ico_bin,t2_400_ico_pbin,
     t2_400_ico_nat,t2_400_ico_zf, file = "reformatted_raw_data.RData")

#### Table of indicies for trimming, cutting, and splitting ####

ordered_df_names <- as.character(alist(t1_60_man_bin, t2_60_man_bin, t1_60_ico_bin, t2_60_ico_bin,
                                       t2_400_ico_bin, t2_400_man_bin,
                                       t2_60_man_nat, t1_60_ico_nat, t2_60_ico_nat, t1_60_man_nat, t2_400_ico_pbin, t2_400_man_pbin,
                                       t2_60_man_zf, t1_60_ico_zf, t2_60_ico_zf, t1_60_man_zf,
                                       t2_400_ico_nat, t2_400_man_nat,
                                       t2_400_ico_zf, t2_400_man_zf))
saveRDS(ordered_df_names, file = "ordered_df_names.rds")


# Manually determined in mestrenova

mrn_names <- readRDS("ordered_df_names.rds")

mrn_points <- data.frame(names = mrn_names,
                         untrim_length = sapply(mrn_names, function(x) length(get(x))),
                         # Taken off the start ppm = 0 when trimmed to full df
                         #              1  2  3  4  5  6  7  8  9  10   11   12 13 14 15  16   17 18   19 20
                         trim_start = c(0, 1, 1, 1, 4, 2, 0, 0, 0, 15, 365, 344, 0, 0, 0, 30, 100, 0, 100, 0),
                         # Taken off the end ppm = 11/14 when trimmed to full df
                         #              1  2  3  4  5  6  7  8  9  10   11   12 13 14 15  16 17   18 19   20
                         trim_end   = c(1, 3, 3, 3, 0, 3, 0, 0, 0,  7, 489, 512, 0, 1, 1, 15, 0, 110, 0, 119),
                         # Index closest to '0' without losing info (via MestReNova, delta between ie: 244:232 MRN point index backwards) 
                         #                  1   2   3   4    5    6    7     8    9   10    11    12    13    14    15    16    17    18     19     20
                         split_cut_0   = c(12, 13, 13, 13,  56,  55,  95,   95,  95, 109,  460,  432,  201,  196,  197,  202, 3057, 3057,  5794,  5794), 
                         # Index that 7.5ppm occurs at roughly (via MestReNova)
                         #                  1   2   3   4    5    6    7     8    9   10    11    12    13    14    15    16    17    18     19     20
                         cut_7.5_raw   = c(69, 71, 71, 71, 129, 131, 600,  600, 600, 608, 1089, 1119, 1204, 1210, 1209, 1248, 6626, 6636, 13248, 13267),
                         # Index closest to 7.5 (outside) without losing info (via MestReNova, max ppm (from len) - 7.5ppm point (MRN point index backwards))
                         #                  1   2   3   4    5    6    7    8     9    10    11    12    13    14    15    16    17    18     19     20
                         split_cut_7.5_raw = c(73, 72, 73, 72,  131, 131, 625,  610,  625,  610, 1091, 1091, 1258, 1234, 1258, 1234, 6764, 6764, 13526, 13526),
                         # Start of first split, make sure that start ppm is adjusted for the offset of the length vs the expected length in MRN
                         #                     1  2  3  4   5   6   7    8    9   10    11   12   13   14   15   16    17    18    19    20
                         split1_start_raw = c(NA,NA,NA,NA,138,138, 676,678, 676, 678, 1158,1158,1350,1372,1350,1372, 7038, 7038,14082,14082),
                         split1_end_raw   = c(NA,NA,NA,NA,141,141, 722,715, 722, 715, 1205,1205,1460,1441,1464,1441, 7327, 7327,14718,14718),
                         split2_start_raw = c(NA,NA,NA,NA, NA, NA,  NA,772,  NA, 772, 1547,1547,  NA,1581,  NA,1581, 7881, 7881,15762,15762),
                         split2_end_raw   = c(NA,NA,NA,NA, NA, NA,  NA,858,  NA, 858, 1570,1570,  NA,1710,  NA,1710, 7976, 7976,15976,15976),
                         split3_start_raw = c(NA,NA,NA,NA, NA, NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA, 9411, 9411,18810,18810),
                         split3_end_raw   = c(NA,NA,NA,NA, NA, NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA, 9577, 9577,19175,19175),
                         split4_start_raw = c(NA,NA,NA,NA, NA, NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,10848,10848,19439,19439),
                         split4_end_raw   = c(NA,NA,NA,NA, NA, NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,11073,11073,19624,19624),
                         split5_start_raw = c(NA,NA,NA,NA, NA, NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,   NA,   NA,21666,21666),
                         split5_end_raw   = c(NA,NA,NA,NA, NA, NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,   NA,   NA,21877,21877))

mrn_points <- mrn_points %>%
  add_column(cut_7.5 = .$untrim_length - .$cut_7.5_raw) %>%
  add_column(split_cut_7.5 = .$untrim_length - .$split_cut_7.5_raw) %>%
  add_column(split1_start = .$untrim_length - .$split1_end_raw) %>%
  add_column(split1_end = .$untrim_length - .$split1_start_raw) %>%
  add_column(split2_start = .$untrim_length - .$split2_end_raw) %>%
  add_column(split2_end = .$untrim_length - .$split2_start_raw) %>%
  add_column(split3_start = .$untrim_length - .$split3_end_raw) %>%
  add_column(split3_end = .$untrim_length - .$split3_start_raw) %>%
  add_column(split4_start = .$untrim_length - .$split4_end_raw) %>%
  add_column(split4_end = .$untrim_length - .$split4_start_raw) %>%
  add_column(split5_start = .$untrim_length - .$split5_end_raw) %>%
  add_column(split5_end = .$untrim_length - .$split5_start_raw) %>% 
  `rownames<-`(NULL)

save(mrn_points, file = "mrn_points.RData")

#### Full (Trimmed) Spectral Window ####

# load("mrn_points.RData")
# readRDS("ordered_df_names.rds")
# load("reformatted_raw_data.RData")

full_df_names <- clean.full_fn(input_rds = "ordered_df_names.rds",
                               cut_start = mrn_points$trim_start,
                               cut_end = mrn_points$trim_end,
                               save_rdata = "full_data.RData")
saveRDS(full_df_names, file = "full_df_names.rds")

full_ppm_info <- ppm_info_fn(input_rds = "full_df_names.rds",
                             data = "full_data.RData",
                             output_rds = "full_ppm_info.rds")

full_ppm_vectors <- ppm_vector_fn(input_rds = "full_df_names.rds",
                                  save_rdata = "full_ppm_vectors.RData")
saveRDS(full_ppm_vectors, file = "full_ppm_vectors.rds")

full_lineplot_data_names <- create_long_df_fn(input_rds = "full_df_names.rds",
                                              ppm_rdata = "full_ppm_vectors.RData",
                                              ppm_list = full_ppm_vectors,
                                              save_rdata = "full_lineplot_data.RData")
saveRDS(full_lineplot_data_names, file = "full_lineplot_data_names.rds")

full_lineplot_names_list <- raw_nmr_lineplot_fn(input_rds = "full_lineplot_data_names.rds",
                                                sample_class = "full",
                                                save_rdata = "full_lineplots.RData")

#### Cut Spectral Window ####

# load("mrn_points.RData")
# readRDS("ordered_df_names.rds")
# load("reformatted_raw_data.RData")

cut_df_names <- clean.cut_fun(input_rds = "ordered_df_names.rds",
                              cut_start = mrn_points$split_cut_0,
                              cut_end = mrn_points$cut_7.5,
                              save_rdata = "cut_data.RData")
saveRDS(cut_df_names, file = "cut_df_names.rds")

cut_ppm_info <- ppm_info_fn(input_rds = "cut_df_names.rds",
                            data = "cut_data.RData",
                            output_rds = "cut_ppm_info.rds")

cut_ppm_vectors <- ppm_vector_fn(input_rds = "cut_df_names.rds",
                                 save_rdata = "cut_ppm_vectors.RData")
saveRDS(cut_ppm_vectors, file = "cut_ppm_vectors.rds")

cut_lineplot_data_names <- create_long_df_fn(input_rds = "cut_df_names.rds",
                                             ppm_rdata = "cut_ppm_vector.RData",
                                             ppm_list = cut_ppm_vectors,
                                             save_rdata = "cut_lineplot_data.RData")
saveRDS(cut_lineplot_data_names, file = "cut_lineplot_data_names.rds")

cut_lineplot_names_list <- raw_nmr_lineplot_fn(input_rds = "cut_lineplot_data_names.rds",
                                               sample_class = "cut",
                                               save_rdata = "cut_lineplots.RData")

#### Split spectral window ####

# load("mrn_points.RData")
# readRDS("ordered_df_names.rds")
# load("reformatted_raw_data.RData")

split_df_names <- clean.split_fn(input_rds = "ordered_df_names.rds",
                                 save_rdata = "split_data.RData")
saveRDS(split_df_names, file = "split_df_names.rds")

split_ppm_info <- ppm_info_fn(input_rds = "split_df_names.rds",
                              data = "split_data.RData",
                              output_rds = "split_ppm_info.rds")

split_ppm_vectors <- ppm_vector_fn(input_rds = "split_df_names.rds",
                                   save_rdata = "split_ppm_vectors.RData")
saveRDS(split_ppm_vectors, file = "split_ppm_vectors.rds")

split_lineplot_data_names <- create_long_df_fn(input_rds = "split_df_names.rds",
                                               ppm_rdata = "split_ppm_vector.RData",
                                               ppm_list = split_ppm_vectors,
                                               save_rdata = "split_lineplot_data.RData")
saveRDS(split_lineplot_data_names, file = "split_lineplot_data_names.rds")

split_lineplot_names_list <- raw_nmr_lineplot_fn(input_rds = "split_lineplot_data_names.rds",
                                                 sample_class = "split",
                                                 save_rdata = "split_lineplots.RData")

#### oliveID mutation ####

oliveID.full_names <- oliveID_rm.ppm_fn(input_rds = "full_df_names.rds",
                                        save_rdata = "oliveID.full_data.RData")
saveRDS(oliveID.full_names, "oliveID.full_names.rds")
oliveID.cut_names <- oliveID_rm.ppm_fn(input_rds = "cut_df_names.rds",
                                       save_rdata = "oliveID.cut_data.RData")
saveRDS(oliveID.cut_names, "oliveID.cut_names.rds")
oliveID.split_names <- oliveID_rm.ppm_fn(input_rds = "split_df_names.rds",
                                         save_rdata = "oliveID.split_data.RData")
saveRDS(oliveID.split_names, "oliveID.split_names.rds")

#### Data partition indices ####

seed_list <- c(1,2,3)

load("names_list.RData")

partition_list <- partition_fn(names_list = names_list,
                               partition = 0.8,
                               seed_list = seed_list)

#### inTrain data partition ####

# load("mrn_points.RData")
# load("oliveID.full_data.RData")
# load("oliveID.cut_data.RData")
# load("oliveID.split_data.RData")


inTrain.full_names <- inTrain_fn(input_rds = "oliveID.full_names.rds",
                                 partition_list = partition_list,
                                 seed_list = seed_list,
                                 save_rdata = "inTrain.full_data.RData")
saveRDS(inTrain.full_names, "inTrain.full_names.rds")

inTrain.cut_names <- inTrain_fn(input_rds = "oliveID.cut_names.rds",
                                partition_list = partition_list,
                                seed_list = seed_list,
                                save_rdata = "inTrain.cut_data.RData")
saveRDS(inTrain.cut_names, "inTrain.cut_names.rds")

inTrain.split_names <- inTrain_fn(input_rds = "oliveID.split_names.rds",
                                  partition_list = partition_list,
                                  seed_list = seed_list,
                                  save_rdata = "inTrain.split_data.RData")
saveRDS(inTrain.split_names, "inTrain.split_names.rds")

#### inTest data partition ####

inTest.full_names <- inTest_fn(input_rds = "oliveID.full_names.rds",
                               partition_list = partition_list,
                               seed_list = seed_list,
                               save_rdata = "inTest.full_data.RData")
saveRDS(inTest.full_names, "inTest.full_names.rds")

inTest.cut_names <- inTrain_fn(input_rds = "oliveID.cut_names.rds",
                               partition_list = partition_list,
                               seed_list = seed_list,
                               save_rdata = "inTest.cut_data.RData")
saveRDS(inTest.cut_names, "inTest.cut_names.rds")

inTest.split_names <- inTrain_fn(input_rds = "oliveID.split_names.rds",
                                 partition_list = partition_list,
                                 seed_list = seed_list,
                                 save_rdata = "inTest.split_data.RData")
saveRDS(inTest.split_names, "inTest.split_names.rds")

#### Centering data ####

# load("inTrain.full_data.RData")
# load("inTrain.cut_data.RData")
# load("inTrain.split_data.RData")
# load("inTest.full_data.RData")
# load("inTest.cut_data.RData")
# load("inTest.split_data.RData")

c_inTrain.full_names <- center_fn(input_rds = "inTrain.full_names.rds",
                                  save_rdata = "c_inTrain.full_data.RData")
saveRDS(c_inTrain.full_names, file = "c_inTrain.full_names.rds")
c_inTest.full_names <- center_fn(input_rds = "inTest.full_names.rds",
                                 save_rdata = "c_inTest.full_data.RData")
saveRDS(c_inTest.full_names, file = "c_inTest.full_names.rds")

c_inTrain.cut_names <- center_fn(input_rds = "inTrain.cut_names.rds",
                                 save_rdata = "c_inTrain.cut_data.RData")
saveRDS(c_inTrain.cut_names, file = "c_inTrain.cut_names.rds")
c_inTest.cut_names <- center_fn(input_rds = "inTrain.cut_names.rds",
                                save_rdata = "c_inTest.cut_data.RData")
saveRDS(c_inTest.cut_names, file = "c_inTest.cut_names.rds")

c_inTrain.split_names <- center_fn(input_rds = "inTrain.split_names.rds",
                                   save_rdata = "c_inTrain.split_data.RData")
saveRDS(c_inTrain.split_names, file = "c_inTrain.split_names.rds")
c_inTest.split_names <- center_fn(input_rds = "inTrain.split_names.rds",
                                  save_rdata = "c_inTest.split_data.RData")
saveRDS(c_inTest.split_names, file = "c_inTest.split_names.rds")

#### Long centered nested data ####

# load("c_inTrain.full_data.RData")
# load("c_inTest.full_data.RData")
# load("c_inTrain.cut_data.RData")
# load("c_inTest.cut_data.RData")
# load("c_inTrain.split_data.RData")
# load("c_inTest.split_data.RData")

c_inTrain.full_lineplot_data_names <- create_nested_long_df_fn(input_rds = "c_inTrain.full_names.rds",
                                                               ppm_rdata = "full_ppm_vectors.RData",
                                                               ppm_names_rds = "full_ppm_vectors.rds",
                                                               save_rdata = "c_inTrain.full_lineplots_data.RData")
saveRDS(c_inTrain.full_lineplot_data_names, "c_inTrain.full_lineplot_data_names.rds")
c_inTest.full_lineplot_data_names <- create_nested_long_df_fn(input_rds = "c_inTest.full_names.rds",
                                                              ppm_rdata = "full_ppm_vectors.RData",
                                                              ppm_names_rds = "full_ppm_vectors.rds",
                                                              save_rdata = "c_inTest.full_lineplots_data.RData")
saveRDS(c_inTest.full_lineplot_data_names, "c_inTest.full_lineplot_data_names.rds")

c_inTrain.cut_lineplot_data_names <- create_nested_long_df_fn(input_rds = "c_inTrain.cut_names.rds",
                                                              ppm_rdata = "cut_ppm_vectors.RData",
                                                              ppm_names_rds = "cut_ppm_vectors.rds",
                                                              save_rdata = "c_inTrain.cut_lineplots_data.RData")
saveRDS(c_inTrain.cut_lineplot_data_names, "c_inTrain.cut_lineplot_data_names.rds")
c_inTest.cut_lineplot_data_names <- create_nested_long_df_fn(input_rds = "c_inTest.cut_names.rds",
                                                             ppm_rdata = "cut_ppm_vectors.RData",
                                                             ppm_names_rds = "cut_ppm_vectors.rds",
                                                             save_rdata = "c_inTest.cut_lineplots_data.RData")
saveRDS(c_inTest.cut_lineplot_data_names, "c_inTest.cut_lineplot_data_names.rds")


c_inTrain.split_lineplot_data_names <- create_nested_long_df_fn(input_rds = "c_inTrain.split_names.rds",
                                                                ppm_rdata = "split_ppm_vectors.RData",
                                                                ppm_names_rds = "split_ppm_vectors.rds",
                                                                save_rdata = "c_inTrain.split_lineplots_data.RData")
saveRDS(c_inTrain.split_lineplot_data_names, "c_inTrain.split_lineplot_data_names.rds")
c_inTest.split_lineplot_data_names <- create_nested_long_df_fn(input_rds = "c_inTest.split_names.rds",
                                                               ppm_rdata = "split_ppm_vectors.RData",
                                                               ppm_names_rds = "split_ppm_vectors.rds",
                                                               save_rdata = "c_inTest.split_lineplots_data.RData")
saveRDS(c_inTest.split_lineplot_data_names, "c_inTest.split_lineplot_data_names.rds")

#### Plot centered data ####

# load("c_inTrain.full_lineplots_data.RData")
# load("c_inTest.full_lineplots_data.RData")
# load("c_inTrain.cut_lineplots_data.RData")
# load("c_inTest.cut_lineplots_data.RData")
# load("c_inTrain.split_lineplots_data.RData")
# load("c_inTest.split_lineplots_data.RData")
# load("reformatted_raw_data.RData")

c_inTrain.full_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "c_inTrain.full_lineplot_data_names.rds",
                                                        sample_class = "full",
                                                        save_rdata = "c_inTrain.full_lineplots.RData")
c_inTest.full_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "c_inTest.full_lineplot_data_names.rds",
                                                       sample_class = "full",
                                                       save_rdata = "c_inTest.full_lineplots.RData")

c_inTrain.cut_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "c_inTrain.cut_lineplot_data_names.rds",
                                                       sample_class = "cut",
                                                       save_rdata = "c_inTrain.cut_lineplots.RData")
c_inTest.cut_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "c_inTest.cut_lineplot_data_names.rds",
                                                      sample_class = "cut",
                                                      save_rdata = "c_inTest.cut_lineplots.RData")

c_inTrain.split_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "c_inTrain.split_lineplot_data_names.rds",
                                                         sample_class = "split",
                                                         save_rdata = "c_inTrain.split_lineplots.RData")
c_inTest.split_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "c_inTest.split_lineplot_data_names.rds",
                                                        sample_class = "split",
                                                        save_rdata = "c_inTest.split_lineplots.RData")

#### Auto scaling data ####

# load("c_inTrain.full_data.RData")
# load("c_inTrain.cut_data.RData")
# load("c_inTrain.split_data.RData")
# load("c_inTest.full_data.RData")
# load("c_inTest.cut_data.RData")
# load("c_inTest.split_data.RData")

as_inTrain.full_names <- autoscale_fn(input_rds = "c_inTrain.full_names.rds",
                                      save_rdata = "as_inTrain.full_data.RData")
saveRDS(as_inTrain.full_names, file = "as_inTrain.full_names.rds")
as_inTest.full_names <- autoscale_fn(input_rds = "c_inTest.full_names.rds",
                                     save_rdata = "as_inTest.full_data.RData")
saveRDS(as_inTest.full_names, file = "as_inTest.full_names.rds")

as_inTrain.cut_names <- autoscale_fn(input_rds = "c_inTrain.cut_names.rds",
                                     save_rdata = "as_inTrain.cut_data.RData")
saveRDS(as_inTrain.cut_names, file = "as_inTrain.cut_names.rds")
as_inTest.cut_names <- autoscale_fn(input_rds = "c_inTest.cut_names.rds",
                                    save_rdata = "as_inTest.cut_data.RData")
saveRDS(as_inTest.cut_names, file = "as_inTest.cut_names.rds")

as_inTrain.split_names <- autoscale_fn(input_rds = "c_inTrain.split_names.rds",
                                       save_rdata = "as_inTrain.split_data.RData")
saveRDS(as_inTrain.split_names, file = "as_inTrain.split_names.rds")
as_inTest.split_names <- autoscale_fn(input_rds = "c_inTest.split_names.rds",
                                      save_rdata = "as_inTest.split_data.RData")
saveRDS(as_inTest.split_names, file = "as_inTest.split_names.rds")

#### Long autoscaled nested data ####

# load("as_inTrain.full_data.RData")
# load("as_inTest.full_data.RData")
# load("as_inTrain.cut_data.RData")
# load("as_inTest.cut_data.RData")
# load("as_inTrain.split_data.RData")
# load("as_inTest.split_data.RData")

as_inTrain.full_lineplot_data_names <- create_nested_long_df_fn(input_rds = "as_inTrain.full_names.rds",
                                                                ppm_rdata = "full_ppm_vectors.RData",
                                                                ppm_names_rds = "full_ppm_vectors.rds",
                                                                save_rdata = "as_inTrain.full_lineplots_data.RData")
saveRDS(as_inTrain.full_lineplot_data_names, "as_inTrain.full_lineplot_data_names.rds")
as_inTest.full_lineplot_data_names <- create_nested_long_df_fn(input_rds = "as_inTest.full_names.rds",
                                                               ppm_rdata = "full_ppm_vectors.RData",
                                                               ppm_names_rds = "full_ppm_vectors.rds",
                                                               save_rdata = "as_inTest.full_lineplots_data.RData")
saveRDS(as_inTest.full_lineplot_data_names, "as_inTest.full_lineplot_data_names.rds")

as_inTrain.cut_lineplot_data_names <- create_nested_long_df_fn(input_rds = "as_inTrain.cut_names.rds",
                                                               ppm_rdata = "cut_ppm_vectors.RData",
                                                               ppm_names_rds = "cut_ppm_vectors.rds",
                                                               save_rdata = "as_inTrain.cut_lineplots_data.RData")
saveRDS(as_inTrain.cut_lineplot_data_names, "as_inTrain.cut_lineplot_data_names.rds")
as_inTest.cut_lineplot_data_names <- create_nested_long_df_fn(input_rds = "as_inTest.cut_names.rds",
                                                              ppm_rdata = "cut_ppm_vectors.RData",
                                                              ppm_names_rds = "cut_ppm_vectors.rds",
                                                              save_rdata = "as_inTest.cut_lineplots_data.RData")
saveRDS(as_inTest.cut_lineplot_data_names, "as_inTest.cut_lineplot_data_names.rds")


as_inTrain.split_lineplot_data_names <- create_nested_long_df_fn(input_rds = "as_inTrain.split_names.rds",
                                                                 ppm_rdata = "split_ppm_vectors.RData",
                                                                 ppm_names_rds = "split_ppm_vectors.rds",
                                                                 save_rdata = "as_inTrain.split_lineplots_data.RData")
saveRDS(as_inTrain.split_lineplot_data_names, "as_inTrain.split_lineplot_data_names.rds")
as_inTest.split_lineplot_data_names <- create_nested_long_df_fn(input_rds = "as_inTest.split_names.rds",
                                                                ppm_rdata = "split_ppm_vectors.RData",
                                                                ppm_names_rds = "split_ppm_vectors.rds",
                                                                save_rdata = "as_inTest.split_lineplots_data.RData")
saveRDS(as_inTest.split_lineplot_data_names, "as_inTest.split_lineplot_data_names.rds")

#### Plot autoscaled data ####

# load("as_inTrain.full_lineplots_data.RData")
# load("as_inTest.full_lineplots_data.RData")
# load("as_inTrain.cut_lineplots_data.RData")
# load("as_inTest.cut_lineplots_data.RData")
# load("as_inTrain.split_lineplots_data.RData")
# load("as_inTest.split_lineplots_data.RData")

as_inTrain.full_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "as_inTrain.full_lineplot_data_names.rds",
                                                         sample_class = "full",
                                                         save_rdata = "as_inTrain.full_lineplots.RData")
as_inTest.full_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "as_inTest.full_lineplot_data_names.rds",
                                                        sample_class = "full",
                                                        save_rdata = "as_inTest.full_lineplots.RData")

as_inTrain.cut_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "as_inTrain.cut_lineplot_data_names.rds",
                                                        sample_class = "cut",
                                                        save_rdata = "as_inTrain.cut_lineplots.RData")
as_inTest.cut_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "as_inTest.cut_lineplot_data_names.rds",
                                                       sample_class = "cut",
                                                       save_rdata = "as_inTest.cut_lineplots.RData")

as_inTrain.split_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "as_inTrain.split_lineplot_data_names.rds",
                                                          sample_class = "split",
                                                          save_rdata = "as_inTrain.split_lineplots.RData")
as_inTest.split_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "as_inTest.split_lineplot_data_names.rds",
                                                         sample_class = "split",
                                                         save_rdata = "as_inTest.split_lineplots.RData")

#### Pareto scaling data ####

# load("c_inTrain.full_data.RData")
# load("c_inTrain.cut_data.RData")
# load("c_inTrain.split_data.RData")
# load("c_inTest.full_data.RData")
# load("c_inTest.cut_data.RData")
# load("c_inTest.split_data.RData")

ps_inTrain.full_names <- paretoscale_fn(input_rds = "c_inTrain.full_names.rds",
                                        save_rdata = "ps_inTrain.full_data.RData")
saveRDS(ps_inTrain.full_names, file = "ps_inTrain.full_names.rds")
ps_inTest.full_names <- paretoscale_fn(input_rds = "c_inTest.full_names.rds",
                                       save_rdata = "ps_inTest.full_data.RData")
saveRDS(ps_inTest.full_names, file = "ps_inTest.full_names.rds")

ps_inTrain.cut_names <- paretoscale_fn(input_rds = "c_inTrain.cut_names.rds",
                                       save_rdata = "ps_inTrain.cut_data.RData")
saveRDS(ps_inTrain.cut_names, file = "ps_inTrain.cut_names.rds")
ps_inTest.cut_names <- paretoscale_fn(input_rds = "c_inTest.cut_names.rds",
                                      save_rdata = "ps_inTest.cut_data.RData")
saveRDS(ps_inTest.cut_names, file = "ps_inTest.cut_names.rds")

ps_inTrain.split_names <- paretoscale_fn(input_rds = "c_inTrain.split_names.rds",
                                         save_rdata = "ps_inTrain.split_data.RData")
saveRDS(ps_inTrain.split_names, file = "ps_inTrain.split_names.rds")
ps_inTest.split_names <- paretoscale_fn(input_rds = "c_inTest.split_names.rds",
                                        save_rdata = "ps_inTest.split_data.RData")
saveRDS(ps_inTest.split_names, file = "ps_inTest.split_names.rds")

#### Long paretoscaled nested data ####

# load("ps_inTrain.full_data.RData")
# load("ps_inTest.full_data.RData")
# load("ps_inTrain.cut_data.RData")
# load("ps_inTest.cut_data.RData")
# load("ps_inTrain.split_data.RData")
# load("ps_inTest.split_data.RData")

ps_inTrain.full_lineplot_data_names <- create_nested_long_df_fn(input_rds = "ps_inTrain.full_names.rds",
                                                                ppm_rdata = "full_ppm_vectors.RData",
                                                                ppm_names_rds = "full_ppm_vectors.rds",
                                                                save_rdata = "ps_inTrain.full_lineplots_data.RData")
saveRDS(ps_inTrain.full_lineplot_data_names, "ps_inTrain.full_lineplot_data_names.rds")
ps_inTest.full_lineplot_data_names <- create_nested_long_df_fn(input_rds = "ps_inTest.full_names.rds",
                                                               ppm_rdata = "full_ppm_vectors.RData",
                                                               ppm_names_rds = "full_ppm_vectors.rds",
                                                               save_rdata = "ps_inTest.full_lineplots_data.RData")
saveRDS(ps_inTest.full_lineplot_data_names, "ps_inTest.full_lineplot_data_names.rds")

ps_inTrain.cut_lineplot_data_names <- create_nested_long_df_fn(input_rds = "ps_inTrain.cut_names.rds",
                                                               ppm_rdata = "cut_ppm_vectors.RData",
                                                               ppm_names_rds = "cut_ppm_vectors.rds",
                                                               save_rdata = "ps_inTrain.cut_lineplots_data.RData")
saveRDS(ps_inTrain.cut_lineplot_data_names, "ps_inTrain.cut_lineplot_data_names.rds")
ps_inTest.cut_lineplot_data_names <- create_nested_long_df_fn(input_rds = "ps_inTest.cut_names.rds",
                                                              ppm_rdata = "cut_ppm_vectors.RData",
                                                              ppm_names_rds = "cut_ppm_vectors.rds",
                                                              save_rdata = "ps_inTest.cut_lineplots_data.RData")
saveRDS(ps_inTest.cut_lineplot_data_names, "ps_inTest.cut_lineplot_data_names.rds")


ps_inTrain.split_lineplot_data_names <- create_nested_long_df_fn(input_rds = "ps_inTrain.split_names.rds",
                                                                 ppm_rdata = "split_ppm_vectors.RData",
                                                                 ppm_names_rds = "split_ppm_vectors.rds",
                                                                 save_rdata = "ps_inTrain.split_lineplots_data.RData")
saveRDS(ps_inTrain.split_lineplot_data_names, "ps_inTrain.split_lineplot_data_names.rds")
ps_inTest.split_lineplot_data_names <- create_nested_long_df_fn(input_rds = "ps_inTest.split_names.rds",
                                                                ppm_rdata = "split_ppm_vectors.RData",
                                                                ppm_names_rds = "split_ppm_vectors.rds",
                                                                save_rdata = "ps_inTest.split_lineplots_data.RData")
saveRDS(ps_inTest.split_lineplot_data_names, "ps_inTest.split_lineplot_data_names.rds")

#### Plot paretoscaled data ####

# load("ps_inTrain.full_lineplots_data.RData")
# load("ps_inTest.full_lineplots_data.RData")
# load("ps_inTrain.cut_lineplots_data.RData")
# load("ps_inTest.cut_lineplots_data.RData")
# load("ps_inTrain.split_lineplots_data.RData")
# load("ps_inTest.split_lineplots_data.RData")
# load("reformatted_raw_data.RData")
# load("split_data.RData")

ps_inTrain.full_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "ps_inTrain.full_lineplot_data_names.rds",
                                                         sample_class = "full",
                                                         save_rdata = "ps_inTrain.full_lineplots.RData")
ps_inTest.full_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "ps_inTest.full_lineplot_data_names.rds",
                                                        sample_class = "full",
                                                        save_rdata = "ps_inTest.full_lineplots.RData")

ps_inTrain.cut_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "ps_inTrain.cut_lineplot_data_names.rds",
                                                        sample_class = "cut",
                                                        save_rdata = "ps_inTrain.cut_lineplots.RData")
ps_inTest.cut_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "ps_inTest.cut_lineplot_data_names.rds",
                                                       sample_class = "cut",
                                                       save_rdata = "ps_inTest.cut_lineplots.RData")

ps_inTrain.split_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "ps_inTrain.split_lineplot_data_names.rds",
                                                          sample_class = "split",
                                                          save_rdata = "ps_inTrain.split_lineplots.RData")
ps_inTest.split_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "ps_inTest.split_lineplot_data_names.rds",
                                                         sample_class = "split",
                                                         save_rdata = "ps_inTest.split_lineplots.RData")


#### SNV scaling data ####

# load("inTrain.full_data.RData")
# load("inTrain.cut_data.RData")
# load("inTrain.split_data.RData")
# load("inTest.full_data.RData")
# load("inTest.cut_data.RData")
# load("inTest.split_data.RData")

snv_inTrain.full_names <- snv_fn(input_rds = "inTrain.full_names.rds",
                                 save_rdata = "snv_inTrain.full_data.RData")
saveRDS(snv_inTrain.full_names, file = "snv_inTrain.full_names.rds")
snv_inTest.full_names <- snv_fn(input_rds = "inTest.full_names.rds",
                                save_rdata = "snv_inTest.full_data.RData")
saveRDS(snv_inTest.full_names, file = "snv_inTest.full_names.rds")

snv_inTrain.cut_names <- snv_fn(input_rds = "inTrain.cut_names.rds",
                                save_rdata = "snv_inTrain.cut_data.RData")
saveRDS(snv_inTrain.cut_names, file = "snv_inTrain.cut_names.rds")
snv_inTest.cut_names <- snv_fn(input_rds = "inTest.cut_names.rds",
                               save_rdata = "snv_inTest.cut_data.RData")
saveRDS(snv_inTest.cut_names, file = "snv_inTest.cut_names.rds")

snv_inTrain.split_names <- snv_fn(input_rds = "inTrain.split_names.rds",
                                  save_rdata = "snv_inTrain.split_data.RData")
saveRDS(snv_inTrain.split_names, file = "snv_inTrain.split_names.rds")
snv_inTest.split_names <- snv_fn(input_rds = "inTest.split_names.rds",
                                 save_rdata = "snv_inTest.split_data.RData")
saveRDS(snv_inTest.split_names, file = "snv_inTest.split_names.rds")

#### SNV scaled nested data ####

# load("snv_inTrain.full_data.RData")
# load("snv_inTest.full_data.RData")
# load("snv_inTrain.cut_data.RData")
# load("snv_inTest.cut_data.RData")
# load("snv_inTrain.split_data.RData")
# load("snv_inTest.split_data.RData")

snv_inTrain.full_lineplot_data_names <- create_nested_long_df_fn(input_rds = "snv_inTrain.full_names.rds",
                                                                 ppm_rdata = "full_ppm_vectors.RData",
                                                                 ppm_names_rds = "full_ppm_vectors.rds",
                                                                 save_rdata = "snv_inTrain.full_lineplots_data.RData")
saveRDS(snv_inTrain.full_lineplot_data_names, "snv_inTrain.full_lineplot_data_names.rds")
snv_inTest.full_lineplot_data_names <- create_nested_long_df_fn(input_rds = "snv_inTest.full_names.rds",
                                                                ppm_rdata = "full_ppm_vectors.RData",
                                                                ppm_names_rds = "full_ppm_vectors.rds",
                                                                save_rdata = "snv_inTest.full_lineplots_data.RData")
saveRDS(snv_inTest.full_lineplot_data_names, "snv_inTest.full_lineplot_data_names.rds")

snv_inTrain.cut_lineplot_data_names <- create_nested_long_df_fn(input_rds = "snv_inTrain.cut_names.rds",
                                                                ppm_rdata = "cut_ppm_vectors.RData",
                                                                ppm_names_rds = "cut_ppm_vectors.rds",
                                                                save_rdata = "snv_inTrain.cut_lineplots_data.RData")
saveRDS(snv_inTrain.cut_lineplot_data_names, "snv_inTrain.cut_lineplot_data_names.rds")
snv_inTest.cut_lineplot_data_names <- create_nested_long_df_fn(input_rds = "snv_inTest.cut_names.rds",
                                                               ppm_rdata = "cut_ppm_vectors.RData",
                                                               ppm_names_rds = "cut_ppm_vectors.rds",
                                                               save_rdata = "snv_inTest.cut_lineplots_data.RData")
saveRDS(snv_inTest.cut_lineplot_data_names, "snv_inTest.cut_lineplot_data_names.rds")


snv_inTrain.split_lineplot_data_names <- create_nested_long_df_fn(input_rds = "snv_inTrain.split_names.rds",
                                                                  ppm_rdata = "split_ppm_vectors.RData",
                                                                  ppm_names_rds = "split_ppm_vectors.rds",
                                                                  save_rdata = "snv_inTrain.split_lineplots_data.RData")
saveRDS(snv_inTrain.split_lineplot_data_names, "snv_inTrain.split_lineplot_data_names.rds")
snv_inTest.split_lineplot_data_names <- create_nested_long_df_fn(input_rds = "snv_inTest.split_names.rds",
                                                                 ppm_rdata = "split_ppm_vectors.RData",
                                                                 ppm_names_rds = "split_ppm_vectors.rds",
                                                                 save_rdata = "snv_inTest.split_lineplots_data.RData")
saveRDS(snv_inTest.split_lineplot_data_names, "snv_inTest.split_lineplot_data_names.rds")

#### plot SNV scaled data ####

# load("snv_inTrain.full_lineplots_data.RData")
# load("snv_inTest.full_lineplots_data.RData")
# load("snv_inTrain.cut_lineplots_data.RData")
# load("snv_inTest.cut_lineplots_data.RData")
# load("snv_inTrain.split_lineplots_data.RData")
# load("snv_inTest.split_lineplots_data.RData")
# load("reformatted_raw_data.RData")
# load("split_data.RData")

snv_inTrain.full_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "snv_inTrain.full_lineplot_data_names.rds",
                                                          sample_class = "full",
                                                          save_rdata = "snv_inTrain.full_lineplots.RData")
snv_inTest.full_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "snv_inTest.full_lineplot_data_names.rds",
                                                         sample_class = "full",
                                                         save_rdata = "snv_inTest.full_lineplots.RData")

snv_inTrain.cut_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "snv_inTrain.cut_lineplot_data_names.rds",
                                                         sample_class = "cut",
                                                         save_rdata = "snv_inTrain.cut_lineplots.RData")
snv_inTest.cut_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "snv_inTest.cut_lineplot_data_names.rds",
                                                        sample_class = "cut",
                                                        save_rdata = "snv_inTest.cut_lineplots.RData")

snv_inTrain.split_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "snv_inTrain.split_lineplot_data_names.rds",
                                                           sample_class = "split",
                                                           save_rdata = "snv_inTrain.split_lineplots.RData")
snv_inTest.split_lineplot_names <- nested_nmr_lineplot_fn(input_rds = "snv_inTest.split_lineplot_data_names.rds",
                                                          sample_class = "split",
                                                          save_rdata = "snv_inTest.split_lineplots.RData")

#### ####

