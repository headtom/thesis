#### Packages ####

# Not sure if these are even needed since the work is done elsewhere
# Included for redundancy

# data_cleaning.R
library(tidyr)
library(purrr)
library(dplyr)
library(ggplot2)
library(tidyverse)

#### Stored Objects ####

#### data_cleaning ####

# Data prep'd for preprocessing

##### Pre-cut, trimmed, full ####

# A list of names for cleaned spectral data before processing
# n = 20
# Parameters:
# - t1/t2
# - 60/400
# - man/ico
# - bin/nat/pbin
preCut_list

# A list of df names, ppm start, ppm end, delta, and n_points
# for df before trimming heads/tails
ppm_untrim_eval_list

uncut_60_bin_list
uncut_400_bin_list
uncut_60_nat_list
uncut_400_nat_list
uncut_60_zf_list
uncut_400_zf_list

# A list of df names, ppm start, ppm end, delta, and n_points
# after initial df's have had heads/tails trimmed and n_points
# standardized.
# Parameters:
# - Full spectral width, not adjusted from acq (beyond trim)
ppm_uncut_eval_list

# A list of df names for the full spectral window df's
uncut_df_names
full_df_names <- uncut_df_names

# A named list for each full spectral width ppm vector
ppm_list_uncut

# Dataframe containing narrow data for 
uncut_nmr_lp_df

# A named list of ggplot objects:
# NMR lineplots of each full spectral width df grouped by sample ID
# probably don't run this, calls all 20 plots
uncut_nmr_lplot_list

###### Easy full_nmr_lp call list #####
uncut_nmr_lplot_list$t1_60_man_bin_full_nmr_lplot
uncut_nmr_lplot_list$t2_60_man_bin_full_nmr_lplot
uncut_nmr_lplot_list$t1_60_ico_bin_full_nmr_lplot
uncut_nmr_lplot_list$t2_60_ico_bin_full_nmr_lplot
uncut_nmr_lplot_list$t2_400_ico_bin_full_nmr_lplot
uncut_nmr_lplot_list$t2_400_man_bin_full_nmr_lplot
uncut_nmr_lplot_list$t2_60_man_nat_full_nmr_lplot
uncut_nmr_lplot_list$t1_60_ico_nat_full_nmr_lplot
uncut_nmr_lplot_list$t2_60_ico_nat_full_nmr_lplot
uncut_nmr_lplot_list$t1_60_man_nat_full_nmr_lplot
uncut_nmr_lplot_list$t2_400_ico_pbin_full_nmr_lplot
uncut_nmr_lplot_list$t2_400_man_pbin_full_nmr_lplot
uncut_nmr_lplot_list$t2_60_man_zf_full_nmr_lplot
uncut_nmr_lplot_list$t1_60_ico_zf_full_nmr_lplot
uncut_nmr_lplot_list$t2_60_ico_zf_full_nmr_lplot
uncut_nmr_lplot_list$t1_60_man_zf_full_nmr_lplot
uncut_nmr_lplot_list$t2_400_ico_nat_full_nmr_lplot
uncut_nmr_lplot_list$t2_400_man_nat_full_nmr_lplot
uncut_nmr_lplot_list$t2_400_ico_zf_full_nmr_lplot
uncut_nmr_lplot_list$t2_400_man_zf_full_nmr_lplot

##### Cut df's ####

# A list of df names for cut spectral window df's
cut_df_names

# A named list for each cut spectral width df's
ppm_cut_eval_list

# Dataframe containing narrow data for ggplot
cut_nmr_lp_df

# A named list of ggplot objects:
# NMR lineplots of each cut spectral width df grouped by sample ID
# probably don't run this, calls all 20 plots
cut_nmr_lplot_list

###### Easy cut_nmr_lp call list #####
cut_nmr_lplot_list$t1_60_man_bin_cut_nmr_lplot
cut_nmr_lplot_list$t2_60_man_bin_cut_nmr_lplot
cut_nmr_lplot_list$t1_60_ico_bin_cut_nmr_lplot
cut_nmr_lplot_list$t2_60_ico_bin_cut_nmr_lplot
cut_nmr_lplot_list$t2_400_ico_bin_cut_nmr_lplot
cut_nmr_lplot_list$t2_400_man_bin_cut_nmr_lplot
cut_nmr_lplot_list$t2_60_man_nat_cut_nmr_lplot
cut_nmr_lplot_list$t1_60_ico_nat_cut_nmr_lplot
cut_nmr_lplot_list$t2_60_ico_nat_cut_nmr_lplot
cut_nmr_lplot_list$t1_60_man_nat_cut_nmr_lplot
cut_nmr_lplot_list$t2_400_ico_pbin_cut_nmr_lplot
cut_nmr_lplot_list$t2_400_man_pbin_cut_nmr_lplot
cut_nmr_lplot_list$t2_60_man_zf_cut_nmr_lplot
cut_nmr_lplot_list$t1_60_ico_zf_cut_nmr_lplot
cut_nmr_lplot_list$t2_60_ico_zf_cut_nmr_lplot
cut_nmr_lplot_list$t1_60_man_zf_cut_nmr_lplot
cut_nmr_lplot_list$t2_400_ico_nat_cut_nmr_lplot
cut_nmr_lplot_list$t2_400_man_nat_cut_nmr_lplot
cut_nmr_lplot_list$t2_400_ico_zf_cut_nmr_lplot
cut_nmr_lplot_list$t2_400_man_zf_cut_nmr_lplot

##### Split df's ####

# Table of relevent info from manually obs of MestReNova for split and cut
mrn_points_df

# A list of df names for split spectral window df's
split_df_names

# A named list for each split spectral width df's
ppm_split_eval_list

# A named list of all the ppm values
ppm_list_split

# Dataframe containing narrow data for ggplot
split_nmr_lp_df

# A named list of ggplot objects:
# NMR lineplots of each cut spectral width df grouped by sample ID
# probably don't run this, calls all 20 plots
split_nmr_lplot_list

###### Easy split_nmr_lp call list #####
split_nmr_lplot_list$t1_60_man_bin_split_nmr_lplot
split_nmr_lplot_list$t2_60_man_bin_split_nmr_lplot
split_nmr_lplot_list$t1_60_ico_bin_split_nmr_lplot
split_nmr_lplot_list$t2_60_ico_bin_split_nmr_lplot
split_nmr_lplot_list$t2_400_ico_bin_split_nmr_lplot
split_nmr_lplot_list$t2_400_man_bin_split_nmr_lplot
split_nmr_lplot_list$t2_60_man_nat_split_nmr_lplot
split_nmr_lplot_list$t1_60_ico_nat_split_nmr_lplot
split_nmr_lplot_list$t2_60_ico_nat_split_nmr_lplot
split_nmr_lplot_list$t1_60_man_nat_split_nmr_lplot
split_nmr_lplot_list$t2_400_ico_pbin_split_nmr_lplot
split_nmr_lplot_list$t2_400_man_pbin_split_nmr_lplot
split_nmr_lplot_list$t2_60_man_zf_split_nmr_lplot
split_nmr_lplot_list$t1_60_ico_zf_split_nmr_lplot
split_nmr_lplot_list$t2_60_ico_zf_split_nmr_lplot
split_nmr_lplot_list$t1_60_man_zf_split_nmr_lplot
split_nmr_lplot_list$t2_400_ico_nat_split_nmr_lplot
split_nmr_lplot_list$t2_400_man_nat_split_nmr_lplot
split_nmr_lplot_list$t2_400_ico_zf_split_nmr_lplot
split_nmr_lplot_list$t2_400_man_zf_split_nmr_lplot


#### data_preprocessing ####

# Data processed and ready for model training

clean.full_names
clean.cut_names
clean.split_names

partition_list

full_names_inTrain
cut_names_inTrain
split_names_inTrain

c_inTrain.full_nmr_lp_df
c_inTrain.full_nmr_lplot_list

###### Easy call list ######
c_inTrain.full_nmr_lplot_list[[1]][1]
c_inTrain.full_nmr_lplot_list[[1]][2]
c_inTrain.full_nmr_lplot_list[[1]][3]
c_inTrain.full_nmr_lplot_list[[1]][4]
c_inTrain.full_nmr_lplot_list[[1]][5]
c_inTrain.full_nmr_lplot_list[[1]][6]
c_inTrain.full_nmr_lplot_list[[1]][7]
c_inTrain.full_nmr_lplot_list[[1]][8]
c_inTrain.full_nmr_lplot_list[[1]][9]
c_inTrain.full_nmr_lplot_list[[1]][10]
c_inTrain.full_nmr_lplot_list[[1]][11]
c_inTrain.full_nmr_lplot_list[[1]][12]
c_inTrain.full_nmr_lplot_list[[1]][13]
c_inTrain.full_nmr_lplot_list[[1]][14]
c_inTrain.full_nmr_lplot_list[[1]][15]
c_inTrain.full_nmr_lplot_list[[1]][16]
c_inTrain.full_nmr_lplot_list[[1]][17]
c_inTrain.full_nmr_lplot_list[[1]][18]
c_inTrain.full_nmr_lplot_list[[1]][19]
c_inTrain.full_nmr_lplot_list[[1]][20]

#####
c_inTest.full_nmr_lp_df
c_inTest.full_nmr_lplot_list

###### Easy call list ######
c_inTest.full_nmr_lplot_list[[1]][1]
c_inTest.full_nmr_lplot_list[[1]][2]
c_inTest.full_nmr_lplot_list[[1]][3]
c_inTest.full_nmr_lplot_list[[1]][4]
c_inTest.full_nmr_lplot_list[[1]][5]
c_inTest.full_nmr_lplot_list[[1]][6]
c_inTest.full_nmr_lplot_list[[1]][7]
c_inTest.full_nmr_lplot_list[[1]][8]
c_inTest.full_nmr_lplot_list[[1]][9]
c_inTest.full_nmr_lplot_list[[1]][10]
c_inTest.full_nmr_lplot_list[[1]][11]
c_inTest.full_nmr_lplot_list[[1]][12]
c_inTest.full_nmr_lplot_list[[1]][13]
c_inTest.full_nmr_lplot_list[[1]][14]
c_inTest.full_nmr_lplot_list[[1]][15]
c_inTest.full_nmr_lplot_list[[1]][16]
c_inTest.full_nmr_lplot_list[[1]][17]
c_inTest.full_nmr_lplot_list[[1]][18]
c_inTest.full_nmr_lplot_list[[1]][19]
c_inTest.full_nmr_lplot_list[[1]][20]

#####
c_inTrain.cut_nmr_lp_df
c_inTrain.cut_nmr_lplot_list

###### Easy call list ######
c_inTrain.cut_nmr_lplot_list[[1]][1]
c_inTrain.cut_nmr_lplot_list[[1]][2]
c_inTrain.cut_nmr_lplot_list[[1]][3]
c_inTrain.cut_nmr_lplot_list[[1]][4]
c_inTrain.cut_nmr_lplot_list[[1]][5]
c_inTrain.cut_nmr_lplot_list[[1]][6]
c_inTrain.cut_nmr_lplot_list[[1]][7]
c_inTrain.cut_nmr_lplot_list[[1]][8]
c_inTrain.cut_nmr_lplot_list[[1]][9]
c_inTrain.cut_nmr_lplot_list[[1]][10]
c_inTrain.cut_nmr_lplot_list[[1]][11]
c_inTrain.cut_nmr_lplot_list[[1]][12]
c_inTrain.cut_nmr_lplot_list[[1]][13]
c_inTrain.cut_nmr_lplot_list[[1]][14]
c_inTrain.cut_nmr_lplot_list[[1]][15]
c_inTrain.cut_nmr_lplot_list[[1]][16]
c_inTrain.cut_nmr_lplot_list[[1]][17]
c_inTrain.cut_nmr_lplot_list[[1]][18]
c_inTrain.cut_nmr_lplot_list[[1]][19]
c_inTrain.cut_nmr_lplot_list[[1]][20]

#####
c_inTest.cut_nmr_lp_df
c_inTest.cut_nmr_lplot_list

###### Easy call list ######
c_inTest.cut_nmr_lplot_list[[1]][1]
c_inTest.cut_nmr_lplot_list[[1]][2]
c_inTest.cut_nmr_lplot_list[[1]][3]
c_inTest.cut_nmr_lplot_list[[1]][4]
c_inTest.cut_nmr_lplot_list[[1]][5]
c_inTest.cut_nmr_lplot_list[[1]][6]
c_inTest.cut_nmr_lplot_list[[1]][7]
c_inTest.cut_nmr_lplot_list[[1]][8]
c_inTest.cut_nmr_lplot_list[[1]][9]
c_inTest.cut_nmr_lplot_list[[1]][10]
c_inTest.cut_nmr_lplot_list[[1]][11]
c_inTest.cut_nmr_lplot_list[[1]][12]
c_inTest.cut_nmr_lplot_list[[1]][13]
c_inTest.cut_nmr_lplot_list[[1]][14]
c_inTest.cut_nmr_lplot_list[[1]][15]
c_inTest.cut_nmr_lplot_list[[1]][16]
c_inTest.cut_nmr_lplot_list[[1]][17]
c_inTest.cut_nmr_lplot_list[[1]][18]
c_inTest.cut_nmr_lplot_list[[1]][19]
c_inTest.cut_nmr_lplot_list[[1]][20]

#####
c_inTrain.split_nmr_lp_df
c_inTrain.split_nmr_lplot_list

###### Easy call list ######
c_inTrain.split_nmr_lplot_list[[1]][1]
c_inTrain.split_nmr_lplot_list[[1]][2]
c_inTrain.split_nmr_lplot_list[[1]][3]
c_inTrain.split_nmr_lplot_list[[1]][4]
c_inTrain.split_nmr_lplot_list[[1]][5]
c_inTrain.split_nmr_lplot_list[[1]][6]
c_inTrain.split_nmr_lplot_list[[1]][7]
c_inTrain.split_nmr_lplot_list[[1]][8]
c_inTrain.split_nmr_lplot_list[[1]][9]
c_inTrain.split_nmr_lplot_list[[1]][10]
c_inTrain.split_nmr_lplot_list[[1]][11]
c_inTrain.split_nmr_lplot_list[[1]][12]
c_inTrain.split_nmr_lplot_list[[1]][13]
c_inTrain.split_nmr_lplot_list[[1]][14]
c_inTrain.split_nmr_lplot_list[[1]][15]
c_inTrain.split_nmr_lplot_list[[1]][16]
c_inTrain.split_nmr_lplot_list[[1]][17]
c_inTrain.split_nmr_lplot_list[[1]][18]
c_inTrain.split_nmr_lplot_list[[1]][19]
c_inTrain.split_nmr_lplot_list[[1]][20]

#####
c_inTest.split_nmr_lp_df
c_inTest.split_nmr_lplot_list 

###### Easy call list ######
c_inTest.split_nmr_lplot_list[[1]][1]
c_inTest.split_nmr_lplot_list[[1]][2]
c_inTest.split_nmr_lplot_list[[1]][3]
c_inTest.split_nmr_lplot_list[[1]][4]
c_inTest.split_nmr_lplot_list[[1]][5]
c_inTest.split_nmr_lplot_list[[1]][6]
c_inTest.split_nmr_lplot_list[[1]][7]
c_inTest.split_nmr_lplot_list[[1]][8]
c_inTest.split_nmr_lplot_list[[1]][9]
c_inTest.split_nmr_lplot_list[[1]][10]
c_inTest.split_nmr_lplot_list[[1]][11]
c_inTest.split_nmr_lplot_list[[1]][12]
c_inTest.split_nmr_lplot_list[[1]][13]
c_inTest.split_nmr_lplot_list[[1]][14]
c_inTest.split_nmr_lplot_list[[1]][15]
c_inTest.split_nmr_lplot_list[[1]][16]
c_inTest.split_nmr_lplot_list[[1]][17]
c_inTest.split_nmr_lplot_list[[1]][18]
c_inTest.split_nmr_lplot_list[[1]][19]
c_inTest.split_nmr_lplot_list[[1]][20]
