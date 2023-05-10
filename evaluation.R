library(caret)
library(tidyr)
library(tidyverse)
library(dplyr)

set.seed(1)

#### Identify correlated predictors ####

# Section 3.3: http://topepo.github.io/caret/pre-processing.html

#### Find linear dependencies ####

# Section 3.4: http://topepo.github.io/caret/pre-processing.html


load("as_inTest.full_data.RData")
load("as_inTest.cut_data.RData")
load("as_inTest.split_data.RData")

load("ps_inTest.full_data.RData")
load("ps_inTest.cut_data.RData")
load("ps_inTest.split_data.RData")

load("snv_inTest.full_data.RData")
load("snv_inTest.cut_data.RData")
load("snv_inTest.split_data.RData")