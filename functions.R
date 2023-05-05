library(tidyr)
library(purrr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(readr)
library(caret)

set.seed(1)

#### General Functions ####

default_colname_fn <- function(input) {
  new_colnames <- sapply(colnames(input), function(x) {
    if (grepl("^X\\d+", x) || grepl("^V\\d+", x)) {
      x <- paste0("X", as.character(match(x, colnames(input))))
    }
    return(x)
  })
  
  # Assign the new column names to the input data.frame
  colnames(input) <- new_colnames
  
  # Return the modified data.frame
  return(input)
}

default_rowname_fn <- function(input) {
  new_rownames <- sapply(rownames(input), function(x) {
    if (grepl("^X\\d+", x) || grepl("^V\\d+", x)) {
      x <- paste0("V", as.character(match(x, rownames(input))))
    }
    return(x)
  })
  
  # Assign the new row names to the input data.frame
  rownames(input) <- new_rownames
  
  # Return the modified data.frame
  return(input)
}


##### format_MAN_data #####

format_MAN_data <- function(input) {

  load("names_list.RData")
  
  data_raw <- input
  
  data_raw.list <- strsplit(data_raw$V1, "\t")
  data.df <- as.data.frame(do.call(rbind, data_raw.list), stringsAsFactors = FALSE)
  data.df[1, 1] <- "ppm"
  
  t_data.df <- as.data.frame(t(data.df)) # Take input .raw data and transpose it
  data <- lapply(t_data.df[, -1], function(x) as.numeric(x)) # Other than ID col, transform characters into numeric
  
  data <- default_colname_fn(as.data.frame(data)) # Rename col to state with V1
  data <- default_rowname_fn(as.data.frame(data)) # Rename rows to start with X1
  
  # Check parameters to see which names to assign
  if (any(grepl("t1_60", deparse(substitute(input))))) { # t1_60 names
    names_vec <- names_list$T1_60_names
    ID_vec <- names_list$T1_60_ID
  } else if (any(grepl("t2_60", deparse(substitute(input))))) { # t2_60 names
    names_vec <- names_list$T2_60_names
    ID_vec <- names_list$T2_60_ID
  } else if (any(grepl("t2_400", deparse(substitute(input))))) { # t3_400 names
    names_vec <- names_list$T2_400_names
    ID_vec <- names_list$T2_400_ID
  } else {
    stop("Error parsing names") # else print error
  }

  output <- data.frame("ID" = c("ppm", ID_vec), 
                       data) # create output df with ID vec and numeric data
  
  rownames(output) <- c("ppm", names_vec) # Name rows sample name
  return(output) # return output to object
}

##### format_ICO_data #####

format_ICO_data <- function(input) {
 
  load("names_list.RData")
  
  modified_name <- lapply(deparse(substitute(input)), function(x) {x <- sub('.raw', '_ppm', x);x}) # take name.raw and mutate to name_ppm
  modified_name <- modified_name[[1]] # take list of lenght 1 and turn into unlisted vector
  
  data <- input[-1,] # Remove ppm col from input .raw data
  
  data <- default_colname_fn(data) # renaming col names to start at V1
  data <- default_rowname_fn(data) # renaming row names to start at X1
  
  # Check parameters to see which names to assign
  if (any(grepl("t1_60", deparse(substitute(input))))) { # t1_60 names
    names_vec <- names_list$T1_60_names
    ID_vec <- names_list$T1_60_ID
  } else if (any(grepl("t2_60", deparse(substitute(input))))) { # t2_60 names
    names_vec <- names_list$T2_60_names
    ID_vec <- names_list$T2_60_ID
  } else if (any(grepl("t2_400", deparse(substitute(input))))) { # t2_400 names
    names_vec <- names_list$T2_400_names
    ID_vec <- names_list$T2_400_ID
  } else {
    stop("Error parsing names") # else print error
  }
  
  output <- cbind(data.frame(ID = c("ppm", ID_vec)), # ID vec with "ppm" in prev empty [1,1] index
                  t(data.frame(ppm = ico_ppm_list[[modified_name]], # Create a data frame with a ppm row from ico_ppm_list calling the modified name we mutated
                               t(data)))) # transpose the data to be horizontal instead of vertical to add to ppm vector, and then restransposs
  
  rownames(output) <- c("ppm", names_vec)   # Names rows after sample names
  return(output) # return output df
}

#### clean.full_fn ####

clean.full_fn <- function(input_rds, cut_start, cut_end, save_rdata){
  
  load("reformatted_raw_data.RData")
  
  call_order <- readRDS(input_rds) # order to evaluate the data (ie: reordered names of objects in file_name)
  output_names <- lapply(call_order, function(x) paste0(as.character(x), "_full")) # Take the name each call_order element and suffix '_full' to create list of new names for transformed objects
  
  for(i in 1:length(call_order)){
    
    data <- get(call_order[i])[,c((2+cut_start[i]):(ncol(get(call_order[i]))-cut_end[i]))] # Get the data associated with call_order object name indicies, remove ID col, and then find how many indicies are cut from the left and right
    
    data <- default_colname_fn(data) # renaming col names to start at V1
    data <- default_rowname_fn(data) # renaming row names to start at X1
    
    ID_vec <- get(call_order[i])[,1]
    
    output <- cbind(ID_vec, # input_rds ID vec
                    data) # trimmed and default named data
    
    name <- output_names[[i]] # name of new data is the ith index of the mutated output_names
    assign(name, data, envir = globalenv()) # assign the new data to the new name in the global envir
    assign(name, data) # assign the new data to the new name in the local envir
    }
  
  save(list = unlist(output_names), file = save_rdata)
  return(output_names) # return list of names
}

#### clean.cut_fun ####

clean.cut_fun <- function(input_rds, cut_start, cut_end, save_rdata){
  
  input_list <- readRDS(input_rds) # read the .rds names and store as a list
  output_names <- lapply(input_list, function(x) paste0(as.character(x), "_cut")) # Create a list of transformed output names, suffixing '_cut' to the end of it
  
  for(i in 1:length(input_list)){
    
    name <- output_names[[i]] # ith transformed output
    
    # get associated df
    data <- get(input_list[i])
    len <- length(data)
    output <- rbind(data.frame(ID = data[,1],
                               data[,-(c(1:cut_start[i],cut_end[i]:len))]))
    
    output <- default_colname_fn(output)
    output <- default_rowname_fn(output)
    
    assign(name, output, envir = globalenv()) # assign the new data to the new name in the global envir
    assign(name, output) # assign the new data to the new name in the local envir
    
    }
  
  save(list = unlist(output_names), file = save_rdata)
  return(output_names) # return list of names
}

#### clean.split_fn ####

clean.split_fn <- function(input_rds, save_rdata){
  
  input_list <- readRDS(input_rds)
  load("mrn_points.RData")
  # generate empty list to store objects
  output_names <- lapply(input_list, function(x) paste0(as.character(x), "_split"))
  
  for(i in 1:length(input_list)){
    
    name <- output_names[[i]]
    
    # get associated df
    data <- get(input_list[i])
    len <- length(data)
    
    # Create a list of indices to remove
    remove_indices <- c(1:mrn_points$split_cut_0[i])
    
    if (!is.na(mrn_points$split1_start[i]) && !is.na(mrn_points$split1_end[i])) {
      remove_indices <- c(remove_indices, mrn_points$split1_start[i]:mrn_points$split1_end[i])
    }
    
    if (!is.na(mrn_points$split2_start[i]) && !is.na(mrn_points$split2_end[i])) {
      remove_indices <- c(remove_indices, mrn_points$split2_start[i]:mrn_points$split2_end[i])
    }
    
    if (!is.na(mrn_points$split3_start[i]) && !is.na(mrn_points$split3_end[i])) {
      remove_indices <- c(remove_indices, mrn_points$split3_start[i]:mrn_points$split3_end[i])
    }
    
    if (!is.na(mrn_points$split4_start[i]) && !is.na(mrn_points$split4_end[i])) {
      remove_indices <- c(remove_indices, mrn_points$split4_start[i]:mrn_points$split4_end[i])
    }
    
    if (!is.na(mrn_points$split5_start[i]) && !is.na(mrn_points$split5_end[i])) {
      remove_indices <- c(remove_indices, mrn_points$split5_start[i]:mrn_points$split5_end[i])
    }
    
    remove_indices <- c(0, remove_indices, mrn_points$split_cut_7.5[i]:len)
    remove_indices <- sort(remove_indices[!duplicated(remove_indices)])
    remove_indices <- remove_indices + 1
    
    output <- rbind(data.frame(ID = data[,1],
                               data[,-remove_indices]))
    
    output <- default_colname_fn(output)
    output <- default_rowname_fn(output)
    assign(name, output, envir = globalenv()) # assign the new data to the new name in the global envir
    assign(name, output) # assign the new data to the new name in the local envir
  }
  
  save(list = unlist(output_names), file = save_rdata)
  return(output_names) # return list of names
}

#### ppm_info_fn ####

ppm_info_fn <- function(input_rds, data, output_rds){
  
  # generate empty list to store objects
  output_list <- list()
  load(data)
  input_list <- readRDS(input_rds)
  
  # for ith iteration of the input list
  for(i in 1:length(input_list)){
    # get the associated df
    input <- get(input_list[[i]])
    # take only the ppm column and remove the character header
    ppm <- input[1,-1]
    # store the df name, min ppm, max ppm, average point delta, and number of points
    output <- data.frame("name" = input_list[[i]],
                         "start" = min(ppm),
                         "end" = max(ppm),
                         "mean_delta" = mean(diff(t(ppm))),
                         "n_points" = length(ppm))
    # stores the output in the output_list
    output_list[[i]] <- output
  }
  
  # transform nested list into df
  output_list <- bind_rows(output_list)
  saveRDS(output_list, file = output_rds)
  return(output_list)
}

#### ppm_vector_fn ####

ppm_vector_fn <- function(input_rds, save_rdata){
  
  input_list <- readRDS(input_rds)
  output_names <- lapply(input_list, function(x) paste0(as.character(x), "_ppm_vec"))
  
  for(i in 1:length(input_list)){
    
    name <- output_names[[i]]
    
    # get associated df
    data <- get(input_list[[i]])
    # remove ID column and ppm row
    vec <- data[1,-1]
    # remove names
    names(vec) <- NULL
    
    output <- as.numeric(vec)
    
    assign(name, output, envir = globalenv()) # assign the new data to the new name in the global envir
    assign(name, output) # assign the new data to the new name in the local envir
    }
  
  save(list = unlist(output_names), file = save_rdata)
  return(output_names) # return list of names
}

#### create_long_df_fn ####

create_long_df_fn <- function(input_rds, ppm_rdata, ppm_list, save_rdata){
  
  output_list <- list()
  input_list <- readRDS(input_rds)
  output_names <- lapply(input_list, function(x) paste0(as.character(x), "_lineplot.df"))
  
  load(ppm_rdata)
  
  for(i in 1:length(input_list)){

    name <- output_names[[i]]
    
    input_data <- get(input_list[[i]])[-1,-1]
    
    ppm_vector <- get(ppm_list[[i]])
    
    temp_df <- data.frame(t(input_data),
                          row.names = NULL) %>%
      gather(.) %>%
      add_column(index = rep(c(1:length(input_data)),count(input_data))) %>%
      add_column(ppm = rep(unlist(ppm_vector), count(input_data)))
    
    assign(name, temp_df, envir = globalenv())
    assign(name, temp_df) # assign the new data to the new name in the local envir
    
    output_list[[i]] <- name
  }

  save(list = unlist(output_names), file = save_rdata)
  return(output_names) # return list of names
}

#### raw_nmr_lineplot_fn ####

raw_nmr_lineplot_fn <- function(input_rds, sample_class, save_rdata){
  
  input_list <- readRDS(input_rds)
  plot_names <- gsub(".df", "", input_list)
  
  if(sample_class == "full"){
    
    for(i in 1:length(input_list)){
      
      input_data <- get(input_list[[i]])
      name <- plot_names[i]
      
      plot <- ggplot(data = input_data) +
        geom_line(aes(x = ppm,
                      y = value,
                      color = key)) +
        labs(title = input_list[[i]],
             x = "ppm",
             y = "") +
        theme_minimal() +
        theme(legend.position = "none")
      
      assign(name, plot, envir = globalenv())
      assign(name, plot) # assign the new data to the new name in the local envir
      
    }
  }
  
  if(sample_class == "cut"){
    
    for(i in 1:length(input_list)){
      
      input_data <- get(input_list[[i]])
      name <- plot_names[i]
      
      plot <- ggplot(data = input_data) +
        geom_line(aes(x = ppm,
                      y = value,
                      color = key)) +
        labs(title = input_list[[i]],
             x = "ppm",
             y = "") +
        theme_minimal() +
        theme(legend.position = "none")
      
      assign(name, plot, envir = globalenv())
      assign(name, plot) # assign the new data to the new name in the local envir
      
    }
  }
  
  if(sample_class == "split"){
    
    ordered_df_names <- readRDS("ordered_df_names.rds")
    load("mrn_points.RData")
    
    for(i in 1:length(input_list)){
      
      input_data <- get(input_list[[i]])
      name <- plot_names[i]
      length_split <- (length(get(split_df_names[[i]])) - 1) # Find length of the 'line'; calls wide data, -1 for ID col
      count <- (length(get(split_df_names[[i]])$X1) - 1)
      
      length_cut <- length(get(ordered_df_names[i])) - mrn_points$split_cut_0[i] - mrn_points$cut_7.5_raw[i]
      
      # Pull the ppm_vector of a 'line'
      ppm_vector <- as.numeric(unlist(get(ordered_df_names[i])[1,-1][mrn_points$split_cut_0[i]:(length_cut+mrn_points$split_cut_0[i])]))
      
      # Which index has a difference of > 1 compared to prev value
      # ie: where do the ppm breaks happen in the index
      working_end_break_index <- c(mrn_points$split1_end[i],
                                   mrn_points$split2_end[i],
                                   mrn_points$split3_end[i],
                                   mrn_points$split4_end[i],
                                   mrn_points$split5_end[i])
      working_end_break_index <- working_end_break_index %>%
        unlist(.) %>%
        na.omit(.) %>%
        lapply(., function(x) x - mrn_points$split_cut_0[i]) %>%
        sapply(., '[[', 1) %>%
        as.numeric(.)
      
      working_start_break_index <- c(mrn_points$split1_start[i],
                                     mrn_points$split2_start[i],
                                     mrn_points$split3_start[i],
                                     mrn_points$split4_start[i],
                                     mrn_points$split5_start[i])
      working_start_break_index <- working_start_break_index %>%
        unlist(.) %>%
        na.omit(.) %>%
        lapply(., function(x) x - mrn_points$split_cut_0[i]) %>%
        sapply(., '[[', 1) %>%
        as.numeric(.)
      
      start_cut_ppm <- ppm_vector[working_start_break_index] %>%
        round(.,2)
      end_cut_ppm <- ppm_vector[working_end_break_index] %>%
        round(.,2)
      
      plot <- ggplot(data = input_data) +
        geom_line(aes(x = ppm,
                      y = value,
                      color = key)) +
        labs(title = input_list[[i]],
             x = "ppm",
             y = "") +
        scale_x_continuous(labels = c(0:7),
                           breaks = c(0:7),
                           guide = guide_axis(check.overlap = FALSE,
                                              order = 1,
                                              position = "bottom"),
                           sec.axis = dup_axis(labels = c(start_cut_ppm,end_cut_ppm),
                                               breaks = c(start_cut_ppm,end_cut_ppm),
                                               guide = guide_axis(check.overlap = FALSE,
                                                                  angle = -90,
                                                                  position = "top"))) +
        theme_minimal() +
        theme(legend.position = "none",
              axis.title.x.top = element_blank())+
        annotate("rect", 
                 xmin = c(start_cut_ppm), 
                 xmax = c(end_cut_ppm),
                 ymin = 0,
                 ymax = max(input_data$value),
                 fill = "red",
                 alpha = 0.2)
      
      assign(name, plot, envir = globalenv())
      assign(name, plot) # assign the new data to the new name in the local envir
      
    }
  }
  
  save(list = plot_names, file = save_rdata)
  return(plot_names) # return list of names
}

#### oliveID_rm.ppm_fn ####

oliveID_rm.ppm_fn <- function(input_rds, save_rdata){
  
  input_list <- readRDS(input_rds)
  
  output_names <- lapply(input_list, function(x) paste0("oliveID.", as.character(x)))
  
  for(i in 1:length(input_list)){
    
    name <- output_names[[i]]
    data <- get(input_list[[i]])
    data_clean <- data[-1,-1]
    
    data_clean <- default_colname_fn(data_clean)
    data_clean <- default_rowname_fn(data_clean)
    
    ID_clean <- data[-1,1]

    class_ID <- ifelse(ID_clean == "OOE", "EVOO", "Other")
    
    output <- data.frame(index = c(1:length(ID_clean)),
                         ID = ID_clean,
                         class_ID = class_ID,
                         data_clean)
    
    assign(name, output, envir = globalenv())
    assign(name, output) # assign the new data to the new name in the local envir
    
  }
  
  save(list = unlist(output_names), file = save_rdata)
  return(output_names) # return list of names
}

#### partition_fn ####

partition_fn <- function(names_list, partition, seed_list){
  
  output_list <- list()
  
  for(i in 1:length(seed_list)){
    
    set.seed(seed_list[i])
    
    t1_60_inTrain <- createDataPartition(names_list$t1_60_classID, p = partition)
    t2_60_inTrain <- createDataPartition(names_list$t2_60_classID, p = partition)
    t2_400_inTrain <- createDataPartition(names_list$t2_400_classID, p = partition)
    
    t1_60_name <- paste0("t1_60_inTrain_seed.", i)
    t2_60_name <- paste0("t2_60_inTrain_seed.", i)
    t2_400_name <- paste0("t2_400_inTrain_seed.", i)
    
    assign(t1_60_name, t1_60_inTrain, envir = globalenv())
    assign(t1_60_name, t1_60_inTrain) # assign the new data to the new name in the local envir
    
    assign(t2_60_name, t2_60_inTrain, envir = globalenv())
    assign(t2_60_name, t2_60_inTrain) # assign the new data to the new name in the local envir
    
    assign(t2_400_name, t2_400_inTrain, envir = globalenv())
    assign(t2_400_name, t2_400_inTrain) # assign the new data to the new name in the local envir
    
    output_list[[i]] <- list(t1_60_name, t2_60_name, t2_400_name)
  }
  
  return(output_list)
}

#### inTrain_fn ####

inTrain_fn <- function(input_rds, partition_list, seed_list, save_rdata){
  
  input_list <- readRDS(input_rds)
  output_list <- list()
  which_partition <- gsub("(_man|_ico).*", "", gsub("oliveID.", "", input_list))
  for(i in 1:length(seed_list)){
    
    nested_list <- list()
    nested_list <- lapply(input_list, function(x) paste0(gsub("oliveID.", "", x), "_train.", seed_list[i]))
    for(j in 1:length(input_list)){
      
      name <- nested_list[[j]]
      data <- get(input_list[[j]])
      
      if(which_partition[j] == "t1_60"){
        
        partition_t1_60 <- unlist(get(partition_list[[i]][[1]]))
        output <- data[partition_t1_60,]
      }
      
      if(which_partition[j] == "t2_60"){
        
        partition_t2_60 <- unlist(get(partition_list[[i]][[2]]))
        output <- data[partition_t2_60,]
      }
      
      if(which_partition[j] == "t2_400"){
        
        partition_t2_400 <- unlist(get(partition_list[[i]][[3]]))
        output <- data[partition_t2_400,]
      }
      
      assign(name, output, envir = globalenv())
      assign(name, output) # assign the new data to the new name in the local envir
      
    }
    
    output_list[i] <- list(nested_list)
  }
  
  names(output_list) <- paste0("inTrain.", seed_list)
  save(list = unlist(output_list), file = save_rdata)
  return(output_list) # return nested list
}

#### inTest_fn ####

inTest_fn <- function(input_rds, partition_list, seed_list, save_rdata){
  
  input_list <- readRDS(input_rds)
  output_list <- list()
  which_partition <- gsub("(_man|_ico).*", "", gsub("oliveID.", "", input_list))
  for(i in 1:length(seed_list)){
    
    nested_list <- list()
    nested_list <- lapply(input_list, function(x) paste0(gsub("oliveID.", "", x), "_test.", seed_list[i]))
    for(j in 1:length(input_list)){
      
      name <- nested_list[[j]]
      data <- get(input_list[[j]])
      
      if(which_partition[j] == "t1_60"){
        
        partition_t1_60 <- unlist(get(partition_list[[i]][[1]]))
        output <- data[-partition_t1_60,]
      }
      
      if(which_partition[j] == "t2_60"){
        
        partition_t2_60 <- unlist(get(partition_list[[i]][[2]]))
        output <- data[-partition_t2_60,]
      }
      
      if(which_partition[j] == "t2_400"){
        
        partition_t2_400 <- unlist(get(partition_list[[i]][[3]]))
        output <- data[-partition_t2_400,]
      }
      
      assign(name, output, envir = globalenv())
      assign(name, output) # assign the new data to the new name in the local envir
      
    }
    
    output_list[i] <- list(nested_list)
  }
  
  names(output_list) <- paste0("inTest.", seed_list)
  save(list = unlist(output_list), file = save_rdata)
  return(output_list) # return nested list
}

#### center_fn ####

center_fn <- function(input_rds, save_rdata){
  
  input_list <- readRDS(input_rds)
  output_list <- list()
  
  for(i in 1:length(input_list)){
    
    temp_list <- list()
    
    for(j in 1:length(input_list[[i]])){
      
      raw <- get(input_list[[i]][[j]])
      ID <- raw[,c(1:3)]
      data <- raw[,-c(1:3)]
      name <- paste0(input_list[[i]][[j]],"_c")
      
      data_c <- scale(data, center = TRUE, scale = FALSE)
      
      data_c <- default_colname_fn(data_c)
      
      output <- data.frame(ID,
                           data_c)
      
      assign(name, output, envir = globalenv())
      assign(name, output) # assign the new data to the new name in the local envir
      
      
      temp_list[[j]] <- name
    }
    
    output_list[[i]] <- temp_list
  }
  
  names(output_list) <- paste0(names(input_list), "_c")
  
  save(list = unlist(output_list), file = save_rdata)
  return(output_list) # return nested list
}

#### create_nested_long_df_fn ####

create_nested_long_df_fn <- function(input_rds, ppm_rdata, ppm_names_rds, save_rdata){
  
  output_list <- list()
  input_list <- readRDS(input_rds)
  output_names <- lapply(input_list, function(x) paste0(as.character(x), "_lineplot.df"))
  ppm_list <- readRDS(ppm_names_rds)
  load(ppm_rdata)
  
  for(i in 1:length(input_list)){
    
    temp_df <- list()
    temp_list <- list()
    
    for(j in 1:length(input_list[[i]])){
      
      input_data <- get(input_list[[i]][[j]])[-c(1:3)]
      
      ppm_vector <- get(ppm_list[[j]])
      
      temp_df <- data.frame(t(input_data),
                            row.names = NULL) %>%
        gather(.) %>%
        add_column(index = rep(c(1:length(input_data)),count(input_data))) %>%
        add_column(ppm = rep(ppm_vector, count(input_data)))
      
      name <- paste0(input_list[[i]][[j]],"_lineplot.df")
      
      assign(name, temp_df, envir = globalenv())
      assign(name, temp_df) # assign the new data to the new name in the local envir
      
      temp_list[[j]] <- name
    }
    
    output_list[[i]] <- temp_list
  }
  
  names(output_list) <- paste0(names(input_list), "_lineplot.df")
  
  save(list = unlist(output_list), file = save_rdata)
  return(output_list) # return nested list
}

#### nested_nmr_lineplot_fn ####

nested_nmr_lineplot_fn <- function(input_rds, sample_class, save_rdata){
  
  input_list <- readRDS(input_rds)
  plot_names <- lapply(input_list, function(x) gsub(".df", "", x))
  output_list <- list()
  
  for(i in 1:length(input_list)){
    
    temp_list <- list()
    
    for(j in 1:length(input_list[[i]])){
      
      input_data <- get(input_list[[i]][[j]])
      name <- plot_names[[i]][[j]]
      
      if(sample_class == "full"){
        
        full_df_names <- readRDS("full_df_names.rds")
        
        plot <- ggplot(data = input_data) +
          geom_line(aes(x = ppm,
                        y = value,
                        color = key)) +
          labs(title = gsub("_lineplot", "", name),
               x = "ppm",
               y = "") +
          theme_minimal() +
          theme(legend.position = "none")
      }
      
      if(sample_class == "cut"){
        
        cut_df_names <- readRDS("cut_df_names.rds")
        
        plot <- ggplot(data = input_data) +
          geom_line(aes(x = ppm,
                        y = value,
                        color = key)) +
          labs(title = gsub("_lineplot", "", name),
               x = "ppm",
               y = "") +
          theme_minimal() +
          theme(legend.position = "none")
      }
      
      if(sample_class == "split"){
        
        split_df_names <- readRDS("split_df_names.rds")
        
        ordered_df_names <- readRDS("ordered_df_names.rds")
        load("mrn_points.RData")
        
        length_split <- (length(get(split_df_names[[j]])) - 1) # Find length of the 'line'; calls wide data, -1 for ID col
        count <- (length(get(split_df_names[[j]])$X1) - 1)
        
        length_cut <- length(get(ordered_df_names[j])) - mrn_points$split_cut_0[j] - mrn_points$cut_7.5_raw[j]
        
        # Pull the ppm_vector of a 'line'
        ppm_vector <- as.numeric(unlist(get(ordered_df_names[j])[1,-1][mrn_points$split_cut_0[j]:(length_cut+mrn_points$split_cut_0[j])]))
        
        # Which index has a difference of > 1 compared to prev value
        # ie: where do the ppm breaks happen in the index
        working_end_break_index <- c(mrn_points$split1_end[j],
                                     mrn_points$split2_end[j],
                                     mrn_points$split3_end[j],
                                     mrn_points$split4_end[j],
                                     mrn_points$split5_end[j])
        working_end_break_index <- working_end_break_index %>%
          unlist(.) %>%
          na.omit(.) %>%
          lapply(., function(x) x - mrn_points$split_cut_0[j]) %>%
          sapply(., '[[', 1) %>%
          as.numeric(.)
        
        working_start_break_index <- c(mrn_points$split1_start[j],
                                       mrn_points$split2_start[j],
                                       mrn_points$split3_start[j],
                                       mrn_points$split4_start[j],
                                       mrn_points$split5_start[j])
        working_start_break_index <- working_start_break_index %>%
          unlist(.) %>%
          na.omit(.) %>%
          lapply(., function(x) x - mrn_points$split_cut_0[j]) %>%
          sapply(., '[[', 1) %>%
          as.numeric(.)
        
        start_cut_ppm <- ppm_vector[working_start_break_index] %>%
          round(.,2)
        end_cut_ppm <- ppm_vector[working_end_break_index] %>%
          round(.,2)
        
        plot <- ggplot(data = input_data) +
          geom_line(aes(x = ppm,
                        y = value,
                        color = key)) +
          labs(title = gsub("_lineplot", "", name),
               x = "ppm",
               y = "") +
          scale_x_continuous(labels = c(0:7),
                             breaks = c(0:7),
                             guide = guide_axis(check.overlap = FALSE,
                                                order = 1,
                                                position = "bottom"),
                             sec.axis = dup_axis(labels = c(start_cut_ppm,end_cut_ppm),
                                                 breaks = c(start_cut_ppm,end_cut_ppm),
                                                 guide = guide_axis(check.overlap = FALSE,
                                                                    angle = -90,
                                                                    position = "top"))) +
          theme_minimal() +
          theme(legend.position = "none",
                axis.title.x.top = element_blank())+
          annotate("rect", 
                   xmin = c(start_cut_ppm), 
                   xmax = c(end_cut_ppm),
                   ymin = 0,
                   ymax = max(input_data$value),
                   fill = "red",
                   alpha = 0.2)
      }
      
      assign(name, plot, envir = globalenv())
      assign(name, plot) # assign the new data to the new name in the local envir
      
      temp_list[[j]] <- name
    }
    
    names(temp_list) <- gsub(".df", "", unlist(input_list[[i]]))
    output_list[[i]] <- temp_list
  }
  
  save(list = unlist(plot_names), file = save_rdata)
  
  names(output_list) <- gsub(".df", "", names(input_list))
  return(output_list) # return nested list
}

#### autoscale_fn ####

autoscale_fn <- function(input_rds, save_rdata){
  
  input_list <- readRDS(input_rds)
  output_list <- list()
  
  for(i in 1:length(input_list)){
    
    temp_list <- list()
    
    for(j in 1:length(input_list[[i]])){
      
      raw <- get(input_list[[i]][[j]])
      ID <- raw[,c(1:3)]
      data <- raw[,-c(1:3)]
      name <- gsub("_c", "_as", input_list[[i]][[j]])
      name <- gsub("_asut", "_cut", name)
      
      
      data_as <- scale(data, center = FALSE, scale = TRUE)
      data_as <- default_colname_fn(data_as)
      
      output <- data.frame(ID,
                           data_as)
      
      assign(name, output, envir = globalenv())
      assign(name, output) # assign the new data to the new name in the local envir
      
      temp_list[[j]] <- name
    }
    
    output_list[[i]] <- temp_list
  }
  
  names(output_list) <- gsub("_c", "_as", names(input_list))
  save(list = unlist(output_list), file = save_rdata)
  return(output_list)
}

#### paretoscale_fn ####

paretoscale_fn <- function(input_rds, save_rdata){
  
  input_list <- readRDS(input_rds)
  output_list <- list()
  
  for(i in 1:length(input_list)){
    
    temp_list <- list()
    
    for(j in 1:length(input_list[[i]])){
      
      raw <- get(input_list[[i]][[j]])
      ID <- raw[,c(1:3)]
      data <- raw[,-c(1:3)]
      name <- gsub("_c", "_ps", input_list[[i]][[j]])
      name <- gsub("_psut", "_cut", name)
      
      stdev_sqrt <- sqrt(apply(data, 2, sd))
      
      data_ps <- scale(data, center = FALSE, scale = stdev_sqrt)
      
      data_ps <- default_colname_fn(data_ps)
      
      output <- data.frame(ID,
                           data_ps)
      
      assign(name, output, envir = globalenv())
      assign(name, output) # assign the new data to the new name in the local envir
      
      temp_list[[j]] <- name
    }
    
    output_list[[i]] <- temp_list
  }
  
  names(output_list) <- gsub("_c", "_ps", names(input_list))
  save(list = unlist(output_list), file = save_rdata)
  return(output_list)
}

#### snv_fn ####

snv_fn <- function(input_rds, save_rdata){
  
  input_list <- readRDS(input_rds)
  output_list <- list()
  
  for(i in 1:length(input_list)){
    
    temp_list <- list()
    
    for(j in 1:length(input_list[[i]])){
      
      raw <- get(input_list[[i]][[j]])
      ID <- raw[,c(1:3)]
      data <- raw[,-c(1:3)]
      name <- paste0(input_list[[i]][[j]],"_snv")
      
      data_t <- t(data)
      mean_centered <- apply(data_t, 1, function(x) x - mean(x))
      snv_scaled <- t(apply(mean_centered, 1, function(x) x / sd(x)))
      
      snv_scaled <- default_colname_fn(snv_scaled)
      
      output <- data.frame(ID,
                           snv_scaled)
      
      assign(name, output, envir = globalenv())
      assign(name, output) # assign the new data to the new name in the local envir
      
      temp_list[[j]] <- name
    }
    
    output_list[[i]] <- temp_list
  }
  
  names(output_list) <- paste0(names(input_list), "_snv")
  save(list = unlist(output_list), file = save_rdata)
  return(output_list)
}
