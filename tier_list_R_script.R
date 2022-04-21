# Select the file.
char_file_select = 6
# Harsher duplicate mode selection =>   harsh_mode = TRUE.
# Fairer duplicate mode selection =>   harsh_mode = FALSE.
harsh_mode = TRUE

in_filename = switch(char_file_select,
                     "rivals_tier_list.csv",
                     "rivals_tier_list_reduced.csv",
                     "rivals_tier_list_noanom.csv",
                     "rivals_tier_list_wclust.csv",
                     "rivals_tier_list_wcreduced.csv",
                     "rivals_tier_list_wcnoanom.csv"
       )

char_tier_data = read.csv(in_filename)
rm(char_file_select)
# Renaming the empty column heading for the reduced data sets.
# Reduced data sets are generated from data_cleaning_script.R.
if(char_file_select %in% c(2,5)){
  colnames(char_tier_data)[1] = "Character"
}

# FUNCTIONS.

# Calculates the dissimilarity between
# a tier character object and a cluster
# mode.
dissim = function(obj,clust_mode){
  return(length(obj) - (sum(obj == clust_mode)))
}

# Used in the find_mode function.
# Calculates a vector of modes of
# the variable x.
mode_vector = function(x){
  u = unique(x)
  tab = tabulate(match(x, u))
  return(u[tab == max(tab)])
}

# This function will return the mode
# of a given vector x.
find_mode = function(x){
  temp = sort(mode_vector(x),harsh_mode)
  length_temp = length(temp)
  # If there is only one mode.
  if (length_temp == 1){
    return(temp)
  }
  # For multiple modes, the middle value is
  # selected. For even numbered list sizes,
  # the left-most middle value is used.
  # For harsh_mode = TRUE, this means
  # the higher of the two values is used,
  # and vice versa for harsh_mode = FALSE.
  if (length_temp%%2 == 0){
    return(temp[length_temp/2])
  }
  return(temp[ceiling(length_temp/2)])
}

# Saves a bar graph at the location
# given by file_name.
save_bar = function(x, file_name, color_str){
  u = unique(x)
  u = sort(u)
  tab = tabulate(match(x, u))
  png(file = file_name)
  barplot(height = tab,names.arg=u,
          xlab = "Placing",
          ylab = "Freq.",
          col = color_str,
          cex.axis = 1.75,
          cex.names = 1.75,
          cex.lab = 1.5)
  dev.off()
  rm(u, x, tab, file_name, color_str)
}


# Calculates the cluster allocation
# of a given character based on the dissimilarity
# function.
char_cluster_alloc = function(obj, clust_modes, char_mode){
  # Start with a large (unphysical) number for the
  # minimum dissimilarity, as well as arbitrary cluster no. and mode.
  min_dissim = 1e20
  clust_no = 0
  mode_temp = 0
  k = 1
  # Support is a vector of dissimilarities for each cluster.
  support = list()
  for (clust_mode in clust_modes){
    dissim_temp = dissim(obj,clust_mode)
    support = append(support,dissim_temp)
    # If less than the min_dissim, we assign
    # this as the new cluster.
    if (dissim_temp < min_dissim){
      min_dissim = dissim_temp
      clust_no = k
      mode_temp = clust_mode
    # If the temp val equals the min_dissim,
    # then we assign the cluster based on the
    # proximity to the character's mode.
    }else if(dissim_temp == min_dissim){
      if(abs(clust_mode - char_mode) < abs(mode_temp - char_mode)){
        min_dissim = dissim_temp
        clust_no = k
        mode_temp = clust_mode
      }
    }
    k = k + 1
  }
  rm(min_dissim,mode_temp,k,dissim_temp)
  return(list("cluster" = clust_no, "support" = support))
}


# We can now create the collection
# of characters as a list of vectors,
# which allows us to use functions more
# easily.
n_players = ncol(char_tier_data) - 1
n_chars = nrow(char_tier_data)
char_names = char_tier_data$Character

# Casting data into list forms
# for use in the above functions.
char_mat = list()
char_modes = list()
char_votes = list()
for (i in 1:n_chars){
  temp = c()
  for (j in 2:(n_players+1)){
    temp = append(temp,as.numeric(char_tier_data[i,][j]))
  }
  char_mat[[i]] = temp
  char_modes[[i]] = find_mode(temp)
  # Used for sorting within tiers, since there are ties
  # for the mode.
  char_votes[[i]] = sum(char_mat[[i]] == char_modes[[i]])
}
rm(temp)
# We can save histograms of the character placing distributions.
# We colour code this by character.
colour_codes = c("#FF007B",
                 "#D3081E",
                 "#FF5C00",
                 "#62B0C9", 
                 "#9C4C79", 
                 "#F3CF84", 
                 "#C9D658", 
                 "#81AF77", 
                 "#946050", 
                 "#43C7FF", 
                 "#1CF5FF", 
                 "#6CC73B", 
                 "#33FFFF",
                 "#FCCA22")


# Save bar plots as individual files, and format them
# together in the latex document.
for (i in 1:n_chars){
  data = char_mat[[i]]
  file_name = paste("bar_",char_names[[i]],sep = "")
  file_name = paste(file_name,".png",sep = "")
  save_bar(x = data, file_name = file_name, color_str = colour_codes[i])
}
rm(data, file_name)

# Initial conditions.
n_clust = 4
clust_modes_ini = floor(seq(1,n_chars,length.out = n_clust))
clust_modes = clust_modes_ini
clust_modes_prev = rep(-1,n_clust)
n_iter = 100
# main()
for (iter in 1:n_iter){
  cluster_ranks = c()
  support_ranks = c()
  for (i in 1:n_chars){
    # Allocating character i to the cluster.
    clust_alloc_temp = char_cluster_alloc(obj = char_mat[[i]],
                                     clust_modes = clust_modes,
                                     char_mode = char_modes[[i]])
    cluster_ranks = append(cluster_ranks,clust_alloc_temp$"cluster")
    support_ranks[[i]] = clust_alloc_temp$"support"
  }
  rm(i)
  char_sort_ranks = data.frame(char_id = c(1:n_chars),
                          char_name = char_names,
                          cluster = cluster_ranks)
  rm(clust_alloc_temp,cluster_ranks)

  # Adding new columns to the char_sort_ranks dataframe
  # to show the support for each cluster.
  for (j in 1:n_clust){
    my_str = paste("dissim_",as.character(j), sep = "")
    temp = c()
    for (i in 1:n_chars){
      temp = append(temp,support_ranks[[i]][[j]])
    }
    char_sort_ranks[my_str] = temp
  }
  rm(i, j, temp, my_str, support_ranks)
  char_sort_ranks = char_sort_ranks[order(char_sort_ranks$cluster),]


  # Appending character data to each cluster.
  i = 1
  cluster_nums = c()
  cluster_chars = c()
  cluster_modes = c()
  cluster_votes = c()
  k = 1
  temp = c()
  temp_2 = c()
  temp_3 = c()
  temp_4 = c()
  mode = 0
  while (i <= n_chars){
    if(char_sort_ranks$cluster[i] == k){
      # temp_var is the slice of the original dataframe
      # corresponding to the player rankings.
      temp_var = char_mat[[char_sort_ranks$char_id[i]]]
      # We append the character rankings to the temp variable.
      temp = append(temp,temp_var)
      # We append the name of the character to the temp_2 variable.
      temp_2 = append(temp_2,char_sort_ranks$char_name[i])
      mode = char_modes[[char_sort_ranks$char_id[i]]]
      # We append the mode to temp_3.
      temp_3 = append(temp_3,mode)
      # And the vote to temp_4.
      vote = char_votes[[char_sort_ranks$char_id[i]]] 
      temp_4 = append(temp_4, vote)
      i = i + 1
    }else{
      # In this condition, the cluster has changed value,
      # so now we append and then cycle to the next cluster.
      cluster_nums[[k]] = temp
      cluster_chars[[k]] = temp_2
      cluster_modes[[k]] = temp_3
      cluster_votes[[k]] = temp_4
      k = k + 1
      temp = c()
      temp_2 = c()
      temp_3 = c()
      temp_4 = c()
    }
  }
  rm(i)
  # Repeat of the above else condition, since it
  # does not trigger for the last cluster.
  cluster_nums[[k]] = temp
  cluster_chars[[k]] = temp_2
  cluster_modes[[k]] = temp_3
  cluster_votes[[k]] = temp_4
  rm(k, temp_var, mode, vote, temp, temp_2, temp_3, temp_4)

  # Calculating the new cluster modes, which is calculated
  # as the modal player ranking of each character in the cluster.
  clust_modes = c()
  for (k in 1:n_clust){
    mode_temp =  find_mode(cluster_nums[[k]])
    clust_modes = append(clust_modes,mode_temp)
    if (is.nan(mode_temp)){
      print("ERROR: cluster mode not a number. Breaking.")
      print(c(cluster_nums[[k]], mode_temp))
      break
    }
  }
  rm(k, mode_temp, cluster_nums)

  # If the cluster modes do not change from the previous
  # iteration, then the cluster allocation step will not change.
  # This is considered to be converged.
  if (sum(clust_modes == clust_modes_prev) == length(clust_modes)){
    print("Break condition achieved (convergence).")
    break
  }
  clust_modes_prev = clust_modes
}
rm(char_mat)

# Now we can create a tier list, which is a list
# of dataframes corresponding to the cluster.
tier_list = list()
for (k in 1:n_clust){
  temp_df = data.frame("Characters" = cluster_chars[[k]],
                       "Mode" = cluster_modes[[k]],
                       "Vote" = cluster_votes[[k]])
  temp_df = temp_df[order(temp_df$"Mode", decreasing = FALSE),]
  tier_list[[k]] = temp_df
}
rm(k,temp_df,cluster_chars,cluster_modes,cluster_votes,char_modes,char_votes)

# Now we can add a column which shows the
# dissimilarity per character.
dissim_vec = c()
for (i in 1:nrow(char_sort_ranks)){
  row_temp = char_sort_ranks[i, ]
  my_str = paste("dissim_",as.character(row_temp$cluster),sep = "")
  dissim_vec = append(dissim_vec,as.numeric(row_temp[my_str]))
}
rm(i, row_temp, my_str)
char_sort_ranks["dissim"] = dissim_vec
rm(dissim_vec)
dissum = sum(char_sort_ranks$dissim)
rm(char_names, clust_modes_prev)
