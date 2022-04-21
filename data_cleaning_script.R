# Select the file.
player_file_select = 2
# Harsher duplicate mode selection =>   harsh_mode = TRUE.
# Fairer duplicate mode selection =>   harsh_mode = FALSE.
harsh_mode = TRUE
in_filename = switch(player_file_select,
					 "rivals_tier_list_transposed.csv",
					 "rivals_tier_list_wctransposed.csv"
					 )
player_tier_data = read.csv(in_filename)

# FUNCTIONS.

# Calculates the dissimilarity between
# a player tier list and a cluster
# mode.
dissim = function(obj,clust_mode){
  return(length(obj) - (sum(obj == clust_mode)))
}

# Calculates the proximity of a player
# tier list to a cluster mode. Used
# for tie-breaking to reduce randomness.
proxim = function(obj,clust_mode){
  return(sum(abs(obj-clust_mode)))
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
  # For dup_mode_select = 1, this means
  # the higher of the two values is used,
  # and vice versa for dup_mode_select = 2.
  if (length_temp%%2 == 0){
    return(temp[length_temp/2])
  }
  return(temp[ceiling(length_temp/2)])
}

# Calculates the cluster allocation
# of a given character based on the dissimilarity
# function.
player_cluster_alloc = function(obj, clust_modes){
  # Start with a large (unphysical) number for the
  # minimum dissimilarity, as well as arbitrary cluster no. and mode.
  min_dissim = 1e20
  clust_no = 0
  mode_temp = 0
  prox_temp = 0
  prox_temp_2 = 0
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
      prox_temp = proxim(obj,clust_mode)
      # If the temp val equals the min val,
      # then we assign the cluster using the proxim
      # function (less random than coinflip).
    }else if(dissim_temp == min_dissim){
      prox_temp_2 = proxim(obj,clust_mode)
      if(prox_temp_2 < prox_temp){
        min_dissim = dissim_temp
        clust_no = k
        mode_temp = clust_mode
        prox_temp = prox_temp_2
      }
    }
    k = k + 1
  }
  rm(min_dissim,mode_temp,k,dissim_temp, prox_temp, prox_temp_2)
  return(list("cluster" = clust_no, "support" = support))
}

# We can now create the collection
# of characters as a list of vectors,
# which allows us to use functions more
# easily.
n_players = nrow(player_tier_data)
n_chars = ncol(player_tier_data) - 1
player_names = player_tier_data$Player
player_mat = list()
for (i in 1:n_players){
  temp = c()
  for (j in 2:(n_chars+1)){
    temp = append(temp,as.numeric(player_tier_data[i,][j]))
  }
  player_mat[[i]] = temp
}
rm(i, temp)
# Initial conditions.
clust_modes = c()
n_clust = 9
clust_choice = c(1:n_clust)
for (i in 1:n_clust){
  clust_modes[[i]] = player_mat[[clust_choice[i]]]
}
rm(i)

n_iter = 100
cluster_players_prev = rep("",n_clust)
# main()
for (iter in 1:n_iter){
  cluster_ranks = c()
  support_ranks = c()
  # Allocating players to clusters.
  for (i in 1:n_players){
    clust_alloc_temp = player_cluster_alloc(obj = player_mat[[i]],
                                     clust_modes = clust_modes)
    cluster_ranks = append(cluster_ranks,clust_alloc_temp$"cluster")
    support_ranks[[i]] = clust_alloc_temp$"support"
  }
  rm(i)
  
  player_sort_ranks = data.frame(player_id = c(1:n_players),
                          player_name = player_names,
                          cluster = cluster_ranks)
  rm(clust_alloc_temp,cluster_ranks)
  
  # Adding new columns to the player_sort_ranks dataframe
  # to show the support for each cluster.
  for (j in 1:n_clust){
    my_str = paste("dissim_",as.character(j), sep = "")
    temp = c()
    for (i in 1:n_players){
      temp = append(temp,support_ranks[[i]][[j]])
    }
    player_sort_ranks[my_str] = temp
  }
  rm(i, j, temp, my_str, support_ranks)  
  player_sort_ranks = player_sort_ranks[order(player_sort_ranks$cluster),]
  
  i = 1
  j = 1
  cluster_nums = c()
  cluster_players = c()
  k = 1
  temp = c()
  temp_2 = c()
  while (i <= n_players){
    if(player_sort_ranks$cluster[i] == k){
      # temp_var is the slice of the original dataframe
      # corresponding to the player rankings.
      temp[[j]] = player_mat[[player_sort_ranks$player_id[i]]]
      # We append the name of the player to the temp_2 variable.
      temp_2 = append(temp_2,player_sort_ranks$player_name[i])
      i = i + 1
      j = j + 1
    }else{
      # In this condition, the cluster has changed value,
      # so now we append and then cycle to the next cluster.
      cluster_nums[[k]] = temp
      cluster_players[[k]] = temp_2
      k = k + 1
      j = 1
      temp = c()
      temp_2 = c()
    }
  }
  # Repeat of the above else condition, since it
  # does not trigger for the last cluster.
  cluster_nums[[k]] = temp
  cluster_players[[k]] = temp_2
  rm(temp,temp_2, i, j, k)
  
  # More verbose code for testing if the actual
  # clusters are different.
  flag = FALSE
  for (k in 1:n_clust){
    temp_clust_names = cluster_players[[k]]
    temp_clust_names_prev = cluster_players_prev[[k]]
    if (length(temp_clust_names) != length(temp_clust_names_prev)){
      flag = FALSE
      break
    }else{
      if (sum(sort(temp_clust_names) == sort(temp_clust_names_prev)) == length(temp_clust_names)){
        flag = TRUE
      }else{
        flag = FALSE
        break
      }
    }
  }
  rm(temp_clust_names, temp_clust_names_prev, k)
  
  if (flag){
    print("Break condition achieved (convergence).")
    break
  }
  
  
  # Now re-calculating the cluster modes.
  for (i in 1:n_clust){
    temp_clust_mode = c()
    for (k in 1:n_chars){
      modes_temp = c()
      for (j in 1:length(cluster_nums[[i]])){
        modes_temp = append(modes_temp, cluster_nums[[i]][[j]][k])
      }
      temp_clust_mode = append(temp_clust_mode, find_mode(x = modes_temp))
    }
    clust_modes[[i]] = temp_clust_mode
  }
  rm(temp_clust_mode,modes_temp, i, j, k)
  
  # Now reassigning the cluster_chars_prev
  # to cluster_chars.
  cluster_players_prev = cluster_players
}
rm(flag)

# Now we can add a column which shows the
# dissimilarity per player
dissim_vec = c()
for (i in 1:nrow(player_sort_ranks)){
  row_temp = player_sort_ranks[i, ]
  my_str = paste("dissim_",as.character(row_temp$cluster),sep = "")
  dissim_vec = append(dissim_vec,as.numeric(row_temp[my_str]))
}
rm(i, row_temp, my_str)
# Calculating the dissimilarity/cost function
# for this clustering.
player_sort_ranks["dissim"] = dissim_vec
rm(dissim_vec)
dissum = sum(player_sort_ranks$dissim)


# Removing the anomalous tier lists.
# All clusters containing the anomalous lists in the video are removed.
# If the number of players is scaled up, this method could be implemented
# to remove clusters of average ELO/MMR below a certain cut-off value.
anom_player_ids = c(14,31,36,20)
clusters_to_remove = c(unique(player_sort_ranks[player_sort_ranks$player_id %in% anom_player_ids,]$cluster))
player_names_new = player_sort_ranks[!(player_sort_ranks$cluster %in% clusters_to_remove),]$player_name
player_tier_data_new = player_tier_data[player_tier_data$Player %in% player_names_new,]
# To transpose the data properly, we need to remove the string column "Player".
# Setting the row names as the player names.
row.names(player_tier_data_new) = player_tier_data_new$Player
# Dropping the player column.
player_tier_data_new = subset(player_tier_data_new, select = -c(Player))
# Transposing the dataframe.
player_tier_data_tred = t(player_tier_data_new)
file_name = getwd()
out_str = switch(player_file_select,
				 "/rivals_tier_list_reduced.csv",
				 "/rivals_tier_list_wcreduced.csv"
				 )
file_name = paste(file_name, out_str, sep = "")
# Writing the reduced data-set to the file.
write.csv(player_tier_data_tred,file_name,row.names = TRUE)
rm(file_name,out_str)
