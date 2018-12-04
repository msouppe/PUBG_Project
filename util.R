## File description: 

# Obtain response from dataset
response <- function(dataset) {
  resp <- dataset$winPlacePerc
  return(resp)
}

# Take out any data fields that will not be used for data analysis
omit_data_fields <- function(dataset, matchtype) {
  matchtype_data <- dataset[which(dataset$matchType==matchtype),]
  
  if (matchtype == "solo" | matchtype == "solo-fpp") {
    omit_categories <- c("winPlacePerc", "winPoints","killPoints","Id","groupId",
                     "matchId","matchType","DBNOs","revives","rankPoints")    
  } else {
    omit_categories <- c("winPlacePerc", "winPoints","killPoints","Id","groupId",
                     "matchId","matchType", "rankPoints") 
  }
  
  subset_matchtype_data <- matchtype_data[ , !(names(matchtype_data) %in% omit_categories)]
  return(subset_matchtype_data)
}

# Subset data into map sizes and response
preprocess <- function(dataset, matchtype) { 
  dataset <- dataset[complete.cases(dataset), ]
  
  # Map sizes; large and small 
  large_map <- dataset$matchDuration >= 1600
  small_map <- dataset$matchDuration < 1600
  
  # Split whole dataset into two different map sizes
  large_map_dataset <- dataset[which(large_map),]
  small_map_dataset <- dataset[which(small_map),]
  
  # Subset map data by match type
  matchtype_large_map_dataset <- large_map_dataset[which(large_map_dataset$matchType==matchtype),]
  matchtype_small_map_dataset <- small_map_dataset[which(small_map_dataset$matchType==matchtype),]
  
  # Get response
  large_map_response <- response(matchtype_large_map_dataset)
  small_map_response <- response(matchtype_small_map_dataset)
  
  # Omit data fields that won't be used for analyzing
  mt_large_map_dataset <- omit_data_fields(large_map_dataset, matchtype)
  mt_small_map_dataset <- omit_data_fields(small_map_dataset, matchtype)
  
  data_out <- list(first=mt_small_map_dataset, second=small_map_response)
  return(data_out)
}

# Find the best k-means
min_k_means <- function(df,num_times,num_clusters) {
  set.seed(34529078)
  best_fit <- kmeans(df, num_clusters)
  best_fit_score <- best_fit$betweenss/best_fit$totss
  for (i in 1:num_times) {
    candidate_fit <- kmeans(df, num_clusters)
    candidate_fit_score <- candidate_fit$betweenss/candidate_fit$totss
    if (candidate_fit_score >  best_fit_score) {
      best_fit <- candidate_fit
      best_fit_score <- candidate_fit_score
    }
  }
  return(best_fit)
}

# Add cluster feature to the dataset
append_clustering_feature <- function(covariate_type_frame, full_type_frame) {
  # Mutate Data Frame do not return a new full_type_frame
  k_means_results <- min_k_means(covariate_type_frame,30,4)
  cluster <- k_means_results$cluster
  new_df <- data.frame(full_type_frame,cluster)
  return(new_df)
}

# Split cluster
split_by_the_four_clusters <- function(full_type_frame) {
  # For now only assuming that we are using four clusters
  cluster_1_df <- full_type_frame[which(full_type_frame$cluster == 1), ] 
  cluster_2_df <- full_type_frame[which(full_type_frame$cluster == 2), ] 
  cluster_3_df <- full_type_frame[which(full_type_frame$cluster == 3), ]
  cluster_4_df <- full_type_frame[which(full_type_frame$cluster == 4), ] 
  return(list(first = cluster_1_df,second = cluster_2_df,third = cluster_3_df,fourth = cluster_4_df))
}

# Logistic regression function
logistic <- function(covariants, response) {
  threhold <- (response >= 0.9)*1
  recombined_data <- data.frame(covariants, threhold)
  # colnames(recombined_data)[20] <- 'thresh'
  # forumla <-threhold  ~ .
  mod <- glm(threhold ~ ., family=binomial(link='logit'), data=recombined_data)
  return(list(model=mod, thresh=threhold))
}

# Get accuracy()
accuracy <- function(model, covariants, threhold) {
  prob <- predict(model, covariants, type='response')
  pred <- ifelse(prob > 0.5, 1, 0)
  actual <- ifelse(threhold > 0.5, 1, 0)
  acc <- mean(pred == actual)
  paste(toString(round(acc*100, 2)), "%")
  return(acc)
}


dropNA <- function(dataset) {
  df <- data.frame(dataset$first, dataset$second)
  newData <- df[complete.cases(df), ]
  return(newData)
}