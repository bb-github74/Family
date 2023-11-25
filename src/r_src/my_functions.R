
pairwise_func = function(independent_vars_praise){
  
  # all possible pair combinations
  combinations_pairs_praise = combn(1:ncol(independent_vars_praise), 2)
  
  # calculate pairwise interactions and store them as a list
  pairs_list_praise = lapply(1:ncol(combinations_pairs_praise), function(i){
    indices = combinations_pairs_praise[, i]
    
    # interaction
    independent_vars_praise[[indices[1]]] *
      independent_vars_praise[[indices[2]]]
  })
  
  # convert the list to a dataframe
  interaction_pairs_praise = as.data.frame(do.call(cbind, pairs_list_praise))
  
  # assign column names
  colnames(interaction_pairs_praise) = apply(
    combinations_pairs_praise,2, function(indices){
      paste(names(independent_vars_praise)[indices], collapse = "_")
    })
  return(interaction_pairs_praise)
  
}


triplets_func = function(independent_vars_praise){
  # all possible triplets combinations
  combinations_triplets_praise = combn(1:ncol(independent_vars_praise), 3)
  
  # calculate pairwise interactions and store them as a list
  triplets_list_praise = lapply(1:ncol(combinations_triplets_praise), function(i){
    indices = combinations_triplets_praise[, i]
    
    # interaction
    independent_vars_praise[[indices[1]]] *
      independent_vars_praise[[indices[2]]]*
      independent_vars_praise[[indices[3]]]
  })
  
  # convert the list to a dataframe
  interaction_triplets_praise = as.data.frame(do.call(cbind, triplets_list_praise))
  
  # assign column names
  colnames(interaction_triplets_praise) = apply(
    combinations_triplets_praise,2, function(indices){
      paste(names(independent_vars_praise)[indices], collapse = "_")
    })
  return(interaction_triplets_praise)
}

lasso_convert = function(symptoms_df){
  symptoms = symptoms_df %>% names() # all symptoms
  
  # Initialize a matrix to store counts
  symptom_matrix = matrix(0, nrow=length(symptoms), ncol=length(symptoms))
  
  rownames(symptom_matrix) = symptoms
  colnames(symptom_matrix) = symptoms
  
  # Fill the matrix with pairwise counts
  for (i in 1:length(symptoms)) {
    for (j in 1:length(symptoms)) {
      if (i != j) {
        # Count where symptom i appears but symptom j doesn't
        symptom_matrix[i, j] = sum(symptoms_df[[symptoms[i]]] == 1 & 
                                     symptoms_df[[symptoms[j]]] == 0, na.rm = TRUE)
      }
    }
  }
  
  # Determine order based on the sum of rows
  symptom_order = order(-rowSums(symptom_matrix))
  
  ordered_symptoms = symptoms_df[symptom_order]
  return(ordered_symptoms)
}
