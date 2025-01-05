source('__init__.R')

process_row <- function(row) {
  # Check for missing values
  if (is.null(row$Star.Name) | is.null(row$Interested.Club) | is.null(row$Rumor.Content)) {
    # Handle missing data (e.g., return NULL or error message)
    return(NULL)
  }
  
  tryCatch({
    prompt <- paste(
      "Given the transfer news text, analyze and evaluate the likelihood of ",
      row$Star.Name,
      " transferring to ",
      row$Interested.Club,
      ". Provide your response in JSON format with the following keys:",
      "{",
      "'language': (e.g., 'english', 'german', 'scots', etc.),",
      "'confidence': -1, -0.5, 0, 0.5, 1 (where -1 means 'very unlikely', -0.5 'unlikely', 0 'neutral', 0.5 'possible', 1 'very possible'),",
      "'reason': A brief explanation of the confidence score.",
      "}", ". Respond strictly in JSON format. No additional text. News content: \n", row$Rumor.Content
    )
    
    body = list(model = "gpt-3.5-turbo-1106",
                messages = list(list(role = 'user', content = prompt)))
    req = request(base_url)
    resp <- req |>
      req_auth_bearer_token(token = api_key) |>
      req_headers(
        "Content-Type" = "application/json"
      ) |>
      req_user_agent("Shenghan Wen | Undergrad working on text processing") |>
      req_body_json(body) |>
      req_retry(max_tries = 4) |>
      req_throttle(rate = 18) |>
      req_perform()
    
    response1 = resp |> resp_body_json(simplifyVector = T)
    text1 = response1$choices$message$content %>% str_extract("\\{[^}]+\\}") %>% fromJSON
    
    gpt_response = data.frame(GPT.language = text1$language,
                              GPT.confidence = text1$confidence,
                              GPT.reason = text1$reason)
    
    return(cbind(row, gpt_response))
  }, error = function(e) {
    warning("API call failed after multiple retries. Returning NULL.")
  })

  
  return(NULL)
}

# Main function to traverse all rows with progress bar and parallel processing
traverse_and_process <- function(final_df) {
  # Increase error logging
  processed_list <- pbapply::pblapply(
    split(final_df, seq(nrow(final_df))), 
    function(row) {
      tryCatch({
        result <- process_row(row)
        return(result)
      }, error = function(e) {
        # Log the specific error for debugging
        message(paste("Error processing row:", 
                      paste(row, collapse = ", "), 
                      "Error:", e$message))
        return(NULL)
      })
    }, 
    cl = availableCores()
  )
  
  # Filter out NULL results and provide a count
  processed_list <- Filter(Negate(is.null), processed_list)
  message(paste("Processed", length(processed_list), "out of", nrow(final_df), "rows"))
  
  result_df <- do.call(rbind, processed_list)
  return(result_df)
}

# result_df = traverse_and_process(final_df = final_df[4001:5000, ])

process_batches <- function(final_df, batch_size = 500, sleep_time = 5) {
  # Split the data frame into batches
  row_indices <- seq(1, nrow(final_df), by = batch_size)
  num_batches <- length(row_indices)
  
  # Initialize a global progress bar
  pb <- txtProgressBar(min = 0, max = num_batches, style = 3)
  
  # Initialize an empty list to store results
  all_results <- list()
  
  # Loop through each batch sequentially
  for (i in seq_along(row_indices)) {
    # Define the range for the current batch
    start_index <- row_indices[i]
    end_index <- min(start_index + batch_size - 1, nrow(final_df))
    batch <- final_df[start_index:end_index, ]
    
    message(paste("Processing rows", start_index, "to", end_index, "out of", nrow(final_df)))
    
    # Process the batch using traverse_and_process
    batch_result <- tryCatch({
      traverse_and_process(batch)
    }, error = function(e) {
      message(paste("Error processing batch:", e$message))
      return(NULL)
    })
    
    # Append the result to the list
    if (!is.null(batch_result)) {
      all_results <- append(all_results, list(batch_result))
    }
    
    # Update the global progress bar
    setTxtProgressBar(pb, i)
    
    # Pause between batches
    if (i < num_batches) { # No need to sleep after the last batch
      message(paste("Sleeping for", sleep_time, "seconds before the next batch..."))
      Sys.sleep(sleep_time)
    }
  }
  
  # Close the progress bar
  close(pb)
  
  # Combine all batch results and remove duplicates
  combined_result <- do.call(rbind, all_results)
  combined_result <- unique(combined_result)
  
  return(combined_result)
}

result_df = process_batches(filtered_final_df, batch_size = 100, sleep_time = 3)

# Usage
# results_list <- list()

# # Run the loop 5 times
# for (i in 1:5) {
#   results_list[[i]] <- traverse_and_process(final_df = final_df[51:13274, ])
# }

# Combine the results with row binding
# result_df <- do.call(rbind, results_list)

# final_df_with_gpt_3.5 = data.frame()

final_df_with_gpt_3.5 = rbind(result_df, final_df_with_gpt_3.5)
final_df_with_gpt_3.5 <- final_df_with_gpt_3.5 %>% 
  distinct(Rumor_dt, Rumor.Source, Interested.Club, Star.Name, Rumor.Content, .keep_all = TRUE)

filtered_final_df <- final_df %>%
  anti_join(final_df_with_gpt_3.5)


save(file="final_df_with_gpt_3.5.RData", "final_df_with_gpt_3.5")
load("final_df_with_gpt_3.5.RData")

final_df_with_gpt <- final_df_with_gpt %>% 
  distinct(Rumor_dt, Rumor.Source, Interested.Club, Star.Name, .keep_all = TRUE)

save(file="final_df_with_gpt.RData", "final_df_with_gpt")
load("final_df_with_gpt.RData")
