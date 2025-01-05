source('__init__.R')

# Input the ID, e.g., 35518
fetch_transfer_data <- function(id) {
  url <- paste0("https://www.transfermarkt.co.uk/ceapi/transferHistory/list/", id)
  response <- GET(url)
  if (status_code(response) != 200) {
    print("Failed to fetch data. Please check the ID or network connection.")
    return(NA)
  }
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  write_json(data$transfers, "test_transfer_data.json", pretty = TRUE)
  transfers <- data$transfers
  if (is.null(transfers)) {
    print("No transfer data available for the provided ID.")
    return(NA)
  }
  
  result <- lapply(seq_len(nrow(transfers)), function(i) {
    transfer <- transfers[i, ]
    list(
      fromClub = transfer$from$clubName[[1]],
      toClub = transfer$to$clubName[[1]],
      date = transfer$dateUnformatted,
      season = transfer$season,
      marketValue = transfer$marketValue,
      fee = transfer$fee
    )
  })
  result_df <- do.call(rbind, lapply(result, as.data.frame))
  
  return(result_df)
}
