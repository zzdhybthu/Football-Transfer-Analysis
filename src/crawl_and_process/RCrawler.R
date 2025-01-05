library(tidyverse)
library(Rcrawler)
library(rvest)
library(xml2)
library(furrr)
library(data.table)
library(jsonlite)
library(progress)

final_INDEX <- data.frame()

# Define the range of years
years <- 2005:2023

# Loop through each year and crawl the URLs
for (year in years) {
  # Construct the URL with the current year
  url <- paste0("https://www.transfermarkt.co.uk/transfers/transferrekorde/statistik/top/ajax/yw2/saison_id/", 
                year, 
                "/land_id//ausrichtung//spielerposition_id//altersklasse//jahrgang/0/leihe/exclude_leihe_zuvor_gleicher_verein/w_s//plus/1/galerie/0/")
  
  # Run Rcrawler for the current year
  Rcrawler(Website = url, 
           crawlUrlfilter = "page/[0-9]{1,2}$", 
           MaxDepth = 1, 
           no_cores = 6,
           no_conn = 6,
           saveOnDisk = FALSE)
  
  # Append the crawled URLs to the final dataframe
  if (exists("INDEX")) {
    final_INDEX <- rbind(final_INDEX, INDEX)
  }
}

# Remove duplicate URLs if necessary
final_INDEX <- final_INDEX[!duplicated(final_INDEX$Url), ]

url_list = final_INDEX$Url

table <- lapply(url_list, function(url) {
  # Read the HTML page
  page <- read_html(url)
  
  # Extract URLs from hauptlink nodes
  hauptlink_urls <- page %>%
    html_nodes("td:nth-child(2) .hauptlink a") %>%
    html_attr("href")  # Get the href attribute
  
  # Extract the text from each <td> element inside the table with ID 'yw2'
  table_data <- page %>%
    html_nodes("#yw2 td") %>%
    html_text(trim = TRUE)
  
  num_columns <- 18  
  tab_data <- matrix(table_data, ncol = num_columns, byrow = TRUE) %>% as.data.frame()
  
  # Combine the extracted URLs (hauptlink) as the first column
  result <- cbind(url = hauptlink_urls, tab_data)
  
  return(result)
}) %>%
  do.call(rbind, .) %>%  # Combine all lists into a single data frame
  as.data.frame(stringsAsFactors = FALSE)

PlayerTransfersDataFrame <- table %>%
  select(
    url,     
    name = V4,
    position = V5,
    age = V6,
    market_value_at_time = V7,
    season = V8,
    left_club = V12,
    left_league = V13,
    joined_club = V16,
    joined_league = V17,
    fee = V18
  )

PlayerRumourUrls <- lapply(PlayerTransfersDataFrame$url, \(url) {
  data.frame(url = paste("https://www.transfermarkt.co.uk", sub("profil", "geruechte", url), sep = ""))
}) %>%
  do.call("rbind", .)

### Extract Rumor Posts
plan(multisession, workers = parallel::detectCores())

# Define the desired URL pattern once
desired_pattern <- "^https://www\\.transfermarkt\\.co\\.uk/[A-Za-z0-9-]+-to-[A-Za-z0-9-]+/thread/forum/[0-9]+/thread_id/[0-9]+/?$"

# Function to Crawl and Extract URLs
crawl_and_extract <- function(url) {
  Rcrawler(
    Website = url,
    crawlUrlfilter = "https://www.transfermarkt.co.uk/[a-z]{1,16}(-[a-z]{1,16}){0,2}-to-([a-z]{1,16}-){1,3}/thread/forum/[0-9]{1,5}/thread_id/[0-9]{1,5}",
    MaxDepth = 1,
    no_cores = 1,        # Set to 1 to prevent nested parallelism
    no_conn = 6,
    saveOnDisk = FALSE
  )
  
  if (!is.null(INDEX) && "Url" %in% names(INDEX)) {
    return(INDEX$Url)
  } else {
    return(character(0))
  }
}

# Parallel Crawl and Collect URLs
all_urls <- future_map(PlayerRumourUrls$url, crawl_and_extract, .progress = TRUE) %>%
  unlist()

# Convert to data.table for Efficient Processing
dt_urls <- data.table(Url = all_urls)

# Truncate URLs to the desired portion using stringr for speed
dt_urls[, TruncatedUrl := str_extract(Url, "^https://www\\.transfermarkt\\.co\\.uk/[A-Za-z0-9-]+-to-[A-Za-z0-9-]+/thread/forum/[0-9]+/thread_id/[0-9]+")]

# Filter URLs that match the desired pattern
dt_matched <- dt_urls[grepl(desired_pattern, TruncatedUrl)]

# Remove duplicate URLs
unique_matched_urls <- unique(dt_matched$TruncatedUrl)

# Identify Non-Matching URLs
unique_non_matching_urls <- unique(dt_urls[!grepl(desired_pattern, TruncatedUrl)]$TruncatedUrl)

# Log Non-Matching URLs (if any)
if (length(unique_non_matching_urls) > 0) {
  message("The following URLs did not match the desired pattern and were excluded:")
  print(unique_non_matching_urls)
}

# Compile the Final Filtered Data Frame
filtered_rumours_df <- data.frame(Url = unique_matched_urls, stringsAsFactors = FALSE)

# Optional: Convert to tibble for Further Processing
# library(tibble)
# filtered_rumours_df <- as_tibble(filtered_rumours_df)

save(RumoursPostsDataFrame, file = "RumoursPostsDataFrame.RData")
save(PlayerTransfersDataFrame, file = "load")

##### transfer list with dates
PlayerTransfersUrls <- lapply(PlayerTransfersDataFrame$url, \(url) {
  data.frame(url = paste("https://www.transfermarkt.co.uk", sub("profil", "transfers", url), sep = ""))
}) %>%
  do.call("rbind", .)


# Extract player IDs using regex
PlayerTransfersUrls$player_id <- sub(".*/(\\d+)$", "\\1", PlayerTransfersUrls$url)

# Define the new base URL
base_url <- "https://www.transfermarkt.co.uk/ceapi/transferHistory/list/"

# Create new URLs by appending player IDs
PlayerTransfersUrls$new_url <- paste0(base_url, PlayerTransfersUrls$player_id)

# Initialize progress bar
pb <- progress_bar$new(
  format = "[:bar] :current/:total (:percent) ETA: :eta",
  total = length(PlayerTransfersUrls$new_url),
  clear = FALSE,
  width = 60
)

# Process URLs with progress tracking and added delay
TransfersGroundTruths <- lapply(PlayerTransfersUrls$new_url, \(url) {
  pb$tick()  # Update the progress bar
  
  # Try to fetch data and handle errors
  tryCatch({
    dat <- fromJSON(url)
    df <- data.frame(
      url = dat$transfers$url,
      from = dat$transfers$from$clubName,
      to = dat$transfers$to$clubName,
      date = dat$transfers$dateUnformatted,
      season = dat$transfers$season,
      marketValue = dat$transfers$marketValue,
      fee = dat$transfers$fee
    )
    Sys.sleep(1)  # Pause for 1 second
    df
  }, error = function(e) {
    # Handle errors (e.g., return NA for problematic URLs)
    warning(sprintf("Failed to fetch data from URL: %s\nError: %s", url, e$message))
    Sys.sleep(5)  # Wait longer in case of an error
    NULL  # Return NULL for this URL
  })
}) %>% do.call(rbind, .)

TransfersGroundTruths = unique(TransfersGroundTruths)
save(TransfersGroundTruths, file = "TransfersGroundTruths.RData")
load("TransfersGroundTruths.RData")
