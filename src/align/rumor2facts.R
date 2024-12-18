source('__init__.R')

load('../../data/tndf091458_clean.Rdata')
load('../../data/TransfersGroundTruths.Rdata')

print(sort(unique(transfer_news_df$Star.Name))[1:50])
print(sort(unique(TransfersGroundTruths$name))[1:50])

transfer_news_df$Rumor.Date <- trimws(transfer_news_df$Rumor.Date)
transfer_news_df$Nationality <- gsub("^[^a-zA-Z]*", "", trimws(transfer_news_df$Nationality))

transfer_news_df$Rumor.FormatDate <- sapply(transfer_news_df$Rumor.Date, function(date_str) {
  cleaned_date <- gsub(" hours", "", date_str)
  parsed_date <- strptime(cleaned_date, "%b %d, %Y - %I:%M %p", tz = "UTC")
  formatted_date <- format(parsed_date, "%Y%m%d%H%M")
  return(formatted_date)
})

recitfy_club <- function(club) {
  club <- gsub("[^éa-zA-Z\\-\\ ]","",club)
  club <- gsub("\\s+", " ", club)
  club <- gsub("[^a-zA-Z][A-Z]+$", "", club)
  club <- gsub("^Al-", "", club)
  club <- gsub("^[A-Z]+[^a-zA-Z]", "", club)
  club <- gsub("[0-9.]", "", club)
  club <- gsub("\\(.*?\\)", "", club)
  club <- trimws(club)
  club <- tolower(club)
  club <- gsub("atlético", "athletic", club)
  club <- gsub(" de ", " ", club)
  club <- gsub(" city$", "", club)
  club <- gsub(" club$", "", club)
  club <- gsub(" town$", "", club)
  return(club)
}

transfer_news_df$Interested.Club <- sapply(transfer_news_df$Interested.Club, recitfy_club)
TransfersGroundTruths$to <- sapply(TransfersGroundTruths$to, recitfy_club)
TransfersGroundTruths$from <- sapply(TransfersGroundTruths$from, recitfy_club)

# print(sort(unique(transfer_news_df$Interested.Club))[50:100])
# print(sort(unique(TransfersGroundTruths$from))[50:100])

TransfersGroundTruths$name <- sapply(TransfersGroundTruths$url, function(url) {
  name <- gsub("^/(.*?)/.*$", "\\1", url)
  return (name)
})

recitfy_name <- function(name) {
  name <- gsub("-", ' ', name)
  name <- tolower(name)
  return(name)
}

transfer_news_df$Star.Name <- sapply(transfer_news_df$Star.Name, recitfy_name)
TransfersGroundTruths$name <- sapply(TransfersGroundTruths$name, recitfy_name)



transfer_news_df <- transfer_news_df %>%
  mutate(Rumor_dt = as.POSIXct(Rumor.FormatDate, format = "%Y%m%d%H%M", tz = "UTC"))

TransfersGroundTruths <- TransfersGroundTruths %>%
  mutate(
    start_year = 2000 + as.integer(substr(season, 1, 2)), # xx
    end_year = 2000 + as.integer(substr(season, 4, 5)),   # yy
    season_start_date = as.Date(paste0(start_year, "-04-01")),
    season_end_date = as.Date(paste0(end_year, "-03-31"))
  )
player_cols <- names(TransfersGroundTruths)

final_df <- transfer_news_df %>%
  rowwise() %>%
  mutate(matched = list(
    TransfersGroundTruths %>%
      filter(name == Star.Name,
             Rumor_dt >= season_start_date,
             Rumor_dt <= season_end_date)
  )) %>%
  unnest(matched, keep_empty = TRUE)

for (col in player_cols) {
  fact_col <- paste0("fact.", col)
  final_df[[fact_col]] <- final_df[[col]]
}

final_df <- final_df %>%
  group_by(Star.Name, Rumor_dt) %>%
  filter(as.POSIXct(fact.date) >= Rumor_dt | is.na(fact.date)) %>%
  mutate(diff_time = ifelse(
    is.na(fact.date),
    Inf,
    abs(as.numeric(difftime(fact.date, Rumor_dt, units = "secs")))
  )) %>%
  slice_min(diff_time) %>%
  select(-diff_time) %>%
  ungroup()


# final_df <- final_df %>%
#   group_by(Star.Name, Rumor_dt) %>%
#   filter(as.POSIXct(fact.date) >= Rumor_dt | is.na(fact.date)) %>%
#   ungroup()

final_df <- final_df %>%
  mutate(
    fact.real_rumor = ifelse(!is.na(fact.to) & fact.to == Interested.Club, TRUE, FALSE)
  )

final_df <- final_df %>%
  select(
    all_of(names(transfer_news_df)), 
    starts_with("fact.")
  )

# final_df <- final_df[!is.na(final_df$fact.name), ]

# save(final_df, file = "rumor_facts.RData")
# save(final_df, file = "rumor_facts_duplic.RData")
# save(final_df, file = "rumor_facts_dirty.RData")
save(final_df, file = "rumor_facts_with_na.RData")
