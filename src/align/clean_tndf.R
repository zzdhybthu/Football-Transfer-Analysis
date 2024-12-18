source('__init__.R')

load('../../data/tndf091458.Rdata')

update_rumor_source <- function(dataframe, column_name, source_mapping) {
  dataframe[[column_name]] <- sapply(dataframe[[column_name]], function(x) {
    if (x %in% names(source_mapping)) {
      return(source_mapping[x])
    } else {
      return(x)
    }
  })
  return(dataframe)
}

# Starts with "Source: " and followed by a URL, remove "Source: "
filtered_sources <- transfer_news_df$Rumor.Source[grepl("^Source: [^ ]*\\.[^ ]*", transfer_news_df$Rumor.Source)]
cleaned_sources <- sub("^Source: ", "", filtered_sources)
source_mapping <- setNames(cleaned_sources, filtered_sources)
transfer_news_df <- update_rumor_source(transfer_news_df, "Rumor.Source", source_mapping)

# # Starts with "Source: " and not followed by a URL
# # Use Google search to get the correct URL, save as a dataframe for later replacement
# filtered_sources <- transfer_news_df$Rumor.Source[grepl("^Source: ", transfer_news_df$Rumor.Source)]
# unique_sources <- unique(filtered_sources)
# source("google_organic.R")
# source_mapping <- sapply(unique_sources, get_first_google_url)
# source_mapping <- list()
# for (i in seq_along(unique_sources)) {
#   src <- unique_sources[i]
#   modified_url <- source_mapping[[i]]
#   source_mapping[[src]] <- modified_url
# }
# print(source_mapping)
# save(source_mapping, file = "source_mapping.RData")


# Read the source mapping and manually fill in some NA values
load("source_mapping.RData")
modify_url <- function(source_mapping, src, modified_url) {
  if (src %in% names(source_mapping)) {
    source_mapping[[src]] <- modified_url
  } else {
    cat("Source not found: ", src, "\n")
  }
  return(source_mapping)
}
na_sources <- names(source_mapping)[sapply(source_mapping, is.na)]
print(na_sources)

url_mapping <- list(
  "Source: BBC Gossip" = "https://www.bbc.co.uk",
  "Source: BBC Sport" = "https://www.bbc.com",
  "Source: Guardian" = "https://www.theguardian.com",
  "Source: L´Equipe" = "https://www.lequipe.fr",
  "Source: Martí Miràs" = "https://x.com",
  "Source: Mail Online" = "https://www.dailymail.co.uk",
  "Source: Díario de Transferências" = "https://x.com",
  "Source: Sky Sports News" = "https://www.skysports.com/",
  "Source: RMC" = "https://rmc.com",
  "Source: Nürnberger Nachrichten" = "https://www.nn.de",
  "Source: Alfredo Pedullà" = "https://www.alfredopedulla.com",
  "Source: Real Sociedad Diariovasco" = "https://www.diariovasco.com",
  "Source: Le Progrès" = "https://www.leprogres.fr",
  "Source: Nicolò Schira" = "https://www.nicoloschira.com",
  "Source: @SkySportsNewsHQ" = "https://www.skysports.com",
  "Source: Diario Récord" = "https://www.record.com.mx",
  "Source: Pedro Sepúlveda" = "https://www.instagram.com",
  "Source: Gazetta dello Sport" = "https://www.gazzetta.it",
  "Source: 24horas" = "https://www.24horas.cl",
  "Source: François Plateau" = "https://www.sportsmole.co.uk",
  "Source: Ed Aarons (The Guardian)" = "https://www.theguardian.com",
  "Source: L'Équipe" = "https://www.lequipe.fr",
  "Source: leicestermercury" = "https://www.leicestermercury.co.uk"
)
for (src in names(url_mapping)) {
  modified_url <- url_mapping[[src]]
  source_mapping <- modify_url(source_mapping, src, modified_url)
}

# Apply the source mapping to the original data
transfer_news_df <- update_rumor_source(transfer_news_df, "Rumor.Source", source_mapping)


# Obtain the final URL
transfer_news_df <- transfer_news_df[!is.na(transfer_news_df$Rumor.Source) & transfer_news_df$Rumor.Source != '.', ]
transfer_news_df$Rumor.Source <- sub("www1\\.", "www.", transfer_news_df$Rumor.Source)
transfer_news_df$Rumor.Source <- sub("www2\\.", "www.", transfer_news_df$Rumor.Source)
transfer_news_df$Rumor.Source <- sub("http://", "https://", transfer_news_df$Rumor.Source)
transfer_news_df$Rumor.Source <- sapply(transfer_news_df$Rumor.Source, function(src) {
  if (!grepl("^https://", src)) {
    src <- paste0("https://", src)
  }
  if (src == "https://1.skysports") {
    src <- "https://www.skysports.com"
  }
  if (src == "https://.transfermarkt") {
    src <- "https://www.transfermarkt.us"
  }
  return(src)
})
transfer_news_df$Rumor.Source <- sub("^https://([^/]+).*", "https://\\1", transfer_news_df$Rumor.Source)
transfer_news_df$Rumor.Source <- sub("'", "", transfer_news_df$Rumor.Source)
transfer_news_df$Rumor.Source <- tolower(transfer_news_df$Rumor.Source)

save(transfer_news_df, file = "tndf091458_clean.RData")
