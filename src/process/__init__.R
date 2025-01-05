library(stringr)
library(dplyr)
library(tidyr)
library(jsonlite)
library(pbapply)
library(future)
library(httr2)

load("rumor_facts_with_na.RData")

final_df = distinct(final_df)
base_url = "https://api.openai.com/v1/chat/completions"
api_key = Sys.getenv("OPENAI_API_KEY")
