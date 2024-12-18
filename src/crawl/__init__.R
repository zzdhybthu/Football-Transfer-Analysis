# ------------------------------------------
# Crawling and scraping data from the web
# ------------------------------------------

library(httr)
library(jsonlite)
Sys.setenv(http_proxy = "http://127.0.0.1:7890")  # Replace with your proxy
Sys.setenv(https_proxy = "http://127.0.0.1:7890")