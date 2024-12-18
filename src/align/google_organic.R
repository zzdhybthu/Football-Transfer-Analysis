source('__init__.R')

Sys.setenv(http_proxy = "http://127.0.0.1:7890")
Sys.setenv(https_proxy = "http://127.0.0.1:7890")

get_first_google_url <- function(query) {
  google_url <- tryCatch({
    query_0 <- gsub(" ", "+", query)
    url <- paste0("https://www.google.com.hk/search?q=", query_0, "&newwindow=1&sca_esv=\{SCA_ESV\}&ei=\{EI\}&ved=\{VED\}&uact=5&oq=", query_0, "&gs_lp=\{GS_LP\}&sclient=gws-wiz-serp")  # Replace \{SCA_ESV\}, \{EI\}, \{VED\}, \{GS_LP\} with actual values, could be obtained from your own browser
    page <- read_html(url)
    links <- page %>%
      html_nodes(".sCuL3") %>%
      html_text()
    
    if (length(links) > 0) {
      google_url <- paste0("https://", strsplit(links[1], " ")[[1]][1])
      print(google_url)
      return(google_url)
    } else {
      return(NA)
    }
  }, error = function(e) {
    print(paste("Error occurred:", e$message))
    return(NA)
  })
  
  return(google_url)
}


get_complete_url <- function(url) {
  if (grepl("^https://", url)) {
    return(url)
  }
  if (!grepl("^https?://", url)) {
    url <- paste0("https://", url)
  }
  response <- tryCatch({
    req <- GET(url, followlocation = TRUE)
    final_url <- req$url
    print(final_url)
    return(final_url)
  }, error = function(e) {
    print(paste("Error occurred:", e$message))
    return(url)
  })
  return(response)
}
