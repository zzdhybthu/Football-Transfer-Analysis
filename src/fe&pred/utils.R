downSample <- function(df) {
    num_true <- sum(df$fact.real_rumor == TRUE)
    num_false <- sum(df$fact.real_rumor == FALSE)
    df_true <- df[df$fact.real_rumor == TRUE, ]
    df_false <- df[df$fact.real_rumor == FALSE, ]
    df_false_downsampled <- df_false[sample(nrow(df_false), num_true), ]
    df <- rbind(df_true, df_false_downsampled)
    return(df)
}


cleanCorpus <- function(corpus, default_lang = "english") {
    clean_fun <- content_transformer(function(text) {
        lang_detected <- textcat(text)  # bottle neck for speed, may use origin annotation instead
        lang_detected <- strsplit(lang_detected, "-")[[1]][1]
        if (!lang_detected %in% c(
            "english", "italian", "french", "danish", "spanish", "german", 
            "catalan", "portuguese", "dutch","romanian", "russian", "swedish"
            )
        ) {
            lang_detected <- default_lang
        }
        text <- tolower(text)
        text <- removePunctuation(text)
        text <- removeNumbers(text)
        text <- removeWords(text, stopwords(lang_detected))
        text <- stripWhitespace(text)
        
        return(text)
    })

    corpus <- tm_map(corpus, clean_fun)
    return(corpus)
}


textPrep <- function(content, sparsity = 0.99, selected_words = NULL) {
    corpus_content <- VCorpus(VectorSource(content))
    corpus_content_clean <- cleanCorpus(corpus_content)
    # corpus_content_clean <- corpus_content   # for faster debugging
    dtm_content <- DocumentTermMatrix(corpus_content_clean)
    if (!is.null(selected_words)) {
        selected_terms <- intersect(selected_words, colnames(dtm_content))
        dtm_content <- dtm_content[, selected_terms, drop = FALSE]
    } else {
        dtm_content <- removeSparseTerms(dtm_content, sparsity)
    }
    X_content <- as.data.frame(as.matrix(dtm_content))

    return(X_content)
}


randomForestPipeline <- function(X, y, p, ntree) {
    train_index <- createDataPartition(y, p = p, list = FALSE)
    X_train <- X[train_index, ]
    X_test <- X[-train_index, ]
    y_train <- y[train_index]
    y_test <- y[-train_index]
    model <- randomForest(x = X_train, y = y_train, ntree = ntree)

    pred <- predict(model, X_test)
    conf_mat <- confusionMatrix(pred, y_test)
    print(conf_mat)
    
    return(model)
}


cleanSource <- function(s) {
    s <- sub("https://", "", s)
    s <- sub("www\\.", "", s)
    s <- gsub("\\.", " ", s)
    return(s)
}


svmPipeline <- function(X, y, p, kernel = "radial", cost = 1) {
    train_index <- createDataPartition(y, p = p, list = FALSE)
    X_train <- X[train_index, ]
    X_test <- X[-train_index, ]
    y_train <- y[train_index]
    y_test <- y[-train_index]
    
    model <- svm(x = X_train, y = y_train, kernel = kernel, cost = cost, scale = TRUE)
    
    pred <- predict(model, X_test)
    conf_mat <- confusionMatrix(pred, y_test)
    print(conf_mat)
    
    return(model)
}


elasticNetPipeline <- function(X, y, p, alpha = 0.5, lambda = NULL, family = "binomial") {
    train_index <- createDataPartition(y, p = p, list = FALSE)
    X_train <- X[train_index, , drop = FALSE]
    X_test <- X[-train_index, , drop = FALSE]
    y_train <- y[train_index]
    y_test <- y[-train_index]
    
    if (is.null(lambda)) {
        cv_fit <- cv.glmnet(as.matrix(X_train), y_train, alpha = alpha, family = family)
        lambda_opt <- cv_fit$lambda.min
    } else {
        lambda_opt <- lambda
    }
    
    model <- glmnet(as.matrix(X_train), y_train, alpha = alpha, lambda = lambda_opt, family = family)

    pred <- predict(model, as.matrix(X_test), type = "class")
    conf_mat <- confusionMatrix(factor(pred), factor(y_test))
    print(conf_mat)
    
    return(model)
}
