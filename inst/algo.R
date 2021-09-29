algo = function(id) {
  # Library
  require(tidyverse)
  require(janitor)
  require(purrr)
  require(caret)
  require(text2vec)
  require(xgboost)
  require(DiagrammeR)
  require(tm)
  # Remove uneeded word
  clean_txt = function(x) {
    x %>% 
      rownames_to_column("id") %>% 
      mutate_at(
        vars(desc), 
        ~ .x %>% 
          tolower() %>% 
          str_replace_all("\\<[^()]*\\>", " ") %>% 
          str_replace_all("[[:digit:]]", " ") %>% 
          str_replace_all("[[:punct:]]", " ") %>% 
          str_replace_all(paste0('\\b', paste(stopwords('fr'), collapse = '\\b|\\b'), '\\b'), " ") %>% 
          str_replace_all(paste0('\\b', paste(stopwords('en'), collapse = '\\b|\\b'), '\\b'), " ") %>% 
          str_replace_all("\\s+", " ") %>% 
          trimws()
      )
  }
  # Scored data
  df = read_csv(paste0("data/get/", id,"/score/score.csv")) %>% clean_txt()
  # Unscored data
  unscore = read_csv(paste0("data/get/", id,"/unscore/unscore.csv")) %>% clean_txt()
  # Transform function
  prep_xgb = function(df) {
    # tranform token
    it_df = itoken(df$desc,
                   tokenizer = word_tokenizer, 
                   ids = df$id,
                   progressbar = F)
    # freq table
    vocab = create_vocabulary(it_df)
    # Create Document-Term Matrix
    vectorizer = vocab_vectorizer(vocab)
    dtm_df = create_dtm(it_df, vectorizer)
    # Complet with price 
    final_mat = as.matrix(dtm_df)
    return(final_mat)
  }
  # Label new data 
  xgb_apply = function(df, unscore) {
    # Prep all 
    fn = bind_rows(df, unscore)
    fnp = prep_xgb(fn)
    # Isolate scored and unscored
    fn_l = fnp[1:nrow(df),]
    fn_p = fnp[(nrow(df)+1):nrow(fnp),]
    fn_labels = as.matrix(fn[, 'note'])[1:nrow(df)]
    # Coerce
    fn_xgb = xgb.DMatrix(data = fn_l, label = fn_labels, missing = NA)
    # XGB model 
    mod = xgboost(
      data = fn_xgb,
      booster = "gbtree", 
      objective = "binary:logistic", 
      eval_metric = "error",
      max.depth = 6,
      nthread = 6,
      nrounds = 20)
    # Get predictions
    pred = predict(mod, newdata = fn_p, missing = NA)
    binpred = ifelse(pred > 0.5, 1, 0)
    # Merge
    unscore$score_pred = pred
    unscore$note_pred = binpred
    # Return
    return(unscore)
  }
  # Do it
  prev = xgb_apply(df, unscore)
  # Arrange results
  prev = arrange(prev, desc(score_pred))
  # Save it 
  write_rds(prev, paste0("data/get/", id,"/top/prev.rds"))
  # Test function  
  # xgb_test = function(df) {
  #   # Prep data 
  #   prep_mat = prep_xgb(df)
  #   # Split 
  #   train_ind = sample(seq_len(nrow(prep_mat)), size = floor(0.75 * nrow(prep_mat)))
  #   # Train matrix
  #   train = prep_mat[train_ind, ]
  #   train_labels = as.matrix(df[train_ind, 'note'])
  #   # Test matrix
  #   test = prep_mat[-train_ind, ]
  #   test_labels = as.matrix(df[-train_ind, 'note'])
  #   # Dmatrix
  #   train_xgb = xgb.DMatrix(data = train, label = train_labels)
  #   test_xgb = xgb.DMatrix(data = test, label = test_labels)
  #   # XGB model simple 
  #   mod = xgb.train(
  #     data = train_xgb,
  #     watchlist = list(train = train_xgb, test = test_xgb),
  #     booster = "gbtree", 
  #     objective = "binary:logistic", 
  #     eval_metric = "error",
  #     max.depth = 6,
  #     nthread = 6,
  #     nrounds = 20)
  #   # Result plot
  #   # xgb.plot.tree(model = mod, trees = 0:2)
  #   # Plot imp
  #   # xgb.plot.importance(xgb.importance(model = mod))
  #   # Prediction  
  #   pred = predict(mod, newdata = test_xgb)
  #   pred = ifelse (pred > 0.5, 1, 0)
  #   # Matrice de confusion 
  #   confusionMatrix(as.factor(pred), as.factor(as.vector(test_labels)))
  # }
  # Do it 
  # xgb_test(df)
}