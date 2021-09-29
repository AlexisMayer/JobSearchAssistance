get = function(mc,id) {
  # Better ? 
  ## require(Rlinkedin) ## \!/
  # Library
  require(tidyverse)
  require(rvest)
  require(janitor)
  require(purrr)
  require(rJava)
  require(RSelenium)
  require(lubridate)
  # Get Pole emploi data
  scrap_pole_emploi = function(x) {
  # Lien 
  link = paste0("https://candidat.pole-emploi.fr/offres/recherche?motsCles=", x, "&offresPartenaires=true&rayon=10&tri=0")
  # Navigate to page
  ffd$navigate(link)
  Sys.sleep(2)
  if ( x == mc[1] ) {
    # Accept coockies
    load_btn = ffd$findElement(using = "css selector", "#footer_tc_privacy_button")
    load_btn$clickElement()
    Sys.sleep(2)
  }
  # Expand menu (10-19)
  load_btn = ffd$findElement(using = "css selector", "#zoneAfficherPlus .btn")
  load_btn$clickElement()
  Sys.sleep(2)
  # 20-29
  load_btn = ffd$findElement(using = "css selector", "#zoneAfficherPlus .btn")
  load_btn$clickElement()
  Sys.sleep(2)
  # 30-39
  load_btn = ffd$findElement(using = "css selector", "#zoneAfficherPlus .btn")
  load_btn$clickElement()
  Sys.sleep(2)
  # 40-49
  load_btn = ffd$findElement(using = "css selector", "#zoneAfficherPlus .btn")
  load_btn$clickElement()
  Sys.sleep(2)
  # Get final page
  html_data = ffd$getPageSource()[[1]]
  # Read final page
  val = read_html(html_data)
  # Store nodes
  page = c("0-9", "10-19", "20-29", "30-39", "40-49") %>%
    paste0("#page_", ., " > li") %>%
    map( ~ html_nodes(val, .x))
  # Initialize loop
  df = tribble(~ "title", ~ "localisation", ~ "desc", ~ "link", ~ "date")
  # Loop
  for (j in 0:4) {
    for (i in 1:10) {
      pr = paste0("https://candidat.pole-emploi.fr/offres/recherche/detail/", html_attrs(page[[j + 1]][i])[[1]][1])
      prr = read_html(pr) %>% html_nodes("#contents > div.container-wrapper > div > div.modal-details.modal-details-offre > div > div > div.modal-body") 
      df[j * 10 + i, 1] = prr %>% html_nodes("h1") %>% html_text(trim = TRUE)
      df[j * 10 + i, 2] = prr %>% html_nodes("p.t4.title-complementary") %>% html_text(trim = TRUE)
      df[j * 10 + i, 3] = prr %>% html_nodes("div.row") %>% html_text(trim = TRUE)
      df[j * 10 + i, 4]  = pr
      df[j * 10 + i, 5]  = prr %>% html_nodes("p.t5.title-complementary > span:nth-child(1)") %>% html_text()
    }
  }
  # Process
  df = df %>% 
    mutate_at(
      vars(desc), 
      ~ .x %>% 
        str_remove_all("\n|\\(|\\)|\\-") %>% 
        trimws()
      ) %>% 
    mutate_at(
      vars(localisation), 
      ~ .x %>% 
        str_remove("Localiser avec Mappy") %>% 
        str_remove("([\n])[^([\n])]+$") %>% 
        trimws()
    ) %>% 
    mutate_at(
      vars(date), 
      ~ .x %>% 
        str_remove_all("\n") %>% 
        trimws() %>% 
        str_extract("[[:digit:]].*$") %>% 
        as.Date(format = "%d %B %Y")
    )
  
  df$entreprise = NA
  # Return 
  return(df)
}
  # Get Linkedin data 
  scrap_linkedin = function(x) {
  link = paste0("https://www.linkedin.com/jobs/search/?geoId=105015875&keywords=", x,"&location=France&redirect=false&position=1&pageNum=0")
  
  ffd$navigate(link)
  Sys.sleep(2)
  
  webElem = ffd$findElement("css", "body")             
  webElem$sendKeysToElement(list(key = "end"))
  Sys.sleep(1)
  
  webElem = ffd$findElement("css", "body")              
  webElem$sendKeysToElement(list(key = "end"))
  Sys.sleep(1)
  
  webElem = ffd$findElement("css", "body")              
  webElem$sendKeysToElement(list(key = "end"))
  Sys.sleep(1)
  
  webElem = ffd$findElement("css", "body")              
  webElem$sendKeysToElement(list(key = "end"))
  Sys.sleep(1)
  
  webElem = ffd$findElement("css", "body")              
  webElem$sendKeysToElement(list(key = "end"))
  Sys.sleep(1)
  
  html_data = ffd$getPageSource()[[1]]
  
  ref = read_html(html_data) %>% 
    html_nodes("main > div > section > ul > li > a") %>% 
    html_attr("href")
  
  df = tribble(~ "title", ~ "entreprise", ~ "localisation", ~ "desc", ~ "link", ~ "date")
  
  for (i in seq_along(ref)) {
    ffd$navigate(ref[i])
    
    html_data = ffd$getPageSource()[[1]]
    
    all = read_html(html_data) %>% 
      html_nodes("main > section.core-rail")
    
    df[i, 1] = html_nodes(all, "section.sub-nav-cta.global-alert-offset > div > div > h3") %>% html_text()
    df[i, 2] = html_nodes(all, "section.topcard > div.topcard__content > div.topcard__content-left > h3:nth-child(2) > span:nth-child(1)") %>% html_text()
    df[i, 3] = html_nodes(all, "section.sub-nav-cta.global-alert-offset > div > div > div > span") %>% html_text()
    df[i, 4] = html_nodes(all, "section.description") %>% html_text()
    df[i, 5] = ref[i]
    df[i, 6] = html_nodes(all, "section.topcard > div.topcard__content > div.topcard__content-left > h3:nth-child(3) > span.topcard__flavor--metadata.posted-time-ago__text") %>% html_text()
  }
  
  # Process date
  df = df %>% 
    mutate_at(
      vars(date), 
      ~ .x %>% 
        str_extract("[[:digit:]].*$") %>% 
        (function(x) {
          if (str_detect(x, "heure")) {Sys.Date()}
          else if (str_detect(x, "jour")) {Sys.Date() - as.numeric(str_extract(x, "[[:digit:]]"))}
          else if (str_detect(x, "semaine")) {Sys.Date() - 7 * as.numeric(str_extract(x, "[[:digit:]]"))}
          else if (str_detect(x, "mois")) {Sys.Date() - 30 * as.numeric(str_extract(x, "[[:digit:]]"))}
        })
    )
  
  # Return 
  return(df)
}
  # Download binaries, start driver
  rd = rsDriver(browser = "phantomjs",  port = 4515L)
  # Get client object
  ffd = rd$client
  Sys.sleep(2)
  # Scrap function over key words
  df_pe = map_dfr(mc, scrap_pole_emploi)
  df_li = map_dfr(mc, scrap_linkedin)
  # Close navigate
  ffd$close()
  rm(rd)
  rm(ffd)
  # Process 
  df = bind_rows(df_pe, df_li) %>%
    # GARDER SEULEMENT LES NOUVELLES -> remove this line for initialise data/get/ 
    filter(!date < as_date(last(file.info(list.files(paste0("data/get/", id, "/"), full.names = T))$mtime)))
  df$note = NA
  # Load old data  
  old = map_dfr(list.files(paste0("data/get/", id, "/"), full.names = TRUE), read_csv)
  # Filter dupli.
  new = df %>%
    filter(!link %in% old$link) %>% 
    filter(!duplicated(desc)) %>% 
    filter(!str_detect(title, "bio|Bio|BIO|MÉD|ALT|Stage|STG|Stagiaire|STAGE|Alternance"))
  # Save it 
  write_csv(new, paste0(paste0("data/get/", id, "/"), Sys.Date(),".csv"))
}
app = function(id) {
  # Library
  require(tidyverse)
  require(janitor)
  require(purrr)
  require(shiny)
  require(DT)
  # Load last data and unscrored data
  df = file.info(list.files(paste0("data/get/", id, "/"), full.names = T)) %>%
    rownames_to_column("name") %>% 
    filter(mtime == last(mtime)) %>% 
    .$name %>% 
    read_csv() %>% 
    bind_rows(read_csv(paste0("data/get/", id,"/unscore/unscore.csv"))) %>% 
    filter(!link %in% read_csv(paste0("data/get/", id,"/score/score.csv"))$link) %>% 
    filter(!desc %in% read_csv(paste0("data/get/", id,"/score/score.csv"))$desc) %>% 
    filter(!duplicated(desc)) 
  # Appli
  shinyApp(
    # UI side
    ui = fluidPage(fluidRow(column(12, h1("Emploi-R App"), hr(), DTOutput("x")))),
    # Server side
    server = function(input, output, session) {
      options(DT.options = list(pageLength = 1))
      # server-side processing
      output$x = renderDT(df, selection = 'none', editable = "cell")
      # edit a single cell
      proxy = dataTableProxy('x')
      observeEvent(input$x_cell_edit, {
        info = input$x_cell_edit
        str(info)  # check what info looks like 
        df <<- editData(df, info)
        replaceData(proxy, df, resetPaging = FALSE)  # the above steps can be merged into a single editData() call; see examples below
      })
    }
  )
  # Save unscored df
  read_csv(paste0("data/get/", id,"/unscore/unscore.csv")) %>% 
    bind_rows(filter(df, is.na(note))) %>% 
    filter(!duplicated(link)) %>% 
    filter(!duplicated(desc)) %>% 
    write_csv(paste0("data/unscore/unscore.csv"))
  # Save scored df
  read_csv(paste0("data/get/", id,"/score/score.csv")) %>% 
    bind_rows(filter(df, !is.na(note)) %>% mutate_at(vars(note), as.numeric)) %>% 
    filter(!duplicated(link)) %>% 
    filter(!duplicated(desc)) %>% 
    write_csv(paste0(paste0("data/get/", id,"score/score.csv")))
  
  # AMELIORATION DU FRONT END 
  
  # # Appli
  # shinyApp(
  #   # UI side
  #   ui = fluidPage(
  #     titlePanel("Emploi-R"),
  #     sidebarLayout(
  #       sidebarPanel(width = "100%", textOutput("nText"), br(),
  #         div(
  #           div(actionButton("No", "no", style = "text-align: center;"),
  #               style = "display:inline-block; vertical-align:top;"),
  #           div(" - ", style = "display:inline-block; vertical-align:top;"),
  #           div(actionButton("Yes", "yes", style = "text-align: center;"), 
  #               style = "display:inline-block; vertical-align:top;"),
  #           style = "display: block; margin-left: auto; margin-right: auto;"
  #         ),
  #         br(),
  #         actionButton("Next", "next", style = "text-align: center;"),
  #         style = "text-align: center; display: block;"
  #       ),
  #       mainPanel()
  #     )
  #   ),
  #   # Server side
  #   server = function(input, output, session) {
  #     values = reactiveValues()
  #     values$count = 0
  #     ntext = eventReactive(input$Next, {
  #       values$count = values$count + 1
  #       return(df$desc[values$count])
  #     })
  #     output$nText = renderText({ ntext() })  
  #   }
  # )
}
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