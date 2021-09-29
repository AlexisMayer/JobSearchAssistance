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
