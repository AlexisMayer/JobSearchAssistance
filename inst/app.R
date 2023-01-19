#' Title: Application Shiny pour l'évaluation et l'édition de données d'emploi
#' 
#' Description: La fonction app lance une application Shiny qui permet à l'utilisateur de visualiser et d'évaluer les offres d'emploi obtenues à l'aide de la fonction get en utilisant un système de "swipe" similaire à celui des applications de rencontre. Les données évaluées et éditées sont ensuite sauvegardées pour être utilisées pour entraîner un modèle de machine learning.
#' 
#' @param id: L'identifiant de l'utilisateur pour accéder aux données obtenues avec la fonction get.
#' 
#' @examples
#' id <- "123456"
#' app(id)
#' 
#' @export
app = function(id) {
  
  # Library
  require(tidyverse)
  require(janitor)
  require(purrr)
  require(shiny)
  require(DT)
  require(shinySwipe)
  
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
    ui = ui <- fluidPage(
      
    # Titre de l'application
    titlePanel("Emploi-R App"),  
      
    # Zone pour swiper les offres d'emploi
    shinySwipe(id = "swipe", choices = c("Pass", "Like"), swipe = "Pass"),   
      
    # Zone pour afficher les offres d'emploi
    fluidRow(
        column(12, DTOutput("dataTable"))
    )
    ),
    
    # Server side
    server <- function(input, output) {
      
    # Charger les données
    df <- read_csv("offres_emploi.csv")
      
    # Afficher les données dans le composant DT
    output$dataTable <- renderDT(df)
      
    # Observer l'événement de swipe
    observeEvent(input$swipe, {
      
        # Récupérer l'index de l'offre d'emploi swipée
        swipeIndex <- input$swipe_index
      
        # Récupérer la valeur de swipe (Pass ou Like)
        swipeValue <- input$swipe_value
      
        # Si l'utilisateur a "liké" l'offre d'emploi
        if (swipeValue == "Like") {
            # Ajouter une colonne "note" avec la valeur 1
            df[swipeIndex, "note"] <- 1
        }
        # Si l'utilisateur a "passé" l'offre d'emploi
        if (swipeValue == "Pass") {
            # Ajouter une colonne "note" avec la valeur 0
            df[swipeIndex, "note"] <- 0
        }
        
        # Enregistrer les données modifiées
        write_csv(df, "offres_emploi.csv")
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
  
}
