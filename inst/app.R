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
}
