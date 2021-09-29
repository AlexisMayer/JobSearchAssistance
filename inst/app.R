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