library(tidyverse)


test_politicians <-
  c("Michael von Abercron", "Doris Achelwilm", "Gökay Akbulut")

news_articles_politicians <-
  read.csv("data/df_deu_politicians_articles.csv",
           encoding = "UTF-8") %>%
  mutate(url = str_trim(url)) %>%
  mutate(
    description = case_when(
      description == "" ~ NA_character_,
      TRUE ~ description,
    )
  ) %>%
  drop_na(description) %>%
  distinct(url, .keep_all = TRUE)

if (interactive()) {
  library(shiny)
  library(DT)
  library(bs4Dash)
  
  shinyApp(
    ui = dashboardPage(
      header = dashboardHeader(title = "AcceleratoR"),
      sidebar = dashboardSidebar(sidebarMenu(
        menuItem(text = "Item 1",
                 tabName = "tab1"),
        menuItem(text = "Item 2",
                 tabName = "tab2")
      )),
      controlbar = dashboardControlbar(
        selectInput(
          "select",
          h3("Select box", style = "color: white"),
          choices = test_politicians,
          selected = 1
        )
      ),
      footer = dashboardFooter(),
      title = "test",
      body = dashboardBody(
      tabItems(
        tabItem(
          tabName = "tab1",
          column(width = 6,
                 
                 
                 
                 fluidRow(
                   column(width = 2,
                          htmlOutput("politician_image")),
                   column(
                     width = 4,
                     
                     fluidRow(
                       infoBox(
                         title = "Messages",
                         value = 1410,
                         icon = icon("envelope"),
                         color = "orange",
                         fill = TRUE,
                       )
                     ),
                     
                     fluidRow(
                       infoBox(
                         title = "Bookmarks",
                         color = "info",
                         value = 240,
                         icon = icon("bookmark"),
                         tabName = "tab2",
                         fill = TRUE,
                       )
                     )
                     
                     
                   ),
                   column(
                     width = 4,
                     
                     fluidRow(
                       infoBox(
                         title = "Messages",
                         value = 1410,
                         icon = icon("envelope"),
                         color = "orange",
                         fill = TRUE,
                       )
                     ),
                     fluidRow(
                       infoBox(
                         title = "Bookmarks",
                         color = "info",
                         value = 240,
                         icon = icon("bookmark"),
                         tabName = "tab2",
                         fill = TRUE,
                       )
                     )
                   )
                 ),
          ),
          
          column(width = 4,
                 DT::dataTableOutput('news_table'))
        ),
        tabItem(tabName = "tab2",
                infoBoxOutput("ibox",
                              width = 6))
      ), )
    ),
    
    server = function(input, output) {
      output$ibox <- renderInfoBox({
        infoBox(
          title = userBox(
            title = userDescription(
              title = "Alexander Pierce",
              subtitle = "Founder & CEO",
              type = 1,
              image = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
            ),
            width = 6,
            status = "indigo",
            closable = TRUE,
            "Some text here!",
            footer = "The footer here!"
          ),
          
        )
      })
      
      output$news_table <- DT::renderDataTable({
        DT::datatable(
          news_table_react(),
          colnames = c("Source", "Thumbnail", "Title"),
          escape = FALSE,
          options = list(
            pageLength = 5,
            dom = "t",
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
              "}"
            )
          )
        )
      })
      
      output$politician_image <- renderText({
        src <- portrait_table()
        
        c('<img src="', src, '" width="200">')
      })
      
      news_table_react <- reactive(
        news_articles_politicians %>%
          arrange(desc(date_published)) %>%
          mutate(
            url_image = paste0('<img src="', url_image, '" width="350"></img>')
          ) %>%
          filter(politician == input$select) %>%
          select(source, url_image, title) %>%
          head(20)
      )
      
      portrait_table <- reactive(
        inner_join(deu_portraits, deu_core, by = "pageid") %>%
          filter(name == input$select) %>%
          select(image_url) %>%
          head(1) %>%
          pull(image_url)
      )
      
    }
  )
}
