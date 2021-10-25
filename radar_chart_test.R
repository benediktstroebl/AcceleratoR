library(tidyverse)
library(shiny)
library(legislatoR)
library(lubridate)


# Data section ------------------------------------------------------------

# legislatoR data
core_de <- get_core("deu")
political_de <- get_political("deu")
traffic_de <- get_traffic("deu")

# traffic per MP
traffic_per_mp_de <- traffic_de %>%
    group_by(pageid) %>% 
    summarise(
        min_date_traffic = min(date),
        max_date_traffic = max(date),
        sum_traffic = sum(traffic),
        date_range_years_traffic = difftime(max_date_traffic, min_date_traffic) %>% lubridate::time_length("years")
    ) %>% 
    ungroup

# sessions per MP
sessions_per_mp_de <- political_de %>% 
    group_by(pageid) %>% 
    summarise(session_n = n()) %>% 
    ungroup()

# entry age first session
entry_age_first_session_de <- political_de %>% 
    left_join(core_de) %>% 
    group_by(pageid) %>% 
    filter(session == min(session)) %>% 
    ungroup() %>% 
    mutate(
        age_session_start = if_else(
            session_start > birth,
            time_length(difftime(session_start, birth), "years") %>% round(3),
            NA_real_
        )
    ) %>% 
    select(pageid, wikidataid, age_session_start) %>% 
    distinct(pageid, .keep_all = T)

## check uniqueness
entry_age_first_session_de %>% 
    group_by(pageid) %>% 
    mutate(n = n()) %>% 
    filter(n > 1)

# age at death
age_mp_de <- core_de %>% 
    mutate(
        age = case_when(
            death > birth ~ time_length(difftime(death, birth), "years") %>% round(3),
            is.na(death) ~ time_length(difftime(Sys.Date(), birth), "years") %>% round(3),
            TRUE ~ NA_real_
        )
    ) %>% 
    select(pageid, wikidataid, age)

# parlspeech metrics
parlspeech_metrics_de <- read_csv("parlspeech_data_de.csv") %>% 
    filter(!is.na(pageid)) %>% 
    mutate(pageid = pageid %>% as.character)

df_metrics_de <- core_de %>% 
    distinct(pageid) %>% 
    left_join(traffic_per_mp_de) %>% 
    left_join(sessions_per_mp_de) %>% 
    left_join(entry_age_first_session_de) %>% 
    left_join(age_mp_de) %>% 
    left_join(parlspeech_metrics_de)


# Input vector (MPs) ------------------------------------------------------

mp_vec <- df_metrics_de %>% 
    arrange(desc(mean_terms_speech)) %>% 
    pull(parlspeech_id)


# Custom functions --------------------------------------------------------

coord_radar <- function (theta = "x", start = 0, direction = 1) {
    theta <- match.arg(theta, c("x", "y"))
    r <- if (theta == "x") 
        "y"
    else "x"
    ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
            direction = sign(direction),
            is_linear = function(coord) TRUE)
}

# rescale variables!
normalize <- function(x) {
    if (is.numeric(x)) {
        (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    } else {
        print("Vector is not numeric")
    }
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MP Metrics \n Radar Chart"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "mp_input",
                "Select MP",
                choices = mp_vec,
                multiple = TRUE
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("radar_chart")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    mp_df <- reactive({
        
        df_metrics_de %>% 
            filter(parlspeech_id %in% input$mp_input) %>% 
            select(pageid, parlspeech_id, everything()) %>% 
            select(
                # ids
                pageid, 
                parlspeech_id, 
                # metrics
                session_n,
                n_speech,
                sum_traffic,
                age,
                age_session_start,
                mean_terms_speech
            ) %>% 
            mutate(
                across(where(is.numeric), ~ normalize(.x), .names = "{col}_normalized")
            ) %>% 
            select(pageid, parlspeech_id, ends_with("normalized")) %>% 
            pivot_longer(
                cols = -c(pageid, parlspeech_id),
                names_to = "var",
                values_to = "value"
            ) %>% 
            # right order
            arrange(var)
        
    })
    
    mp_plot <- reactive({
        
        mp_df() %>% 
            ggplot(
                aes(
                    x = var,
                    y = value,
                    group = parlspeech_id,
                    color = parlspeech_id,
                    fill = parlspeech_id,
                )
            ) + 
            # scale_x_discrete(labels = var_vec) +
            geom_point(show.legend = FALSE) +
            geom_polygon(alpha = 0.5) +
            # geom_vline(xintercept = -100:100) +
            # geom_hline(yintercept = 1) +
            # geom_radar() +
            coord_radar() +
            # coord_fixed(12) +
            labs(x = "", y = "", title = "test") +
            # theme_linedraw() +
            theme(
                # axis.text.y = element_blank(),
                # axis.text.y = element_text(vjust = 10),
                axis.ticks.y = element_blank(),
                panel.spacing = unit(10, "lines"),
                legend.position = "top",
                plot.title = element_text(hjust = 0.5),
                panel.grid.major.x = element_line(color = "grey", size = 1.5),
                # plot.margin = unit(c(4, 4, 4, 4), "cm"),
                # axis.text.x = element_text(angle = 0, vjust = 10, hjust = 10, size = 10)
                # axis.text.x = element_text(margin = margin(t = 10, r = 2, b = 3, l = 4))
                # axis.text.x.bottom = element_text(hjust = 10)
                # panel.grid.minor = element_line(color = "red", size = 2)
            ) 
        
    })

    output$radar_chart <- renderPlot({
        
        validate(
            need(input$mp_input, "Select MP")
        )
        
        mp_plot()
   
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
