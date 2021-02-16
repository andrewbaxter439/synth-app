library(tidyverse)
library(Synth)
library(plotly)

load('data/synth_data_c.rdata') # outputted from 'Synth_data.R'
load("data/filtered_itsp.rdata")  # outputted from 'Synth_create_sps.R'
load("data/other_predictors.rdata")

countries <- unique(synthData_u18_filt %>% 
                        filter(Country!="England and Wales") %>% 
                        pull(Country))


ui <- fluidPage(
    theme = "theme.css",
    titlePanel(
        div(id = "headtext", "Synthetic Control analysis of England's Teenage Pregnancy Strategy"),
        windowTitle = "Synthetic Control analysis of England's Teenage Pregnancy Strategy"),
    
    div(id = "main-input",
        class="container container-sm",
        # style = "max-width:600px; margin-top: 100px;",
        fluidRow(
            selectizeInput("countries", 
                           "Select countries to include in control",
                           choices = countries, 
                           selected = countries, 
                           multiple = TRUE,
                           width = '100%',
                           options = list(
                               'plugins' = list('remove_button')
                           )
            ),
            column(6,
                   selectInput("agegrp",
                               "Select outcome to view",
                               choices = c("Under-18 birth rates",
                                           "Under-20 pregnancy rates"),
                               selected = "Under-18 birth rates")),
            column(6,
                   checkboxInput("post", "Show post-period", value = TRUE))
        ),
        fluidRow(
            plotlyOutput("synth_graph"),
        )
    )
)


server <- function(input, output) {
    
    
    opt_y1 <- 1990
    md <- reactive({
        
        if (input$agegrp == "Under-18 birth rates") {
            df <- synthData_u18_filt
            sps <- sp_u18_filt
            dependent = "rate"
            grp = "u18_sp"
        } else {
            df <- synthData_u20_filt
            sps <- sp_u20_filt
            dependent = "pRate"
            grp = "u20_sp"
        }
        
        synthPrep(
            as.data.frame(df %>% 
                              filter(Country %in% c("England and Wales", input$countries))), 
            assign_global = FALSE,
            grp = grp,
            dependent = dependent,
            special.predictors = sps,
            time.optimise.ssr = opt_y1:1998,
            time.predictors.prior = opt_y1:1998,
            time.plot = opt_y1:2013
        )
    })
    
    output$synth_graph <- renderPlotly(
        ggplotly(
            gg_synth(md = md(), 
                     post = input$post,
                     agegrp = input$agegrp) +
                xlim(opt_y1,2014) +
                theme(rect = element_rect(fill = "#f4f7f9"),
                      legend.position = "bottom")
        ) %>% 
            layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
    )
    
}


shinyApp(ui = ui, server = server)
