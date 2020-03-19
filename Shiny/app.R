library(shiny)
library(plotly)
library(tidyverse)

# Load Main Data Set
df <- read_csv("~/Dropbox/COVID-19/Ontario-COVID-19-Research/Data/ontario_corona_cases.csv")

df_province <- df %>% 
  group_by (Date) %>% 
  count () %>% 
  ungroup() %>% 
  mutate (cumsum = cumsum(n),
          region = "Province") 

# Create data frame of all dates and Public Health Units
names <- unique(df$Public_Health_Unit)

for (i in names) {
  assign (paste0(i), 
          seq (as.Date("2020-01-25"), Sys.Date(), "days") %>% 
            data.frame() %>% 
            rename (Date = 1) %>% 
            mutate (Public_Health_Unit = i)
  )
}

names_list <- lapply(names, get)
dates_all <- do.call(rbind, names_list)
remove(list = names)

# Create master data frame
cases_by_region <- dates_all %>% 
  left_join (df, by = c ("Date", "Public_Health_Unit")) %>% 
  group_by(Public_Health_Unit) %>% 
  mutate(Cases = cumsum (!is.na(Case_number))) %>% 
  ungroup() %>% 
  mutate (Public_Health_Unit = parse_factor(Public_Health_Unit))

# Begin Shiny App

ui <- fluidPage(
  titlePanel('COVID-19 Cases in Ontario'),
  
  mainPanel(
    # Ooutput with tabs
    tabsetPanel(type = "tabs",
                tabPanel("Background",
                         h3("Data User Group (DUG)"),
                         p("We are a group of researchers and analysts who are interested in data science and would like to use our expertise to contribute to the understanding of COVID-19 in our communities."),
                         h3("Looking for data"),
                         p("One of the challenges we encountered trying to understand the spread of COVID-19 was finding a data source in a format that is easily accessible for analysis.  When we were unable to locate such a file (and finding that the process to scrape data through R was too messy given the formats that the information has been released) we decided to take a manual approach.  Using a few different sources, we have compiled data tables which are easily accessible in R (our favorite) and Python."),
                         ),
                tabPanel("Ontario", 
                         h3("Ontario"),
                         p("Plot 1 shows the growth in total cases for all of Ontario."),
                         br(),
                         plotlyOutput('plot1', width = "100%")),
                tabPanel("Public Heatlh Units",
                         h3("Public Health Units"),
                         p("Plot 2 shows the growth in total cases for each of the Public Health Units (PHU). Double click on the name of the PHU to see the number of cases for that unit."),
                         br(),
                         plotlyOutput('plot2', width = "100%")
                         )
                )
    

  )
)


server <- function(input, output) {
  
  x <- reactive({
    cases_by_region %>% dplyr::filter(PHU == input$PHU)
  })
  
  output$plot1 <- renderPlotly(
    plot1 <- plot_ly(df_province, 
                     x = ~Date, 
                     y = ~cumsum,
                     width = 1000, height = 400) %>% 
      add_lines() %>% 
      layout(
        title = "Total Number of COVID-19 Cases in Ontario",
        xaxis = list (title = "Date"),
        yaxis = list (title = "Number of Cases")
      )
  )
  output$plot2 <- renderPlotly(
    plot2 <- plot_ly(cases_by_region, 
                     x = ~Date, 
                     y = ~Cases,
                     width = 1200, height = 400) %>% 
      add_lines (color = ~Public_Health_Unit) %>% 
      layout(
        title = "Total Number of COVID-19 Cases by Public Health Unit",
        xaxis = list (title = "Date"),
        yaxis = list (title = "Number of Cases")
      )
  )
  
}

shinyApp(ui,server)
