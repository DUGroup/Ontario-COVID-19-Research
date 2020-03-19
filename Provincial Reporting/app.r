library(plotly)
library(shiny)
library(dplyr)
library(googlesheets4)

Provincial.Reporting <- read_sheet("https://docs.google.com/spreadsheets/d/1Y3_qYkTJK6Vnhw3RCOhOiwafm4-PQxd5l0Hxk4x1ZxE/edit?ts=5e6f7e06#gid=0", 
                                   sheet = 1)

Ontario.Reporting <- Provincial.Reporting %>% clean_names()

Ontario.Reporting <- Ontario.Reporting %>% mutate(patient_age_and_gender = str_to_title(patient_age_and_gender),
                                                  
                                                  gender = ifelse(str_detect(patient_age_and_gender, "Male"), "Male", 
                                                                  ifelse(str_detect(patient_age_and_gender, "Female"), "Female", 
                                                                         NA))) %>% 
  cbind(age = as.numeric(gsub("\\D", "", Ontario.Reporting$patient_age_and_gender))) %>% 
  mutate(age = ifelse(!is.na(age), paste0(age, "-", age + 9), NA)) %>% 
  select(-patient_age_and_gender, - https_www_ontario_ca_page_2019_novel_coronavirus_number_section_0)

region.summary <- Ontario.Reporting %>% count(public_health_unit, name = "Cases")

sp.Canada <- readShapeSpatial('C:/Users/mn209073/Documents/March Break/Coronavirus Analysis/gcd_000a06a_e/gcd_000a07a_e.shp', 
                              proj4string = CRS("+proj=longlat +datum=WGS84") )
#slotNames(sp.Canada)
#names(sp.Canada@data)
#head(sp.Canada@data, 5)
sp.Ontario = sp.Canada[sp.Canada@data$PRNAME == "Ontario",]

sp.Ontario.df <- fortify(sp.Ontario, region ="CDNAME")

#join cases count to the shapefile by region name
sp.Ontario.COVID = left_join(x = sp.Ontario.df, y = region.summary, by = c("id" = "public_health_unit"))

#sp.Ontario.COVID$breaks <- cut(sp.Ontario.COVID$Cases, 5)

#table(sp.Ontario.COVID$n, exclude = NULL)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  #titlePanel("Hello Shiny!"),
  
  fluidRow(
    column(12, 
           # Output: Histogram ----
           plotlyOutput(outputId = "distPlot")
           
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlotly(expr={
    #plot map
    ggplotly(ggplot(data = sp.Ontario.COVID, aes(x=long, y=lat, group = group)) + 
      geom_polygon(aes(fill = Cases, group = id), color = 'black') + #scale_fill_gradient(low = "yellow", high = "red")+
      labs(title = "(COVID-19) Cases by Ontario Region"), height = 800)
    
  })
  
}

shinyApp(ui = ui, server = server)