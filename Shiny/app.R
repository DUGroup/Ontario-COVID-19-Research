library(shiny)
library(plotly)
library(tidyverse)
library(googlesheets4)

# Load Main Data Set
df <- read_csv("../Data/ontario_corona_cases.csv")

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

#############################################
#provincial report data extract and wrangling
#############################################

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
                         
                         h3("Opening the data from the source"),
                         
                         p("A ", a(href="https://docs.google.com/spreadsheets/d/1Y3_qYkTJK6Vnhw3RCOhOiwafm4-PQxd5l0Hxk4x1ZxE/edit?usp=sharing", "googlesheet"), "has been created and is being maintained using data from an Ontario government website and resources available on two Wikipedia pages. We will continue to update these tables until a more authoritative source of case records is made available, ideally by Public Health Ontario."),
                         
                         p("You can also access the data directly from DUG's ", a(href= "https://raw.githubusercontent.com/DUGroup/Ontario-COVID-19-Research/master/Data/ontario_corona_cases.csv", "github page.")),
                         
                         h3("Resources: An invitation to explore and dive deeper"),
                         p("As we explore this data we will be sharing visualizations and insights on the Data User Group website. Our hope is that others will find our summaries useful.  We extend an open invitation to others interested in data science to engage in additional analysis and use this data set for your own exploration. A ", strong("github"), " has also been created which will include the R code of our members as well as a .csv file that will be updated regularly.  The ", strong ("googlesheet"), " will serve as our authoritative data source and the github will serve as our central repository for code.  We invite any who are interested to contribute to the github."),
                         
                         h3("Data Background and Sources"),
                         p("The “Provincial Reporting” tab in the googelsheet is a compilation of data from this", strong("Ontario government website"), a(href = "https://www.ontario.ca/page/2019-novel-coronavirus#section-0)", "https://www.ontario.ca/page/2019-novel-coronavirus"), ". This webpage provides a table on new cases of COVID-19 diagnosed in the province.  Following are notes about the data that is published on this page:"),
                         tags$ul(
                           tags$li("Using the Wayback Machine, the earliest records that could be obtained began at case 32."),
                           tags$li("The first 31 cases were then compiled by parsing the press releases available at the bottom of the page."),
                           tags$li("Currently, case numbers 6, 16, 17 and 18 have not been found in the available press releases."),
                           tags$li("Coding with respect to regional health unit appears to have changed over time.  A new column has been added with recoded health unit labels for consistency."),
                           tags$li("On March 18th the website stopped posting the hospitals the cases are related to."),
                           tags$li("The data in the googlesheet and github is updated daily from this website.")
                         )
                         
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
                         ),
                tabPanel("Ontario Map", 
                         h3("Ontario Map"),
                         p("Plot 1 shows the number of confirmed COVID-19 cases in Ontario by census division."),
                         br(),
                         plotlyOutput('plot3', width = "100%"))
                )
    

  )
)


server <- function(input, output) {
  
  output$plot1 <- renderPlotly(
    plot1 <- plot_ly(df_province, 
                     x = ~Date, 
                     y = ~cumsum,
                     width = 1000, height = 400) %>% 
      add_lines() %>% 
      layout(
        title = "Total Number of COVID-19 Cases in Ontario",
        xaxis = list (title = "Date"),
        yaxis = list (title = "Number of Cases",
                      range = c(0, max(df_province$cumsum) + 50)
                      )
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
  
  output$plot3 <- renderPlotly(
    ggplotly(ggplot(data = sp.Ontario.COVID, aes(x=long, y=lat, group = group)) + 
               geom_polygon(aes(fill = Cases, group = id), color = 'black') + #scale_fill_gradient(low = "yellow", high = "red")+
               labs(title = "(COVID-19) Cases by Ontario Region"), 
      height = 650, width = 650)
    
  )
  
}

shinyApp(ui,server)
