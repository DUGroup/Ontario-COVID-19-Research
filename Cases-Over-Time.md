Ontario COVID-19 Dataset
================

## Load Libraries

``` r
knitr::opts_chunk$set(echo = TRUE)

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
```

## Load Data

``` r
df <- read_csv("Data/ontario_corona_cases.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Case_number = col_double(),
    ##   age_gender = col_character(),
    ##   Public_Health_Unit = col_character(),
    ##   Hospital = col_character(),
    ##   Transmission = col_character(),
    ##   Status = col_character(),
    ##   Date = col_date(format = "")
    ## )

## Total Cases in Ontario

This chart shows the exponential growth in COVID-19 cases in Ontario as
of 2020-03-18 14:44:15. To date there are 212 cases of COVID-19 as
reported by the Ontario government.

``` r
current_time <- Sys.time()

df %>% 
  group_by (Date) %>% 
  count () %>% 
  ungroup() %>% 
  mutate (cumsum = cumsum(n)) %>% 
  # print (n = Inf)
  ggplot (aes (x = Date, y = cumsum)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0, length (df$Case_number) + 5)) +
  labs (title = "Number of COVID-19 Cases in Ontario",
        x = "Date Reported",
        y = "Number of Cases") +
  geom_text (aes (label = cumsum), 
             vjust = -1) +
  annotate("text", x = as.Date("2020-02-01"), y = length(df$Case_number) - 5, 
           label = paste0("Current as of\n", current_time )
           )
```

![](Cases-Over-Time_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## Cases Reported by Health Unit

Most of cases are reported in Toronto however the virus is spreading to
other regions.

``` r
dates <- seq (as.Date("2020-01-25"), Sys.Date(), "days") %>% 
  data.frame() %>% 
  rename (Date = 1)
  

cases_by_region <- dates %>% 
  left_join (df, by = c ("Date" = "Date")) %>% 
  group_by (Date, Public_Health_Unit) %>% 
  count () %>% 
  group_by (Public_Health_Unit) %>% 
  mutate (cumsum = cumsum (n)) %>%
  arrange (Public_Health_Unit, Date ) %>% 
  mutate (date_first_case = min (Date)) %>% 
  ungroup() %>% 
  mutate (label = parse_factor(Public_Health_Unit),
          label = fct_reorder(label, date_first_case)) 

temp <- c("Toronto", "RichmondHill", "Grey Bruce")

cases_by_region %>%
  #dplyr::filter(Public_Health_Unit %in% temp) %>% 
  ggplot (aes (x = Date, y = cumsum, colour = label)) +
  geom_line (aes (group = 1)) +
  geom_point(size = 1) +
  geom_text(aes (label = cumsum), 
            vjust = -1, 
            size = 2, 
            colour = "black") +
  scale_y_continuous(limits = c(0, (max(cases_by_region$cumsum) + 5))) +
  facet_wrap(~label, 
             scales = "free",
             ncol = 1) +
  theme (legend.position = "none")
```

![](Cases-Over-Time_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
