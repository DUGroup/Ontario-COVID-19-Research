# Create data frame of all dates and Public Health Units

df <- read_csv("~/Dropbox/COVID-19/Ontario-COVID-19-Research/Data/ontario_corona_cases.csv")

df <- df %>% 
  mutate (Public_Health_Unit = ifelse (Public_Health_Unit == "RichmondHill" | 
                                         Public_Health_Unit == "Newmarket", "York", Public_Health_Unit )) %>% 
  mutate (Public_Health_Unit = ifelse(Public_Health_Unit == "Haliburton Kawartha", 
                                      "Haliburton,Kawartha,Pine Ridge",
                                      Public_Health_Unit)) %>% 
  mutate(Public_Health_Unit = ifelse(Public_Health_Unit == "pending" |
                                       Public_Health_Unit == "NotAvailable", "Pending or Not Available", Public_Health_Unit))


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

write_csv (dates_all, 
           path = "/Users/gregrousell/Dropbox/COVID-19/Ontario-COVID-19-Research/Data/dates_all.csv")

unique(dates_all$Public_Health_Unit)
