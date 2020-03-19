# Create data frame of all dates and Public Health Units

df <- read_csv("Data/ontario_corona_cases.csv")

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

