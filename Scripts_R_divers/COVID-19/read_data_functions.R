read_data_covid19datahub <- function() {
  data_covid_ts <- covid19(verbose = FALSE)
  x <- as.data.table(data_covid_ts)
  x <- rename(x, Country = administrative_area_level_1)
  x_max <- filter(x, date == "2020-10-31")
  x_max <- rename(x_max, Total_tests = tests)
  x_max <- mutate(x_max, Tests_1M_pop = ceiling((Total_tests / population) * 1e6), .after = Total_tests)
  return(x_max %>% filter(!is.na(Total_tests) & Total_tests != 0) %>% select(Country, Total_tests, Tests_1M_pop))
}

read_data_cases <- function() {
  data_confirmed <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  data_confirmed <- rename(data_confirmed, Country = "Country/Region") %>%
    select(-"Province/State") %>%
    mutate(Lat = replace(Lat, Country == "Australia", "-25"), Long = replace(Long, Country == "Australia", "133")) %>%
    mutate(Lat = replace(Lat, Country == "Canada", "60"), Long = replace(Long, Country == "Canada", "-95")) %>%
    mutate(Lat = replace(Lat, Country == "China", "31"), Long = replace(Long, Country == "China", "114"))
  data_confirmed_melt <- data_confirmed %>%
    pivot_longer(cols =  c(-Country, -Lat, -Long), names_to = "DateRep", values_to = "Cases_cumsum")
  
  # Long format
  data_confirmed_melt <- mutate(data_confirmed_melt, DateRep = lubridate::mdy(DateRep))
  
  # Cumulative cases for countries split into regions
  data_confirmed_melt_agg <- data_confirmed_melt %>%
    group_by(Country, DateRep, Lat, Long) %>%
    summarise_all(list(Cases_cumsum = sum))
  
  # Daily new Cases
  data_confirmed_melt_agg <- arrange(data_confirmed_melt_agg, Country, DateRep)
  data_confirmed_melt_agg <- mutate(data_confirmed_melt_agg, Cases = c(Cases_cumsum[1], diff(Cases_cumsum)))
  data_confirmed_melt_agg <- data_confirmed_melt_agg %>% mutate(Cases = replace(Cases, Cases < 0, 0))

  return(data_confirmed_melt_agg)
}

read_data_deaths <- function() {
  
  data_deaths <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  data_deaths <- rename(data_deaths, Country = "Country/Region") %>%
    select(-"Province/State") %>%
    mutate(Lat = replace(Lat, Country == "Australia", "-25"), Long = replace(Long, Country == "Australia", "133")) %>%
    mutate(Lat = replace(Lat, Country == "Canada", "60"), Long = replace(Long, Country == "Canada", "-95")) %>%
    mutate(Lat = replace(Lat, Country == "China", "31"), Long = replace(Long, Country == "China", "114"))
  data_deaths_melt <- data_deaths %>%
    pivot_longer(cols = c(-Country, -Lat, -Long), names_to = "DateRep", values_to = "Deaths_cumsum")
  
  # Long format
  data_deaths_melt <- mutate(data_deaths_melt, DateRep = lubridate::mdy(DateRep))
  
  # Cumulative cases for countries split into regions
  data_deaths_melt_agg <- data_deaths_melt %>%
    group_by(Country, DateRep, Lat, Long) %>%
    summarise_all(list(Deaths_cumsum = sum))
  
  # Daily new Cases
  data_deaths_melt_agg <- arrange(data_deaths_melt_agg, Country, DateRep)
  data_deaths_melt_agg <- mutate(data_deaths_melt_agg, Deaths = c(Deaths_cumsum[1], diff(Deaths_cumsum)))
  data_deaths_melt_agg <- data_deaths_melt_agg %>% mutate(Deaths = replace(Deaths, Deaths < 0, 0))
  
  return(data_deaths_melt_agg)
}

read_data_recovered <- function() {
  
  data_recovered <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
  data_recovered <- rename(data_recovered, Country = "Country/Region") %>%
    select(-"Province/State") %>%
    mutate(Lat = replace(Lat, Country == "Australia", "-25"), Long = replace(Long, Country == "Australia", "133")) %>%
    mutate(Lat = replace(Lat, Country == "Canada", "60"), Long = replace(Long, Country == "Canada", "-95")) %>%
    mutate(Lat = replace(Lat, Country == "China", "31"), Long = replace(Long, Country == "China", "114"))
  data_recovered_melt <- data_recovered %>%
    pivot_longer(cols = c(-Country, -Lat, -Long), names_to = "DateRep", values_to = "Recovered_cumsum")
  
  # Long format
  data_recovered_melt <- mutate(data_recovered_melt, DateRep = lubridate::mdy(DateRep))
  
  # Cumulative cases for countries split into regions
  data_recovered_melt_agg <- data_recovered_melt %>%
    group_by(Country, DateRep, Lat, Long) %>%
    summarise_all(list(Recovered_cumsum = sum))
  
  # Daily new Cases
  data_recovered_melt_agg <- arrange(data_recovered_melt_agg, Country, DateRep)
  data_recovered_melt_agg <- mutate(data_recovered_melt_agg, Recovered = c(Recovered_cumsum[1], diff(Recovered_cumsum)))
  data_recovered_melt_agg <- data_recovered_melt_agg %>% mutate(Recovered = replace(Recovered, Recovered < 0, 0))
  
  return(data_recovered_melt_agg)
}

join_all_corona_data <- function() {
  
  data_all <- read_data_cases()
  data_deaths <- read_data_deaths()
  data_recov <- read_data_recovered()
  data_tests <- read_data_covid19datahub()

  # join deaths, recovered
  data_all <- left_join(data_all, data_deaths)
  data_all <- left_join(data_all, data_recov)
  
  data_all <- data_all %>% mutate(Country = replace(Country, Country == "Burma", "Myanmar"))
  data_all <- data_all %>% mutate(Country = replace(Country, Country == "West Bank and Gaza", "Palestine"))
  
  # join tests
  data_all <- left_join(data_all, data_tests)
  
  # compute active cases cumsum
  data_all <- mutate(data_all, Active_cases_cumsum = Cases_cumsum - Deaths_cumsum - Recovered_cumsum)
  
  data_all[is.na(Recovered_cumsum), Recovered_cumsum := 0]
  data_all[is.na(Recovered), Recovered := 0]
  data_all[is.na(Active_cases_cumsum), Active_cases_cumsum := 0]
  
  # data_all[.("Slovakia"), on = .(Country)]
  # data_all[.("Italy"), on = .(Country)]
  
  return(data_all)
  
}