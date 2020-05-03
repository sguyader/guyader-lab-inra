### inclure les données sur les tests
download.file("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv", destfile = "data/covid-testing-all-observations.csv")

covid_testing <- read_csv("data/covid-testing-all-observations.csv")
covid_testing <- separate(covid_testing, col=Entity, into=c("Country", "type_test"), sep=" - ") %>%
  select(Country, "Cumulative total", type_test, Date) %>%
  rename(cumul_tests="Cumulative total")

last_tests <- covid_testing %>%
  group_by(Country) %>%
  arrange(Date) %>%
  filter(row_number()==n()) %>%
  arrange(Country) %>%
  filter(Country != "Hong Kong")

last_tests$Country[16] <- "Czechia"
last_tests$Country[31] <- "Iran, Islamic Rep"
last_tests$Country[59] <- "Russian Federation"
last_tests$Country[64] <- "Slovak Republic"
last_tests$Country[67] <- "Korea, Rep."


first_cases <- jhu.covid.3 %>%
  group_by(Country) %>%
  arrange(date) %>%
  filter(confirmed > 0 ) %>%
  filter(row_number() == 1) %>%
  arrange(Country) %>%
  select(Country, date) %>%
  rename(start_epid=date)

last_cases <- jhu.covid.3 %>%
  group_by(Country) %>%
  arrange(date) %>%
  filter(confirmed > 0 ) %>%
  filter(row_number() == n()) %>%
  arrange(Country) %>%
  select(Country, date) %>%
  rename(last_epid=date)

testing <- left_join(first_cases, last_cases) %>%
  left_join(last_tests) %>%
  mutate(epid_duration = last_epid-start_epid) %>%
  relocate(epid_duration, .after=last_epid)
