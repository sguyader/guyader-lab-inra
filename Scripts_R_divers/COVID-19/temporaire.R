### Covid response policy

download.file("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv", destfile = "data/covid_response.csv")

covid_response <- read_csv("data/covid_response.csv")
covid_response <- covid_response %>% mutate(Date=lubridate::ymd(Date)) %>%
  filter(Date < "2020-05-01")

# logistic : Asym/(1+exp(b*(as.numeric(Date)-xmid)))^f ; start=list(b=-10, Asym=90, xmid=1.835e4, f=1)
# Weibull 1 : Asym*exp(-exp(b*(log(as.numeric(Date))-log(xmid))))
# Weibull 2 : Asym*(1-exp(-exp(b*(log(as.numeric(Date))-log(xmid)))))
# Gaussian : ymax*exp(-((as.numeric(Date)-xloc)^2)/(2*width^2))

#test <- covid_response %>% filter(CountryCode == "AFG")

fun1 <- StringencyIndex ~ Asym*exp(-exp(-b*(as.numeric(Date)-xmid)))
fun2 <- StringencyIndex ~ ymax*exp(-((as.numeric(Date)-xloc)^2)/(2*width^2))

# resp_mod1 <- nlsLM(StringencyIndex ~ Asym*exp(-exp(-b*(as.numeric(Date)-xmid))), data=test, start=list(b=0.1, Asym=90, xmid=1.83e4), control = nls.lm.control(maxiter=100))
# 
# resp_mod2 <- nlsLM(StringencyIndex ~ ymax*exp(-((as.numeric(Date)-xloc)^2)/(2*width^2)), data=test, start=list(ymax=80, xloc=1.837e4, width=30), control = nls.lm.control(maxiter=100))

# results.response.1 <- tibble(params=c("b_response","Asym_response","xmid_response", "P(b)_response", "P(Asym)_response", "P(xmid)_response", "converged_response", "nbiter_response"))
# results.response.2 <- tibble(params=c("ymax_response","xloc_response","width_response", "P(ymax)_response", "P(xloc)_response", "P(width)_response", "converged_response", "nbiter_response"))

response_table <- tibble(CountryCode=unique(covid_response$CountryCode), CountryName=unique(covid_response$CountryName), AUC_SI=NA, date_SI_sup0=NA, date_SI_sup50=NA, date_SI_max=NA, SI_max=NA)

for(i in unique(covid_response$CountryName)) {
  tryCatch({ 
    dat <- filter(covid_response, CountryName == i, Date <= "2020-04-25") # filtrer par région/pays
    # AUC et SI
    AUC_value <- DescTools::AUC(as.numeric(dat$Date), dat$StringencyIndex, method="linear")
    date_SI_sup0 <- dat$Date[which(dat$StringencyIndex > 0)][1]
    date_SI_sup50 <- dat$Date[which(dat$StringencyIndex > 50)][1]
    date_SI_max <- dat$Date[which(dat$StringencyIndex == max(dat$StringencyIndex, na.rm=T))][1]
    SI_max <- max(dat$StringencyIndex, na.rm=T)
    response_table$AUC_SI[response_table$CountryName == i] <- AUC_value
    response_table$date_SI_sup0[response_table$CountryName == i] <- date_SI_sup0
    response_table$date_SI_sup50[response_table$CountryName == i] <- date_SI_sup50
    response_table$date_SI_max[response_table$CountryName == i] <- date_SI_max
    response_table$SI_max[response_table$CountryName == i] <- SI_max
    # # modèles
    # model1 <- nlsLM(fun1, data=dat, start=list(b=0.1, Asym=90, xmid=1.83e4), control = nls.lm.control(maxiter=100), na.action = na.exclude)
    # model2 <- nlsLM(fun2, data=dat, start=list(ymax=80, xloc=1.837e4, width=30), control = nls.lm.control(maxiter=100), na.action = na.exclude)
    # res1 <- tibble(coef(model1))
    # res2 <- tibble(coef(model2))
    # names(res1) <- i
    # names(res2) <- i
    # res1 <- rbind(res1, summary(model1)$coefficients[10], summary(model1)$coefficients[11], summary(model1)$coefficients[12], model1$convInfo$isConv, model1$convInfo$finIter)
    # res2 <- rbind(res2, summary(model2)$coefficients[10], summary(model2)$coefficients[11], summary(model2)$coefficients[12], model1$convInfo$isConv, model1$convInfo$finIter)
    # results.response.1 <- cbind(results.response.1, res1)
    # results.response.2 <- cbind(results.response.2, res2)
    # newdata = expand.grid(date=as.Date(dat$Date))
    # pred1 <- predict(model1, newdata=newdata)
    # pred2 <- predict(model2, newdata=newdata)
    # # graphe
    # p <- ggplot(dat, aes(x=as.Date(Date), y=StringencyIndex)) +
    #   geom_point() +
    #   #geom_line(aes(x=as.Date(Date), y=pred1), color="red") +
    #   #geom_line(aes(x=as.Date(Date), y=pred2), color="blue") +
    #   scale_x_date(date_breaks="1 week", labels=scales::date_format("%d-%m-%Y")) +
    #   theme(axis.text.x = element_text(angle = 90)) +
    #   labs(title = paste("Evolution de la réponse au COVID-19 en", i), x="date", y="Indice d'intensité")
    # ggsave(p, filename=paste("dyn_stringency_",i,".png"),
    #        path="stringency_plots", width=8, height=5, units="in", dpi=72)
  }, error=function(e){cat("ERROR :",i, "\n")})
}

response_table[151,1] <- "XKX"

response_table <- response_table %>%
  rename(ISO_A3=CountryCode) %>%
  arrange(ISO_A3)

test <- left_join(covid_data_all[,1:3], response_table)

ggplot(covid_response, aes(x=Date, y=StringencyIndex)) +
  geom_line(aes(group=CountryCode), alpha=0.3)


### Covid mobility

covid_mobility <- read_csv("data/covid_mobility.csv")
covid_mobility <- covid_mobility %>%
  filter(date < "2020-05-01", is.na(sub_region_1)) %>%
  filter(country_region != "Réunion") %>%
  rename(retail_and_recreation=retail_and_recreation_percent_change_from_baseline) %>%
  rename(grocery_and_pharmacy=grocery_and_pharmacy_percent_change_from_baseline) %>%
  rename(parks=parks_percent_change_from_baseline) %>%
  rename(transit_stations=transit_stations_percent_change_from_baseline) %>%
  rename(residential=residential_percent_change_from_baseline) %>%
  rename(workplaces=workplaces_percent_change_from_baseline)
length(unique(covid_mobility[[1]]))

# retail_and_recreation
# grocery_and_pharmacy
# parks
# transit_stations
# residential
# workplaces

mobility_table <- tibble(CountryCode=unique(covid_mobility$country_region_code), CountryName=unique(covid_mobility$country_region), retail_and_recreation_change = NA, retail_and_recreation_date_split = NA, grocery_and_pharmacy_change = NA, grocery_and_pharmacy_date_split = NA, parks_change = NA, parks_date_split = NA, transit_stations_change = NA, transit_stations_date_split = NA, residential_change = NA, residential_date_split = NA, workplaces_change = NA, workplaces_date_split = NA)

for(i in unique(covid_mobility$country_region)) {
  tryCatch({ 
    dat <- filter(covid_mobility, country_region == i)
    party.fit <- partykit::ctree(workplaces ~ as.numeric(date), data=dat, control=ctree_control(maxdepth=1))
    if(length(nodeids(party.fit))==1){
      mobility_table$workplaces_change[mobility_table$CountryName == i] <- 0
      mobility_table$workplaces_date_split[mobility_table$CountryName == i] <- NA
    }
    else{
      date_split <- party.fit[2]$data[2][nrow(party.fit[2]$data[2]),1]
      step_1 <- mean(party.fit[2]$data[1][,1])
      step_2 <- mean(party.fit[3]$data[1][,1])
      change <- step_2-step_1
      mobility_table$workplaces_change[mobility_table$CountryName == i] <- change
      mobility_table$workplaces_date_split[mobility_table$CountryName == i] <- as.Date(date_split, start="1970-01-01")
    }

    dat$parks.party.predict <- predict(party.fit)
    p <- ggplot(data=dat, aes(date, workplaces)) +
      geom_point() +
      geom_line(aes(y=parks.party.predict)) +
      scale_x_date(date_breaks="1 week", labels=scales::date_format("%d-%m-%Y")) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(title = paste("Evolution de la mobilité en", i), x="date", y="Changement de fréquentation des lieux de travail (en %)")
    ggsave(p, filename=paste("dyn_mobility_",i,"_workplaces.png"),
           path="mobility_plots", width=8, height=5, units="in", dpi=72)
  }, error=function(e){cat("ERROR :",i, "\n")})
}

test <- filter(covid_mobility, country_region == "Belarus")

# plot(test$date, test[[8]], type="b", lty=2)
# lines(supsmu(test$date, test[[8]], span="cv"), col="red")
# lines(lowess(test$date, test[[8]], f=0.16), col="blue")

#library(partykit)

party.fit <- ctree(parks_percent_change_from_baseline ~ as.numeric(date), data=test, control=ctree_control(maxdepth=1))

#party.plot <- plot(party.fit, main="Conditional Inference Tree for parks")
#print(party.fit)

test$parks.party.predict <- predict(party.fit)
ggplot(data=test, aes(date, parks_percent_change_from_baseline)) +
  geom_point() +
  geom_line(aes(y=parks.party.predict))

split_date <- as.Date(party.fit[2]$data[2][nrow(party.fit[2]$data[2]),1], start="1970-01-01")
step_1 <- mean(party.fit[2]$data[1][,1])
step_2 <- mean(party.fit[3]$data[1][,1])

