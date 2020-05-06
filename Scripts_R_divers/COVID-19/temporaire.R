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

response_table <- tibble(CountryCode=unique(covid_response$CountryCode), CountryName=unique(covid_response$CountryName), AUC=NA, date_SI_sup0=NA, date_SI_sup50=NA, date_SI_max=NA, SI_max=NA)

for(i in unique(covid_response$CountryName)) {
  tryCatch({ 
    dat <- filter(covid_response, CountryName == i, Date <= "2020-04-25") # filtrer par région/pays
    # AUC et SI
    AUC_value <- DescTools::AUC(as.numeric(dat$Date), dat$StringencyIndex, method="linear")
    date_SI_sup0 <- dat$Date[which(dat$StringencyIndex > 0)][1]
    date_SI_sup50 <- dat$Date[which(dat$StringencyIndex > 50)][1]
    date_SI_max <- dat$Date[which(dat$StringencyIndex == max(dat$StringencyIndex, na.rm=T))][1]
    SI_max <- max(dat$StringencyIndex, na.rm=T)
    response_table$AUC[response_table$CountryName == i] <- AUC_value
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


ggplot(covid_response, aes(x=Date, y=StringencyIndex)) +
  geom_line(aes(group=CountryCode), alpha=0.3)

### Covid mobility
library(lubridate)

download.file("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=e0c5a582159f5662", destfile = "data/covid_mobility.csv")

covid_mobility <- read_csv("data/covid_mobility.csv")

#(6:11)
ggplot(group_by(covid_mobility, country_region_code)) +
  geom_line(aes(x=date, y=retail_and_recreation_percent_change_from_baseline, group=country_region_code, color=country_region_code))
