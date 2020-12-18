library(strucchange)
library(tsoutliers)


for(i in c("AW","AG","BS","BM","BB","CU","CW","KY","DM","DO","FR","GD","HT","JM","KN","LC","TC","TT","VC","VG")) {
  tryCatch({ 
    dat <- filter(covid_mobility, country_region_code == "FR")
    dat.ts <- NA
    dat.ts <- ts(dat$transit_stations)
    AUC_value <- DescTools::AUC(as.numeric(dat$date), abs(dat$transit_stations))
    plot(dat.ts)
    data.ts.outliers <- NA
    data.ts.outliers <- tso(dat.ts, types=c("LS"), cval=3)
    data.ts.outliers
    jpeg(paste("mobility_plots/dyn_mobility_",i,"_transit_stations.jpg"))
    plot(data.ts.outliers, main=paste(i, "- transit_stations"))
    dev.off()
  }, error=function(e){cat("ERROR :",i, "\n")})
}

summary(aov(confirmed_p_million ~ pop_density, data=caribean))
