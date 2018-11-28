my_qqplot <- function(m){
  require(envelope)
  resid <- sort(residuals(m,type="pearson"))
  resid <- resid[!is.na(resid[])]
  envl <- envl.plot(resid,conf=95, plot.it=F)
  theor <- sort(envl$quantiles)
  low.band <- envl$low.band
  high.band <- envl$high.band
  obs <- envl$obs
  qq.data <- data.frame(obs,theor,low.band,high.band)
  ggplot(data=qq.data, aes(x=sort(theor), y=sort(obs))) +
    geom_point(size=2)+
    geom_ribbon(data=qq.data, aes(ymin=low.band, ymax=high.band), alpha=0.2)+
    geom_line(aes(x=theor,y=low.band), linetype=2)+
    geom_line(aes(x=theor,y=high.band), linetype=2)+
    stat_smooth(method = "lm", se = F, color = "black", size=0.5)+
    scale_x_continuous(name="Theoretical quantiles")+
    scale_y_continuous(name="Studentized residuals")+
    my_ggplot_theme()
}