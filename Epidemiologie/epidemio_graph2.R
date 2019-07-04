ggplot(sever.mean, aes(x=as.numeric(levels(date))[date], y=severity, colour=treatment)) +
    facet_grid(year ~ .) +
    geom_smooth(aes(group=treatment), method = "nls",
                                  formula = y~100/(1+exp((xmid-x)/ scal)), 
                                  method.args=list(start=c(xmid=180, scal=10)),
                                  se = FALSE, linetype = 1) + +
    geom_errorbar(aes(ymin=severity-ci.student, ymax=severity+ci.student), width=.1) +
    geom_point() +
    # theme_bw() + # use the custom theme "my_ggplot2_theme"
  my_cowplot_theme() +
  theme(legend.position = "right")

data.long.2 <- data.long
data.long.2$id <- with(data.long.2, interaction(x, y, drop=TRUE))

data.long.2 %>% group_by(year, treatment, date) %>% na.omit() %>% filter(treatment=="plat" & year==2011 & x>2 & x<8 & y>2 & y<8) %>%
  ggplot(aes(x=as.numeric(levels(date))[date], y=sever)) +
  facet_wrap(~id) +
  geom_smooth(method = "nls",
              formula = y~100/(1+exp((xmid-x)/ scal)), 
              method.args=list(start=c(xmid=150, scal=10)),
              se = FALSE, linetype = 1) +
  geom_line(aes(group=id)) +
  # theme_bw() + # use the custom theme "my_ggplot2_theme"
  my_cowplot_theme() +
  theme(legend.position = "right")