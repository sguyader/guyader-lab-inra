library(SimInf)
library(tidyverse)

Rt = 3.2
incub = 5.2
recov = 10
reps = 100

SEIR.mod <- SEIR(u0 = data.frame(S=rep(480000, reps), E=rep(0, reps), I=rep(1, reps), R=rep(0, reps)),
              tspan = 1:40,
              beta = 0.9,
              epsilon = 1/incub,
              gamma = 1/recov)

result.SEIR <- run(SEIR.mod)
plot(result.SEIR, compartments=c("S","E","I","R"))

SIR.mean <- trajectory(result.SIR, compartments=c("I","R")) %>% dplyr::group_by(time) %>% dplyr::summarize_at(vars(I:R), mean)

#SEIR.mean %>% mutate(E+I)

ggplot(data=SEIR.mean, aes(x=time)) +
  geom_line(aes(y=I, color="I")) +
  geom_line(aes(y=R, color="R"), lty=3) +
  geom_point(data=covid_gpe, aes(y=cas_averes)) +
  #scale_y_continuous(trans='log2') +
  xlim(0,100)


SIR.alt  <- mparse(transitions = c("S -> alpha*S*I/((S+I+R)) -> I",
                                    "I -> beta*I -> R",
                                    "I -> gamma*I -> D"),
                 compartments = c("S", "I", "R", "D"),
                 gdata = c(alpha=1/2, beta=1/8, gamma=0.8/100),
                 u0 = data.frame(S=rep(800, reps), I=rep(1, reps), R=rep(0, reps), D=rep(0, reps)),
                 tspan = 1:120)

result.SIR <- run(SIR.alt)
plot(result.SIR, compartments=c("S","I","R","D"))

SIR.mean <- trajectory(result.SIR, compartments=c("S","I","R","D")) %>% dplyr::group_by(time) %>% dplyr::summarize_at(vars(S:D), mean)

ggplot(data=SIR.mean, aes(x=time)) +
  geom_line(aes(y=I, color="I")) +
  geom_line(aes(y=S, color="S"), lty=2) +
  geom_line(aes(y=R, color="R"), lty=3) +
  geom_line(aes(y=D, color="D"), lty=3) +
  geom_point(data=covid_gpe, aes(x=time, y=malades), col="green") +
  geom_line(data=covid_gpe, aes(x=time, y=exppred)) +
  geom_point(data=covid_gpe, aes(x=time, y=deces_cum), col="red") +
  geom_point(data=covid_gpe, aes(x=time, y=gueris_cum), col="blue") +
  #scale_y_log10()
  ylim(0,1000)

