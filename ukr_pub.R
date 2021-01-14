setwd("~")

library(foreign)
library(readxl)
library(xtable)
library(texreg)
library(bayesplot)
library(MASS)
library(data.table)
library(dplyr)
library(ggplot2)
library(arm) 
library(rstan)
library(rstanarm)


###Russian and Ukrainian language proficiency

## Figure 5

lang<-read.csv("oblast_attribute_table.csv", header=T, sep=",")
names(lang)
myvars<-c("NAME_LAT","russian_lang_pct","ukrainian_lang_pct")
lang2<-lang[myvars]

lang2$NAME_LAT<-factor(lang2$NAME_LAT, levels=lang2$NAME_LAT[order(lang2$ukrainian_lang_pct)])

lang3<-melt(lang2, id.vars = "NAME_LAT")


p <- ggplot(lang3, aes(NAME_LAT, value, shape = factor(variable)))
p + geom_point(aes(color=factor(variable), size = 5))+
  ylim(0,100)+
  labs(title = "", 
       x="", 
       y="Population Speaking Mother Tongue, %")+
  guides(size=FALSE)+
  scale_color_discrete(name="Mother Tongue",
                       breaks=c("russian_lang_pct", "ukrainian_lang_pct"),
                       labels=c("Russian", "Ukrainian"))+
  scale_shape_discrete(name="Mother Tongue",
                       breaks=c("russian_lang_pct", "ukrainian_lang_pct"),
                       labels=c("Russian", "Ukrainian"))+
  theme(axis.text.x = element_text(colour="grey20",size=10,angle=90,hjust=1,vjust=0,face="plain"))

ggsave("lang.pdf", width = 7, height = 7)
dev.off()




###negative binomial model fitting using stan 
###

m<-read.csv("replication_ukraine.csv",header=T,sep=",")
names(m)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

##
## Run the main analysis
##

b1<-stan_glm.nb(lost_no_air_all~Yanukovich_ratio+distance_100+density_100, 
                 data=m, weights = NULL, 
                 na.action = NULL, offset = NULL,
                 model = TRUE, x = FALSE, y = TRUE, contrasts = NULL, link = "log",
                 prior = laplace(location = 0, scale = NULL, autoscale = TRUE), 
                 prior_intercept = normal(), chains = 10, iter = 10000,
                 prior_aux = exponential(), prior_PD = FALSE, algorithm = c("sampling"), 
                 adapt_delta = NULL, QR = T)
b1


b2<-stan_glm.nb(lost_no_air_all~Yanukovich_ratio+distance_100+density_100+
                Russian_ratio+
                population_100+turnout+salary_100, data=m, weights = NULL, 
                 na.action = NULL, offset = NULL,
                 model = TRUE, x = FALSE, y = TRUE, contrasts = NULL, link = "log",
                 prior = laplace(location = 0, scale = NULL, autoscale = TRUE), 
                 prior_intercept = normal(), chains = 10, iter = 10000,
                 prior_aux = exponential(), prior_PD = FALSE, algorithm = c("sampling"), 
                 adapt_delta = NULL, QR = T)
b2


b3<-stan_glm.nb(lost_no_air_all~Yanukovich_ratio+distance_100+density_100+
                  Russian_ratio+Russian_ratio2+
                  population_100+turnout+salary_100, data=m, weights = NULL, 
                  na.action = NULL, offset = NULL,
                  model = TRUE, x = FALSE, y = TRUE, contrasts = NULL, link = "log",
                  prior = laplace(location = 0, scale = NULL, autoscale = TRUE), 
                  prior_intercept = normal(), chains = 10, iter = 10000,
                  prior_aux = exponential(), prior_PD = FALSE, algorithm = c("sampling"), 
                  adapt_delta = NULL, QR = T)
b3

##
## Create summary plots and tables
##

## Figure 7a

print(names(b1$stanfit))
names(b1$stanfit)[1] <- "Intercept"
names(b1$stanfit)[2] <- "Yanukovych"
names(b1$stanfit)[3] <- "Distance"
names(b1$stanfit)[4] <- "Density"

p1<-plot(b1, plotfun = "areas", prob = 0.95, # 
         pars = c("Yanukovych", "Distance","Density"))
p1+theme(axis.text.y =element_text(size=20),
         axis.text.x =element_text(size=20))
ggsave(file="density_plot_hypotheses.pdf",width=7,height=7)
dev.off()

## Table 1A in the Appendix

b1ci <- cbind(b1$coefficients, posterior_interval(b1, prob = 0.95, 
                                                   pars = c("Intercept", "Yanukovych", 
                                                            "Distance","Density")))
xtable(round(b1ci, 2)) # switch places for mean column and 2.5% column

## Figure 7b

print(names(b2$stanfit))
names(b2$stanfit)[1] <- "Intercept"
names(b2$stanfit)[2] <- "Yanukovych"
names(b2$stanfit)[3] <- "Distance"
names(b2$stanfit)[4] <- "Density"
names(b2$stanfit)[5] <- "Russian"
names(b2$stanfit)[6] <- "Population"
names(b2$stanfit)[7] <- "Activism"
names(b2$stanfit)[8] <- "Income"



p2<-plot(b2, plotfun = "areas", prob = 0.95, # 
         pars = c("Yanukovych", "Distance","Density","Russian",
                  "Population","Activism","Income"))
p2+theme(axis.text.y =element_text(size=20),
         axis.text.x =element_text(size=20))
ggsave(file="density_plot_full.pdf",width=7,height=7)
dev.off()

## Table 2A in the Appendix

b2ci <- cbind(b2$coefficients, posterior_interval(b2, prob = 0.95, 
                                                   pars = c("Intercept", "Yanukovych", "Distance","Density",
                                                            "Russian","Population","Activism","Income")))
xtable(round(b2ci, 2)) # switch places for mean column and 2.5% column


## Figure 7c

print(names(b3$stanfit))
names(b3$stanfit)[1] <- "Intercept"
names(b3$stanfit)[2] <- "Yanukovych"
names(b3$stanfit)[3] <- "Distance"
names(b3$stanfit)[4] <- "Density"
names(b3$stanfit)[5] <- "Russian"
names(b3$stanfit)[6] <- "Russian sq."
names(b3$stanfit)[7] <- "Population"
names(b3$stanfit)[8] <- "Activism"
names(b3$stanfit)[9] <- "Income"

p3<-plot(b3, plotfun = "areas", prob = 0.95, 
         pars = c("Yanukovych", "Distance","Density","Russian", "Russian sq.",
                  "Population","Activism","Income"))
p3+theme(axis.text.y =element_text(size=20),
         axis.text.x =element_text(size=20))
ggsave(file="density_plot_full_w_sq_Rus_lang.pdf",width=7,height=7)
dev.off()

## Table 3A in the Appendix

b3ci <- cbind(b3$coefficients, posterior_interval(b3, prob = 0.95, 
                                                     pars = c("Intercept", "Yanukovych", "Distance","Density",
                                                              "Russian", "Russian sq.","Population","Activism",
                                                              "Income")))
xtable(round(b3ci, 2)) # switch places for mean column and 2.5% column



## Alternative measurement of ethnicity - ethnic self-identification, in Appendix

##
## run the regressions
##

b4<-stan_glm.nb(lost_no_air_all~Yanukovich_ratio+distance_100+density_100+
                  ruspop+
                  population_100+turnout+salary_100, data=m, weights = NULL, 
                na.action = NULL, offset = NULL,
                model = TRUE, x = FALSE, y = TRUE, contrasts = NULL, link = "log",
                prior = laplace(location = 0, scale = NULL, autoscale = TRUE), 
                prior_intercept = normal(), chains = 10, iter = 10000,
                prior_aux = exponential(), prior_PD = FALSE, algorithm = c("sampling"), 
                adapt_delta = NULL, QR = T)
b4


b5<-stan_glm.nb(lost_no_air_all~Yanukovich_ratio+distance_100+density_100+
                  ruspop+ruspop2+
                  population_100+turnout+salary_100, data=m, weights = NULL, 
                na.action = NULL, offset = NULL,
                model = TRUE, x = FALSE, y = TRUE, contrasts = NULL, link = "log",
                prior = laplace(location = 0, scale = NULL, autoscale = TRUE), 
                prior_intercept = normal(), chains = 10, iter = 10000,
                prior_aux = exponential(), prior_PD = FALSE, algorithm = c("sampling"), 
                adapt_delta = NULL, QR = T)
b5

## Table 4A in the Appendix

print(names(b4$stanfit))
names(b4$stanfit)[1] <- "Intercept"
names(b4$stanfit)[2] <- "Yanukovych"
names(b4$stanfit)[3] <- "Distance"
names(b4$stanfit)[4] <- "Density"
names(b4$stanfit)[5] <- "Russian"
names(b4$stanfit)[6] <- "Population"
names(b4$stanfit)[7] <- "Activism"
names(b4$stanfit)[8] <- "Income"

b4ci <- cbind(b4$coefficients, posterior_interval(b4, prob = 0.95, 
                                                  pars = c("Intercept", "Yanukovych", "Distance","Density",
                                                           "Russian","Population","Activism","Income")))
xtable(round(b4ci, 2)) # switch places for mean column and 2.5% column

## Table 5A in the Appendix

print(names(b5$stanfit))
names(b5$stanfit)[1] <- "Intercept"
names(b5$stanfit)[2] <- "Yanukovych"
names(b5$stanfit)[3] <- "Distance"
names(b5$stanfit)[4] <- "Density"
names(b5$stanfit)[5] <- "Russian"
names(b5$stanfit)[6] <- "Russian sq."
names(b5$stanfit)[7] <- "Population"
names(b5$stanfit)[8] <- "Activism"
names(b5$stanfit)[9] <- "Income"



b5ci <- cbind(b5$coefficients, posterior_interval(b5, prob = 0.95, 
                                                  pars = c("Intercept", "Yanukovych", "Distance","Density",
                                                           "Russian", "Russian sq.","Population","Activism",
                                                           "Income")))
xtable(round(b5ci, 2)) # switch places for mean column and 2.5% column






### Marginal effects, based on the full model: b2

## rerun model

b2<-stan_glm.nb(lost_no_air_all~Yanukovich_ratio+distance_100+density_100+
                  Russian_ratio+
                  population_100+turnout+salary_100, data=m, weights = NULL, 
                na.action = NULL, offset = NULL,
                model = TRUE, x = FALSE, y = TRUE, contrasts = NULL, link = "log",
                prior = laplace(location = 0, scale = NULL, autoscale = TRUE), 
                prior_intercept = normal(), chains = 10, iter = 10000,
                prior_aux = exponential(), prior_PD = FALSE, algorithm = c("sampling"), 
                adapt_delta = NULL, QR = T)
b2


## Figure 8a

datyan <-with(m, data.frame( Yanukovich_ratio=rep(seq(from = min(m$Yanukovich_ratio, na.rm=T), 
                                                   to=max(m$Yanukovich_ratio,na.rm=T),
                                                   length.out=1000)),
                          distance_100=mean(distance_100,na.rm=T),
                          density_100=mean(density_100,na.rm=T),
                          Russian_ratio=mean(Russian_ratio,na.rm=T),
                          Russian_ratio2=mean(Russian_ratio2,na.rm=T),
                          population_100=mean(population_100,na.rm=T),
                          turnout=mean(turnout,na.rm=T),
                          salary_100=mean(salary_100,na.rm=T)))

ndatyan <- cbind(datyan, predict(b2, newdata=datyan, type = "link", se.fit=T)) 
newdatyan <- melt(ndatyan, id.vars = c("Yanukovich_ratio","fit","se.fit"))
newdatyan2 <- newdatyan[1:1000,]

newdatyan2 <- within(newdatyan2, {
  vio <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

yan<-ggplot(newdatyan2, aes(x = Yanukovich_ratio, y = vio)) +
  geom_smooth(aes(ymin = LL, ymax = UL),alpha=0.5, stat="identity")+
  labs(title = "", x="Vote for Yanukovych (%)", 
       y="Intensity of Violence")+
  scale_x_continuous(breaks = c(0.65, 0.70, 0.75, 0.80),
                     labels=c("65","70","75","80"))+
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'black'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.text.y =element_text(size=20),
        axis.text.x =element_text(size=20),
        axis.title=element_text(size=20))
yan  
ggsave(file="me_yanukovych_stan.pdf",width=7,height=7)
dev.off()


## Figure 8b

datden <-with(m, data.frame( density_100=rep(seq(from = min(m$density_100, na.rm=T), 
                                              to=max(m$density_100,na.rm=T),
                                              length.out=1000)),
                          Yanukovich_ratio=mean(Yanukovich_ratio,na.rm=T),
                          distance_100=mean(distance_100,na.rm=T),
                          Russian_ratio=mean(Russian_ratio,na.rm=T),
                          Russian_ratio2=mean(Russian_ratio2,na.rm=T),
                          population_100=mean(population_100,na.rm=T),
                          turnout=mean(turnout,na.rm=T),
                          salary_100=mean(salary_100,na.rm=T)))

ndatden <- cbind(datden, predict(b2, newdata=datden, type = "link", se.fit=T)) 
newdatden <- melt(ndatden, id.vars = c("density_100","fit","se.fit"))
newdatden2 <- newdatden[1:1000,]

newdatden2 <- within(newdatden2, {
  vio <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

den<-ggplot(newdatden2, aes(x = density_100, y = vio),method=glm) +
  geom_smooth(aes(ymin = LL, ymax = UL),alpha=0.5, stat="identity")+
  labs(title = "", 
       x="Density of Population (persons/sq.km)", 
       y="Intensity of Violence")+
  theme(legend.position="bottom")+
  scale_x_continuous(breaks=c(0, 0.2, 0.4, 0.6),
                     labels=c("0", "2000", "4000", "6000"))+
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'black'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.text.y =element_text(size=20),
        axis.text.x =element_text(size=20),
        axis.title=element_text(size=20))

den
ggsave(file="me_density_stan.pdf",width=7,height=7)
dev.off()



## Figure 8c

datdis <-with(m, data.frame( distance_100=rep(seq(from = min(m$distance_100, na.rm=T), 
                                               to=max(m$distance_100,na.rm=T),
                                               length.out=1000)),
                          Yanukovich_ratio=mean(Yanukovich_ratio,na.rm=T),
                          density_100=mean(density_100,na.rm=T),
                          Russian_ratio=mean(Russian_ratio,na.rm=T),
                          Russian_ratio2=mean(Russian_ratio2,na.rm=T),
                          population_100=mean(population_100,na.rm=T),
                          turnout=mean(turnout,na.rm=T),
                          salary_100=mean(salary_100,na.rm=T)))
ndatdis <- cbind(datdis, predict(b2, newdata=dat, type = "link", se.fit=T)) 
newdatdis <- melt(ndatdis, id.vars = c("distance_100","fit","se.fit"))
newdatdis2 <- newdatdis[1:1000,]

newdatdis2 <- within(newdatdis2, {
  vio <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})
dis<-ggplot(newdatdis2, aes(x = distance_100, y = vio)) +
  geom_smooth(aes(ymin = LL, ymax = UL), alpha=0.5,stat="identity")+
  labs(title = "", x="Distance (km)", 
       y="Intensity of Violence")+
  theme(legend.position="bottom")+
  scale_x_continuous(breaks=c(0.4,0.5,0.6,0.7,0.8), 
                     labels=c("400", "500", "600", "700", "800"))+
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'black'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.text.y =element_text(size=20),
        axis.text.x =element_text(size=20),
        axis.title=element_text(size=20))
dis
ggsave(file="me_distance_stan.pdf",width=7,height=7)
dev.off()




## Appendix

## Figure 1A in the Appendix

t<-read_xlsx("/Volumes/GoogleDrive/My Drive/ukraine/replication/time.xlsx", sheet = 1, 
             range = "A1:G1627", col_names = TRUE,
             col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
             guess_max = min(1000))
t$event<-1
t2<-subset(t, type2=="lost_arm" | type2=="lost_art" | type2=="lost_roc")
t2$date2<-as.Date(t2$date, format="%Y-%m-%d")

###note that with dates you should use special procedure in ifelse statements
t2$date4 <- as.Date( with(t2, ifelse(is.na(date2), date3, date2)), origin="1970-01-01")
t2$month<- strftime(t2$date2, "%Y/%m")
t2$year<- strftime(t2$date2, "%Y")
vars<-c("event","month","year")
t3<-t2[vars]
t4 <- setDT(t3)[, lapply(.SD, sum,na.rm=T), by=.(month,year),.SDcols = c("event")] 

g <- ggplot(t4, aes(as.factor(month),event,fill=as.factor(year)))

g + geom_bar(stat = "identity")+
  scale_fill_discrete(name="Year")+
  theme(axis.text.x = element_text(colour="grey20",size=11,angle=90,hjust=1,vjust=0.5,face="plain"))+
  ylab("Destroyed Heavy Military Equipment Units") +
  xlab("")
ggsave("timed_violence.pdf",width=7,height=7)
dev.off()

## Table 6A in the Appendix

nb1<-glm.nb(lost_no_air_all~Russian_ratio+population_100+turnout+salary_100,
            data=m)
summary(nb1)

nb2<-glm.nb(lost_no_air_all~Yanukovich_ratio+distance_100+density_100,
            data=m )
summary(nb2)

nb3<-glm.nb(lost_no_air_all~Yanukovich_ratio+distance_100+density_100+Russian_ratio+
              population_100+turnout+salary_100,
            data=m )
summary(nb3)

nb4<-glm.nb(lost_no_air_all~Yanukovich_ratio+distance_100+density_100+Russian_ratio+
              Russian_ratio2+
              population_100+turnout+salary_100,
            data=m )
summary(nb4)

texreg(list(nb1,nb2,nb3,nb4) ,booktabs = TRUE, dcolumn = TRUE)
