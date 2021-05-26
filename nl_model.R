library(abind)
library(tidyr)
library(splines)
library(lme4)
library(pscl)
library(rjags)
library(ggpubr)

remove(list = ls())

### Data Loading

setwd("~/Github/covid_wildfire")
source("src/Utilities.R")
source("src/bayes/model.R")
source("src/bayes/bayes_fun.R")

dff <- load.data()
dff$FIPS <- as.numeric(as.character(dff$FIPS))
dff$pm_counter <- dff$pm25
dff$pm_counter[dff$pm_wildfire != 0] <- dff$pm25_history[dff$pm_wildfire != 0]
dff$FIPS <- as.numeric(as.character(dff$FIPS))

### Optimal DF

causes <- c("cases", "deaths")
idx.list <- list(1:8, 1:15, 1:22, 1:29)

### execute modelling 
result <- c()
for (idx in idx.list) {
  for (cause in causes) {
      
    glmer_result <- pm_avg_model(dff, df.date = 6, df.tmmx = 2, df.rmax = 2, cause = cause, idx = idx)
    out <- data.frame(glmer_result, cause = cause)
    result <- rbind(result, out)
    
  }
}

result = data.frame(result)
write.csv(result, file = "~/Dropbox/Projects/Wildfires/Output/bayes/nl_test.csv", row.names = FALSE)

### Sensitivity of DF

glmer_result <- read.csv("~/Dropbox/Projects/Wildfires/Output/bayes/nl_test.csv")
glmer_result$cause <- stringr::str_to_title(glmer_result$cause)
glmer_result$sma <- paste0("0-", glmer_result$sma," days")
glmer_result$sma <- factor(glmer_result$sma, levels = c("0-7 days", "0-14 days", "0-21 days", "0-28 days"))

nl_cases <- ggplot(aes(x = pm, y = rr, group = sma, colour = sma), 
                    data = subset(glmer_result, cause == "Cases")) +
  geom_line(size = 1) +
  geom_line(aes(y = lo), size = 1, linetype=3) +
  geom_line(aes(y = hi), size = 1, linetype=3) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylim(-1, 1) +
  theme_bw() +
  labs(x = "PM2.5", y = "log(Risk Ratio)", color = "Simple Moving Averages:")

nl_deaths <- ggplot(aes(x = pm, y = rr, group = sma, colour = sma), 
                   data = subset(glmer_result, cause == "Deaths")) +
  geom_line(size = 1) +
  geom_line(aes(y = lo), size = 1, linetype=3) +
  geom_line(aes(y = hi), size = 1, linetype=3) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylim(-1, 1) +
  theme_bw() +
  labs(x = "PM2.5", y = "log(Risk Ratio)", color = "Simple Moving Averages:")

plot.list = list()
plot.list[[1]] = nl_cases
plot.list[[2]] = nl_deaths
pdf("~/Dropbox/Projects/Wildfires/Output/bayes/nl_test.pdf", width = 10, height = 10)
do.call('ggarrange', c(plot.list, ncol = 2, common.legend = TRUE, legend="bottom"))
dev.off()
