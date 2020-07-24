library(tidyverse)
library(lubridate)
hosp <- read_csv("corona_hospitalization_ver_0027.csv", na = c("", "NA", "<15", "NULL"))
colnames(hosp) <- c("date", "hospitalized", "F_perc_hosp", "mean_age_hosp", "std_age_hosp", "ventilators", "F_perc_vent", "mean_age_vent", "std_age_vent", "mild_cases", "F_perc_mild", "mean_age_mild", "std_age_mild", "moderate_cases", "F_perc_moderate", "mean_age_moderate", "std_age_moderate", "severe_cases", "F_perc_severe", "mean_age_severe", "std_age_severe")
hosp$mod_sev <- hosp$moderate_cases + hosp$severe_cases
hos_rate <- (hosp$mod_sev[2:nrow(hosp)] / hosp$mod_sev[1:(nrow(hosp)-1)] - 1 ) * 100
hosp$change_rate <- c(NA, hos_rate)
ggplot(hosp, aes(date, mod_sev)) + geom_point() + geom_smooth(method = "loess", formula = "y ~ x", span=0.3) + theme_bw()
ggplot(hosp, aes(date, change_rate)) + geom_point() + geom_smooth(method = "loess", formula = "y ~ x") + theme_bw()
hosp$week <- week(hosp$date)
weekly <- group_by(hosp, week) %>% summarise_all(mean) %>% ungroup()
weekly <- weekly %>% filter(week<max(week)) # remove the last week which is incomplete
weekly$change_rate <- c(NA, (weekly$mod_sev[2:nrow(weekly)] / weekly$mod_sev[1:(nrow(weekly)-1)] - 1 ) * 100)
ggplot(weekly, aes(date, mod_sev))+ geom_point() + geom_smooth(method = "loess", formula = "y ~ x", span=0.3) + theme_bw()
ggplot(weekly, aes(date, change_rate)) + geom_point() + geom_smooth(method = "loess", formula = "y ~ x") + theme_bw()
ggplot(hosp, aes(date, mod_sev)) + geom_point() + geom_smooth(method = "loess", formula = "y ~ x", span=0.3) + theme_bw() + scale_y_log10() + scale_x_date(limits = as.Date(c("2020-06-15","2020-07-19")))
hosp$dbl_time <- 100*log(2)/hosp$change_rate
ggplot(hosp, aes(date, dbl_time)) + geom_point() + scale_x_date(limits = as.Date(c("2020-06-15","2020-07-19"))) +  theme_bw() + ylim(0,50)
