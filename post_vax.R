library(tidyverse)
pres <- data.frame(age_group = c("16-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"),
                   week_1_tests = c(1315, 6474, 10353, 14507, 15247, 16270, 9323, 5955, 3123),
                   week_1_positive_rate = c(6.4, 6.1, 4.2, 3.8, 4.6, 5.1, 4.9, 4.5, 3.3),
                   week_1_hospitalized = c(0, 2, 3, 5, 20, 50, 64, 68, 32),
                   week_2_tests = c(283, 2000, 3781, 5071, 5133, 8453, 4947, 3498, 1857),
                   week_2_positive_rate = c(8.1, 6.7, 6.2, 5.4, 6.3, 7.9, 8, 4.3, 2.7),
                   week_2_hospitalized = c(0, 1, 1, 5, 6, 28, 40, 29, 14),
                   week_3_tests = c(17, 207, 673, 810, 729, 1201, 669, 165, 42),
                   week_3_positive_rate = c(5.9, 2.9, 2.2, 2.5, 3.3, 5.2, 4.9, 4.8, 9.5),
                   week_3_hospitalized = c(0, 1, 0, 0, 0, 2, 2, 1, 1),
                   age = c(17.5, 25, 35, 45, 55, 65, 75, 85, 95))
pres$week_1_positive <- round(pres$week_1_tests * pres$week_1_positive_rate / 100)
pres$week_2_positive <- round(pres$week_2_tests * pres$week_2_positive_rate / 100)
pres$week_3_positive <- round(pres$week_3_tests * pres$week_3_positive_rate / 100)
pres$`Week 1 hosp ratio` <- pres$week_1_hospitalized / pres$week_1_positive
pres$`Week 2 hosp ratio` <- pres$week_2_hospitalized / pres$week_2_positive
pres$`Week 3 hosp ratio` <- pres$week_3_hospitalized / pres$week_3_positive


pl <- pivot_longer(select(pres, age, ends_with("positive"), ends_with("zed")), cols = -age, names_to = "category", values_to = "number")
pl$week <- sapply(strsplit(pl$category, "_"), `[[`, 2)
pl$measure <- sapply(strsplit(pl$category, "_"), `[[`, 3)
pml <- pivot_wider(pl, id_cols = c(age, week), names_from = "measure", values_from = "number")
lmo <- lm(hospitalized ~ age:positive + week:positive , pml)
summary(lmo)
pml$age <- as.factor(pml$age)
pml %>% ggplot(aes(positive, hospitalized)) + geom_point() +geom_smooth(method="lm") + facet_wrap(~age, scales = "free")
