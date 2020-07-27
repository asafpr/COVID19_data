library(tidyverse)
library(RColorBrewer)
library(UpSetR)
library(cowplot)
pncol <- RColorBrewer::brewer.pal(3, "Set1")
rescol <- RColorBrewer::brewer.pal(3, "Set2")
names(rescol) <- c("Abroad", "Contact with confirmed", "Other")
names(pncol) <- c("Positive", "Negative", "Other")
allt <- read_csv("corona_tested_individuals_ver_0048.csv")
allt$gender <- recode_factor(allt$gender, "נקבה" = "F", "זכר" = "M")
allt$age_60_and_above <- recode_factor(allt$age_60_and_above, "Yes" = TRUE, "No" = FALSE, .default = NA)
allt$corona_result <- recode_factor(allt$corona_result, "חיובי" = "Positive", "שלילי" = "Negative", "אחר" = "Other")
allt <- allt %>% filter(corona_result != "Other")

# Group by test_date and plot the % positive and # of tests
daily <- allt %>% 
  group_by(test_date, test_indication) %>% summarise(tests = n(), positive = sum(corona_result=="Positive")) %>% ungroup()
prec <-
  ggplot(daily, aes(test_date, positive / tests, fill = test_indication)) + 
  geom_col(width = 1) + 
  facet_grid( ~ test_indication) + theme_bw() + ylab("% positive") + scale_fill_manual(values=rescol) + 
  theme(legend.position = "none") + scale_x_date(limits = as.Date(c("2020-06-01", "2020-07-23")))
pnt <- ggplot(daily, aes(test_date, tests, fill = test_indication)) + 
  geom_col(width = 1) + scale_y_log10() + facet_grid( ~ test_indication) + theme_bw() + 
  scale_fill_manual(values = rescol) + theme(legend.position = "none") + 
  scale_x_date(limits = as.Date(c("2020-06-01", "2020-07-23")))
plot_grid(prec, pnt, nrow = 2, ncol = 1)
