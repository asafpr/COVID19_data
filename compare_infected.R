library(tidyverse)
# Download the data from a github repository that downloads the dashboard data daily
dash <- read_csv(url("https://raw.githubusercontent.com/dancarmoz/israel_moh_covid_dashboard_data/master/hospitalized_and_infected.csv"))
# Read the tested individuals table (too heavy to put in the repo, should download first from: https://data.gov.il/dataset/covid-19/resource/d337959a-020a-4ed3-84f7-fca182292308 )
allt <- read_csv("corona_tested_individuals_ver_0060.csv")
allt$gender <- recode_factor(allt$gender, "נקבה" = "F", "זכר" = "M")
allt$age_60_and_above <- recode_factor(allt$age_60_and_above, "Yes" = TRUE, "No" = FALSE, .default = NA)
allt$corona_result <- recode_factor(allt$corona_result, "חיובי" = "Positive", "שלילי" = "Negative", "אחר" = "Other")
allt <- allt %>% filter(corona_result != "Other")

# Sum the number of new infected daily
daily <- allt %>% 
  group_by(test_date, test_indication) %>% summarise(tests = n(), positive = sum(corona_result=="Positive"), head_ache = sum(head_ache), fever = sum(fever), cough = sum(cough), sore_throat = sum(sore_throat), shortness_of_breath = sum(shortness_of_breath)) %>% ungroup()
alld <- group_by(daily, test_date) %>% summarize(positive = sum(positive)) %>% ungroup()
# Combine the two
dcomb <- left_join(alld, dash[, c("Date", "New infected")], by = c("test_date" = "Date")) %>% select(test_date, database = positive, dashboard = `New infected`) %>% pivot_longer(cols = -test_date, names_to = "Source", values_to = "infected")
# Plot the two against each other
ggplot(dcomb, aes(test_date, infected, color = Source, alpha = Source)) + geom_point() + scale_color_manual(values = c("blue", "red"), labels = c(paste0("dashboard, total = ", sum(dcomb$infected[dcomb$Source == "dashboard"])), paste0("database, total = ", sum(dcomb$infected[dcomb$Source == "database"])))) + scale_alpha_manual(values = c(1, 0.5), guide = 'none')  
