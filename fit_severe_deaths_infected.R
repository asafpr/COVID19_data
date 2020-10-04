library(tidyverse)
library(RColorBrewer)
colors <- brewer.pal(3, "Dark2")
ns <-
  read_csv(
    url(
      "https://raw.githubusercontent.com/asafpr/COVID19_data/master/new_severe_cases.csv"
    )
  )
dash <-
  read_csv(
    url(
      "https://raw.githubusercontent.com/dancarmoz/israel_moh_covid_dashboard_data/master/hospitalized_and_infected.csv"
    )
  )

ns <- ns %>% filter(!is.na(severe_new))

# Join the new severe with the dashboard data
ns2 <-
  left_join(ns,
            select(
              dash,
              Date,
              `New infected`,
              `Tests for idenitifaction`,
              `New deaths`
            ),
            by = "Date") %>% mutate(
              infected = `New infected`,
              tests = `Tests for idenitifaction`,
              day = weekdays(Date),
              deaths = `New deaths`
            ) %>% select(Date, day, severe_new, infected, tests, deaths)

# Run loess on infected and tests to smooth the data
span = 0.2
ns2$idx <- 1:nrow(ns2)
infl <- loess(infected ~ idx, ns2, span = span)
tesl <- loess(tests ~ idx, ns2, span = span)
ns2 <-
  ns2 %>% mutate(infected_sm = infl$fitted, tests_sm = tesl$fitted)

# Fit a linear model using new infected to predict severe and death
for (i in 1:21) {
  # Generate a lag in severe and deaths
  nt <-
    ns2 %>% mutate(severe_new = c(severe_new[-1:-i], rep(NA, i)),
                   deaths = c(deaths[-1:-i], rep(NA, i)))
  # lm for severe. intercept is forced to be 0
  lmout <- lm(severe_new ~ 0 + infected_sm, nt)
  print(i)
  print(summary(lmout))
  # Plot the lagged severe vs infected
  print(ggplot(nt, aes(severe_new, infected_sm)) + geom_point())
  # Main plot - plot the
  print(
    ggplot(nt, aes(Date, severe_new)) + geom_line(aes(color = colors[2])) + geom_line(aes(
      y = infected_sm * lmout$coefficients[1], color = colors[1]
    )) + scale_y_continuous(
      "New severe",
      sec.axis = sec_axis( ~ . / lmout$coefficients[1], name = "New infected")
    ) + theme_bw() + ggtitle(
      bquote("New severe cases vs new infecetd," ~ .(i) ~ "days lagging"),
      subtitle = bquote(
        R ^ 2 == .(summary(lmout)$r.squared) ~ "," ~ beta == .(lmout$coefficients[1])
      )
    ) + scale_color_manual(
      name = "",
      labels = c("infected", "severe"),
      values = c(colors[1], colors[2])
    )
  )
  
  # lm for deaths
  lmoutd <- lm(deaths ~ 0 + infected_sm, nt)
  print(i)
  print(summary(lmoutd))#$fstatistic)
  print(ggplot(nt, aes(deaths, infected_sm)) + geom_point())
  print(
    ggplot(nt, aes(Date, deaths)) + geom_line(aes(color = colors[3])) + geom_line(aes(
      y = infected_sm * lmoutd$coefficients[1], color = colors[1]
    )) + scale_y_continuous(
      "New deaths",
      sec.axis = sec_axis( ~ . / lmoutd$coefficients[1], name = "New infected")
    ) + theme_bw() + ggtitle(
      bquote("New deaths vs new infecetd," ~ .(i) ~ "days lagging"),
      subtitle = bquote(
        R ^ 2 == .(summary(lmoutd)$r.squared) ~ ", " ~ beta == .(lmoutd$coefficients[1])
      )
    ) + scale_color_manual(
      name = "",
      labels = c("infected", "deaths"),
      values = c(colors[1], colors[3])
    )
  )
}
