oecd <- read_csv("data/oecd_sep_2022.csv")

####
oecd <- oecd %>% filter(MEASURE == "IDX2015") %>% 
  filter(SUBJECT == "TOT_FOODENRG" | SUBJECT == "TOT") %>%
  mutate(year = str_sub(TIME,1,4)) %>%
  mutate(qualifer  = str_sub(TIME, 6,6)) %>%
  mutate(month = str_sub(TIME, 6,7))
oecd$date <- paste(oecd$month, "01", oecd$year, sep="/")
oecd$date <- as.Date(oecd$date, "%m/%d/%Y")

# Some countries and times only have quarterly data, this creates their
# dates
quarterly <- oecd %>% filter(FREQUENCY == "Q") %>%
  mutate(month = case_when(
    month == "Q1" ~ 3,
    month == "Q2" ~ 6,
    month == "Q3" ~ 9,
    month == "Q4" ~ 12)) %>%
  filter(LOCATION == "AUS" | LOCATION == "NZL")
quarterly$date <- paste(quarterly$month, "01", quarterly$year, sep="/")
quarterly$date <- as.Date(quarterly$date, "%m/%d/%Y")

oecd <- oecd %>% filter(FREQUENCY == "M") %>%
  rbind(quarterly)

monthsbefore <- interval(ymd("2017-12-01"),ymd("2019-12-01"))
monthsbefore = monthsbefore %/% months(1)

pre_change <- oecd %>% filter(date == "2017-12-01" | date == "2019-12-01") %>%
  group_by(LOCATION, SUBJECT) %>% arrange(date) %>%
  mutate(pre_change = Value/lag(Value,1),
         pre_change = pre_change^(12/24)-1) %>%
  ungroup() %>%
  filter(!is.na(pre_change)) %>%
  select(LOCATION, SUBJECT, pre_change)

months2022 <- interval(ymd("2020-12-01"),ymd("2022-05-01"))
months2022 = months2022 %/% months(1)

oecd <- oecd %>% filter(date == "2022-05-01" | date == "2020-12-01") %>%
  group_by(LOCATION, SUBJECT) %>% arrange(date) %>%
  mutate(post_change = Value/lag(Value,1),
         post_change = post_change^(12/months2022)-1) %>%
  ungroup() %>%
  filter(!is.na(post_change)) %>%
  left_join(pre_change, by=c("LOCATION","SUBJECT")) %>%
  mutate(change = post_change - pre_change) %>%
  select(LOCATION, SUBJECT, post_change, pre_change, change)

oecd <- oecd %>% filter(LOCATION != "TUR") %>%
  group_by(SUBJECT) %>%
  summarize(post_change = mean(post_change), pre_change = mean(pre_change)) %>%
  ungroup() %>%
  mutate(LOCATION = "OECD Avg", change = post_change - pre_change) %>%
  select(LOCATION, SUBJECT, post_change, pre_change, change) %>%
  rbind(oecd)



######

comparison_countries <- c("ESP","USA","DEU","ITA","CAN","GBR","FRA","OECD Avg")

oecd %>% filter(SUBJECT == "TOT", LOCATION %in% comparison_countries) %>%
  mutate(ordered = fct_reorder(LOCATION, change)) %>%
  ggplot(aes(ordered, change, fill = ifelse(ordered == "USA", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + theme_inflation +
  labs(x="", y="", title="Increase in Total Inflation (Pre- and Post-Pandemic)",
       subtitle="OECD data on annual growth rate of consumer price index",
       caption = "Turkey as an outlier was dropped from OECD average. Change in inflation is determined by 
                  calculating the annualized rate of change in CPI from (Dec 2020 to May 2022) and subtracting 
                  that from the annualized rate of inflation from December 2017 to December 2019.") +
  theme(legend.position = "none", legend.title = element_blank()) +
  guides(fill = "none") +  scale_y_continuous(labels = percent)

ggsave("graphics/Figure_4_1.png", dpi="retina", width = 8.5, height=4.78, units = "in")

#### FIGURE 4.2 ####
oecd %>% filter(SUBJECT == "TOT_FOODENRG", LOCATION %in% comparison_countries) %>%
  mutate(ordered = fct_reorder(LOCATION, change)) %>%
  ggplot(aes(ordered, change, fill = ifelse(ordered == "USA", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + theme_inflation +
  labs(x="", y="", title="Increase in Core Inflation (Pre- and Post-Pandemic)",
       subtitle="OECD data on annual growth rate of consumer price index",
       caption = "Turkey as an outlier was dropped from OECD average. Change in inflation is determined by 
                  calculating the annualized rate of change in CPI from (Dec 2020 to May 2022) and subtracting 
                  that from the annualized rate of inflation from December 2017 to December 2019.") +
  theme(legend.position = "none", legend.title = element_blank()) +
  guides(fill = "none") +  scale_y_continuous(labels = percent)

ggsave("graphics/Figure_4_2.png", dpi="retina", width = 8.5, height=4.25, units = "in")
