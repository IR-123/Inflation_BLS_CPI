setwd("/Users/iregmii/Documents/Data Projects/Inflation_BLS_CPI")
#install.packages("RColorBrewer")
library(RColorBrewer)
library(hrbrthemes)
library(janitor)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(huxtable)
library(scales)
library(lubridate)
library(tidytext)
library(viridis)
library(dplyr)
library(RColorBrewer)
library(ggplot2)



##### SET UP SOME THINGS #####
source(file = "/Users/iregmii/Documents/Data Projects/Inflation_BLS_CPI/1_load_cpi_data.R")
#source(file = "1_load_cpi_data.R")

item_basket_topline <- c("All items", "All items less food and energy", "Energy", "Food", "Commodities less food and energy commodities",
                         "Services less energy services", "Airline fares",
                         "Shelter", "Transportation commodities less motor fuel", "New and used motor vehicles", "Used cars and trucks")

item_basket_watch_categories <- c("All items", "New and used motor vehicles", "Shelter", "Other services",
                                  "Medical care services", "Food", "Energy", "Commodities less food and energy commodities")

cpi <- cpi_data %>%
  filter(period != "M13") %>%
  filter(seasonal == "S") %>%
  arrange(date) %>%
  group_by(item_name) %>%
  mutate(Pchange1 = (value/lag(value)-1)) %>%
  mutate(Pchange1a = (1 + Pchange1)^12 - 1) %>%
  mutate(Wchange1 = (Pchange1*weight)/100) %>%
  mutate(Wchange1a = (1 + Wchange1)^12 - 1) %>%
  mutate(Pchange3 = (value/lag(value, 3)-1)) %>%
  mutate(three_months = (lag(value, 1)/lag(value, 4)-1)) %>%
  mutate(Wthree_months = (three_months*weight)/100) %>%
  mutate(Wthree_monthsA = (1 + Wthree_months)^4 - 1) %>%
  mutate(Pchange12 = (value/lag(value, 12)-1)) %>%
  mutate(Wchange12 = (Pchange12*weight)/100) %>%
  ungroup()



############################# MAKE CUSTOM THEME ######################### 

##### CUSTOM THEMES - LASS #####
theme_lass <-   theme_modern_rc(ticks = TRUE) + theme(legend.position = "none", legend.title = element_blank(),
                                                      panel.grid.major.y = element_line(size=0.5),
                                                      panel.grid.minor.y = element_blank(),
                                                      plot.title.position = "plot",
                                                      axis.title.x = element_blank(),
                                                      axis.title.y = element_blank(),
                                                      plot.title = element_text(size = 15, face="bold"),
                                                      plot.subtitle = element_text(size=10, color="white"),
                                                      plot.caption = element_text(size=8, face="italic"),
                                                      legend.text = element_text(size=12),
                                                      axis.text.y = element_text(size=12, face="bold"),
                                                      axis.text.x = element_text(size=12, face="bold"),
                                                      strip.text = element_text(face = "bold", color="white", hjust = 0.5, size = 10),
                                                      panel.background = element_rect(fill = "white"),
                                                      panel.grid.major.x = element_blank(),
                                                      panel.grid.minor.x = element_blank(),
                                                      strip.background = element_blank()) +
  theme(text = element_text(family = "Larsseit"),
        plot.title = element_text(family = "Larsseit"),
        plot.subtitle = element_text(family = "Larsseit"),
        plot.caption = element_text(family="Larsseit"),
        strip.text = element_text(family="Larsseit"))

##### CUSTOM THEMES - INFLATION #####
theme_inflation <- theme_classic() +
  theme(text = element_text(family = "Larsseit"),
        plot.title = element_text(family = "Larsseit"),
        plot.subtitle = element_text(family = "Larsseit"),
        plot.caption = element_text(family="Larsseit"),
        strip.text = element_text(family="Larsseit")) +
  theme(panel.grid.major.y = element_line(size=0.5),
        plot.title = element_text(size = 15, face="bold"),
        plot.subtitle = element_text(size = 10),
        plot.title.position = "plot",
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_blank())

######################################################## END ###########################################3

############## CORE ############3
##### Graphic1: Core Inflation ####
trend <- cpi %>% filter(date >= "2021-10-01") %>%
  filter(item_name == "All items less food and energy") %>%
  summarize(n = exp(mean(log(Wchange1a))))
trend <- as.numeric(trend)

trend2 <- cpi %>% filter(date >= "2021-10-01") %>%
  filter(item_name == "All items less food and energy") %>%
  summarize(n = exp(mean(log(Pchange1a))))
trend2 <- as.numeric(trend2)

cpi %>% filter(date > "2020-12-01", item_name == "All items less food and energy") %>%
  select(date, item_name, Pchange1a) %>%
  mutate(num_label = round(100*Pchange1a, 2)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  mutate(trend = ifelse(date >= "2022-01-01", trend2, NA)) %>%
  ggplot(aes(x = date, y = Pchange1a, fill = item_name, label = num_label)) +
  geom_bar(stat = 'identity') + theme(legend.position = "bottom", legend.title = element_blank()) + 
  labs(y = NULL,
       x = NULL,
       title = "Monthly Percent Increase in Core Goods and Services, Annualized",
       subtitle = "Description", 
       caption ="BLS, CPI, 2022 weights, seasonally adjusted. Author's calculation. Ira Regmi, Roosevelt Institute") +
  theme_inflation +
  scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels = percent) +  scale_x_date(date_labels = "%b %y", breaks= "1 month") + 
  geom_line(aes(x=date, y=trend), linetype="dotted", size=1.2, color = "black", alpha=1)
geom_text(aes(x=date, y=place_y, label=num_label), nudge_y = 0.003, size=4, color="black") +
  theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size=18),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))

ggsave("Fig 1.0.png", dpi="retina", width = 12, height=6.75, units = "in")

#############################################################################################
######### Make Graph - Total and Core_Combined 
cpi %>% filter(item_name %in% c("All items","All items less food and energy"), date >= "2020-01-01") %>%
  ggplot(aes(date, Pchange12, color = item_name)) + geom_line(size=2) +
  labs(y = NULL, x = NULL, title="Increase in Prices - Total (All Items) and Core (All Items Less Food and Energy) ",
       subtitle="Year-over-year change in CPI, all items",
       caption = "Source: Bureau of Labor Statistics (BLS) Consumer Price Index (CPI)") +
  theme_lass +
  scale_x_date(date_labels = "%b %y") +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "bottom")

ggsave("Figure_1_0_combined.png", dpi="retina", width = 8.5, height=4.25, units = "in")


#############################################################################################
######### Make Graph - Contribution (Core Goods and Core Services Contribution to Inflation)


Trial <- cpi %>% filter(item_name %in% c("Services less energy services", "Commodities less food and energy commodities"), date > "2019-01-01") %>%
  mutate(in_range = ifelse(date >= "2021-10-01", trend, NA), trend = mean(in_range, na.rm=TRUE),
         trend = ifelse(date >= "2021-10-01", trend, NA))

placement <- Trial %>%
  select(item_name, date, Pchange1, Wchange1a, Wchange12) %>%
  mutate(Wchange12trial = if_else(Wchange12 < 0, 0, Wchange12)) %>%
  group_by(date) %>% summarize(place_y = sum(Wchange12trial)) %>%
  ungroup() %>% mutate(item_name = "Services less energy services")

Trial <- cpi %>% filter(item_name == "All items less food and energy", date > "2021-12-01") %>%
  mutate(num_label = round(100*Wchange12, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  select(date, num_label) %>% full_join(Trial, by="date") %>%
  left_join(placement, by=c("date","item_name"))

Trial$item_name <- str_replace_all(Trial$item_name, "Services less energy services", "Core Services")
Trial$item_name <- str_replace_all(Trial$item_name, "Commodities less food and energy commodities", "Core Goods")

Trial_dates <- unique(Trial$date)
Trial_dates <- sort(Trial_dates, decreasing = TRUE)
Trial_dates = Trial_dates[seq(1, length(Trial_dates), 3)]

#select(item_name, date, Pchange1, Wchange1a, Wchange12) %>%

Trial %>% ggplot(aes(x = date, y = Wchange12, fill = item_name)) +  geom_bar(stat = 'identity', size=0) + theme_lass +
  labs(y = NULL,
       x = NULL,
       title = "Contribution of Core Services Increase Slightly while that of Core Goods Decrease",
       subtitle = paste("Monthly Contribution to Inflation, Annualized. Dotted line reflects an average of ", round(trend,3)*100, "% from October 2021 to ", format(max(Trial_dates), "%B %Y"), sep= ""),
       caption ="BLS, CPI, 2022 weights, seasonally adjusted. Author's calculation. Ira Regmi, Roosevelt Institute") +
  scale_fill_brewer(palette="Paired") +
  geom_line(aes(x=date, y=trend), linetype=2, lineend="square", size=1, color="black") +
  scale_y_continuous(labels = percent) +  scale_x_date(date_labels = "%b %y", breaks=Trial_dates) +
  geom_text(aes(x=date, y=place_y, label=num_label), nudge_y = 0.003, size=4, color="black") +
  theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size=18),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))
ggsave("Fig 2.1.png", dpi="retina", width = 12, height=6.75, units = "in")

################################################################## END OF GRAPH 1 ############################################################################## 

######### Make Graph - Contribution (ANYTHING to Inflation) ########################################


cpi %>% filter(date >= "2021-12-01", item_name %in% c("Transportation commodities less motor fuel")) %>%
  mutate(item_name = "Core Goods") 

cpi %>% filter(date >= "2021-12-01", item_name %in% c("Transportation commodities less motor fuel")) %>%
  select(date, item_name, Wchange12) %>%
  mutate(num_label = round(100*Wchange12, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  select(date, num_label) %>% full_join(cpi, by="date") %>%
  left_join(placement, by=c("date","item_name"))

ContributionOF <- unique(cpi$date)
ContributionOF_dates <- sort(ContributionOF, decreasing = TRUE)
ContributionOF_dates = ContributionOF_dates[seq(1, length(ContributionOF_dates), 3)]

cpi %>% ggplot(aes(x = date, y = Wchange12, fill = item_name)) +
  geom_bar(stat = 'identity', size=0) +
  labs(y = NULL,
       x = NULL,
       title = "Monthly _____ Contribution to Inflation, Annualized",
       subtitle = "Description", 
       caption ="BLS, CPI, 2022 weights, seasonally adjusted. Author's calculation. Ira Regmi, Roosevelt Institute")+
  theme_lass +
  #geom_text(size = 4, position = position_stack(vjust = 0.5), color="black") +
  scale_fill_brewer(palette="Accent") + scale_y_continuous(labels = percent) +  scale_x_date(date_labels = "%b %y", breaks=ContributionOF_dates) +
  geom_text(aes(x=date, y=place_y, label=num_label), nudge_y = 0.003, size=4, color="black") +
  theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size=18),
        axis.text.x = element_text(size=14), axis.text.y = element_text(size=19))

ggsave("Fig 2.2.png", dpi="retina", width = 12, height=6.75, units = "in")

######### END  ########################################


tail(cpi %>% filter(item_name == "Medical care") %>%
       select(item_name, date, Pchange1, Pchange12, Wchange12))

tail(cpi %>% filter(item_name == "Energy"))

