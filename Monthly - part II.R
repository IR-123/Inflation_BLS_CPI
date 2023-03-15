# For a job writing assignment memo. Calculatinf ocntribution
setwd("/Users/iregmii/Documents/Data Projects/Inflation_BLS_CPI")

library(janitor)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(huxtable)
library(scales)
library(lubridate)

##### SET UP SOME THINGS ##### ##### so 
##source(file = "data/cpi_data.RData") #### This downloads directly 

########load(file = "cpi_data.RData") ### This downloads from the computer 

item_basket_topline <- c("All items", "All items less food and energy", "Energy", "Food", "Commodities less food and energy commodities",
                         "Services less energy services", "Airline fares",
                         "Shelter", "Transportation commodities less motor fuel", "New and used motor vehicles", "Used cars and trucks")

item_basket_watch_categories <- c("All items", "New and used motor vehicles", "Shelter", "Other services",
                                  "Medical care services", "Food", "Energy", "Commodities less food and energy commodities")

cpi0 <- cpi_data %>% 
  filter(period != "M13") %>% 
  filter(seasonal == "S") %>%
  arrange(date) %>%
  group_by(item_name) %>%
  mutate(Pchange1 = (value/lag(value)-1)) %>% #1 month percent change (MOM as a percent) Pchange1a would be annualized 
  mutate(Pchange1a = (1 + Pchange1)^12 - 1) %>%
  mutate(Wchange1 = (Pchange1*weight)/100) %>% #contribution to month of month 
  mutate(Wchange1a = (1 + Wchange1)^12 - 1) %>% #monthly annualized 
  mutate(Pchange3 = (value/lag(value, 3)-1)) %>% # 3 month change 
  mutate(Wchange3 = (Pchange3*weight)/100) %>% 
  mutate(Wchange3a = (1 + Wchange3)^4 - 1) %>%
  mutate(Pchange6 = (value/lag(value, 6)-1)) %>%
  mutate(Wchange6 = (Pchange3*weight)/100) %>%
  mutate(Wchange6a = (1 + Wchange3)^2 - 1) %>%
  mutate(Pchange12 = (value/lag(value, 12)-1)) %>% ###(YOY)
  mutate(Wchange12 = (Pchange12*weight)/100) %>% ###(how much it contributes YOY)
  ungroup()




################################################## CONTRIBUTION ###############################################################
#### pre -pandemic contribution as 2014 -2019 --- PRE VALUE

average_month_pre_pandemic <- cpi0 %>%
  filter(date == "2014-01-01" | date == "2019-12-01") %>%
  group_by(item_name) %>%
  arrange(date) %>%
  summarize(pre_value = (1+ (value/lag(value)-1)*weight/100)^(1/6)-1) %>%
  ungroup() %>%
  filter(!is.na(pre_value))
#### value in 2021
value_2021 <- cpi0 %>%
  filter(date == "2020-12-01" | date == "2021-12-01") %>%
  group_by(item_name) %>%
  arrange(date) %>%
  summarize(v_2021 = (1+ (value/lag(value)-1)*weight/100)^(1/1)-1) %>%
  ungroup() %>%
  filter(!is.na(v_2021))
#### value in 2022
value_2022 <- cpi0 %>%
  filter(date == "2021-12-01" | date == "2022-12-01") %>%
  group_by(item_name) %>%
  arrange(date) %>%
  summarize(v_2022 = (1+ (value/lag(value)-1)*weight/100)^(1/1)-1) %>%
  ungroup() %>%
  filter(!is.na(v_2022))
##### number of months since 2021 Jan 
recovery_period_months <- interval(ymd("2020-12-01"), max(cpi0$date))
recovery_period_months = recovery_period_months %/% months(1)

recovery_period <- cpi0 %>%
  filter(date == "2020-12-01" | date == max(date)) %>%
  group_by(item_name) %>%
  arrange(date) %>%
  summarize(recovery_inflaton = (1+ (value/lag(value)-1)*weight/100)^(12/recovery_period_months)-1) %>%
  ungroup() %>%
  filter(!is.na(recovery_inflaton))

#### Changes within 2023 - weighted by the amount of months 
#### How many months we have in 2023 - numeric months
months2023 <- interval(ymd("2022-12-01"), max(cpi0$date)) 
months2023 = months2023 %/% months(1)



cpi <- cpi0 %>%
  group_by(item_name) %>%
  mutate(Pchange_2023 = value/lag(value, months2023)-1) %>% 
  mutate(Wchange_2023 = (Pchange_2023*weight)/100) %>%
  mutate(Wchange_2023a = (1+Wchange_2023)^(12/months2023)-1) %>%
  select(item_name, date, value, weight, Pchange1, Wchange1a, Pchange_2023, Wchange_2023a, Wchange12) %>%
  
         # select(item_name, date, value, weight, Pchange1, Wchange1a, Wchange12) %>%
  ungroup() %>%
  left_join(average_month_pre_pandemic, by="item_name") %>%
  left_join(value_2021, by="item_name") %>%
  left_join(value_2022, by="item_name") %>%
  left_join(recovery_period, by="item_name")


##################
##### Add things here if need be look at BLS detailed expenditure category or remove ############# 

#All items", "All items less food and energy", "Energy", "Food", "Commodities less food and energy commodities",
#"Services less energy services", "Airline fares",
#"Shelter", "Transportation commodities less motor fuel", "New and used motor vehicles", "Used cars and trucks"




################ CONTRIBUTION TABLE  #########################

cpi %>% filter(item_name %in% c("All items less food and energy", "Shelter", "Rent of primary residence", "Services less energy services", "Commodities less food and energy commodities", "New and used motor vehicles", "Used cars and trucks")) %>%
  filter(date == max(date)) %>%
  ####select(item_name, Pchange1)
  #select(`Category` = item_name, `Before Crisis Value` = pre_value, `Contribution to Inflation, 2021` = v_2021, `Contribution to Inflation, 2022` = v_2022, `Contribution to Inflation, 2023` = Wchange_2023a)
select(`Contribution to Inflation, 2023` = Wchange_2023a)
####################### CONTRIBUTION GRAPH ############################





################################################## YOY  ###############################################################value/lag(value, 12)-1



#### pre -pandemic contribution as 2014 -2019 --- PRE VALUE
  
  ##### number of months s 2014 -2019 
  
  #prevalue_months <- interval(ymd("2020-12-01"), max(cpi0$date))
#prevalue_months = prevalue_months %/% months(1)
#### How many months we have in 2023 - numeric months

p12_average_month_pre_pandemic <- cpi0 %>%
  filter(date == "2014-01-01" | date == "2019-12-01") %>%
  group_by(item_name) %>%
  arrange(date) %>%
  summarize(pre_value_p12 = (1 + (value/lag(value)-1))^72 - 1) %>%
  ungroup() %>%
  filter(!is.na(pre_value_p12))
#### value in 2020
p12_value_2020 <- cpi0 %>%
  filter(date == "2019-12-01" | date == "2020-12-01") %>%
  group_by(item_name) %>%
  arrange(date) %>%
  summarize(p12_2020 = (1 + (value/lag(value)-1))^12 - 1) %>%
  ungroup() %>%
  filter(!is.na(p12_2020))
#### value in 2021
p12_value_2021 <- cpi0 %>%
  filter(date == "2020-12-01" | date == "2021-12-01") %>%
  group_by(item_name) %>%
  arrange(date) %>%
  summarize(p12_2021 = (1 + (value/lag(value)-1))^12 - 1) %>%
  ungroup() %>%
  filter(!is.na(p12_2021))
#### value in 2022
p12_value_2022 <- cpi0 %>%
  filter(date == "2021-12-01" | date == "2022-12-01") %>%
  group_by(item_name) %>%
  arrange(date) %>%
  summarize(p12_2022 = (1 + (value/lag(value)-1))^12 - 1) %>% #### 12 month change - no need to annualized 
  ungroup() %>%
  filter(!is.na(p12_2022))

cpi <- cpi0 %>%
  group_by(item_name) %>%
  mutate(P12_change_2023 = value/lag(value, months2023)-1) %>% 
  mutate(W12_change_2023 = (P12_change_2023*weight)/100) %>%
  mutate(W12_change_2023a = (1+W12_change_2023)^(12/months2023)-1) %>%
  #mutate(Pchange_2023_YOY = (1 + (value/lag(value, months2023)-1))^(12/months2023) - 1) %>% #### 1 month change 
  select(item_name, date, value, weight, Pchange1, Wchange1a, P12_change_2023, W12_change_2023a, Wchange12, Pchange_2023_YOY) %>%
   #select(item_name, date, value, weight, Pchange1, Wchange1a, Wchange12) %>%
  ungroup() %>%
  left_join(p12_average_month_pre_pandemic, by="item_name") %>%
  left_join(p12_value_2021, by="item_name") %>%
  left_join(p12_value_2022, by="item_name") %>%
  left_join(p12_value_2020, by="item_name")
  #left_join(recovery_period_p12, by="item_name")


################ RATES TABLE  #########################
cpi %>% filter(item_name %in% c("All items less food and energy", "Shelter", "Rent of primary residence", "Services less energy services", "Commodities less food and energy commodities", "New and used motor vehicles", "Used cars and trucks")) %>%
  filter(date == max(date)) %>%
  ####select(item_name, Pchange1)
  #select(`Category` = item_name, `Before Crisis Value` = pre_value_p12, `Inflation, 2021` = p12_2021, `Inflation, 2022` = p12_2022, `Inflation, 2023` = P12_change_2023)

select(`Inflation, 2023` = Pchange_2023_YOY)   

###, `Inflation, 2022` = p12_2022, `Inflation, 2023` = P12_change_2023)





################################################## ################################################## ################################################## ################################################## 

#### CUSTOM THEMES - LASS #####
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

#############################################################################################################################################################################

cpi_graph %>% filter(date >= "2021-12-01", item_name %in% c("Transportation commodities less motor fuel")) %>%
  mutate(item_name = "Core Goods") 

cpi_graph %>% filter(date >= "2021-12-01", item_name %in% c("Transportation commodities less motor fuel")) %>%
  select(date, item_name, Wchange12) %>%
  mutate(num_label = round(100*Wchange12, 1)) %>% mutate(num_label = ifelse(abs(num_label) < 0.16, NA, num_label)) %>%
  select(date, num_label) %>% full_join(cpi, by="date") %>%
  left_join(placement, by=c("date","item_name"))

ContributionOF <- unique(cpi$date)
ContributionOF_dates <- sort(ContributionOF, decreasing = TRUE)
ContributionOF_dates = ContributionOF_dates[seq(1, length(ContributionOF_dates), 3)]

cpi_graph %>% ggplot(aes(x = date, y = Wchange12, fill = item_name)) +
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
