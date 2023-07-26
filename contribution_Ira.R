# For a job writing assignment memo. May-23-2022. Mike Konczal
setwd("/Users/iregmii/Documents/Data Projects/Stiglitz")


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
  mutate(Pchange1 = (value/lag(value)-1)) %>% 
  mutate(Wchange1 = (Pchange1*weight)/100) %>%
  mutate(Wchange1a = (1 + Wchange1)^12 - 1) %>%
  mutate(Pchange3 = (value/lag(value, 3)-1)) %>%
  mutate(Wchange3 = (Pchange3*weight)/100) %>%
  mutate(Wchange3a = (1 + Wchange3)^4 - 1) %>%
  mutate(Pchange6 = (value/lag(value, 6)-1)) %>%
  mutate(Wchange6 = (Pchange3*weight)/100) %>%
  mutate(Wchange6a = (1 + Wchange3)^2 - 1) %>%
  mutate(Pchange12 = (value/lag(value, 12)-1)) %>% ###(YOY)
  mutate(Wchange12 = (Pchange12*weight)/100) %>% ###(how much it contributes YOY)
  ungroup()



#### How many months we have in 2022 - numeric months
months2022 <- interval(ymd("2021-12-01"), max(cpi0$date))
months2022 = months2022 %/% months(1)
  

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

#### Changes within 2022 - weighted by the amount of months 

cpi <- cpi0 %>%
  group_by(item_name) %>%
  mutate(Pchange_2022 = value/lag(value, months2022)-1) %>%
  mutate(Wchange_2022 = (Pchange_2022*weight)/100) %>%
  mutate(Wchange_2022a = (1+Wchange_2022)^(12/months2022)-1) %>%
  select(item_name, date, value, weight, Pchange1, Wchange1a, Pchange_2022, Wchange_2022a, Wchange12) %>%
  ungroup() %>%
  left_join(average_month_pre_pandemic, by="item_name") %>%
  left_join(value_2021, by="item_name") %>%
  left_join(recovery_period, by="item_name")


##################
##### Add things here if need be look at BLS detailed expenditure category or remove ############# 

item_basket_topline <- c("All items", "All items less food and energy", "Energy", "Food", "Commodities less food and energy commodities",
                         "Services less energy services", "Airline fares",
                         "Shelter", "Transportation commodities less motor fuel", "New and used motor vehicles", "Used cars and trucks")


cpi %>% filter(item_name == item_basket_topline) %>%
  filter(date == max(date)) %>%
  select(item_name, `Contribution to Inflation, 2021` = v_2021, `Contribution to Inflation, 2022` = Wchange_2022a, pre_value)
select(item_name, Pchange1)


################
cpi %>% filter(item_name %in% c("All items less food and energy", "Shelter", "Rent of primary residence", "Services less energy services", "Commodities less food and energy commodities", "New and used motor vehicles", "Used cars and trucks")) %>%
  filter(date == max(date)) %>%
  ####select(item_name, Pchange1)
  select(item_name, `Before Crisis Value` = pre_value, `Contribution to Inflation, 2021` = v_2021, `Contribution to Inflation, 2022` = Wchange_2022a)

a <- cpi %>% filter(item_name == "Food", date > "2020-12-01")

cpi %>% filter(item_name == "Used cars and trucks", date > "2020-12-01")

cpi %>% filter(item_name == "Transportation commodities less motor fuel", date > "2021-12-01")

cpi %>% filter(item_name == "Commodities less food and energy commodities")

cpi %>% filter(item_name == "All items less food and energy") %>%
  select(item_name, date, Pchange1, Wchange1a, Wchange12) %>%
  ggplot(aes(date, Wchange12)) + geom_line() + theme_classic()
###################

tail(cpi %>% filter(item_name == "All items less food and energy") %>%
       select(item_name, date, Pchange1, Wchange12))

tail(cpi %>% filter(item_name == "Energy"))

###### BASIC GRAPH - ALL ITEMS #####

all_cpi <- cpi0 %>% 
  arrange(date) %>%
  filter(date >"2018-12-01") %>%
  filter(period != "M13") %>% filter(seasonal == "S") %>% filter(item_name == "All items") %>% 
 mutate(YOY = value/lag(value,12) - 1)
graph <- ggplot(all_cpi, aes(x=date, y=YOY)) + geom_line(size = 1) +theme(axis.text.x = element_text (size = 8, angle =360))
graph <- graph + theme(legend.title = element_blank(), legend.position = "bottom") + 
  scale_y_continuous(labels = scales::percent) + scale_x_date(date_labels = "%b-%y", breaks = "6 months") + geom_point (color = "black", size = 1) + theme_classic()

graph
ggsave("inflation_basic.png", width = 19, height=10.68, dpi="retina")

graph <- graph + labs(x="",y="",
                              title="Inflation Rate",
                              subtitle="All items",
                              caption = "Seasonally adjusted. Author's calculations") +
  theme(plot.title = element_markdown(size = 18, face="bold"),
        plot.subtitle = element_markdown(size = 10, margin=margin(9,0,15,0)))
graph

ggsave("inflation_basic.png", width = 19, height=10.68, dpi="retina")

###### ALL ITEMS LESS ENERGY 

all_less_energy <- cpi0 %>% 
  arrange(date) %>%
  filter(date >"2018-12-01") %>%
  filter(period != "M13") %>% filter(seasonal == "S") %>% filter(item_name == "Rent of primary residence") %>% 
  mutate(YOY2 = value/lag(value,12) - 1)

######### filter ites 
used_cars <- cpi0 %>% filter(ite_name == "Used cars and trucks")%>% select (item_name, Pchange12, Wchange12, weight)
ggplot(used_cars, aes(x=date, y-Pchange12) +geom_line() +theme_classic)

