# For a job writing assignment memo. May-23-2022. Mike Konczal
setwd("/Users/iregmii/Documents/Data Projects/Inflation_BLS_CPI")


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
library(ggrepel)

theme_inflation <- theme_classic() +
  theme(text = element_text(family = "Larsseit", face="bold", color ="#1E8456"),
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

#### mutate(Pchange3 = (value/lag(value, 3)-1)) %>% # 3 month change 
#3-MOM#
cpi <- cpi_data 
#months2023 <- interval(ymd("2022-12-01"), max(cpi$date)) 
#months2023 = months2023 %/% months(1)
cpi <- cpi_data %>%
  filter(period!= "M13") %>%
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


########Contributions########## 
#### pre -pandemic contribution as 2014 -2019 
contribution_prepandemic <- cpi %>%
  filter(date == "2014-01-01" | date == "2019-12-01") %>%
  group_by(item_name) %>%
  arrange(date) %>%
  summarize(v_prepandemic = (1+ (value/lag(value)-1)*weight/100)^(1/6)-1) %>%
  ungroup() %>%
  filter(!is.na(v_prepandemic))
#### Contribution in 2021
contribution_2021 <- cpi %>%
  filter(date == "2020-12-01" | date == "2021-12-01") %>%
  group_by(item_name) %>%
  arrange(date) %>%
  summarize(v_2021 = (1+ (value/lag(value)-1)*weight/100)^(1/1)-1) %>%
  ungroup() %>%
  filter(!is.na(v_2021))
#### Contribution in 2022
contribution_2022 <- cpi %>%
  filter(date == "2021-12-01" | date == "2022-12-01") %>%
  group_by(item_name) %>%
  arrange(date) %>%
  summarize(v_2022 = (1+ (value/lag(value)-1)*weight/100)^(1/1)-1) %>%
  ungroup() %>%
  filter(!is.na(v_2022))
#### Contribution so far in 2023 
months2023 <- interval(ymd(max(cpi$date)), "2023-12-01")
months2023 = months2023 %/% months(1)
contribution_2023 <- cpi %>%
  filter(date == "2022-12-01" | date == max(date)) %>%
  group_by(item_name) %>%
  arrange(date) %>%
summarize(v_2023 = (1+ ((value)/lag(value)-1)*weight/100)^(12/months2023)-1) %>%
  ungroup() %>%
  filter(!is.na(v_2023))
   
#### CONTRIBION DF 
contribution_All <- contribution_2023 %>%
select(item_name, v_2023) %>%
  #left_join(v_prepandemic, by="item_name") %>%
  left_join(contribution_2022, by="item_name") %>%
  left_join(contribution_2021, by="item_name")



##### Graphs
item_basket_topline <- c("All items", "All items less food and energy", "Energy", "Food", "Commodities less food and energy commodities",
                         "Services less energy services", "Airline fares",
                         "Shelter", "Transportation commodities less motor fuel", "New and used motor vehicles", "Used cars and trucks")


################
contribution_All  %>% filter(item_name %in% c("All items", "Energy commodities", "Services less energy services", "Commodities less food and energy commodities",
                              "Food", "Used cars and trucks")) %>%
  #filter(date == max(date)) %>%
  ####select(item_name, Pchange1)
  select(item_name,
         `Contribution to Inflation, 2021` = v_2021, `Contribution to Inflation, 2022` = v_2022)

################################################## FACET WITH MULTIPLE CATEGORIES ############### 
Goods_List <- c("All items less food and energy", "Shelter", "Services less energy services", "Commodities less food and energy commodities",
                "New and used motor vehicles", "Used cars and trucks") 
contribution1 <- contribution_All %>%
  group_by(item_name) %>% 
  filter(item_name %in% Goods_List)


contribution1 <- contribution1 %>% 
  mutate(num_label = round(100*v_2021, 2))

contribution1 <- contribution1 %>% 
  mutate(num_label = round(100*v_2022, 2))

#contribution1 <- pivot_longer(contribution1, names_to = "contribution_year", values_to = "contribution_tot")

ggplot(contribution1, aes(x = v_2022, y = v_2021)) + 
  geom_point(color="#2D779C", size = 1.5) + facet_wrap(facet = "item_name", nrow=2) +
  theme_inflation + theme(legend.position = "none") +
  scale_y_continuous(labels = percent) + 
  scale_x_continuous(labels = percent) +
  #geom_text(aes(label = num_label), check_overlap = TRUE) +
  geom_text(data = contribution1, aes(x=v_2022, y=v_2021, label=num_label), nudge_y = 0.005, size=3, face="bold", color="#1E8456")+
  labs(y = NULL,
       x = NULL,
       title = "Contribution to Inflation , 2021",
       subtitle = "tktktk", 
       caption ="BLS, CPI, 2022 weights, seasonally adjusted. Author's calculation. Ira Regmi, Roosevelt Institute")

ggsave("Fig c.1.png", dpi="retina", width = 12, height=6.75, units = "in")

################################################## PICK ############### 
Pick_Item <- c("Shelter") 
Pick_Item <- contribution_All %>%
  group_by(item_name) %>% 
  filter(item_name %in% Goods_List)

contribution1 <- contribution1 %>% 
  mutate(num_label = round(100*v_2021, 2))

ggplot(Pick_Item, aes(x = date, y = v_2021, fill = item_name,)) + 
  geom_bar(stat = 'identity', fill = "#6EA4BF")  +  
  geom_point() + 
  geom_line(color="#2D779C", size = 1.5) + 
  theme_inflation + theme(legend.position = "none") +
  scale_y_continuous(labels = percent) + 
  scale_x_date(date_labels = "%b %y", breaks= "3 month") +
  #geom_text(aes(label = num_label), check_overlap = TRUE) +
  geom_text(data = Pick_Item, aes(x=date, y=PchangeMOM3, label=num_label), nudge_y = 0.03, size=3, face="bold", color="#1E8456")+
  labs(y = NULL,
       x = NULL,
       title = "Shelter- Three Month Percentage Change - Annualized",
       subtitle = "tktktk", 
       caption ="BLS, CPI, 2022 weights, seasonally adjusted. Author's calculation. Ira Regmi, Roosevelt Institute")

ggsave("Fig 3M.7.0.png", dpi="retina", width = 12, height=6.75, units = "in")


