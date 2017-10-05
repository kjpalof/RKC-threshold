# K.Palof   8-31-17
# Exploration of regional biomass estimates - historic from Sigma Plot file -  'RKC Regional biomass estiamtes 1979-prsent with COMM catch.JNB'
# Using regional estimates without Port Frederick, only 7 survey areas.

# Objective: determine ideas for biological threshold for opening commericial fishery.

# Load packages -------------
library(tidyverse)
library(readxl)
library(extrafont)
library(grid)
library(gridExtra)
#font_import()
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))
#Load data ----------------

reg_biomass <- read_excel(path = "./data/regional_biomass.xlsx")
biomass_17 <- read_excel(path = "./data/2017_biomass_model.xlsx", sheet = 1)

### regional figure ------------
# replication of Figure 2 from 2017 memo
reg_biomass %>% select(Year, legal, mature) ->biomass1
reg_biomass1_long <- gather(biomass1, type, pounds, legal:mature, factor_key = TRUE)

ggplot(reg_biomass1_long, aes(Year, pounds, group = type))+ 
  geom_point(aes(color = type, shape = type), size =3) +
  geom_line(aes(color = type, group = type))+
  scale_colour_manual(name = "", values = c("grey1", "grey1"))+
  scale_shape_manual(name = "", values = c(16, 1))+
  
  ylim(0,2000000) +ggtitle("regional biomass") + ylab("Biomass (lbs)")+ xlab("Year")+
  theme(plot.title = element_text(hjust =0.5)) + 
  #scale_x_continuous(breaks = seq(min(1993),max(2017), by =2)) +
  theme(legend.position = c(0.8,0.7)) + 
  geom_hline(yintercept = 761292, color = "grey1")+
  geom_hline(yintercept = 1117286, color = "grey1", linetype = "dashed")

reg_biomass %>% filter(Year >= 1993 & Year <= 2007) %>% summarise(mean(legal))
reg_biomass %>% filter(Year >= 1993 & Year <= 2007) %>% summarise(mean(mature))


### 2017 model output average summary -----
biomass_17 %>% 
  select(Year, legal, mature) %>% 
  group_by(Year) %>% 
  summarise(reg_legal = sum(legal), reg_mature = sum(mature)) -> biomass_17a



### 2017 model output figure -------
biomass_17a_long <- gather(biomass_17a, type, pounds, reg_legal:reg_mature, factor_key = TRUE)

ggplot(biomass_17a_long, aes(Year, pounds, group = type))+ 
  geom_point(aes(color = type, shape = type), size =3) +
  geom_line(aes(color = type, group = type))+
  scale_colour_manual(name = "", values = c("grey1", "grey1"))+
  scale_shape_manual(name = "", values = c(16, 1))+
  
  ylim(0,2000000) +ggtitle("regional biomass from 2017 model") + ylab("Biomass (lbs)")+ xlab("Year")+
  theme(plot.title = element_text(hjust =0.5)) + 
  #scale_x_continuous(breaks = seq(min(1993),max(2017), by =2)) +
  theme(legend.position = c(0.8,0.7)) + 
  geom_hline(yintercept = 646753.4, color = "grey1")+
  geom_hline(yintercept = 907430.5, color = "grey1", linetype = "dashed") +
  geom_hline(yintercept = mean(reg_legal), color = "red")

biomass_17a %>% filter(Year >= 1993 & Year <= 2007) %>% summarise(mean(reg_legal))
biomass_17a %>% filter(Year >= 1993 & Year <= 2007) %>% summarise(mean(reg_mature))
