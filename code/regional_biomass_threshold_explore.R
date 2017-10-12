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
#loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))
#Load data ----------------

reg_biomass <- read_excel(path = "./data/regional_biomass.xlsx")
biomass_17 <- read_excel(path = "./data/2017_biomass_model.xlsx", sheet = 1)
harvest <- read_excel(path = "./data/harvest.xlsx", sheet = 1)

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

biomass_17a %>% 
  filter(Year >= 1993) %>%
  summarise(mature_mean = mean(reg_mature), 
            Mmean_50 = 0.50*mature_mean, Mmean_75 = 0.75*mature_mean, 
            Mmean_125 = 1.25*mature_mean) -> avg_93all

biomass_17a %>% 
  filter(Year >= 1993 & Year <= 2007) %>%
  summarise(mature_LT = mean(reg_mature), 
            M_LT_50 = 0.50*mature_LT, M_LT_75 = 0.75*mature_LT, 
            M_LT_125 = 1.25*mature_LT) -> avg_baseline

### 2017 model output figure 2-------
# has long term averages from 1993-2007
biomass_17a_long <- gather(biomass_17a, type, pounds, reg_legal:reg_mature, factor_key = TRUE)

fig1<- ggplot(biomass_17a_long, aes(Year, pounds, group = type))+ 
  geom_point(aes(color = type, shape = type), size =3) +
  geom_line(aes(color = type, group = type))+
  scale_colour_manual(name = "", values = c("grey1", "grey1"))+
  scale_shape_manual(name = "", values = c(16, 1))+
  
  ylim(0,2000000) +ggtitle("regional biomass from 2017 model") + ylab("Biomass (lbs)")+ xlab("Year")+
  theme(plot.title = element_text(hjust =0.5)) + 
  #scale_x_continuous(breaks = seq(min(1993),max(2017), by =2)) +
  theme(legend.position = c(0.8,0.7)) + 
  geom_hline(yintercept = 646753.4, color = "grey1")+
  geom_hline(yintercept = 907430.5, color = "grey1", linetype = "dashed") 
  
biomass_17a %>% filter(Year >= 1993 & Year <= 2007) %>% summarise(mean(reg_legal))
biomass_17a %>% filter(Year >= 1993 & Year <= 2007) %>% summarise(mean(reg_mature))

# save plot 
png('./results/regional_2017.png', res= 300, width = 7.5, height =4.0, units = "in")
fig1
dev.off()

### 2017 output long term baseline average -------------

biomass_17a_long <- gather(biomass_17a, type, pounds, reg_legal:reg_mature, factor_key = TRUE)
avg_baseline_long <- gather(avg_baseline, type, pounds, mature_LT:M_LT_125, factor_key = TRUE)

fig3 <- ggplot(biomass_17a_long, aes(Year, pounds, group = type))+ 
  geom_point(aes(color = type, shape = type), size =3) +
  geom_line(aes(color = type, group = type))+
  scale_colour_manual(name = "", values = c("green", "red","blue", "gray30", "gray48", "gray48"
                                            ))+
  scale_shape_manual(name = "", values = c(16, 1), guide = "none")+
  
  ylim(0,2000000) +ggtitle("regional biomass from 2017 model, 93-07 baseline") + ylab("Biomass (lbs)")+ xlab("Year")+
  theme(plot.title = element_text(hjust =0.5)) + 
  #scale_x_continuous(breaks = seq(min(1993),max(2017), by =2)) +
  theme(legend.position = c(0.8,0.7)) + 
  geom_hline(data = avg_baseline_long, aes(yintercept = pounds, group = type, colour = type),
             show.legend = TRUE)
      
# save plot 
png('./results/lt_baseline_2017.png', res= 300, width = 7.5, height = 5.0, units = "in")
fig3
dev.off()

### 2017 output all years average --------------
biomass_17a_long <- gather(biomass_17a, type, pounds, reg_legal:reg_mature, factor_key = TRUE)
avg_93all_long <- gather(avg_93all, type, pounds, mature_mean:Mmean_125, factor_key = TRUE)

fig4 <- ggplot(biomass_17a_long, aes(Year, pounds, group = type))+ 
  geom_point(aes(color = type, shape = type), size =3) +
  geom_line(aes(color = type, group = type))+
  scale_colour_manual(name = "", values = c("grey1", "green", "red","blue", "gray48", "gray48"
  ))+
  scale_shape_manual(name = "", values = c(16, 1), guide = "none")+
  
  ylim(0,2000000) +ggtitle("regional biomass from 2017 model") + ylab("Biomass (lbs)")+ xlab("Year")+
  theme(plot.title = element_text(hjust =0.5)) + 
  #scale_x_continuous(breaks = seq(min(1993),max(2017), by =2)) +
  theme(legend.position = c(0.8,0.7)) + 
  geom_hline(data = avg_93all_long, aes(yintercept = pounds, group = type, colour = type),
             show.legend = TRUE)

# save plot 
png('./results/allyears_2017.png', res= 300, width = 7.5, height = 5.0, units = "in")
fig4
dev.off()


### regional figure with closures/ openings --------
fig4 <- ggplot(survey_area_biom_long, aes(Year, pounds, group = type))+ 
  geom_point(aes(color = fishery.status, shape = type), size =3) +
  geom_line(aes(color = type, group = type))+
  scale_colour_manual(name = "", values = c("grey1", "grey1", "grey1", "red"))+
  scale_shape_manual(name = "", values = c(16, 1))+
  
  ylim(0,1500000) +ggtitle("Survey areas 2017 Model") + ylab("Biomass (lbs)")+ xlab("")+
  theme(plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1979),max(2019), by =4)) +
  theme(legend.position = c(0.8,0.7)) + 
  geom_hline(yintercept = 646753, color = "grey1")+
  geom_hline(yintercept = 907431, color = "grey1", linetype = "dashed")

# save plot 
png('./results/open_closed_17.png', res= 300, width = 7.5, height =5.0, units = "in")
fig4
dev.off()


### 50% figures ------
### regional -------
biomass_17 %>% 
  select(Year, legal, mature) %>% 
  group_by(Year) %>% 
  summarise(reg_legal = sum(legal), reg_mature = sum(mature)) -> biomass_17a

biomass_17a %>% 
  filter(Year >= 1993) %>%
  summarise(mature_mean = mean(reg_mature), 
            Mmean_50 = 0.50*mature_mean) -> avg50_93all

biomass_17a %>% 
  filter(Year >= 1993 & Year <= 2007) %>%
  summarise(mature_LT = mean(reg_mature), 
            M_LT_50 = 0.50*mature_LT) -> avg50_baseline

#### fig 5 regional with LT baseline ----------
biomass_17a_long <- gather(biomass_17a, type, pounds, reg_legal:reg_mature, factor_key = TRUE)
avg50_baseline_long <- gather(avg50_baseline, type, pounds, mature_LT:M_LT_50, factor_key = TRUE)

fig5 <- ggplot(biomass_17a_long, aes(Year, pounds, group = type))+ 
  geom_line(aes(color = type, group = type, lty = type), size =0.85)+
  scale_colour_manual(name = "", values = c("red", "grey1", "gray48", "black"
  ))+
  scale_linetype_manual(values = c(reg_legal = "dashed", reg_mature = "solid", 
                                   mature_LT = "solid", M_LT_50 = "solid"), guide = "none")+ 
  ylim(0,2000000) +ggtitle("regional biomass from 2017 model, 93 - 07 baseline") + 
  ylab("Biomass (lbs)")+ xlab("Year")+
  theme(plot.title = element_text(hjust =0.5)) + 
  #scale_x_continuous(breaks = seq(min(1993),max(2017), by =2)) +
  theme(legend.position = c(0.8,0.7)) + 
  geom_hline(data = avg50_baseline_long, aes(yintercept = pounds, group = type, colour = type),
             show.legend = TRUE)

# save plot 
png('./results/regional_50_LTbase.png', res= 300, width = 7.5, height = 5.0, units = "in")
fig5
dev.off()


#### fig 6 regional with 93+ avg ---------
biomass_17a_long <- gather(biomass_17a, type, pounds, reg_legal:reg_mature, factor_key = TRUE)
avg50_93all_long <- gather(avg50_93all, type, pounds, mature_mean:Mmean_50, factor_key = TRUE)

fig6 <- ggplot(biomass_17a_long, aes(Year, pounds, group = type))+ 
  geom_line(aes(color = type, group = type, lty = type), size =0.85)+
  scale_colour_manual(name = "", values = c("grey1", "red",  "gray48", "black"
  ))+
  scale_linetype_manual(values = c(reg_legal = "dashed", reg_mature = "solid", 
                                   mature_mean = "solid", Mmean_50 = "solid"), guide = "none")+ 
  ylim(0,2000000) +ggtitle("regional biomass from 2017 model, 93+ average") + 
  ylab("Biomass (lbs)")+ xlab("Year")+
  theme(plot.title = element_text(hjust =0.5)) + 
  #scale_x_continuous(breaks = seq(min(1993),max(2017), by =2)) +
  theme(legend.position = c(0.8,0.7)) + 
  geom_hline(data = avg50_93all_long, aes(yintercept = pounds, group = type, colour = type),
             show.legend = TRUE)

# save plot 
png('./results/regional_50_93all.png', res= 300, width = 7.5, height = 5.0, units = "in")
fig6
dev.off()

### Area figures -------------
biomass_17 %>% 
  select(Year, Location, legal, mature) %>% 
  group_by(Location, Year) %>% 
  summarise(legal = sum(legal), mature = sum(mature)) -> biomass_17_area

biomass_17_area %>% 
  filter(Year >= 1993) %>%
  group_by(Location) %>% 
  summarise(mature_mean = mean(mature), 
            Mmean_50 = 0.50*mature_mean) -> avg50_93area

biomass_17_area %>% 
  filter(Year >= 1993 & Year <= 2007) %>%
  group_by(Location) %>%
  summarise(mature_LT = mean(mature), 
            M_LT_50 = 0.50*mature_LT) -> avg50_baseline_area

#### fig 7 area specific biomass with LT baseline ----------
biomass_17_area_long <- gather(biomass_17_area, type, pounds, legal:mature, factor_key = TRUE)
avg50_baseline_area_long <- gather(avg50_baseline_area, type, pounds, mature_LT:M_LT_50, factor_key = TRUE)

ggplot(biomass_17_area_long, aes(Year, pounds, group = type))+ facet_wrap(~ Location)
  geom_line(aes(color = type, group = type, lty = type), size =0.85)+
  scale_colour_manual(name = "", values = c("red", "grey1", "gray48", "black"
  ))+
  scale_linetype_manual(values = c(legal = "dashed", mature = "solid", 
                                   mature_LT = "solid", M_LT_50 = "solid"), guide = "none")+ 
  ylim(0,2000000) +ggtitle("regional biomass from 2017 model, 93 - 07 baseline") + 
  ylab("Biomass (lbs)")+ xlab("Year")+
  theme(plot.title = element_text(hjust =0.5)) + 
  #scale_x_continuous(breaks = seq(min(1993),max(2017), by =2)) +
  theme(legend.position = c(0.8,0.7)) + 
  geom_hline(data = avg50_baseline_long, aes(yintercept = pounds, group = type, colour = type),
             show.legend = TRUE)

ggplot(biomass_17_area_long, aes(Year, pounds, group = type))+ facet_wrap(~ Location)+
  geom_line(aes(color = type))
  
# save plot 
png('./results/regional_50_LTbase.png', res= 300, width = 7.5, height = 5.0, units = "in")
fig
dev.off()
