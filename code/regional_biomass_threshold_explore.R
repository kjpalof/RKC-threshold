# K.Palof   8-31-17 / 12-3-19
# Exploration of regional biomass estimates - historic from Sigma Plot file -  'RKC Regional biomass estiamtes 1979-prsent with COMM catch.JNB'
# Using regional estimates without Port Frederick, only 7 survey areas.

# Objective: determine ideas for regional biological threshold for opening commericial fishery.

# Load -------------
source('./code/helper.R')

# Data ---------------
cur_yr <- 2019
#mr_adjust <- read.csv('./data/adj_final_stock_assessment.csv')
biomass <- read.csv("./data/biomass_2019.csv") 
# file has current biomass estimates from each survey area for legal and mature biomass. 
#   Also has harvest - these all need to be updated from SE assessments 
fishery.status <- read.csv('C:/Users/kjpalof/Documents/SE_crab_assessments/data/rkc/Juneau/hind_fore_cast_JNU_current.csv')

# old make sure I don't need before I delete
#reg_biomass <- read_excel(path = "./data/regional_biomass.xlsx")
#biomass_17 <- read_excel(path = "./data/2017_biomass_model.xlsx", sheet = 1)
#harvest <- read_excel(path = "./data/harvest.xlsx", sheet = 1)

## clean up -----------------
fishery.status %>% 
  select(Year = year, status) %>% 
  mutate(status = ifelse(status == "PU only", "closed", as.character(status))) %>% #-> fishery.status.update
# add next line to deal with current year which is TBD in file but will most 
# likely be closed in current year (2018)
  mutate(status = ifelse(status == "TBD", "closed", as.character(status))) -> fishery.status.update


# regional biomass ----
biomass %>% 
  group_by(Year) %>% 
  summarise(legal = sum(legal.biomass), mature = sum(mature.biomass), 
            adj_legal = sum(adj.legal), adj_mature = sum(adj.mature)) %>% 
  as.data.frame() -> regional.b

regional.b %>% 
  left_join(fishery.status.update) -> regional.b
 #write.csv(regional.b, paste0('./results/rkc/Region1/', cur_yr, '/regional_biomass_', cur_yr, '.csv'))
# use these values for table A1 in stock health document 


# baseline ---
# 1993 - 2007 baseline years
regional.b %>% 
  filter(Year >= 1993 & Year <= 2007) %>% 
  summarise(legal_baseline = mean(legal), mature_baseline = mean(mature), 
            adj.legal_base = mean(adj_legal), adj.mature.base = mean(adj_mature)) %>% 
  as.data.frame() %>% 
  gather(type, pounds, factor_key = TRUE) %>% 
  mutate(st_yr = 2007, label = c("CSA Legal (1993-2007)", "CSA Mature (1993-2007)", 
                                 "Legal (1993-2007)", "Mature (1993-2007)")) ->reg_baseline

reg_baseline[1:2, ] ->  reg_baseline_CSA
reg_baseline[3:4, ] ->  reg_baseline_MR

# Figure 2 **CLOSED** regional biomass M/R adjusted biomass---------
# should have 2018 model with longterm baselines (1993-2007) and closure status. 
#   also show 2018 forecast as distinct from model output
regional.b %>% 
  select(Year, adj_legal, adj_mature, status) %>%
  gather(type, pounds, adj_legal:adj_mature, factor_key = TRUE) %>%
  mutate(status = replace(status, which(status == "TBD"), "closed")) %>% # can replace the TBD with open or closed
  ggplot(aes(Year, pounds, group = type)) +
  geom_line(aes(colour = type, group = type, linetype = type))+
  geom_point(aes(colour = type, shape = status, fill = type), size =3) +
  geom_hline(data = reg_baseline_MR, aes(yintercept = pounds, 
                                         linetype = type, colour = type)) +
  scale_colour_manual(name = "", values = c("black", "grey60", "black", "grey60"), 
                      guide = FALSE)+
  scale_shape_manual(name = "Fishery Status", values = c(25, 21, 8))+
  scale_linetype_manual(name = "", values = c("solid", "dashed", "solid", "dashed"), 
                        guide = FALSE) +
  scale_fill_manual(name = "", values = c("black", "gray75"), 
                    guide = FALSE) +
  scale_y_continuous(labels = comma, limits = c(0,(max(regional.b$adj_mature,
                                                       na.rm = TRUE) + 100000)),
                     breaks= seq(min(0), max(max(regional.b$adj_mature, na.rm = TRUE) +100000), 
                                 by = 500000)) +
  scale_x_continuous(breaks = seq(min(1975),max(max(regional.b$Year) + 1), by = 2)) +
  #ggtitle("Biomass of surveyed areas for Southeast Alaska red king crab") + 
  ylab("Biomass (lb)") + 
  theme(plot.title = element_text(hjust =0.5)) +
  theme(legend.position = c(0.825,0.793), legend.title = element_text(size = 9), 
        legend.text = element_text(size = 14), axis.text.x = element_text(angle = 45), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 14)) +
  theme(axis.text.x = element_text(vjust = 0.50)) +
  geom_text(data = reg_baseline_MR, aes(x = st_yr, y = pounds, label = label), 
            hjust = -0.05, vjust = 1.5, nudge_y = 0.05, size = 3.5) +
  ggsave(paste0('./figures/rkc/', cur_yr, '/MRregional_biomass2_', cur_yr, '.png'), dpi = 800, width = 7.5, height = 5.5)


### mark-recap regional plan ---------
reg_baseline_MR %>% 
  filter(type == "adj.mature.base") %>% 
  mutate(fifty = pounds*.5, twenty = pounds*.2) %>% 
  as.data.frame() -> MATURE_reg_base_MR_2

str_yr = 1993
end_yr = 2007

regional.b %>% 
  select(Year, adj_legal, adj_mature, status) %>%
  gather(type, pounds, adj_legal:adj_mature, factor_key = TRUE) %>%
  mutate(status = replace(status, which(status == "TBD"), "closed")) %>% # can replace the TBD with open or closed
  ggplot(aes(Year, pounds, group = type)) +
  geom_line(aes(colour = type, group = type, linetype = type))+
  geom_point(aes(colour = type, shape = status, fill = type), size =3) +
  scale_colour_manual(name = "", values = c("black", "grey60", "black", "grey60"), 
                      labels = c("Legal Biomass", "Mature Biomass"))+
  scale_shape_manual(name = "Fishery Status", values = c(25, 21, 8))+
  scale_linetype_manual(name = "", values = c("solid", "dashed", "solid", "dashed"), 
                        guide = FALSE) +
  scale_fill_manual(name = "", values = c("black", "gray75"), 
                    guide = FALSE) +
  scale_y_continuous(labels = comma, limits = c(0,(max(regional.b$adj_mature,
                                                       na.rm = TRUE) + 100000)),
                     breaks= seq(min(0), max(max(regional.b$adj_mature, na.rm = TRUE) +100000), 
                                 by = 500000)) +
  scale_x_continuous(breaks = seq(min(1975),max(max(regional.b$Year) + 1), by = 2)) +
  ylab("Biomass (lb)") + 
  theme(plot.title = element_text(hjust =0.5)) +
  theme(legend.position = c(0.825,0.793), legend.title = element_text(size = 9), 
        legend.text = element_text(size = 14), axis.text.x = element_text(angle = 45), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 14)) +
  #ggtitle("Biomass of surveyed areas for Southeast Alaska red king crab") + 
  theme(axis.text.x = element_text(vjust = 0.50)) -> p1
  
p1 + geom_hline(yintercept = MATURE_reg_base_MR_2$pounds, lwd = 0.5, colour = "green4") +
  geom_text(aes((str_yr + 7), MATURE_reg_base_MR_2$pounds, 
                label = paste0("Mature Target Reference Point (avg ", str_yr, "-", end_yr, ")"), vjust = -1, hjust = 0.05)) +
  
  geom_hline(yintercept = MATURE_reg_base_MR_2$fifty, lwd = 0.5, linetype = "dashed",color = "darkorange1") +
  geom_text(aes((str_yr + 12), MATURE_reg_base_MR_2$fifty, 
                label = ("Trigger 50% of target)"), vjust = -1, hjust = 0.05)) +
  
  geom_hline(yintercept = MATURE_reg_base_MR_2$twenty, lwd = 0.5, color = "red") +
  geom_text(aes((str_yr - 5), MATURE_reg_base_MR_2$twenty, 
                label = ("Limit Reference Point 20% of target)"), vjust = -1, hjust = 0.05)) +
  geom_vline(xintercept = str_yr, linetype = "dashed") +
  geom_vline(xintercept = end_yr, linetype = "dashed") +
  annotate("rect", xmin = str_yr, xmax = end_yr, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "dodgerblue") -> fig1
  #geom_point(size = 3, color = "dodgerblue") 
ggsave('./figures/regional_levels.png', fig1,  
       dpi = 600, width = 10.5, height = 5.5)
  


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
