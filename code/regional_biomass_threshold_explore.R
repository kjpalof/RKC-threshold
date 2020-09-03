# K.Palof   8-31-17 / 12-3-19 / 6-17-2020 / 9-2-2020 
# Exploration of regional biomass estimates - historic from Sigma Plot file -  'RKC Regional biomass estiamtes 1979-prsent with COMM catch.JNB'
# Using regional estimates without Port Frederick, only 7 survey areas.
# current biomass estimates from each area use 2019 model output

# Objective: determine ideas for regional biological threshold for opening commericial fishery.

# Load -------------
source('./code/helper.R')

# Data ---------------
cur_yr <- 2020
#mr_adjust <- read.csv('./data/adj_final_stock_assessment.csv')
biomass <- read.csv("./data/biomass_2020.csv")  # updated with 2020 data
# file has current biomass estimates from each survey area for legal and mature biomass. 
#   Also has harvest - these all need to be updated from SE assessments 
fishery.status <- read.csv('C:/Users/kjpalof/Documents/SE_crab_assessments/data/rkc/Juneau/hind_fore_cast_JNU_current.csv')

#harvest <- read_excel(path = "./data/harvest.xlsx", sheet = 1)
# harvest is included in the biomass file - this is done during the RKC assessment

## commercial fishery status -----------------
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
# write.csv(regional.b, paste0('./results/rkc/Region1/', cur_yr, '/regional_biomass_', cur_yr, '.csv'))
#     use these values for table A1 in stock health document 


# baseline -------
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


## Figures for documents ------------------------------
# Figure 2 **CLOSED** regional biomass M/R adjusted biomass---------
# should have current year's model with longterm baselines (1993-2007) and closure status. 
#   also show current year's forecast as distinct from model output
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
  ggsave(paste0('./figures/', cur_yr, '/MRregional_biomass2_', cur_yr, '.png'), dpi = 800, width = 7.5, height = 5.5)


### mark-recap regional biological threhold plan  ---------
# uses mark-recaputure estimates and current baseline years 1993 to 2007
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
ggsave(paste0('./figures/', cur_yr,'/regional_levels1_', cur_yr, '.png'), fig1,  
       dpi = 600, width = 10.5, height = 5.5)
  


### option 2 mark-recap regional biological threhold plan  ---------
# uses mark-recaputure estimates and all years -1, so 1979 to 2018
# all years NOT baseline years

regional.b %>% 
  #filter(Year >= 1993 & Year <= 2007) %>% 
  summarise(legal_baseline = mean(legal), mature_baseline = mean(mature), 
            adj.legal_base = mean(adj_legal), adj.mature.base = mean(adj_mature)) %>% 
  as.data.frame() %>% 
  gather(type, pounds, factor_key = TRUE) %>% 
  mutate(st_yr = 2007, label = c("CSA Legal (1993-2007)", "CSA Mature (1993-2007)", 
                                 "Legal (1993-2007)", "Mature (1993-2007)")) ->reg_baseline

reg_baseline[1:2, ] ->  reg_baseline_CSA
reg_baseline[3:4, ] ->  reg_baseline_MR

reg_baseline_MR %>% 
  filter(type == "adj.mature.base") %>% 
  mutate(fifty = pounds*.5, twenty = pounds*.2) %>% 
  as.data.frame() -> MATURE_reg_base_MR_2
# update to include all years -1
str_yr = 1979
end_yr = 2019

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
  geom_text(aes((str_yr + 22), MATURE_reg_base_MR_2$pounds, 
                label = paste0("Mature Target Reference Point (avg ", str_yr, "-", end_yr, ")"), vjust = -1, hjust = 0.05)) +
  
  geom_hline(yintercept = MATURE_reg_base_MR_2$fifty, lwd = 0.5, linetype = "dashed",color = "darkorange1") +
  geom_text(aes((str_yr + 12), MATURE_reg_base_MR_2$fifty, 
                label = ("Trigger 50% of target)"), vjust = -1, hjust = 0.05)) +
  
  geom_hline(yintercept = MATURE_reg_base_MR_2$twenty, lwd = 0.5, color = "red") +
  geom_text(aes((str_yr - 2), MATURE_reg_base_MR_2$twenty, 
                label = ("Limit Reference Point 20% of target)"), vjust = -1, hjust = 0.05)) +
  geom_vline(xintercept = str_yr, linetype = "dashed") +
  geom_vline(xintercept = end_yr, linetype = "dashed") +
  annotate("rect", xmin = str_yr, xmax = end_yr, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "dodgerblue") -> fig1
#geom_point(size = 3, color = "dodgerblue") 
ggsave(paste0('./figures/', cur_yr, '/regional_levels2_', cur_yr, '.png'), fig1,  
       dpi = 600, width = 10.5, height = 5.5)


### harvest with mark-recap regional biological threhold plan  ---------
# uses mark-recaputure estimates and current baseline years 1993 to 2007
reg_baseline_MR %>% 
  filter(type == "adj.mature.base") %>% 
  mutate(fifty = pounds*.5, twenty = pounds*.2) %>% 
  as.data.frame() -> MATURE_reg_base_MR_2

# if including harvest data - run harvest.R first to get a file for here
harvest_r <- read.csv("./results/harvest_by_year.csv") 
harvest_r %>% 
  select(Year = year, lbs) -> harvest_r

regional.b %>% 
  right_join(harvest_r) -> regional.b2

str_yr = 1993
end_yr = 2007

regional.b2 %>% 
  filter(Year <= 2019) %>% 
  select(Year, adj_legal, adj_mature, status, lbs) %>%
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
  annotate("rect", xmin = str_yr, xmax = end_yr, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "dodgerblue") +
  geom_col(aes(x = Year, y = lbs), color = "grey", fill = "grey45")-> fig1
#geom_point(size = 3, color = "dodgerblue") 
ggsave(paste0('./figures/', cur_yr, '/harvest_w_regional_levels1_', cur_yr, '.png'), fig1,  
       dpi = 600, width = 10.5, height = 5.5)


### Option 3: mark-recap regional biological threhold plan  ---------
# uses mark-recaputure estimates and current baseline years 1993 to 2007 - minus high outliers - 94, 95 ,96
good_yr <- c(1993, 1997:2007)

regional.b %>% 
  filter(Year >= 1993 & Year <= 2007) %>% 
  filter(Year %in% good_yr) %>% # remove those that could be outliers
  summarise(legal_baseline = mean(legal), mature_baseline = mean(mature), 
            adj.legal_base = mean(adj_legal), adj.mature.base = mean(adj_mature)) %>% 
  as.data.frame() %>% 
  gather(type, pounds, factor_key = TRUE) %>% 
  mutate(st_yr = 2007, label = c("CSA Legal (1993, 1997-2007)", "CSA Mature (1993, 1997-2007)", 
                                 "Legal (1993, 1997-2007)", "Mature (1993, 1997-2007)")) ->reg_baseline

reg_baseline[1:2, ] ->  reg_baseline_CSA
reg_baseline[3:4, ] ->  reg_baseline_MR

reg_baseline_MR %>% 
  filter(type == "adj.mature.base") %>% 
  mutate(fifty = pounds*.5, twenty = pounds*.2) %>% 
  as.data.frame() -> MATURE_reg_base_MR_2
# update to include all years -1
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
  geom_text(aes((str_yr + 11), MATURE_reg_base_MR_2$pounds, 
                label = paste0("Mature Target Ref Point (avg ", str_yr, ", 1997 -", end_yr, ")"), vjust = -1, hjust = 0.05)) +
  
  geom_hline(yintercept = MATURE_reg_base_MR_2$fifty, lwd = 0.5, linetype = "dashed",color = "darkorange1") +
  geom_text(aes((str_yr + 15), MATURE_reg_base_MR_2$fifty, 
                label = ("Trigger 50% of target)"), vjust = -1, hjust = 0.05)) +
  
  geom_hline(yintercept = MATURE_reg_base_MR_2$twenty, lwd = 0.5, color = "red") +
  geom_text(aes((str_yr - 2), MATURE_reg_base_MR_2$twenty, 
                label = ("Limit Reference Point 20% of target)"), vjust = -1, hjust = 0.05)) +
  geom_vline(xintercept = str_yr, linetype = "dashed") +
  geom_vline(xintercept = end_yr, linetype = "dashed") +
  annotate("rect", xmin = str_yr, xmax = end_yr, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "dodgerblue") -> fig1
#geom_point(size = 3, color = "dodgerblue") 
ggsave(paste0('./figures/', cur_yr, '/regional_levels3_', cur_yr, '.png'), fig1,  
       dpi = 600, width = 10.5, height = 5.5)



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
