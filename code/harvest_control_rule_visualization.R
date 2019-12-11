# k.palof 
# katie.palof@alaska.gov

# 12-4-19

# load -------
source('./code/helper.R')

# HCR options setup --------
pct <- c(0:150)/100
hr <- rep(0, 151)

data <- data.frame(pct, hr)

# harvest rate slope options ----
# first slope (20, 0) (50,0.10)
# change in y over change in x
m <- (0.10-0)/(.50-.20) 
#y - 0 = m*(x - 20)
# y = mx - 20m
# second slope - (0.50, 0.10) (1.00, 0.20)
m2 <- (0.20-0.10)/(1.00-0.50)
# y - 0.10 = m2(x - 0.50)
# y = m2*x - 0.50*m2 + 0.10
# third slope - (0.50, 0.00) (1.00, 0.20)
m3 = (0.20-0)/(1.00-0.50)
# y - 0 = m3(x - 0.50)
# y  = m3*x - 0.50*m3


# harvest options -----------------
data %>% 
  mutate(option1 = ifelse(pct <=.20, 0, ifelse(pct >=1.00, .20, 0.10)), 
         option1 = ifelse(pct > .20 & pct <= .50, (m*pct - .20*m), option1), 
         option1 = ifelse(pct >0.50 & pct <1.00, (m2*pct - .50*m2 + 0.10), option1), 
         option2 = ifelse(pct < 0.50, 0, ifelse(pct >=1.00, .20, 0.10)), 
         option2 = ifelse(pct > 0.50 & pct <1.00, (m2*pct - .50*m2 + 0.10), option2), 
         option3 = ifelse(pct <= 0.50, 0, ifelse(pct >=1.00, 0.20, (m3*pct - 0.50*m3)))) %>% 
  gather("Harvest Strategy", "value", 3:5) %>% 
  ggplot(aes(pct, value, group = `Harvest Strategy`)) +
  geom_line(aes(colour = `Harvest Strategy`, group = `Harvest Strategy`, linetype = `Harvest Strategy`), lwd = 1)+
  scale_color_manual (values = c(cbPalette[1], "darkorchid4", cbPalette[3])) +
  scale_x_continuous(labels = scales::percent, breaks = seq(0, 1.5, 0.10)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.40)) +
  #xlab("Mature Biomass / Avg Mature Biomass") + 
  xlab(expression("Mature Male Biomass"[cur_yr]/"Mature Male Biomass"[average])) +
  ylab("Harvest Rate") +
  
  geom_vline(xintercept = 1.00, lwd = 0.75, colour = "green4") +
  geom_text(aes((1.00), 0.20, 
                label = paste0("Mature Target Reference Point"), vjust = -1, hjust = -0.05)) +
  geom_text(aes((1.15), 0.174, 
                label = paste0("(avg MMB 1993-2007)"), vjust = -1, hjust = 0.15)) +
  geom_vline(xintercept = 0.50, lwd = 0.75, linetype = "dashed",color = "darkorange1") +
  geom_text(aes((0.50), 0.17, 
                label = ("Trigger (50% of target)"), vjust = -1, hjust = -0.05)) +
  geom_vline(xintercept = 0.20, lwd = 0.75, color = "red") +
  geom_text(aes(0.20, 0.12, 
                label = ("Limit"), vjust = -1, hjust = 2.20)) +
  geom_text(aes(0.20, 0.115, 
                label = ("(20% of target)"), vjust = 0.10, hjust = 1.04)) -> hcr_fig
ggsave('./figures/harvest_control_rule_options.png', hcr_fig,  
       dpi = 600, width = 10.5, height = 5.5)

# HCR option 1 ------
# two slopes 

# first slope (20, 0) (50,0.10)
# change in y over change in x
m <- (0.10-0)/(.50-.20) 
#y - 0 = m*(x - 20)
# y = mx - 20m
# second slope - (0.50, 0.10) (1.00, 0.20)
m2 <- (0.20-0.10)/(1.00-0.50)
# y - 0.10 = m2(x - 0.50)
# y = m2*x - 0.50*m2 + 0.10

data %>% 
  mutate(hr1 = ifelse(pct <=.20, 0, ifelse(pct >=1.00, .20, 0.10))) %>% 
  mutate(hr1 = ifelse(pct > .20 & pct <=.50, (m*pct - .20*m), hr1)) %>% 
  mutate(hr1 = ifelse(pct >0.50 & pct <1.00, (m2*pct - .50*m2 + 0.10), hr1)) %>%
  ggplot(aes(pct, hr1)) +
  geom_line(lwd =1) +
  scale_x_continuous(labels = scales::percent) +
  xlab("Mature Biomass / Avg Mature Biomass") + 
  ylab("Harvest Rate")

# HCR option 2 ------
# one slope, no fishing less than 50% of the average 

# second slope - (0.50, 0.10) (1.00, 0.20)
m2 <- (0.20-0.10)/(1.00-0.50)
# y - 0.10 = m2(x - 0.50)
# y = m2*x - 0.50*m2 + 0.10

data %>% 
  mutate(hr2 = ifelse(pct <.50, 0, ifelse(pct >=1.00, .20, 0.10))) %>% 
  mutate(hr2 = ifelse(pct >=0.50 & pct <1.00, (m2*pct - .50*m2 + 0.10), hr2)) %>% 
  ggplot(aes(pct, hr2)) +
  geom_line(lwd =1) +
  ylim(0, 0.50) +
  scale_x_continuous(labels = scales::percent) +
  xlab("Mature Biomass / Avg Mature Biomass") + 
  ylab("Harvest Rate")

