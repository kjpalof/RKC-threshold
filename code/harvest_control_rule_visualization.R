# k.palof 
# katie.palof@alaska.gov

# 12-4-19

# load -------
source('./code/helper.R')

# HCR options setup --------
pct <- c(0:150)/100
hr <- rep(0, 151)

data <- data.frame(pct, hr)


# harvest options -----------------
data %>% 
  mutate(option1 = ifelse(pct <=.20, 0, ifelse(pct >=1.00, .20, 0.10))) %>% 
  mutate(option1 = ifelse(pct > .20 & pct <=.50, (m*pct - .20*m), option1)) %>% 
  mutate(option1 = ifelse(pct >0.50 & pct <1.00, (m2*pct - .50*m2 + 0.10), option1)) %>%
  mutate(option2 = ifelse(pct <.50, 0, ifelse(pct >=1.00, .20, 0.10))) %>% 
  mutate(option2 = ifelse(pct >=0.50 & pct <1.00, (m2*pct - .50*m2 + 0.10), option2)) %>% 
  gather("type", "value", 3:4) %>% 
  ggplot(aes(pct, value, group = type)) +
  geom_line(aes(colour = type, group = type, linetype = type), lwd = 1)+
  ylim(0, 0.40) +
  scale_x_continuous(labels = scales::percent) +
  xlab("Mature Biomass / Avg Mature Biomass") + 
  ylab("Harvest Rate") -> hcr_fig
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

