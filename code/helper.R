# R helper file for rkc se threshold work

# k.palof  katie.palof@alaska.gov  
# updated: 10-23-19

# load -----------
library(tidyverse)
library(lubridate)
library(readxl)
library(ggthemes)
library(gridExtra)
library(extrafont)
library(tidyr)
library(padr)
library(anytime)
library(RColorBrewer)
library(cowplot)
library(ggridges)
library(scales)

##THEMES FOR GRAPHS ---------
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')
          +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

#COLOR BLIND PALETTE --------------
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


### logbook data -------------
logbk_cpue <- function(str_yr, end_yr, mg_area, log_cpue, Lper1, Lper2, cur_yr){
  
  log_cpue %>% 
    group_by(mgt_area) %>% 
    filter(YEAR >= str_yr & YEAR <= end_yr) %>%
    summarise(mean = mean(cpue, na.rm = TRUE)) -> avg_ten 
  
  avg_ten %>% 
    filter(mgt_area == mg_area) %>% 
    mutate(fifty = mean*Lper1, twenty = mean*Lper2) -> avg_ten2
  
  log_cpue %>% 
    filter(mgt_area == mg_area) %>% 
    ggplot(aes(YEAR, cpue)) + 
    geom_line(lwd = 1) + 
    geom_hline(yintercept = avg_ten2$mean, lwd = 0.5, color = "green") +
    geom_text(aes((str_yr-10), avg_ten2$mean, 
                  label = paste0("Target Reference Point (avg ", str_yr, "-", end_yr, ")"), vjust = -1, hjust = 0.05)) +
    geom_hline(yintercept = avg_ten2$fifty, lwd = 0.5, linetype = "dashed",color = "orange") +
    geom_text(aes((str_yr-10), avg_ten2$fifty, 
                  label = paste0("Trigger (", Lper1*100, "% of target)"), vjust = -1, hjust = 0.05)) +
    geom_hline(yintercept = avg_ten2$twenty, lwd = 0.5, color = "red") +
    geom_text(aes((str_yr-10), avg_ten2$twenty, 
                  label = paste0("Limit Reference Point (", Lper2*100, "% of target)"), vjust = -1, hjust = 0.05)) +
    geom_vline(xintercept = str_yr, linetype = "dashed") +
    geom_vline(xintercept = end_yr, linetype = "dashed") +
    annotate("rect", xmin = str_yr, xmax = end_yr, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "dodgerblue") +
    geom_point(size = 3, color = "dodgerblue") + 
    expand_limits(y = 0) +
    ylab("Logbook CPUE (lbs/pot)") + 
    xlab("Year") + 
    ggtitle(paste0(mg_area, " logbook data")) -> fig1
  fig1
  ggsave(paste0('./figures/', cur_yr, '/', mg_area, ' logbook_cpue.png'), fig1,  
         dpi = 600, width = 10.5, height = 5.5)
}

