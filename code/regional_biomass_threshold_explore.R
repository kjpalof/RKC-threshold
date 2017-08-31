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

biomass <- read_excel(path = "./data/regional_biomass.xlsx")
