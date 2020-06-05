

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Global script: has to be sourced in both server.R and ui.R
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# PACKAGE & DATA SOURCES -------------------------------------------------------

library(stats); 
library(plyr); 
library(magrittr); 
library(truncnorm);
library(RColorBrewer); 
library(sirad); 
library(tidyr); 
library(readr); 
library(lubridate); 
library(ggplot2); 
library(parcoords); 
library(LaplacesDemon);
library(mvtnorm);
library(shiny); 
library(shinyjs); 
library(shinyBS); 
library(htmlwidgets);
library(DT); 
library(shinythemes); 
library(shinydashboard); 
library(plotly); 
library(dplyr); 


#install.packages("shinythemes")

# FUNCTIONS --------------------------------------------------------------------

# Reservoir simulation model
source("reservoir_sim.R")

# expand grid with data frames
expand_grid_df <- function(...) {
  require(dplyr)
  Reduce(function(...) merge(..., by=NULL), list(...)) %>% as_data_frame()}

# Updated Hargreaves function (PET)
HARGREAVES <- function(date, tavg, tdif, lat) {
  
  #Inputs
  # Dates = vector of dates (as date object)
  # Tavg  = vector of average temperatures (DegC)
  # Tdif  = vector of mean temperatures (DegC)
  # Lat = Basin Latitute (north hemp +, south hemp -)
  
  #Extract years & months from the date object
  years_num <- length(unique(as.numeric(format(date,"%Y"))))
  months    <- as.numeric(format(date,"%m"))
  
  lookUp <- data.frame(
    m = 1:12,
    days.m = c(31,28,31,30,31,30,31,31,30,31,30,31),
    days.j = c(15,46,75,106,136,167,197,228,259,289,320,350))
  
  DaysInMonth <- lookUp$days.m[months]
  JulianDay <- lookUp$days.j[months]
  
  dr = (1+0.033*cos(2*pi/365*JulianDay))
  phi = pi/180*BasinLat
  delta = 0.409*sin((2*pi/365*JulianDay)-1.39)
  ws = acos(-tan(phi)*tan(delta))
  Rs = ((24*60/pi)*0.082*dr*(ws*sin(phi)*sin(delta)+cos(phi)*cos(delta)*sin(ws)))*0.408*DaysInMonth
  PET = 0.0023*Rs*(tavg + 17.8)*sqrt(tdif)
  
}

# ABCD Hydrologic model - 4 paramers (no snow!)
ABCD_QEST <- function(parm, P, PE, S_ini , G_ini, print.all = FALSE) {
  
  #parameters = a vector with a,b,c,d
  #P	= a vector with precip time series for current station
  #PE	= a vector with potential ET time series for current station
  #T	= a vector with tavg time series for current statio
  #Qobs	= a vector with observed streamflow time series for current station
  #Sint = initial soil moisture
  #Gint = initial groundwater storage
  #Aint = initial snow accumulation
  
  #MODEL PARAMETERS
  #a = runoff & recharge (when soil is under-saturated)    [0-1]
  #b = saturation level                                    [?14 - ?1900]
  #c = ratio of groundwater recharge to surface runoff     [0-1]
  #d = the rate of groundwater discharge                   [0-1]
  
  #Calibration period length
  final <- length(PE)
  
  W <- array(0,final)   	 #available water
  Y <- array(0,final)   	 #evapotranspiration opportunity
  S <- array(0,final)  	   #soil moisture
  E <- array(0,final)      #actual evaporation
  G <- array(0,final)  	   #groundwater storage
  Qest <- array(0,final)   #estimated surface runoff
  
  for (i in 1:final) {
    
    W[i] <- ifelse(i == 1, P[i] + S_ini, P[i] + S[i-1])
    
    #w1 and w2 are intermediate values used to calculate Y
    w1 <- (W[i]+parm[2])/(2*parm[1])
    w2 <- W[i]*parm[2]/parm[1]
    
    Y[i] <- w1 - sqrt((w1^2)-w2)
    S[i] <- Y[i]*exp(-1*PE[i]/parm[2])
    E[i] <- Y[i]*(1-exp(-1*(PE[i]/parm[2])))
    
    G[i] <- ifelse(i == 1,
                   (G_ini + parm[3]*round((W[i]-Y[i]),2))/(1+parm[4]),
                   (G[i-1] + parm[3]*round((W[i]-Y[i]),2))/(1+parm[4]))
    
    Qest[i] <- (1-parm[3])*round((W[i]-Y[i]),2)+parm[4]*G[i]
    
  }
  
  if(print.all == FALSE) {
    return(Qest) } else {return(list(Qest, S = S, G = G))}
}

multipleReplace <- function(x, what, by) {
  stopifnot(length(what)==length(by))
  ind <- match(x, what)
  ifelse(is.na(ind),x,by[ind])
}

# INPUT DATA -------------------------------------------------------------------

# Climate forcings
dataset <- "Princeton"

realizations <- read_csv(paste0("./data/realizations.csv"), progress = F) %>%
  filter(dataset == dataset)
  
# DATA TABLES
#CMIP5 <- read_csv("./data/data_CMIP5.csv")
CMIP5 <- read_csv("./data/CMIP5_deltaclim.csv") %>%
  mutate(del_prec = del_prec/100 + 1) %>%
  rename(Scenario = scenario)

parc_data <- read_csv("./data/LHS_output.csv", progress = FALSE)

surface_data <- read_csv("./data/stresstest_ffd.csv", progress = FALSE) %>%
  mutate(reliability = rel, creliability = crel) %>%
  mutate(demand = multipleReplace(demand, what=c(57, 68, 80, 91, 103, 114), 
                                   by=c(50, 80, 110, 140, 170, 200))) 

surface_data_mean <- surface_data %>%
  group_by(dataset, size, demand, temp, prec) %>%
  summarize(reliability = mean(reliability), creliability = mean(creliability),
    safeyield = mean(safeyield)) %>% mutate(nvar = 0)

surface_data %<>% bind_rows(surface_data_mean)



#Crop efficiency and Kc values for each crop type
crop_data <- read_csv("./data/crop_data_irrig_updated.csv", skip = 1)

# Monthly parameters for Penmann - Montheith calculation (fixed)
crop_pars_et0 <- read_csv("./data/crop_data_et0.csv", skip = 1)

Latitude = -4.20

#IRRIGATION CALCULATIONS ++++++++++++++++++++++
#Crop area & efficiency
crop_area <- crop_data %>%
  select(Season, Crop = c, Area_ha, Eff = Efficiency)

#Crop Kc coefficient calculation
crop_Kc <- crop_data %>%select(Season, Crop = c, Jun:May) %>%
  gather(key = Month, value = Kc, -Season, -Crop) %>%
  rowwise() %>%
  mutate(Month = which(Month == month.abb)) %>%
  mutate(Month = as.integer(Month))

# HYDROLOGY PARAMETERS +++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Hydrology parameters
abcd_pars <- data_frame(
  Princeton = c(7.01E-05,    283.515018,  0.931386386, 1.00E-06),
  CRU =       c(1.19E-05,    268.4657258, 0.897705472, 1.00E-06),
  GPCC =      c(0.000445674, 315.8826494, 0.905240881, 1.00E-06)
)

abcd_calib <- abcd_pars[[1]]

# STRESS TEST DESIGN +++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# This section defines the parameters used for the climate stress test
# Parameters: Annual DeltaT, Annual DeltaP, and climate variability

#Delta factors for climatic changes (full period changes)
nvar_num <- 10
DelP <- seq(0.7, 1.5, 0.1)
DelT <- seq(0,5,1)

#Look-up table for climate changes
cc_tbl  <- expand.grid(
  nTavg = 1:length(DelT), nPrec = 1:length(DelP)) %>%
  mutate(Tavg = DelT[nTavg], Prec = DelP[nPrec], nCC = 1:n()) %>%
  select(nCC, nTavg, nPrec, Tavg, Prec)

#Look-up table for climate (variability + change)
clim_tbl <- cc_tbl %>%
  expand_grid_df(., data.frame(nVar = 1:nvar_num)) %>%
  mutate(nClim = 1:n()) %>%
  select(nClim, nCC, nTavg, nPrec, nVar, Tavg, Prec)

#Graphical parameters
label <- list(
  x = expression("Temperature change (" * degree * C *")"),
  y = paste("Precipitation change (%)"))

tick <- list(x = seq(0,5,1), y = seq(0.7,1.5,0.1))
lim  <- list(x = c(-0.5,5.5), y = c(0.65,1.55))

font_size <- 14

theme_set(theme_bw(base_size = font_size))
theme_update(
  plot.title    = element_text(face = "bold"),
  panel.border  = element_rect(color = "black", fill = NA),
  legend.text   = element_text(size=font_size-2),
  legend.title  = element_text(size=font_size-2)
)

# SYSTEM PARAMETERS ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

BasinLat <- -3.6

#Net evaporation (m/month) (NET ET = ET - Precipitation)
netevap <- c(132, 168, 135, 44, -23, 51, 45, 74, 104, 77, 45, 88) * 1e-3

#RESERVOIR DESIGN ALTERNATIVES (Labels, Volume (MCM), PV_COST (MUSD)
designs <- data_frame(
  Labels = paste0(seq(40,140,20), "_MCM"),
  Volume = c(40, 60, 80, 100, 120, 140),
  PVCost = c(45.7778, 59.9176, 74.4364, 88.1147, 100.1927, 109.7938))

#RATING CURVE (From Tahal, 2013)
ratingc <- data_frame(
  elev = c(14,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,85.7,90,95),
  area = c(0,0.01,0.1,0.28,0.42,0.65,0.9,1.16,1.55,1.91,2.32,2.74,3.24,3.8,4.76,6.03,6.33,8.18,12.04),
  volume = c(0,0,0.24,1.14,2.85,5.49,9.35,14.47,21.21,29.84,40.39,53.02,67.95,85.52,106.86,133.77,138.1,169.29,219.8))

#Monthly irrigation demand estimates by CES, 2014
demand_mon <- data_frame(Month = as.factor(1:12),
                         irr = c(3.95,3.17,2.69,2.86,1.93,3.71,4.79,1.94,1.93,1.42,0.82,4.54),
                         env = c(0.83, 0, 0, 1.69, 1.49, 0.82, 0.76, 0.55, 0, 0, 0.4, 1.26),
                         dom = 80.3/12)

#Net evaporation (m/month) (NET ET = ET - Precipitation)
netevap <- c(132, 168, 135, 44, -23, 51, 45, 74, 104, 77, 45, 88) * 1e-3

#RESERVOIR DESIGN ALTERNATIVES (Labels, Volume (MCM), PV_COST (MUSD)
designs <- data_frame(
  Labels = paste0(seq(40,140,20), "_MCM"),
  Volume = c(40, 60, 80, 100, 120, 140),
  PVCost = c(45.7778, 59.9176, 74.4364, 88.1147, 100.1927, 109.7938))

#Monthly irrigation demand estimates by CES, 2014
demand_ces_mcm <- data_frame(Month = as.factor(1:12),
                             irr = c(3.95,3.17,2.69,2.86,1.93,3.71,4.79,1.94,1.93,1.42,0.82,4.54),
                             env = c(0.83, 0, 0, 1.69, 1.49, 0.82, 0.76, 0.55, 0, 0, 0.4, 1.26),
                             dom = 80.3/12)

#Monthly factors
mf <- demand_ces_mcm %>%
  mutate(irr = irr/sum(irr), eco = env / sum(env), dom = dom /sum(dom))

#Misc. conversions
cmd_to_mcmm <- 365/1e6
mcmm_to_cmd <- 1e6/365
month_coef <- as.numeric(days_in_month(1:12)/365)

#Calibration parameters
area <- 2250 #in square kilometers

#elevation = f(volume)
f_elev <- approxfun(x = ratingc$volume, y = ratingc$elev)
#volume = f(elevation)
f_vol  <- approxfun(x = ratingc$elev, y = ratingc$volume)
#area = f(volume)
f_sarea  <- approxfun(x = ratingc$volume, y = ratingc$area)

users <- c(1, 2, 3)
UsePriority <- c("Tar_eco", "Tar_dom", "Tar_irr")
