##### Week 2 - Assignment 
##### Author: Tejovardhan Medamarti
##### Objective: Find 3 additional interesting data points thru visualization from the energy efficiency dataset.

rm(list = ls())
SourceURL_Raw <- "https://raw.githubusercontent.com/StephenElston/DataScience350/master/Lecture1/EnergyEfficiencyData.csv"
energy.efficiency <- read.csv( SourceURL_Raw, header = TRUE)

require(ggplot2)

##categorization of data.
energy.efficiency$Orientation <- as.factor(energy.efficiency$Orientation)

levels(energy.efficiency$Orientation) <- c("North", "East", "South", "West")

energy.efficiency$Glazing.Area.Distribution <- as.factor(energy.efficiency$Glazing.Area.Distribution)

levels(energy.efficiency$Glazing.Area.Distribution) <- c("UnKnown", "Uniform", "North", "East", "South", "West")


energy.efficiency$Glazing.Area <- as.factor(energy.efficiency$Glazing.Area)

levels(energy.efficiency$Glazing.Area) <- c("0%", "10%", "25%", "40%")


##visualization: 1
ggplot(energy.efficiency, aes(x = Cooling.Load, y = Heating.Load), alpha = 0.3)+
  geom_point(aes(colour = Roof.Area ))+
  facet_grid(Overall.Height + Glazing.Area ~ Surface.Area,  space = "free") +
  ggtitle("Load distribuiton of energy by Roof Area and Surface Area \n by Glazing Area and Overall Height")


## Visualization: 2(a)
ggplot(energy.efficiency, aes( Surface.Area, Roof.Area)) +
  geom_raster(aes(fill = Heating.Load), interpolate = TRUE) +
  scale_fill_gradient(low = "steelblue", high = "red")+
  facet_wrap(~Wall.Area, scales = "free" )+
  ggtitle('Measuring Heating Load distribution by Wall Area, Surface Area and Roof Area') +
  xlab('Surface Area') + ylab('Roof Area')

## Visualization: 2(b)
ggplot(energy.efficiency, aes(Surface.Area, Roof.Area)) +
  geom_raster(aes(fill = Cooling.Load), interpolate = TRUE) +
  scale_fill_gradient(low = "grey", high = "steelblue")+
  facet_wrap(~Wall.Area, scales = "free" )+
  ggtitle('Measuring Cooling Load distribution by Wall Area, Surface Area and Roof Area') +
  xlab('Surface Area') + ylab('Roof Area')


##Visualization: 3
energy.eff.sub70 <- energy.efficiency[ energy.efficiency$Overall.Height ==7.0,]
ggplot(energy.eff.sub70,
       aes(x = Cooling.Load, y = Heating.Load, group = factor(round(Wall.Area)), 
           size = Glazing.Area,
           shape = factor(round(Wall.Area)), bins = 40))+
  geom_point(aes(colour= factor(round(Surface.Area))), alpha = 0.3)+
  geom_smooth(method = "lm",se = TRUE )+
  facet_grid(~ Roof.Area) +
  ggtitle('Load efficiency by Roof Area, by Wall Area, \n by Surface Area and by Glazing Area') 
  
