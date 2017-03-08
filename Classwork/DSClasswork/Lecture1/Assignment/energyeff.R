rm(list = ls())
SourceURL_Raw <- "https://raw.githubusercontent.com/StephenElston/DataScience350/master/Lecture1/EnergyEfficiencyData.csv"
energy.efficiency <- read.csv( SourceURL_Raw, header = TRUE)

require(ggplot2)
#install.packages("gridExtra")
require(gridExtra)

energy.efficiency$Orientation <- as.factor(energy.efficiency$Orientation)

levels(energy.efficiency$Orientation) <- c("North", "East", "South", "West")

energy.efficiency$Glazing.Area.Distribution <- as.factor(energy.efficiency$Glazing.Area.Distribution)

levels(energy.efficiency$Glazing.Area.Distribution) <- c("UnKnown", "Uniform", "North", "East", "South", "West")


energy.efficiency$Glazing.Area <- as.factor(energy.efficiency$Glazing.Area)

levels(energy.efficiency$Glazing.Area) <- c("0%", "10%", "25%", "40%")

ggplot(energy.efficiency, aes(x = Heating.Load , y = Cooling.Load)) + 
        geom_point( aes(col = factor(Overall.Height), size= Surface.Area), alpha= 0.3) +
        geom_density2d()+
        xlab('Heating Load') + 
        ylab('Cooling Load') + 
        ggtitle('Heat and Cold Load Comparison by Overall Height')



ggplot(energy.efficiency, aes(x = factor(Roof.Area), y = Heating.Load,  group = Surface.Area)) +
        geom_boxplot(aes(fill = factor(Wall.Area))) + 
        # geom_jitter(alpha= 0.3)+
        facet_grid(.~Overall.Height)+
        xlab('Roof Area') + 
        ylab('Heating Load') + 
        ggtitle('Heating Load analysis of Roof Area by Wall Area by Overall Height')


ggplot(energy.efficiency, aes(Heating.Load, Cooling.Load))+
  geom_point(aes(col = factor(Overall.Height)))+
  facet_grid(Orientation ~ Glazing.Area)+
  xlab('Heating Load') + 
  ylab('Cooling Load') + 
  ggtitle(' Heating and Cooling load distribution by Orientation and Glazing Area.')



ggplot(energy.efficiency, aes(x = Cooling.Load, y = Heating.Load, col = Surface.Area))+
  geom_point()+
  facet_grid(Overall.Height ~ Roof.Area) 


##Additional plots#########################################################################
p1 = ggplot( energy.efficiency, aes( Heating.Load, Surface.Area)) + stat_binhex(bins = 10)
p2 = ggplot( energy.efficiency, aes( Cooling.Load, Surface.Area)) + stat_binhex(bins = 10)
grid.arrange(p1, p2, nrow = 2)

ggplot(energy.efficiency, aes(Heating.Load, fill=Orientation))+geom_histogram()
ggplot(energy.efficiency, aes(Cooling.Load, fill=Glazing.Area.Distribution))+geom_histogram()


ggplot(energy.efficiency, aes(x = Cooling.Load, y = Heating.Load))+
  geom_point(aes(colour= Orientation))+
  facet_grid(Overall.Height ~ Roof.Area) 
#####################################

