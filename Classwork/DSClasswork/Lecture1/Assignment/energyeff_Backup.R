rm(list=ls())
SourceURL_Raw<-"https://raw.githubusercontent.com/StephenElston/DataScience350/master/Lecture1/EnergyEfficiencyData.csv"
energy.efficiency <- read.csv(SourceURL_Raw, header = TRUE)
head(energy.efficiency)
names(energy.efficiency)
lapply(energy.efficiency, unique)
require(ggplot2)


energy.efficiency$Orientation<- as.factor(energy.efficiency$Orientation)

levels(energy.efficiency$Orientation)<-c("North", "East", "South", "West")

energy.efficiency$Glazing.Area.Distribution<- as.factor(energy.efficiency$Glazing.Area.Distribution)

levels(energy.efficiency$Glazing.Area.Distribution)<-c("UnKnown", "Uniform", "North", "East", "South", "West")



ggplot(energy.efficiency, aes(Cooling.Load))+geom_bar()
ggplot(energy.efficiency, aes(Heating.Load))+geom_bar()

ggplot(energy.efficiency, aes(Heating.Load, fill=Orientation))+geom_histogram()
ggplot(energy.efficiency, aes(Cooling.Load, fill=Glazing.Area.Distribution))+geom_histogram()



ggplot(energy.efficiency, aes(Cooling.Load,fill=Orientation)) + geom_density(adjust = 1/5)
ggplot(energy.efficiency, aes(Heating.Load, ..density.., colour = Orientation)) + geom_density(adjust = 1/5)

ggplot(energy.efficiency, aes(x=factor(Overall.Height), y= Heating.Load))+geom_boxplot()
ggplot(energy.efficiency, aes(x=factor(Overall.Height), y= Cooling.Load))+geom_boxplot()

ggplot(energy.efficiency, aes(x= factor(Orientation), Cooling.Load))+geom_violin(trim = FALSE, draw_quantiles = c(0.25,0.50, 0.75))


ggplot(energy.efficiency, aes(x=factor(Orientation), y=Cooling.Load))+geom_point()
ggplot(energy.efficiency, aes(x=Cooling.Load , y= Heating.Load))+geom_point()

##Keep it.
ggplot(energy.efficiency, aes(x=Heating.Load , y= Cooling.Load)) + 
        geom_point( aes(col=Wall.Area)) + 
        geom_density2d()


##Keep it
ggplot(energy.efficiency, aes(Heating.Load, Surface.Area))+stat_binhex(bins=10)


ggplot(energy.efficiency, aes(Heating.Load,Relative.Compactness ))+
        geom_point(aes(color=factor(Overall.Height),size=Surface.Area))

ggplot(energy.efficiency, aes(x=Cooling.Load , y= Heating.Load)) +
        geom_point(aes(col=Surface.Area,size= Roof.Area,  shape = factor(Overall.Height)), alpha= 0.3) + 
        geom_density2d()

install.packages("gridExtra")
install.packages("corrplot")  


ggplot(energy.efficiency, aes(x=Cooling.Load , y= Heating.Load)) +
        geom_point(aes(col=Surface.Area,  shape = factor(Overall.Height)), alpha= 0.3) + 
        geom_density2d()


library(corrplot)
corrplot(R, method="circle", type='lower')
ggplot(energy.efficiency, aes(Heating.Load, fill = Orientation))+geom_bar()

names(energy.efficiency)
##Keep it
ggplot(energy.efficiency, aes(x=Heating.Load, y= Surface.Area))+geom_point()+
        facet_grid(Overall.Height ~ Roof.Area) 


ggplot(energy.efficiency, aes(x=Cooling.Load, y= Heating.Load))+geom_point()+
        facet_grid(Overall.Height ~ Roof.Area) 

head(energy.efficiency, n=10)

ggplot(energy.efficiency, aes(Wall.Area)) + ## Specify the data frame and columns. Note the + chain operator
        geom_bar()  

ggplot(energy.efficiency, aes(Wall.Area)) + ## Specify the data frame and columns. Note the + chain operator
        geom_histogram()  

names(energy.efficiency)
ggplot(energy.efficiency, aes(Roof.Area)) + ## Specify the data frame and columns. Note the + chain operator
        geom_histogram()  

ggplot(energy.efficiency, aes(Cooling.Load, colour=Wall.Area)) + ## Specify the data frame and columns. Note the + chain operator
        geom_histogram() 


ggplot(energy.efficiency, aes(Wall.Area, Surface.Area, col= Relative.Compactness))+
        geom_point()


abc<-as.data.frame(cor(energy.efficiency))
plot(abc)

ggplot(abc, aes(Wall.Area, Surface.Area, col= Relative.Compactness))+
        geom_point()

ggplot(abc, aes(x=Heating.Load , y= Cooling.Load)) + 
        geom_point( aes(col=Wall.Area)) + 
        geom_density2d()


ggplot(abc, aes(Heating.Load, Surface.Area))+stat_binhex(bins=10)


ggplot(energy.efficiency, aes(x = factor(Roof.Area), y = Heating.Load, group =Surface.Area)) +
        geom_boxplot() + 
        facet_grid(.~Overall.Height)
xlab('Fuel type') + ggtitle('Price by Fuel Type')



lapply(energy.efficiency, unique)
names(energy.efficiency)

options(repr.plot.width=6, repr.plot.height=6)
require(gridExtra)
p1 = ggplot(energy.efficiency, aes(x = factor(Roof.Area), y = Heating.Load, group =Surface.Area)) +
        geom_boxplot() + 
        facet_grid(.~Overall.Height)
xlab('Fuel type') + ggtitle('Price by Fuel Type')

p2 = ggplot(energy.efficiency, aes(x = factor(Roof.Area), y = Cooling.Load, group =Surface.Area)) +
        geom_boxplot() + 
        facet_grid(.~Overall.Height)
xlab('Fuel type') + ggtitle('Price by Fuel Type')

grid.arrange(p1, p2, nrow = 2)



ggplot(energy.efficiency, aes(Heating.Load, Surface.Area))+stat_binhex(bins=10)



ggplot(energy.efficiency, aes(x = factor(Roof.Area), y = Heating.Load,  group =Surface.Area)) +
        geom_boxplot(aes(fill = factor(Wall.Area))) + 
        # geom_jitter()+
        facet_grid(.~Overall.Height)
xlab('Fuel type') + ggtitle('Price by Fuel Type')



#####################################

ggplot(energy.efficiency, aes(x = factor(Roof.Area), y = Heating.Load,  group =Surface.Area)) +
        geom_boxplot(aes(fill = factor(Wall.Area))) + 
        #        geom_jitter(alpha= 0.3)+
        facet_grid(.~Overall.Height)
xlab('Fuel type') + ggtitle('Price by Fuel Type')


ggplot(energy.efficiency, aes(x=Heating.Load , y= Cooling.Load)) + 
        geom_point( aes(col=factor(Overall.Height))) + 
        geom_density2d()



ggplot(energy.efficiency, aes(Heating.Load, Surface.Area))+stat_binhex(bins=10)
ggplot(energy.efficiency, aes(Cooling.Load, Surface.Area))+stat_binhex(bins=10)




ggplot(energy.efficiency, aes(x=Heating.Load, y= Surface.Area))+geom_point()+
        facet_grid(Overall.Height ~ Roof.Area) 


ggplot(energy.efficiency, aes(x = factor(Roof.Area), y = Heating.Load, group =Surface.Area)) +
        geom_boxplot() + 
        facet_grid(.~Overall.Height)
xlab('Fuel type') + ggtitle('Price by Fuel Type')



ggplot(energy.efficiency, aes(x=Surface.Area , y= Wall.Area)) + 
        geom_point( aes(col=Cooling.Load)) + 
        geom_density2d()

