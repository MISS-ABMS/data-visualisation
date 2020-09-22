#R Script language
#Example
#-------------------------------------------------------------------------
a <- 3 + 3 + 3
a

sqrt(a)

#Vectorial computation

A <- c(16,9,4)
A

mean(A)

sqrt(A)


#-------------------------------------------------------------------------------------------
# INSTALL PACKAGES & LOAD LIBRARIES --------------------------------------------------------
#-------------------------------------------------------------------------------------------

#go to Tools/install packages (only once on a computer)

#LOAD LIBRARIES
#each session
library(dplyr)
library(ggplot2)
library(tidyr)

# ------------------------------------------------------------------------
# R BASICS ---------------------------------------------------------------
# ------------------------------------------------------------------------

#Load data as dataframe (R table object)
simRes <- read.table(file= file.path("data","firemanModelSimResultsSmall.csv"),
           skip = 7,
           sep = ",",
           dec = ".",
           header=FALSE,
           stringsAsFactors = FALSE)

colnames(simRes) <- c("simID", "firemenSpeed", "nfiremen", "stay_in_forest", "communication",  "neighborood",
                      "setupSpace", "step", "nbFire", "nbForest")

simRes <- tbl_df(simRes) #Nice view of dataframes with dplyr

#---------------------------------------------------------------------------------------------------------------
# Choose variables and observations with dplyr -----------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------

# select variables
simRes <- select(simRes, - stay_in_forest, -communication, -neighborood, -setupSpace)

#choose observations (two ways to do the same thing, without cascade, and with cascade)
simTest <- filter(simRes, (firemenSpeed == 5) &
                    (nfiremen == 50) &
                    (step < 140))

simTest <- simRes %>% filter((firemenSpeed == 5) &
                               (nfiremen == 50) &
                               (step < 140))

#----------------------------------------------------------------------------------
# Dataframe related basic functions -----------------------------------------------
#----------------------------------------------------------------------------------
dim(simRes)
summary(simRes)

#plot distributions
hist(simRes$nbFire)
hist(simRes$nbForest)

#Distinguish Quantitavive variables (numeric) and Qualitative variables (factor)
#Character strings are qualitative variables
#Numbers can either been seen as quantitative variables or as qualitative variables

#Create factor variable from numeric variable:
#Example: Create a new column to see the number of firemen as a qualitative variable
simRes <- simRes %>% mutate(nfiremenFact = as.factor(nfiremen)) %>% #Mutate create a new variable from an exesting one
    mutate(firemenSpeedFact = as.factor(firemenSpeed)) %>%
    mutate(simIDf= as.factor(simID))

#The summary is different, it gives the number of occurence of each modality
summary(simRes)

#The ploting is different.
plot(nbForest~nfiremen, #Plot nb plot in forest versus the number of firemen
    data = filter(simRes, (step==500) & #For all observations of simRes dataframe where timeStep is 500 (end of simulation)
                (firemenSpeed == 5))) #And firemen speed is 5 times higer than fire speed.

plot(nbForest~nfiremenFact, #Plot nb plot in forest versus the number of firemen AS A QUALITATIVE VALUE
     data = filter(simRes, (step==500) & #For all observations of simRes dataframe where timeStep is 500 (end of simulation)
                     (firemenSpeed == 5)),main="5 firemen") #And firemen speed is 5 times higer than fire speed.
#Results are box-plot giving the distributions of quantitative values "nbForest" for each qualitative value of nbFiremen.


#---------------------------------------------------------------------------------------------------------------------
#Ploting with ggplot2 ------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------

#Specifying the graph
graph <- ggplot(data = filter(simRes, (firemenSpeed == 5) &
                                           (nfiremen == 50) &
                                           (step < 100)), #dataset to plot
                aes(x = step,  color = simIDf)) + #Relation between the graphical element (exple x, or color) and variables (step, simID)
        geom_line(aes(y = nbForest))+ #type of graph and eventualy supplementary variables (here, lines with y=nbFire)
  ggtitle("Forest plots with 50 firemen of speed 5")
graph #drawing the graph
#ggsave("trajectories-forestedPlots.pdf") #Saving the Graph
#NOTE that I used simIDf: a simulation Identifier defined as a qualitative value.

#Adding elements to the graph:
graph + geom_point(aes(y = nbForest),# (Add points with y=nbForest)
           size=0.5) #and fixed size= 0.5 (outside of the aes function)
#Note that the color characteristic represent the same variable for points and lines since the color is defined at the 
#general level of the graph. The blue line with a big and fast fire corresponds to the blue points with fast decreasing
#of forested plots.

# ATTENTION: if SimID, is not defined as a qualitative variable, ggplot will not distinguished a simID from another
# as different simulations but as a continum and thus will not be able to organise nice lines:

graph <- ggplot(data = filter(simRes, (firemenSpeed == 5) &
                                (nfiremen == 50) &
                                (step < 140)), #dataset to plot
                aes(x = step,  color = simIDf)) + #Relation between the graphical element (exple x, or color) and variables (step, simID)
  geom_line(aes(y = nbFire)) #type of graph and eventualy supplementary variables (here, lines with y=nbFire)
#drawing the graph
graph
#ggsave("trajectories-firePlots.pdf") #Saving the Graph


#----------------------------------------------------------------------------------------------------------------------
# Agregate data with dplyr --------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------

# Use "group by" to tell how to agregate your data
simResForest <- group_by(simRes, step, nfiremen, firemenSpeed) #Aggregate by timeStep, nb of firemen and speed of firemen

# THEN, use summarise to compute agregated data for these groups
avSimResForest <- summarise(simResForest, avNbFire = mean(nbFire), #compute the average plots in fire
                            avNbForest = mean(nbForest),  #compute the average plots in forest
                            standat_dev = sd(nbForest)) #compute the standart deviation of plots in forest

#----------------------------------------------------------------------------------------------------------------------
# Use facetgrid to display several graphics ---------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------

graph <- avSimResForest %>% #Take the average dataFrame
                    filter((nfiremen < 50) &  #choose only observations of simulations with less than 50 firemen
                                (firemenSpeed < 5) & #choose only observations of simulations speed less than 5
                                (step < 140)) %>% #choose only times steps until 140.
                    rename(number = nfiremen, speed = firemenSpeed) %>% #choose nicer names
  ggplot(aes(x = step, # give this dataframe (cascade) to the ggplot grah where x represent steps, 
             y = avNbForest, # y represents avNumber of forest,
             color=avNbFire)) + # color reprensent the average number of plots in fire
  geom_point(aes(size = standat_dev)) + # say that we plot it as "points" where the size represent the standart dispersion
  facet_grid(speed~number, labeller = label_both) + # Organise graphs in a grid so that a graph corresponds to a value of firemen speed and numer of firmemen
  scale_color_gradient(low="yellow",high="red") + #define a nice scale for color
  ggtitle("Effect of firemen number and firemen speed")
graph
#ggsave("Aggregated-trajectories-forestAndFire.pdf") #Saving the Graph

# Let's say that the normal situation is 10 firemen of speed 1. 
#We see in the top line that increasing the number of firemen has no effect.
#while we see in the left column that if we increase the speed, we can not save the forest (all the forest is burnt in all cases)
# but we can delay the fire and so maybe let time to people to leave the forest.
#Only if we increase the number of firemen and their speed, we can save some forest (for instance for speed= 2 and number=30)


#-------------------------------------------------------------------------------------------------------
# Use 3 dimensions graphics  ---------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------

#let's focus only on the forest saved:
graph <- avSimResForest %>% #Take the average dataFrame
  filter(step == 500) %>% #choose only the last time step.
  rename(number = nfiremen, speed = firemenSpeed) %>% #choose nicer names
  ggplot(aes(x = number, # give this dataframe (cascade) to the ggplot grah where x represent number of firemen, 
             y = speed)) + # y represents speed of firemen,
 geom_tile(aes(fill = avNbForest)) + # Color represent the patches of forest saved
  scale_fill_gradient2(high="green",mid = "grey",midpoint = 300, low="black") + # grey limit is for 300 forest patch saved
 stat_contour(aes(z=avNbForest), bins=20,colour="red") +
  ggtitle("Average number of saved forest plots \n according to firemen number and firemen speed")

graph
  
#ggsave("AverageForestSaved-nbMenVSspeed.pdf") #Saving the Graph

# -> NEED TO STAY IN THE GREEN IF YOU WANT GOOD RESULTS...

# BUT!It only tells you the expected value Maybe we'd better would like to avoid the worst!

#------------------------------------------------------
# Robust decision point of view -----------------------
#------------------------------------------------------

# Use "group by" to tell how to agregate your data
simResForest <- group_by(simRes, step, nfiremen, firemenSpeed) #Aggregate by timeStep, nb of firemen and speed of firemen

# THEN, use summarise to compute agregated data for these groups
worstSimResForest <- summarise(simResForest, maxNbFire = quantile(nbFire, 0.9), #compute the 90% worste fire (max)
                            minNbForest = quantile(nbForest,0.1),  #compute the 10% minimum forest
                            standat_dev = sd(nbForest)) #compute the standart deviation of plots in forest

#let's focus only on the forest saved:
graph <- worstSimResForest %>% #Take the average dataFrame
  filter(step == 500) %>% #choose only the last time step.
  rename(number = nfiremen, speed = firemenSpeed) %>% #choose nicer names
  ggplot(aes(x = number, # give this dataframe (cascade) to the ggplot grah where x represent number of firemen, 
             y = speed)) + # y represents speed of firemen,
  geom_tile(aes(fill = minNbForest)) + # Color represent the patches of forest saved
  scale_fill_gradient2(high="green",mid = "grey",midpoint = 300, low="black") + # grey limit is for 300 forest patch saved
  stat_contour(aes(z=minNbForest), bins=20,colour="red") +
  ggtitle("Minimal number of saved forest plots (90% confidence) \n according to firemen number and firemen speed")
graph

#ggsave("WorseScenario-nbMenVSspeed.pdf")

#In the world of our model (and given the value of other parameters and the initial fire choosen here), 
#if we stay in the green zone when we design the speed of the firemen and their number, we know that
# in at least 90% of the cases we will save more than 300 plots.

#Observe dispersion of results
graph <- simRes %>% #Take the average dataFrame
  filter(step == 500) %>% #choose only the last time step.
  rename(number = nfiremen, speed = firemenSpeed) %>% #choose nicer names
  ggplot() + # y represents speed of firemen,
  facet_grid(speed~number,switch= "both",as.table=FALSE, labeller =  label_both) + # give this dataframe (cascade) to the ggplot grah where x represent number of firemen, 
  geom_histogram(aes(x=nbForest)) + # Color represent the patches of forest saved
  scale_x_continuous(breaks=c(0,200,400))+
  ggtitle("Distributions of nb of forest plot saved \n for each coupe (firemen number,firemen speed )")
graph

#ggsave("Global-Picture.pdf")

