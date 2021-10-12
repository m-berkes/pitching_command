########### Libraries #######################################

# Function to simplify package install
# Requires 'packages' as array of strings using 'c'

check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# check dependencies and install if needed.
# Not all of these packages will be needed, but they cover a broad range of possible analyses and data viz we may want at some point.

packages<-c("ggplot2", "plyr", "dplyr", "tidyverse", "car", "DescTools", "plotrix", "lsr", "e1071", "patchwork", "RColorBrewer",
            "emmeans", "ggpubr", "fastmatch", "BayesianFirstAid", "extrafont", "MatchIt", "FactoMineR", "modelr", "infer",
            "interactions", "factoextra", "twang", "gbm", "survey", "lattice", "cobalt", "pitchRx", "ggalt")
check.packages(packages)

# # run this after installing new fonts on system
# font_import()
# loadfonts()

## needed for extrafont if saving graph to PDF
## Needed only on Windows - run once per R session
## Adjust the path to match your installation of Ghostscript - so obviously you also need to download Ghostscript
Sys.setenv(R_GSCMD = "D:/Program Files/gs9.26/bin/gswin32c.exe")


################# Set up #####################

# load file
pitchcommand <- read_csv(
  file = "fastballcommand.csv", # change location if needed
  col_names = TRUE, na = "NULL"
  )

# check how variables are coded
str(pitchcommand)

# set factors
pitchcommand$YearID <- factor(pitchcommand$YearID)
pitchcommand$PitcherID <- factor(pitchcommand$PitcherID)

# create new variable that determines pitch "status" - may not use, but possibly interesting
pitchcommand <- pitchcommand %>%
  mutate(Situation = if_else(pitchcommand$Balls == pitchcommand$Strikes, "Neutral",
         if_else(pitchcommand$Balls < pitchcommand$Strikes, "Ahead", "Behind")))

# examine range of values for continuous variables to see if they make sense
sapply(pitchcommand[ , 13:17], range, na.rm = TRUE) # if we keep NA values, then range reveals nothing

# use only complete cases
pitchclean <- pitchcommand[complete.cases(pitchcommand), ]






###### Descriptives and quick visual inspections #########

ddply(pitchclean, ~PitcherID+YearID+PitchType, summarise,
      Pitch_N = length(PitchType),
      Vel_mean = mean(Velocity, na.rm = TRUE),
      Vel_sd = sd(Velocity, na.rm = TRUE),
      VBreak_mean = mean(VerticalBreak, na.rm = TRUE),
      VBreak_sd = sd(VerticalBreak, na.rm = TRUE),
      HBreak_mean = mean(HorizontalBreak, na.rm = TRUE),
      HBreak_sd = sd(HorizontalBreak, na.rm = TRUE),
      PlateHeight_mean = mean(PlateLocHeight, na.rm = TRUE),
      PlateHeight_sd = sd(PlateLocHeight, na.rm = TRUE),
      PlateSide_mean = mean(PlateLocSide, na.rm = TRUE),
      PlateSide_sd = sd(PlateLocSide, na.rm = TRUE),
      Perc_bounce = (length(which(PlateLocHeight < 0))/length(PitchType))*100
      )


### Outlier removal per pitcher
# set the IQR factor for outlier analysis
IQR_factor <- 1.5

# The column output is TRUE if it is an outlier. This checks for height outliers
pitchclean <- pitchclean %>%
  group_by(PitcherID, PitchType) %>%
  mutate( (PlateLocHeight > (quantile(PlateLocHeight, .75) + IQR_factor*IQR(PlateLocHeight)))  |
          (PlateLocHeight < (quantile(PlateLocHeight, .25) - IQR_factor*IQR(PlateLocHeight)))
        )

# rename mutate boolean
names(pitchclean)[19] <- "HeightOutlier"

# This checks for horizontal outliers
pitchclean <- pitchclean %>%
  group_by(PitcherID, PitchType) %>%
  mutate( (PlateLocSide > (quantile(PlateLocSide, .75) + IQR_factor*IQR(PlateLocSide)))  |
          (PlateLocSide < (quantile(PlateLocSide, .25) - IQR_factor*IQR(PlateLocSide)))
        )

# rename mutate boolean
names(pitchclean)[20] <- "SideOutlier"


# Outliers check
ggplot(pitchclean, aes(x = PitchType, y = PlateLocHeight, fill = HeightOutlier)) +
  geom_dotplot(binaxis='y', binwidth = .15, stackdir='center', stackratio = 0.7, dotsize = 0.5, alpha = 0.5) +
  facet_wrap( ~ PitcherID)
  # looks like it works, but does not catch some bounced pitches from pitcher 2696

ggplot(pitchclean, aes(x = PitchType, y = PlateLocSide, fill = SideOutlier)) +
  geom_dotplot(binaxis='y', binwidth = .15, stackdir='center', stackratio = 0.7, dotsize = 0.5, alpha = 0.5) +
  facet_wrap( ~ PitcherID)
  # also looks pretty good
  

# Histograms with strike zone included
ggplot(pitchclean, aes(y = PlateLocHeight, colour = PitcherID)) +
  geom_histogram(fill = "white", alpha = 0.3, position = "identity") +
  geom_hline(yintercept = 1.6, size = 1) +
  geom_hline(yintercept = 3.5, size = 1)

ggplot(pitchclean, aes(x = PlateLocSide, colour = PitcherID)) +
  geom_histogram(fill = "white", alpha = 0.3, position = "identity") +
  geom_vline(xintercept = -0.95, size = 1) +
  geom_vline(xintercept = 0.95, size = 1)


# Density plots of pitch types by pitcher and year, with strike zone included
ggplot(pitchclean, aes(y = PlateLocHeight, colour = PitchType)) +
  geom_hline(yintercept = 1.6, size = 1) +
  geom_hline(yintercept = 3.5, size = 1) +
  facet_grid(PitcherID ~ YearID) +
  geom_density(fill = "white", alpha = 0.3, position = "identity")
 # everyone looks fairly consistent on height, but 114013's FA pitches tend to go higher after 2018. Same for 2696 in 2020

ggplot(pitchclean, aes(x = PlateLocSide, colour = PitchType)) +
  geom_vline(xintercept = -0.95, size = 1) +
  geom_vline(xintercept = 0.95, size = 1) +
  facet_grid(PitcherID ~ YearID) +
  geom_density(fill = "white", alpha = 0.3, position = "identity")
  # 857 looks to be most consistent across year and pitch, followed by 114013. 1594 switches pitch location between 2018 and 2019

# Density plots of pitchers by pitch type and year
ggplot(pitchclean, aes(y = PlateLocHeight, colour = PitcherID)) +
  geom_hline(yintercept = 1.6, size = 1) +
  geom_hline(yintercept = 3.5, size = 1) +
  facet_grid(PitchType ~ YearID) +
  geom_density(fill = "white", alpha = 0.3, position = "identity")

ggplot(pitchclean, aes(x = PlateLocSide, colour = PitcherID)) +
  geom_vline(xintercept = -0.95, size = 1) +
  geom_vline(xintercept = 0.95, size = 1) +
  facet_grid(PitchType ~ YearID) +
  geom_density(fill = "white", alpha = 0.3, position = "identity") 






####### Analyses ###########

# remove outliers
pitchclean$Outlier <- if_else(pitchclean$HeightOutlier == 'TRUE' | pitchclean$SideOutlier == 'TRUE', 1, 0)
cleandata <- subset(pitchclean, pitchclean$Outlier == 0)

# are any bounced pitches still included?
which(cleandata$PlateLocHeight < 0)
# # opted to leave in, but this would manually remove 3 bounced pitches that weren't caught in outlier removal
# cleandata <- cleandata[-c(2173, 2502, 11985), ]


# models
# model 1 examining pitch location by pitcher, pitch type, and year
mod1 <- lm(cbind(PlateLocSide, PlateLocHeight) ~ PitcherID + PitchType + YearID, data = cleandata)
summary(mod1)
Anova(mod1)
# check EMMs and pairwise comparisons of each main effect
emm.pitcher <- emmeans(mod1, pairwise ~ PitcherID, adjust = "tukey")
emm.pitcher
emm.pitcher$contrasts %>% confint() # shows confidence intervals of the contrasts

emmeans(mod1, pairwise ~ PitchType, adjust = "tukey")
emmeans(mod1, pairwise ~ YearID, adjust = "tukey")

# create emmGrid object, which we can then use for other functions
mod1.emm.pitcher <- emmeans(mod1, "PitcherID")

# this one gives a nice matrix display of comparisons, with EMMs on diagonal, p values in upper, and difference in lower
pwpm(mod1.emm.pitcher)
plot(mod1.emm.pitcher, comparisons = TRUE) # this nicely shows that pitcher 857 has least variability in their estimate
pwpp(mod1.emm.pitcher) # statistically, 857 and 2696 are not different!


#adjusted means. These are also given in lsmeans, but here does not have CI or pairwise comparisons.
mod1.rg <- ref_grid(mod1)
summary(mod1.rg)




# model 2 adds in pitch situation and velocity
mod2 <- lm(cbind(PlateLocSide, PlateLocHeight) ~ PitcherID + PitchType + YearID + Situation + Velocity, data = cleandata)
summary(mod2)
Anova(mod2)
emmeans(mod2, pairwise ~ PitcherID, adjust = "tukey")

#adjusted means of model 2
mod2.rg <- ref_grid(mod2)
summary(mod2.rg)

# comparison of two models
anova(mod2, mod1) # adding situation and velocity improves the model...but is it actually helpful? Or overly complicated?



# checking descriptives on cleaned data - compare to estimated means in summary(mod1.rg)
ddply(cleandata, ~PitcherID+YearID+PitchType, summarise,
      Pitch_N = length(PitchType),
      Vel_mean = mean(Velocity, na.rm = TRUE),
      Vel_sd = sd(Velocity, na.rm = TRUE),
      PlateHeight_mean = mean(PlateLocHeight, na.rm = TRUE),
      PlateHeight_sd = sd(PlateLocHeight, na.rm = TRUE),
      PlateHeight_med = median(PlateLocHeight, na.rm = TRUE),
      PlateSide_mean = mean(PlateLocSide, na.rm = TRUE),
      PlateSide_sd = sd(PlateLocSide, na.rm = TRUE),
      PlateSide_med = median(PlateLocSide, na.rm = TRUE)
      )





##### Visual Analyses and Graphs #######

## more density plots, quick and dirty inspection
# density plots of velocity by pitcher and pitch type
densityplot(~ Velocity | PitchType + PitcherID, data = cleandata, layout = c(2, 5), plot.points=F, lwd = 2, col = 'steelblue')

# density plots of horizontal plate location by pitcher and pitch type
densityplot(~ PlateLocSide | PitchType + PitcherID, data = cleandata, layout = c(2, 5), plot.points=F, lwd = 2, col = 'steelblue')

# density plots of vertical plate location by pitcher and pitch type
densityplot(~ PlateLocHeight | PitchType + PitcherID, data = cleandata, layout = c(2, 5), plot.points=F, lwd = 2, col = 'steelblue')


# graphing prep:

# create graph of strike zone and pitch locations
x <- c(-.95,.95,.95,-.95,-.95)
z <- c(1.6,1.6,3.5,3.5,1.6)

# store in dataframe
strikezone <- tibble(x,z) 

# create pitch descriptions for graphing
PitchDesc <- cleandata$PitchType

# search abbr. and change to full name
PitchDesc[which(PitchDesc == 'FA')] <- "Four-seam fastball"
PitchDesc[which(PitchDesc == 'SI')] <- "Two-seam fastball"

# Add column to the original dataset
cleandata$PitchDesc <- PitchDesc


# plot pitches by location, pitcher, and year
ggplot() +
  geom_path(data = strikezone, aes(x = x, y = z), size = 1) +
  coord_equal() +
  labs(x = "\n Distance from center of home plate\n (Feet)\n", y = "\n Height above home plate\n (Feet)\n") +
  geom_point(data = cleandata, aes(x = PlateLocSide, y = PlateLocHeight, colour = PitchDesc), alpha = 0.2) +
  scale_size(range = c(0.01, 2)) +
  facet_grid(PitcherID ~ YearID, switch = 'y') +
  xlim(-3, 3) + 
  ylim(0, 6) +
  labs(colour = "Pitch") +
  theme(text = element_text(family = "Segoe UI"),
        legend.title = element_text(size = 10, colour = "#08306B", face = 2),
        legend.text = element_text(size = 10, face = 2),
        legend.position = 'right',
        axis.title.x = element_text(colour = "#08306B", size = 12, face = 2),
        axis.title.y = element_text(colour = "#08306B", size = 12, face = 2),
        axis.text = element_text(colour="darkslategray", size = 10, face = 2),
        strip.text.x = element_text(size = 10, colour = "darkslateblue", face = "bold"),
        strip.text.y = element_text(size = 10, colour = "darkslateblue", face = "bold", angle = 180),
        strip.background = element_rect(fill = "grey90"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(linetype = 'dashed', colour = "grey80"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
        )


# comparable density plots for height and side
# density plot for height
ggplot(cleandata, aes(y = PlateLocHeight, colour = PitchDesc)) +
  labs(x = "\n Density\n", y = "\n Height above home plate\n (Feet)\n") +
  facet_grid(PitcherID ~ YearID, switch = 'y') +
  geom_density(fill = "white", alpha = 0.3, position = "identity", size = 1) +
  labs(colour = "Pitch") +
  xlim(0, 0.7) +
  ylim(0, 6) +
  geom_hline(yintercept = 1.6, size = 1) +
  geom_hline(yintercept = 3.5, size = 1) +
  theme(text = element_text(family = "Segoe UI"),
        legend.title = element_text(size = 10, colour = "#08306B", face = 2),
        legend.text = element_text(size = 10, face = 2),
        legend.position = 'right',
        axis.title.x = element_text(colour = "#08306B", size = 12, face = 2),
        axis.title.y = element_text(colour = "#08306B", size = 12, face = 2),
        axis.text = element_text(colour="darkslategray", size = 10, face = 2),
        strip.text.x = element_text(size = 10, colour = "darkslateblue", face = "bold"),
        strip.text.y = element_text(size = 10, colour = "darkslateblue", face = "bold", angle = 180),
        strip.background = element_rect(fill = "grey90"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(linetype = 'dashed', colour = "grey80"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
        )

# density plot for side
ggplot(cleandata, aes(x = PlateLocSide, colour = PitchDesc)) +
  labs(x = "\n Distance from center of home plate\n (Feet)\n", y = "\n Density\n") +
  facet_grid(PitcherID ~ YearID, switch = 'y') +
  geom_density(fill = "white", alpha = 0.3, position = "identity", size = 1) +
  labs(colour = "Pitch") +
  xlim(-3, 3) +
  ylim(0, 0.8) +
  geom_vline(xintercept = -0.95, size = 1) +
  geom_vline(xintercept = 0.95, size = 1) +
  theme(text = element_text(family = "Segoe UI"),
        legend.title = element_text(size = 10, colour = "#08306B", face = 2),
        legend.text = element_text(size = 10, face = 2),
        legend.position = 'right',
        axis.title.x = element_text(colour = "#08306B", size = 12, face = 2),
        axis.title.y = element_text(colour = "#08306B", size = 12, face = 2),
        axis.text = element_text(colour="darkslategray", size = 10, face = 2),
        strip.text.x = element_text(size = 10, colour = "darkslateblue", face = "bold"),
        strip.text.y = element_text(size = 10, colour = "darkslateblue", face = "bold", angle = 180),
        strip.background = element_rect(fill = "grey90"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(linetype = 'dashed', colour = "grey80"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
        )



