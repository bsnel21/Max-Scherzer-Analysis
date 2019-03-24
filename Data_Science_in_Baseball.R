#-----------------
#Comparing Max Scherzer's 2016 Cy Young season to his 2011 season
#By: Brandon Nelson
#-----------------


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#               DATA GATHERING

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Read in the files
max16og <- read.csv('~/Documents/Senior/SeniorSeminar/Max16.csv', header=T)
max11og <- read.csv('~/Documents/Senior/SeniorSeminar/Max11.csv', header =T)
#-------------------------------------------
install.packages("ggplot2")
install.packages("dplyr")
install.packages("pitchRx")
install.packages("animation")

#Add Imports
require(ggplot2)
require(dplyr)
library(plyr)
require(pitchRx)
require(animation)

#---------------------------------------------

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#               DATA CLEANING

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Removing unidentified pitch types, pitch outs, and intentional balls
max11 = max11og[-c(2310, 2087,1609), ]
max11 = max11[-2310,] #Don't know why this row wasn't getting removed from line above???

max16 = max16og[-3171,]

head(max11)


#---------------------------------------------


#Create new variables for each pitch type in both seasons
fast16 <- subset(max16, mlbam_pitch_name %in% c("FF")) 
two16 <- subset(max16, mlbam_pitch_name %in% c("FT")) 
slider16 <- subset(max16, mlbam_pitch_name %in% c("SL")) 
cutter16 <- subset(max16, mlbam_pitch_name %in% c("FC"))
change16 <- subset(max16, mlbam_pitch_name %in% c("CH")) 
curve16 <- subset(max16, mlbam_pitch_name %in% c("CU")) 
offspeed16 <- subset(max16, mlbam_pitch_name %in% c("CU", "CH", "SL")) 

fast11 <- subset(max11, mlbam_pitch_name %in% c("FF")) 
two11 <- subset(max11, mlbam_pitch_name %in% c("FT")) 
slider11 <- subset(max11, mlbam_pitch_name %in% c("SL")) 
cutter11 <- subset(max11, mlbam_pitch_name %in% c("FC"))
change11 <- subset(max11, mlbam_pitch_name %in% c("CH")) 
curve11 <- subset(max11, mlbam_pitch_name %in% c("CU")) 
offspeed11 <- subset(max11, mlbam_pitch_name %in% c("CU", "CH", "SL")) 

#Ahead / Behind in count
behind_count11 = with(max11, ifelse(bs_count == "1-0" | bs_count == "2-0" | bs_count == "3-0" | bs_count == "2-1" | bs_count == "3-1", TRUE, FALSE))
cbind(max11, behind_count11) -> max11
#max11 = data.frame(max11, behind_count11) #Alternate way of adding column onto df

behind_count16 = with(max16, ifelse(bs_count == "1-0" | bs_count == "2-0" | bs_count == "3-0" | bs_count == "2-1" | bs_count == "3-1", TRUE, FALSE))
cbind(max16, behind_count16 ) -> max16

#---------------------------------------------------------



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#               BASIC ANALYSIS

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Basic Plot of pitch locations for 2016 to make sure imports were correct

#Create Strike Zone based off a 6'2" batter
topKzone <- 3.5
botKzone <- 1.6
inKzone <- -0.95
outKzone <- 0.95
kZone <- data.frame(
  x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
  y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
)

#2016
print(ggplot(max16, aes(px, pz, color=stand)) + geom_point() +
        geom_path(aes(x, y), data=kZone, lwd=2, col="red") +
        ylim(0, 5) + facet_wrap(~ stand, ncol=1))

#2011
print(ggplot(max11, aes(px, pz, color=stand)) + geom_point() +
        geom_path(aes(x, y), data=kZone, lwd=2, col="red") +
        ylim(0, 5) + facet_wrap(~ stand, ncol=1))



#---------------------------------------------------------

#Histogram comparing Fastball Velocity
hist(fast16$start_speed, col="#00009950", xlim=c(89,99), ylim=c(0,450), main="Histogram of Fastball Velocity", xlab="Velocity")
hist(fast11$start_speed, col="#99000050", add=T)
abline(v = mean(fast16$start_speed), col = "#00009950", lwd = 2)
abline(v = mean(fast11$start_speed), col = "#99000050", lwd = 2)
legend("topright", c("2016", "2011"), col=c("#00009950", "#99000050"), ncol=1, cex=.75, lwd=3)
box()


#Histogram comparing Fastball Spin Rate (RPM)
hist(fast16$tm_spin, col="#00009950",ylim=c(0,450), main="Histogram of Spin Rate", xlab="Spin Rate (rpm)")
hist(fast11$tm_spin, col="#99000050", add=T)
abline(v = mean(fast16$tm_spin), col = "#00009950", lwd = 2)
abline(v = mean(fast11$tm_spin), col = "#99000050", lwd = 2)
legend("topright", c("2016", "2011"), col=c("#00009950", "#99000050"), ncol=1, cex=.75, lwd=3)
box()
 # Conclusion:  Although, Scherzer's fastball velocity did not increase much in 2016,
 #             his spin rate increased by nearly 300RPM, making his fastball a lot more decieving.
              #In the future I need to check if this affected his swing and miss percentage
#----------------------------------------------------------

#Histogram Comparing Slider Velocity
hist(slider16$start_speed, col="#00009950", xlim=c(80,90), ylim=c(0,450), main="Histogram of Slider Velocity", xlab="Velocity")
hist(slider11$start_speed, col="#99000050", add=T)
abline(v = mean(slider16$start_speed), col = "#00009950", lwd = 2)
abline(v = mean(slider11$start_speed), col = "#99000050", lwd = 2)
legend("topright", c("2016", "2011"), col=c("#00009950", "#99000050"), ncol=1, cex=.75, lwd=3)
box()

#Histogram Comparing Slider Spin Rates
hist(slider16$tm_spin, col="#00009950",ylim=c(0,450), xlim=c(0,1600), main="Histogram of Slider Spin Rates", xlab="Spin Rate (rpm)")
hist(slider11$tm_spin, col="#99000050", add=T)
abline(v = mean(slider16$tm_spin), col = "#00009950", lwd = 2)
abline(v = mean(slider11$tm_spin), col = "#99000050", lwd = 2)
legend("topright", c("2016", "2011"), col=c("#00009950", "#99000050"), ncol=1, cex=.75, lwd=3)
box()

# Conclusion: Scherzer's slider velocity had a pretty significant 2mph increase along with
# a spin rate increase of 35 rpms from 2011 to 2016.  In the future I need to check to see
# if this affected his swing and miss percentage.

#---------------------------------------------------------
#Ahead / Behind in count
behind_count11 = with(max11, ifelse(bs_count == "1-0" | bs_count == "2-0" | bs_count == "3-0" | bs_count == "2-1" | bs_count == "3-1", TRUE, FALSE))
cbind(max11, behind_count11) -> max11

#Finding swing and miss percentages in both years.
swingmiss16 <- subset(max16, pdes %in% c("Swinging Strike"))
contact16 <- subset(max16, pdes %in% c("Foul", "In play, out(s)", "In play, no out", "In play, run(s)", "Foul Tip"))
swing16 <- subset(max16, pdes %in% c("Foul", "In play, out(s)", "In play, no out", "In play, run(s)", "Foul Tip", "Swinging Strike"))

swingmiss11 <- subset(max11, pdes %in% c("Swinging Strike"))
contact11 <- subset(max11, pdes %in% c("Foul", "In play, out(s)", "In play, no out", "In play, run(s)", "Foul Tip"))
swing11 <- subset(max11, pdes %in% c("Foul", "In play, out(s)", "In play, no out", "In play, run(s)", "Foul Tip", "Swinging Strike"))

300/1456 #=.206 (2011)
523/1796 #= .291 (2016)


#For Fastballs only
fastmiss16 <- subset(fast16, pdes %in% c("Swinging Strike"))
fastswing16 <- subset(fast16, pdes %in% c("Foul", "In play, out(s)", "In play, no out", "In play, run(s)", "Foul Tip", "Swinging Strike"))
74/269 #=.275 (2016)

fastmiss11 <- subset(fast11, pdes %in% c("Swinging Strike"))
fastswing11 <- subset(fast11, pdes %in% c("Foul", "In play, out(s)", "In play, no out", "In play, run(s)", "Foul Tip", "Swinging Strike"))
126/786 #=.160 (2011)

#For Sliders only
slidermiss16 <- subset(slider16, pdes %in% c("Swinging Strike"))
sliderswing16 <- subset(slider16, pdes %in% c("Foul", "In play, out(s)", "In play, no out", "In play, run(s)", "Foul Tip", "Swinging Strike"))
231/514 #=.449 (2016)

slidermiss11 <- subset(slider11, pdes %in% c("Swinging Strike"))
sliderswing11 <- subset(slider11, pdes %in% c("Foul", "In play, out(s)", "In play, no out", "In play, run(s)", "Foul Tip", "Swinging Strike"))
95/270 #= .351 (2011)

#-------------------------------------------------------

#Pitch Selection vs LHB & RHB
pitch_type16 = with(max16, table(mlbam_pitch_name, stand)) #2016
pitch_type11 = with(max11, table(mlbam_pitch_name, stand)) #2011

#Remove Pitch Outs and Intentional Balls

pitch_type11 = pitch_type11[-5:-6,]
pitch_type16 = pitch_type16 [-6,]

pitch_type_bat16 <- table(max16$mlbam_pitch_name, max16$stand)
pitch_type_bat16 = pitch_type_bat16[-6,]
pitch_type_bat_prop16 <- round(prop.table(pitch_type_bat16, margin = 2), digit=3)
 #Proportions 2016
pitch_type_bat_prop16

pitch_type_bat11 <- table(max11$mlbam_pitch_name, max11$stand)
pitch_type_bat11 = pitch_type_bat11[-5:-6,]
pitch_type_bat_prop11 <- round(prop.table(pitch_type_bat11, margin = 2), digit=3)
#Proportions 2011
pitch_type_bat_prop11

pitch_type_bat_diff = pitch_type_bat_prop16 - pitch_type_bat_prop11
pitch_type_bat_diff #difference (2016 - 2011)


# Conclusion: The biggest change is that Scherzer added a curveball to his arsenal, and it made a large
# impact on the way he attacked LHB.  In 2011, a LHB only had to respect a fastball and change,
# but in 2016, they had to be aware of his cutter and curve as well.  His 2 go to pitches
# against RHBs were the slider and fastball in both 2011 and 2016, however, in 2016 he threw 
# his slider about 8% more often and his fastball about 10% less.

#----------------------------

#Pitch Selection in a given inning
type_inn16 <- with(max16, table(mlbam_pitch_name, inning))
type_inn_prop16 <- round(prop.table(type_inn16, margin = 2), digit=3)
type_inn_prop16 =  type_inn_prop16[-6,] #Proportion of pitches thrown in inning (2016)
type_inn_prop16

type_inn11 <- with(max11, table(mlbam_pitch_name, inning))
type_inn_prop11 <- round(prop.table(type_inn11, margin = 2), digit=3)
type_inn_prop11 = type_inn_prop11[-5:-6,] #Proportion of pitches thrown in inning (2011)
type_inn_prop11

type_inn_diff = type_inn_prop16[,-9] - type_inn_prop11 #Have to remove 9th inning because no CG in 2011
type_inn_diff # Difference = 2016 - 2011


# Conclusion: In 2016, his fastball usage would steadily decrease as the game progressed,
# but in 2011 his fastball usage would stagger in the middle/late innings.  In 2016, he used his 
# slider a lot more as the game went on, while in 2011, once again, his slider use was
# more inconsistent.  The biggest change I see with his slider is that in the 8th inning in 2016
# he threw it 34% of the time, while in 8th innings in 2011, he only threw it 9% of the time.
# The biggest jupmp in usage during the middle innings seems to be his change up in both years.


#--------------------
#Finding what pitches were thrown in each count, and how often
#2016
bs_table16 <- table(max16$balls, max16$strikes)
bs_prop_table16 <- round((prop.table(bs_table16)), digit=3)
max16$bs_count <- paste(max16$balls, max16$strikes, sep = "-") #create new column in df
bs_count_tab16 <- table(max16$bs_count)

type_bs16 <- with(max16, table(mlbam_pitch_name, bs_count))
type_bs16 = type_bs16[-6,] #number of times each pitch was thrown in each count
type_bs16

type_bs_prop16 <- round(prop.table(type_bs16, margin = 2), digit = 3)
type_bs_prop16 #Percentage of pitches thrown in each count

#2011
bs_table11 <- with(max11, table(balls, strikes))
bs_prop_table11 <- round((prop.table(bs_table11)), digit=3)
max11$bs_count <- paste(max11$balls, max11$strikes, sep = "-")
bs_count_tab11 <- table(max11$bs_count)
type_bs11 <- with(max11, table(mlbam_pitch_name, bs_count))
type_bs11 = type_bs11[-5:-6,] #number of times each pitch was thrown in each count
type_bs11

type_bs_prop11 <- round(prop.table(type_bs11, margin = 2), digit = 3)
type_bs_prop11 #Percentage of pitches thrown in each count

#Difference
type_bs_diff = type_bs_prop16 - type_bs_prop11
type_bs_diff

# Conclusion: The main thing that sticks out to me is that Scherzer trusted his slider
# when he was behind in the count a lot more in 2016.  He also tended to use his fastball 
# a lot less in nearly every count in 2016 compared to 2011, except for 0-1 and 3-0.  Overall,
# he started throwing his fastball a lot less often in 2016, despite it being the same velocity
# and having a better spin rate.

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#               MACHINE LEARNING

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#-------------------------------------------------------

max16_lhb <- subset(max16, stand == "L") # lhb
max16_rhb <- subset(max16, stand == "R") # rhb

#---------------------------------------------------

#Regression Supervised Machine Learning Algorithm
install.packages("caTools")
library(caTools)

split_fast_speed11 <- sample.split(fast11$start_speed, SplitRatio = .65)
subset(fast11, split_fast_speed11==T) -> train
subset(fast11, split_fast_speed11==F) -> test

lm(start_speed~tm_spin ,data = train) -> mod_regress1
predict(mod_regress1, test) -> result1
cbind(Actual=test$start_speed, Predicted = result1) -> final_data1
as.data.frame(final_data1) -> final_data1
View(final_data1)

(final_data1$Actual - final_data1$Predicted) -> error1
cbind(final_data1, error1) -> final_data1
rmse1 <- sqrt(mean(final_data1$error1^2))
rmse1 # 1.58

#--------

lm(start_speed~tm_spin+inning ,data = train) -> mod_regress2
predict(mod_regress2, test) -> result2
cbind(Actual=test$start_speed, Predicted = result2) -> final_data2
as.data.frame(final_data2) -> final_data2
#View(final_data2)

(final_data2$Actual - final_data2$Predicted) -> error2
cbind(final_data2, error2) -> final_data2
rmse2 <- sqrt(mean(final_data2$error2^2))
rmse2

#-----------------------------------------------
#Decision Tree (Regression)
install.packages("tree")
library(tree)
set.seed(1)
train = sample(1:nrow(max11), nrow(max11)/2)
test = - train
training_data = max11[train,]
testing_data = max11[test,]
testing_velo = with(max11, mlbam_pitch_name[test])


tree_model =  tree(mlbam_pitch_name~tm_spin+inning+bs_count, training_data)
tree_model
plot(tree_model)
text(tree_model, pretty = 0)

#Check how well model is doing using testing set
tree_pred = predict(tree_model, testing_data)
mean((tree_pred - testing_velo)^2)

#Cross Validations for pruning
cv_tree = cv.tree(tree_model)
names(cv_tree)
plot(cv_tree$size,
     cv_tree$dev,
     type="b",
     xlab = "Tree Size",
     ylab = "MSE")

which.min(cv_tree$dev) #gives index
cv_tree$size[1] #gives actual amount of branches neccessary for least error

#If pruning was necessary
#pruned_tree = prune.tree(tree_model, best = 4)


#-----------------------------------------------
pitches11 = max11$mlbam_pitch_name

#Decision Trees in R (Classification)
set.seed(2)
train = sample(1:nrow(max16), nrow(max16)/2)
test = - train
training_data = max16[train,]
testing_data = max16[test,]
Pitch = max16$mlbam_pitch_name
testing_ptype = Pitch[test]

#Fit the tree model using training data
tree_model = with(pitch_characteristics16, tree(Pitch~., training_data))
plot(tree_model)
#----------------------

cbind(Pitch=max16$mlbam_pitch_name, SpinRate = max16$tm_spin, SpinAxis = max16$spin, HorizontalSpinDeflect = max16$pfx_x, VerticalSpinDeflect = max16$pfx_z, Velocity = max16$start_speed) -> pitch_movement_table 
as.data.frame(pitch_movement_table) -> pitch_characteristics16
View(pitch_characteristics16)

#------
#Classification Algorithm
sample.split(max11$mlbam_pitch_name, SplitRatio = .65) -> split_values
subset(max11, split_values==T) -> train3
subset(max11, split_values==F) -> test3

install.packages("rpart")
library(rpart)
rpart(mlbam_pitch_name~start_speed+tm_spin+pfx_x+pfx_z, data = max11) -> mod3
predict(mod3, test3, type ="class") -> result3
prediction <- table(test3$mlbam_pitch_name, result3)
prediction[-5:-6,]

install.packages("caret")
library(caret)

confusionMatrix(table(test3$mlbam_pitch_name, result3))

#--------
#2016
sample.split(max16$mlbam_pitch_name, SplitRatio = .65) -> split_values
subset(max16, split_values==T) -> train4
subset(max16, split_values==F) -> test4

rpart(mlbam_pitch_name~start_speed+tm_spin+pfx_x+pfx_z, data = max16) -> mod4
predict(mod4, test4, type ="class") -> result4
table(test4$mlbam_pitch_name, result4)

confusionMatrix(table(test4$mlbam_pitch_name, result4))

#--------------------------------------------------------


#K Means Clustering
pitch_movement_df16 <- pitch_movement_df16[,-1]
as.matrix(pitch_movement_df16) ->pitch_movement_matrix16
kmeans(pitch_movement_matrix16, 5) -> pitch_move_cluster16
cbind(pitch_movement_df16, pitch_move_cluster16$cluster) -> clustered_data16
cbind(clustered_data16, max16$mlbam_pitch_name) -> final_cluster16
View(final_cluster16)

#------------------------------------------------------
# Comparing Velocity to Spin Rate for all pitches in 2011
g1 = ggplot(max11,aes(x =start_speed,y = tm_spin,color = mlbam_pitch_name)) + geom_point() + ggtitle("Velocity vs Spin Rate in 2011")
g1


#------------------------------------------------------

strikezone <- subset(fast16, pdes %in% c("Called Strike"))

#----------------------------------------------------

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#                  PITCHF/X

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#--------------------------------------------------


#Animation of given pitch
x <- list(
  facet_grid(pitcher_id ~ stand, labeller = label_both), 
  theme_bw(), 
  coord_equal()
)
animateFX(slider16, layer = x) #shows grid in plots. no animation

#Opens web browser + animation
#animation::saveHTML(animateFX(slider16, layer = x))

#---------------------------------------------------------

#Density plot of location vs LHB & RHB of given pitch

install.packages("viridis")
require(viridis)

strikeFX(slider16, geom = "tile") + 
  facet_grid(pitcher_id ~ stand) +
  coord_equal() +
  theme_bw() +
  viridis::scale_fill_viridis()


#---------------------------------------------------------
#Interactive 3D Plots

#install.packages("rgl")
#library(rgl) #Must Download xquartz.org
#interactiveFX(slider16)
