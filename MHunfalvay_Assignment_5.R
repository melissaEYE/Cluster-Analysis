# Melissa Hunfalvay. 
# Last Revised: 8-3-2021
# Data 630
# Eye Tracking Dataset. 
# Assignment 5
# Professor Firdu

##########################

#Section 1: Preparation 

##########################

# Load packages
library("cluster")
library(ggplot2)
#install.packages("ggcorrplot")
library(ggcorrplot)

#Set working directory and read the data
setwd("/Users/melissahunfalvay/Documents/HUN/My Professional Development/Machine Learning Data 630/Assignments/Assignment 5") 
Three_MH_excel_cleaning<-read.csv("Three_MH_excel_cleaning.csv", header=T, sep = ",")

##############################################################

#Section 2: Data Exploration & Section 3: Data Pre-Processing

#############################################################

View(Three_MH_excel_cleaning) # view the data
head(Three_MH_excel_cleaning) # See first few rows of each variable
str(Three_MH_excel_cleaning) # string command
colnames(Three_MH_excel_cleaning) # Names of each column
ncol(Three_MH_excel_cleaning) # Number of columns
nrow(Three_MH_excel_cleaning) #Number of rows

# Remove variables: Phase 1
Three_MH_excel_cleaning$?..<-NULL
Three_MH_excel_cleaning$EYEQ_SCORE<-NULL
Three_MH_excel_cleaning$X<-NULL # Identifer
Three_MH_excel_cleaning$ASSESS_ID<-NULL # Identifer
Three_MH_excel_cleaning$CALIBRATION_pupilary_distance<-NULL # Testing parameter check
Three_MH_excel_cleaning$PARTICIPANT_ID<-NULL # Identifer
Three_MH_excel_cleaning$ETHNICITY<-NULL # Identifer
Three_MH_excel_cleaning$systemType_Generic60Cm<-NULL # Wrong eye tracker
Three_MH_excel_cleaning$systemType_I15<-NULL # Eye tracker i15 Identifier
#Three_MH_excel_cleaning$FIXATION_STABILITY_gaze_positions_lessthan2deg.both_avg<-NULL # Partial value
Three_MH_excel_cleaning$GENDER_male<-NULL # Don't need two separate columns for gender. Keep female. If 0 = male; if 1 = female 
Three_MH_excel_cleaning$HAND_both<-NULL # Not relevant for this analysis
Three_MH_excel_cleaning$HAND_left<-NULL # Not relevant for this analysis
Three_MH_excel_cleaning$HAND_right<-NULL # Not relevant for this analysis
Three_MH_excel_cleaning$HORIZONTAL_SACCADES_band2_under_left_side.both_avg <-NULL # Partial value
Three_MH_excel_cleaning$HORIZONTAL_SACCADES_band2_under_right_side.both_avg <-NULL # Partial value
Three_MH_excel_cleaning$HORIZONTAL_SACCADES_missed_over_left_side.both_avg <-NULL # Partial value
Three_MH_excel_cleaning$HORIZONTAL_SACCADES_missed_under_left_side.both_avg<-NULL # Partial value
Three_MH_excel_cleaning$HORIZONTAL_SACCADES_missed_under_right_side.both_avg<-NULL # Partial value
Three_MH_excel_cleaning$SPEM_gaze_radius_diff_1_diff.both_avg<-NULL # Partial value
Three_MH_excel_cleaning$SPEM_gaze_radius_diff_2_diff.both_avg<-NULL # Partial value
Three_MH_excel_cleaning$SPEM_gaze_radius_diff_3_diff.both_avg<-NULL # Partial value
Three_MH_excel_cleaning$SPEM_gaze_radius_diff_4_diff.both_avg<-NULL # Partial value
Three_MH_excel_cleaning$SPEM_gaze_radius_diff_5_diff.both_avg<-NULL # Partial value
Three_MH_excel_cleaning$SPEM_gaze_radius_diff_6_diff.both_avg<-NULL # Partial value
Three_MH_excel_cleaning$SPEM_gaze_radius_diff_7_diff.both_avg<-NULL # Partial value
Three_MH_excel_cleaning$SPEM_gaze_radius_diff_8_diff.both_avg<-NULL # Partial value
Three_MH_excel_cleaning$SPEM_V_eccentric_gaze_mean_bottom_side.both_avg<-NULL # Partial value
Three_MH_excel_cleaning$SPEM_V_eccentric_gaze_mean_middle.both_avg<-NULL # Partial value
Three_MH_excel_cleaning$SPEM_V_eccentric_gaze_mean_top_side.both_avg<-NULL # Partial value
Three_MH_excel_cleaning$SPEM_V_eccentric_gaze_variability_bottom_side.both_avg<-NULL # Partial value
Three_MH_excel_cleaning$SPEM_V_eccentric_gaze_variability_middle.both_avg<-NULL # Partial value
Three_MH_excel_cleaning$SPEM_V_eccentric_gaze_variability_top_side.both_avg<-NULL # Partial value
Three_MH_excel_cleaning$VERTICAL_SACCADES_band2_under_bottom_side.both_avg<-NULL # Partial value
Three_MH_excel_cleaning$VERTICAL_SACCADES_band3_over_top_side.both_avg<-NULL # Partial value
Three_MH_excel_cleaning$GENDER_female<-NULL                                               
Three_MH_excel_cleaning$HORIZONTAL_SACCADES_qratio.both_avg<-NULL                        
Three_MH_excel_cleaning$HORIZONTAL_SACCADES_saccadic_efficiency_left_side.both_avg<-NULL  
Three_MH_excel_cleaning$SPEM_fixation_pc.both_avg<-NULL                                  
Three_MH_excel_cleaning$SPEM_H_eccentric_gaze_mean_left_side.both_avg<-NULL               
Three_MH_excel_cleaning$SPEM_H_eccentric_gaze_mean_middle.both_avg<-NULL                 
Three_MH_excel_cleaning$SPEM_H_eccentric_gaze_mean_right_side.both_avg<-NULL              
Three_MH_excel_cleaning$SPEM_H_eccentric_gaze_variability_middle.both_avg<-NULL          
Three_MH_excel_cleaning$SPEM_H_eccentric_gaze_variability_right_side.both_avg<-NULL       
Three_MH_excel_cleaning$SPEM_H_eye_target_vel_err.both_avg<-NULL                         
Three_MH_excel_cleaning$SPEM_H_pathway_length_diff_right_side<-NULL                       
Three_MH_excel_cleaning$SPEM_H_predictive_sp_pc.both_avg<-NULL                           
Three_MH_excel_cleaning$SPEM_H_saccade_num.both_avg<-NULL                                 
Three_MH_excel_cleaning$SPEM_H_sp_variance.both_avg<-NULL                                
Three_MH_excel_cleaning$SPEM_horizontal_synchronization_sp.both_avg<-NULL                 
Three_MH_excel_cleaning$SPEM_latent_sp_pc.both_avg<-NULL                                 
Three_MH_excel_cleaning$SPEM_saccade_pc.both_avg<-NULL                                    
Three_MH_excel_cleaning$SPEM_shape_diff.both_avg<-NULL                                   
Three_MH_excel_cleaning$SPEM_sp_efficiency.both_avg<-NULL                                 
Three_MH_excel_cleaning$SPEM_sp_pc.both_avg<-NULL                                        
Three_MH_excel_cleaning$SPEM_sp_variance.both_avg<-NULL                                   
Three_MH_excel_cleaning$SPEM_V_pathway_length_diff_top_side<-NULL                        
Three_MH_excel_cleaning$SPEM_V_saccade_num.both_avg<-NULL                                 
Three_MH_excel_cleaning$SPEM_V_sp_variance.both_avg<-NULL                                
Three_MH_excel_cleaning$VERTICAL_SACCADES_missed_over_top_side.both_avg<-NULL             
Three_MH_excel_cleaning$VERTICAL_SACCADES_missed_under_bottom_side.both_avg<-NULL        
Three_MH_excel_cleaning$VERTICAL_SACCADES_qratio.both_avg<-NULL                           
Three_MH_excel_cleaning$VERTICAL_SACCADES_saccadic_amp_diff.both_avg<-NULL
Three_MH_excel_cleaning$SPEM_eye_target_vel_err.both_avg<-NULL
Three_MH_excel_cleaning$SPEM_on_target_sp_pc.both_avg<-NULL
Three_MH_excel_cleaning$SPEM_H_pathway_length_diff_left_side<-NULL
Three_MH_excel_cleaning$SPEM_predictive_sp_pc.both_avg<-NULL
##############################

# Missing Values

# Data Exploration:  Check for missing values
colSums(is.na(Three_MH_excel_cleaning))

# percentage of missing data in each column
library(dplyr)
Three_MH_excel_cleaning %>% 
  summarise_each(funs(round(100*mean(is.na(.)))))

#Pre-processing: Remove variables/columns with >4% missing data
Three_MH_excel_cleaning<-Three_MH_excel_cleaning[, which(colMeans(is.na(Three_MH_excel_cleaning)) < 0.04)]

#Pre-processing: remove observations with missing values
summary(Three_MH_excel_cleaning)
df<-na.omit(Three_MH_excel_cleaning)
str(df)
head(df) # view the data
colnames(Three_MH_excel_cleaning)

######################################

# Correlation

# Data Exploration: Check for correlation
cor(df[,unlist(lapply(df, is.numeric))], use = "complete.obs")
corr <- round(cor(df), 1)
ggcorrplot(corr, method = "circle") # matrix method = "circle"


# Pre-Processing: Correlation
# No highly correlated variables, no further processing needed

#################################

# Outliers

# Data Exploration:  Check outliers
str(df)
summary(df)
boxplot(df, col = rainbow(ncol(df)))
boxplot(df$AGE) # Cut data below 4 and above 90
boxplot(df$VERTICAL_SACCADES_missed_over_top_side.both_avg)    # Keep all as legit data          
boxplot(df$VERTICAL_SACCADES_missed_under_bottom_side.both_avg) # Keep all as legit data
hist(df$VERTICAL_SACCADES_missed_under_bottom_side.both_avg) # Keep all as legit data
boxplot(df$FIXATION_STABILITY_convergence_point.diff) # Keep all as legit data

# Pre-Processing: Outliers
#Age
table(df$AGE) # Count the number of values within each category
hist(df$AGE)
summary (df$AGE) # Descriptive Statistics
sum(df$AGE<4) # how many records less than 4?
sum(df$AGE>90) # how many records more than 90?
df = df[which(df$AGE >3),] # remove the patients with <4
df = df[which(df$AGE <90),] # remove the patients with >90
summary (df$AGE) # Descriptive Statistics
sum(df$AGE<4) # how many records less than 1?
sum(df$AGE>90) # how many records more than 120?
str(df)

################################

#Skewness

# Data Exploration: Skewness detection
for (x in names(df)) {
  hist(df[,x],xlab=x)
}

# Pre-Processing: Skewed variables
# No further action, some variables are skewed, keep in mind as moving forward

#################################

# Drop the age variable 
df_AGE=df$AGE
df$AGE<-NULL # Remove the age variable from the dataset
colnames(df) # Names of each column

# Prepare the data for the model

set.seed(1234) #make sure that the result is reproducible
# Set gender to a factor
#Scale the variables
for( i in names(df[, names(df) != "GENDER_female"]))
{
  df[i]<-scale(df[i])
  
}
head(df)

# Re-examine the data
summary(df)
View(df)
str(df)
colnames(df)

################################

# Section 4: Model development

###############################

#Cluster Function

#Run the method 
kc<-kmeans(df, 5)
#output the result
kc
kc$centers
kc$betweenss
kc$tot.withinss 
kc$totss
kc$iter 
colnames(df)


# Section 4: Clustering evaluation
# cluster to age evaluation
hist(df_AGE)
df_AGEbins=cut(df_AGE, breaks=c(0,11,17,29,42,56,65,90), labels=c("0-11","11-17","17-29","29-42","42-56","56-65","65-90"))
table(df_AGEbins, kc$cluster)
cbind(df_AGE, kc$cluster)[1:10,]
results=cbind(df_AGE, kc$cluster)
colnames(results) = c("AGE","Cluster")
print(results)
#show matrix of age and which cluster it falls
table(results["AGE"], results["cluster"])

# Section 5: Cluster plot 
clusplot(df, kc$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

#install.packages("fpc")
library(fpc)
best<-pamk(df) # shows optimal number of clusters
clusplot(df,best$pamobject[["clustering"]] , color=T)
plotcluster(df,best$pamobject[["clustering"]] )

#Plot the sum of squared distances between clusters as k value increases
#Use k between 2 and 15
bss<-integer(length(2:15))
for (i in 2:15) bss[i] <- kmeans(df,centers=i)$betweenss
plot(1:15, bss, type="b", xlab="Number of Clusters",
     ylab="Sum of squares", col="blue") 
wss<-integer(length(2:15))
#Plot the sum of squared distances within clusters as k value increases
for (i in 2:15) wss[i] <- kmeans(df,centers=i)$tot.withinss
lines(1:15, wss, type="b" ) 

# Section 6: Anomaly detection
centers <- kc$centers[kc$cluster, ]
head(centers, 15)
distances <- sqrt(rowSums((df - centers)^2))
distances

outliers <- order(distances, decreasing=T)[1:5]
outliers
df[outliers,]

