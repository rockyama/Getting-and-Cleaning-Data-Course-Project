#Getting and Cleaning Data Course Project

Chris Aoyama

## Description
One of the most exciting areas in all of data science right now is wearable computing. 
Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. 
The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone.

##Source Data
The data description may be found here:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

##Data Set Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. 
Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) 
wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 
3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been 
video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 
70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in 
fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which 
has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration 
and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz 
cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time 
and frequency domain.

##Additional information
For each record in the dataset it is provided: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

##Part 1
After setting the source directory for the files, read into tables the data located in

-features.txt
-activity_labels.txt
-subject_train.txt
-x_train.txt
-y_train.txt
-subject_test.txt
-x_test.txt
-y_test.txt

Assign column names and merge to create one data set.

##Part 2
Creates a logical vector that extract the measurements for mean and StdDev.

##Part 3

Merges data subsets.

##Part 4
Uses the gsub function to clean up data labels.

##Part 5
Summarizes and merges the tidy data and writes the table to a file called /tidyData.txt.
