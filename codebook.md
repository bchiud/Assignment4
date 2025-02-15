# Codebook for Assignment 4
Brady Chiu  
February 7, 2016  

******

### Overview
This codebook describes the process and output of cleaned up our data set.

******

### Input

Data for our project was obtained from : [Link](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

******

### Process

We applied our **[run_analysis.R](https://github.com/bchiud/Assignment4/blob/master/run_analysis.R)** script to above data set. Our script executes the following transformations to obtain our final output:

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names.
5. From the data set in *step 4*, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

******

### Output

Our output file, **[tidy.csv](https://github.com/bchiud/Assignment4/blob/master/tidy.csv)**, contains the following variables:

+ **SubjectID** : An ID to identify the participant subject  
+ **ActivityName** : Name of activity subject performed  
+ **BodyAccelerometerMeanXAxis** : Mean of body acceleration on the x-axis  
+ **BodyAccelerometerMeanYAxis** : Mean of body acceleration on the y-axis  
+ **BodyAccelerometerMeanZAxis** : Mean of body acceleration on the z-axis  
+ **BodyAccelerometerStandardDeviationXAxis** : Standard deviation of body acceleration on the x-axis  
+ **BodyAccelerometerStandardDeviationYAxis** : Standard deviation of body acceleration on the y-axis  
+ **BodyAccelerometerStandardDeviationZAxis** : Standard deviation of body acceleration on the z-axis  
+ **GravityAccelerometerMeanXAxis** : Mean of gravity acceleration on the x-axis  
+ **GravityAccelerometerMeanYAxis** : Mean of gravity acceleration on the y-axis  
+ **GravityAccelerometerMeanZAxis** : Mean of gravity acceleration on the z-axis  
+ **GravityAccelerometerStandardDeviationXAxis** : Standard deviation of gravity acceleration on the x-axis  
+ **GravityAccelerometerStandardDeviationYAxis** : Standard deviation of gravity acceleration on the y-axis  
+ **GravityAccelerometerStandardDeviationZAxis** : Standard deviation of gravity acceleration on the z-axis  
+ **BodyAccelerometerJerkMeanXAxis** : Mean of body acceleration on the x-axis, derived in time to obtain jerk signals  
+ **BodyAccelerometerJerkMeanYAxis** : Mean of body acceleration on the y-axis, derived in time to obtain jerk signals  
+ **BodyAccelerometerJerkMeanZAxis** : Mean of body acceleration on the z-axis, derived in time to obtain jerk signals  
+ **BodyAccelerometerJerkStandardDeviationXAxis** : Standard deviation of body acceleration on the x-axis, derived in time to obtain jerk signals  
+ **BodyAccelerometerJerkStandardDeviationYAxis** : Standard deviation of body acceleration on the y-axis, derived in time to obtain jerk signals  
+ **BodyAccelerometerJerkStandardDeviationZAxis** : Standard deviation of body acceleration on the z-axis, derived in time to obtain jerk signals  
+ **BodyGyroscopeMeanXAxis** : Mean of body angular velocity on the x-axis  
+ **BodyGyroscopeMeanYAxis** : Mean of body angular velocity on the y-axis  
+ **BodyGyroscopeMeanZAxis** : Mean of body angular velocity on the z-axis  
+ **BodyGyroscopeStandardDeviationXAxis** : Standard deviation of body angular velocity on the x-axis  
+ **BodyGyroscopeStandardDeviationYAxis** : Standard deviation of body angular velocity on the y-axis  
+ **BodyGyroscopeStandardDeviationZAxis** : Standard deviation of body angular velocity on the z-axis  
+ **BodyGyroscopeJerkMeanXAxis** : Mean of body angular velocity on the x-axis, derived in time to obtain jerk signals  
+ **BodyGyroscopeJerkMeanYAxis** : Mean of body angular velocity on the y-axis, derived in time to obtain jerk signals  
+ **BodyGyroscopeJerkMeanZAxis** : Mean of body angular velocity on the z-axis, derived in time to obtain jerk signals  
+ **BodyGyroscopeJerkStandardDeviationXAxis** : Standard deviation of body angular velocity on the x-axis, derived in time to obtain jerk signals  
+ **BodyGyroscopeJerkStandardDeviationYAxis** : Standard deviation of body angular velocity on the y-axis, derived in time to obtain jerk signals 
+ **BodyGyroscopeJerkStandardDeviationZAxis** : Standard deviation of body angular velocity on the z-axis, derived in time to obtain jerk signals    
+ **BodyAccelerometerMagnitudeMean** : Mean of body acceleration magnitude  
+ **BodyAccelerometerMagnitudeStandardDeviation** : Standard deviation of body acceleration magnitude  
+ **GravityAccelerometerMagnitudeMean** : Mean of gravity acceleration magnitude  
+ **GravityAccelerometerMagnitudeStandardDeviation** : Mean of gravity acceleration magnitude  
+ **BodyAccelerometerJerkMagnitudeMean** : Mean of body acceleration magnitude, derived in time to obtain jerk signals  
+ **BodyAccelerometerJerkMagnitudeStandardDeviation** : Standard deviation of body acceleration magnitude, derived in time to obtain jerk signals  
+ **BodyGyroscopeMagnitudeMean** : Mean of body angular velocity magnitude  
+ **BodyGyroscopeMagnitudeStandardDeviation** : Standard deviation of body angular velocity magnitude  
+ **BodyGyroscopeJerkMagnitudeMean** : Mean of body angular velocity magnitude, derived in time to obtain jerk signals  
+ **BodyGyroscopeJerkMagnitudeStandardDeviation** : Standard deviation of body angular velocity magnitude, derived in time to obtain jerk signals  
+ **FFTBodyAccelerometerMeanXAxis** : Mean of body acceleartion on the x-axis, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyAccelerometerMeanYAxis** : Mean of body acceleartion on the y-axis, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyAccelerometerMeanZAxis** : Mean of body acceleartion on the z-axis, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyAccelerometerStandardDeviationXAxis** : Standard deviation of body acceleartion on the x-axis, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyAccelerometerStandardDeviationYAxis** : Standard deviation of body acceleartion on the y-axis, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyAccelerometerStandardDeviationZAxis** : Standard deviation of body acceleartion on the z-axis, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyAccelerometerMeanFrequencyXAxis** : Mean frequency of body acceleartion on the x-axis, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyAccelerometerMeanFrequencyYAxis** : Mean frequency of body acceleartion on the y-axis, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyAccelerometerMeanFrequencyZAxis** : Mean frequency of body acceleartion on the z-axis, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyAccelerometerJerkMeanXAxis** : Mean of body acceleartion on the x-axis, derived in time to obtain jerk signals, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyAccelerometerJerkMeanYAxis** : Mean of body acceleartion on the y-axis, derived in time to obtain jerk signals, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyAccelerometerJerkMeanZAxis** : Mean of body acceleartion on the z-axis, derived in time to obtain jerk signals, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyAccelerometerJerkStandardDeviationXAxis** : Standard deviation of body acceleartion on the x-axis, derived in time to obtain jerk signals, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyAccelerometerJerkStandardDeviationYAxis** : Standard deviation of body acceleartion on the y-axis, derived in time to obtain jerk signals, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyAccelerometerJerkStandardDeviationZAxis** : Standard deviation of body acceleartion on the z-axis, derived in time to obtain jerk signals, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyAccelerometerJerkMeanFrequencyXAxis** : Mean frequency of body acceleartion on the x-axis, derived in time to obtain jerk signals, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyAccelerometerJerkMeanFrequencyYAxis** : Mean frequency of body acceleartion on the y-axis, derived in time to obtain jerk signals, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyAccelerometerJerkMeanFrequencyZAxis** : Mean frequency of body acceleartion on the z-axis, derived in time to obtain jerk signals, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyGyroscopeMeanXAxis** : Mean of body angular velocity on the x-axis, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyGyroscopeMeanYAxis** : Mean of body angular velocity on the y-axis, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyGyroscopeMeanZAxis** : Mean of body angular velocity on the z-axis, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyGyroscopeStandardDeviationXAxis** : Standard deviation of body angular velocity on the x-axis, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyGyroscopeStandardDeviationYAxis** : Standard deviation of body angular velocity on the y-axis, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyGyroscopeStandardDeviationZAxis** : Standard deviation of body angular velocity on the z-axis, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyGyroscopeMeanFrequencyXAxis** : Mean frequency of body angular velocity on the x-axis, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyGyroscopeMeanFrequencyYAxis** : Mean frequency of body angular velocity on the y-axis, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyGyroscopeMeanFrequencyZAxis** : Mean frequency of body angular velocity on the z-axis, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyAccelerometerMagnitudeMean** : Mean of body acceleration magnitude, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyAccelerometerMagnitudeStandardDeviation** : Standard deviation of body acceleration magnitude, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyAccelerometerMagnitudeMeanFrequency** : Mean frequency of body acceleration magnitude, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyAccelerometerJerkMagnitudeMean** : Mean of body acceleration magnitude, derived in time to obtain jerk signals, with Fast Fourier Transform (FFT) applied     
+ **FFTBodyAccelerometerJerkMagnitudeStandardDeviation** : Standard deviation of body acceleration magnitude, derived in time to obtain jerk signals, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyAccelerometerJerkMagnitudeMeanFrequency** : Mean frequency of body acceleration magnitude, derived in time to obtain jerk signals, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyGyroscopeMagnitudeMean** : Mean of body angular velocity, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyGyroscopeMagnitudeStandardDeviation** : Standard deviation of body angular velocity, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyGyroscopeMagnitudeMeanFrequency** : Mean frequency of body angular velocity, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyGyroscopeJerkMagnitudeMean** : Mean of body angular velocity magnitude, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyGyroscopeJerkMagnitudeStandardDeviation** : Standard Deviation of body angular velocity magnitude, with Fast Fourier Transform (FFT) applied  
+ **FFTBodyGyroscopeJerkMagnitudeMeanFrequency** : Mean frequency of body angular velocity magnitude, with Fast Fourier Transform (FFT) applied  
