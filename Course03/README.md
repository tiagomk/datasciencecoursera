# README File to explain 'run_analysis.R' Script

For the Script to run, position the [zip file containing the dataset](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip) into the current working directory, in the same level as the run_analysis.R script.



The file is self-explanatory.

1. Firstly, it loads the dependecies libraries.

2. Secondly, it merges both datasets from 'test' and 'train' directories to
'total' directory.

3. Thirdly, it composes the **total dataset** containing all the activities, subjects and Inertial Signals for each activity window.

4. Finally, it summarizes the total dataset to a **tidy dataset** with the mean of all Inertials Signals and activity windows for a specific activity and subject, saving it to the file 'tidyset.txt'.
