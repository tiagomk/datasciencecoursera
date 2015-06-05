# CodeBook

## Data

* total.dataset

> Dataset containing the total dataset merged from both test and train sets, with all variables from **Activity**, **Subject**, and each of the **Inertial Signals** registered in the 'features.txt' file (look for it inside the zip file).

* total.tidyset

> Tidy Dataset containing the mean value calculated from every **Inertial Signal** from every activity window, groupd by **Activity** and **Subject**.

## Variables

* activity

> One from six possibilities: 
> 1. WALKING
> 2. WALKING_UPSTAIRS
> 3. WALKING_DOWNSTAIRS
> 4. SITTING
> 5. STANDING
> 6. LAYING

* subject

> A number from 1 to 30 indexing the subject id.

* activity.mean

> The mean value calculated from every **Intertial Signal** and **activity window**, grouped by *activity* and *subject*.