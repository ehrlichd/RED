# RED
R package for Robust Estimator of grade Difference (RED)

This nonparametric method, Robust Estimator of Grade Differences (RED), avoids data compression and allows for missing data. It performs comparably or better to other distance statistics (Mean Measure of Divergence, Mahanobis Distance).

While RED was developed for use with ordinal (graded) data, it can be calculated on any multivariate data be it ordinal, interval, or continuous. All you need in your dataset are 2+ variables with some kind of logical ranking and 2+ groups.

**Note:** Make sure you install/update both [R](https://cran.r-project.org/) and [Rstudio](https://www.rstudio.com/products/rstudio/download/#download) prior to loading RED.

### To Install:
Open Rstudio, run the following code in console to install RED:

```{R}
install.packages("remotes")
remotes::install_github("ehrlichd/RED")
```

Then load the package with
```{R}
library(RED)
```
Think of installing packages as adding a book (package) to your R Library. You only have to go through this step once.
Each time you open Rstudio, you'll need to reload the package if you want to use it using library()

**Note:** While the install() calls required " " around package names, library() does not!








### Alternative Install: 

**Note:** RED requires the [rgl](https://CRAN.R-project.org/package=rgl) package to produce 3D scatter plots. Please ensure rgl is installed prior to following the **Alternative Installation.** 

[Downlaod the .tzr.gz file](RED_0.0.0.9000.tar.gz)

![](https://github.com/ehrlichd/RED/blob/master/images/gitDL.png)

Open Rstudio: 
1. Tools > Install Packages Install from:
2. Click the first dropdown,change: 
    *"Repository (CRAN)"* to **"Package Archive File (.zip; .tar.gz)"**
    
3. Navigate to the .tar.gz

![](https://github.com/ehrlichd/RED/blob/master/images/Rinstall.png)
### References:

(2018) Daniels, J., McKean, J., Willermet, C., Edgar, H., Robust Estimator of Grade Differences: a new statistical solution to an old categorical data problem, Chapter 4, Cambridge University Press.

(2016) Willermet, C., Daniels, J., Edgar, H., Seeing RED: A new statistical solution to an old categorical data problem. 
https://www.researchgate.net/publication/301777157_Seeing_RED_A_new_statistical_solution_to_an_old_categorical_data_problem
