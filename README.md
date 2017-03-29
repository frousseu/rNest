# rNest
**A package for backcalculating first egg and fledging dates from nest observations in birds.**

This package was initialy based on private data from the Project NestWatch database and only an excerpt is given as an example in this version. In its current version, the package can mostly be used to backcalculate first egg and nest departure dates from a set of nest observations and nesting parameters of species invloved. The user has to provide its on data.

## Installation

Provided the latest versions of [Rtools](https://cran.r-project.org/bin/windows/Rtools/) and [MiKTeX](https://miktex.org/) are installed to be able to build source packages, type:

```r
# install and load devtools to be able to install packages from GitHub
install.packages("devtools")
library(devtools)

# install rNest from GitHub
install_github("frousseu/rNest")
library(rNest)
```

Here is a more detailed list of [prerequisites](https://support.rstudio.com/hc/en-us/articles/200486498-Package-Development-Prerequisites) for building source packages for Windows, Mac and Linux. Make sure you have the latest version of R installed on your computer.
