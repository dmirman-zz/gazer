[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/dmirman/gazer/master?urlpath=rstudio)
[![Open in Code Ocean](https://codeocean.com/codeocean-assets/badge/open-in-code-ocean.svg)](https://codeocean.com/capsule/4600160/tree/v1)
gazeR 0.0.1.2


# gazeR:A package to analyze gaze position and pupil size data

For a detailed overview of how to use gazeR, please see the vignettes and posted pre-print for SR Reports (https://psyarxiv.com/gvcxb/) and more generalized processing(https://psyarxiv.com/4qwr8)

<p align="center"><img src="https://user-images.githubusercontent.com/18429968/46034046-472caa80-c0c5-11e8-89c3-ff3f463a1868.jpeg" height="200px" width="200px" />
 
# Help
  
Please use the issues tab (https://github.com/dmirman/gazer/issues) to file any bugs or suggestions.
  
## Updates (May 2020)

Added functionality to scale pupil size to dynamic pupil range (Ayasse et al., 2017; Piquado et al., 2010). This requires you to record participant pupil size in bright and dark conditions. 

## Updates (March 2020)

Good news! Gazer is now up on Code Ocean! Click on the Code Ocean button at the top. You can download the capsule to run the package and analyze your data with it. No more worrying about package or OS incompatabilites--it will run on all systems! 

Here is a link showing how to use the gazeR package with Code Ocean: https://www.youtube.com/watch?v=5IQmP9Yrh1k
  
## Updates (Jan 2020)

Happy New Year!! The gazeR package will soon be up and running on Code Ocean! You can download the gazeR capsule and run your own data using our functions from the cloud! 

## Updates (December 2019)
  
 We just resubmitted our manuscript (find it here: https://psyarxiv.com/4qwr8/)! GazeR now has more generalized functionality. 
 
- GazeR can now read in edf files directly
- Baseline by messages
- Modified downsampling function
- Smoothing and interpolation merged into single function

## Updates (September 2019)

- Read in edf files directly 
- Blink/saccades/fixation algorithm 
- Comptability with other eye trackers
- add Binder functionality to run package in the cloud

## Synopsis

This package contains functions for reading in raw eye-tracking data, formatting it for (growth curve) analysis, converting from gaze coordinates to areas of interest, binifying and aggregating data, and various helper functions for GCA and plotting. In addition to gaze-position data, it can handle pupillometric and other time course data.

## Installing Package

``` r
# install devtools
install.packages("devtools")

# install gazer from GitHub
remotes::install_github("dmirman/gazer")
``` 
# Citation
Geller, J., Winn, M. B., Mahr, T., & Mirman, D. (2020). GazeR: A Package for Processing Gaze Position and Pupil Size Data. Behavior Research Methods. 
