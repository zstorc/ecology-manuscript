# Code repository for the paper "Interannual variation in climate is a causal mechanism underlying year effects in community assembly"

Zachary Storc, University of Kansas; 
Kathryn D. Eckhoff, Northern Arizona University; 
Daniel C. Reuman, University of Kansas; 
Sara G. Baer, University of Kansas

## Introduction 

This repository contains the R script and data files for the multivariate models in the manuscript "Interannual variation in climate is a causal mechanism underlying year effects in community assembly".

## How to run the analyses 

Running the R script requires the following package dependencies: dplyr, tidyr, vegan, ecodist.
To run the code, make your R working directory equal to the directory containing the file main.R,
and then run that file. The run should take no more than 30 seconds.  

Code was run successfully on a machine running Ubuntu linux version 18.04, using R version 4.3.0.
The package versions installed were: dplyr 1.1.4; tidyr 1.3.0; vegan 2.6-6.1; ecodist 2.1.3. The 
code is likely to run on other platforms, and with other versions of R and of these packages,
but it was executed by the authors on these versions. We have endeavored 
to list all dependencies we can think of, but we have only run this on our own machines, so we cannot guarantee 
that additional dependencies will not also be needed on other machines. This repository is intended to record a 
workflow, and is not designed or tested for distribution and wide use on multiple machines. 

All required data files which are accessed by the code are present in the repository and are accessed 
automatically.

## Data file contents:

 - species_comp.csv: species composition data from the sequential restoration experiment
 - AWE012.csv: climate data from the Konza Prairie HQ weather station
 - seq_yr_diff.csv: a matrix corresponding with difference in planting year

## Acknowledgments

This research was funded by the NSF-DEB (2025849, 2343738 and 2414418), and NSF BIO-OCE (2023474). This research would not have been possible without assistance from many students and other researchers at Southern Illinois University and the University of Kansas, Kansas State University agronomy personnel, and staff at the Konza Prairie Biological Station. Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) 
and do not necessarily reflect the views of the National Science Foundation or the other funders. 