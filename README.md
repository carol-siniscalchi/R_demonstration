# R demonstration
R demonstration for the undergraduate Sociology methods course.

## Dataset:
We will use parts of the data collection from the American Housing Survey, 2009: New Orleans Data. The dataset can be downloaded from [ICPSR](https://www.icpsr.umich.edu/web/ICPSR/studies/30943#). To download the data, you'll need to be logged in to the MS State VPN or using an university computer, like those in the Mitchell Memorial Library Computer Commons. You'll also need to create an account on ICPSR. 

On the Data & Documentation tab of the study page, you can select the data format you need on the Download dropdown menu. Here we'll choose the R format. This will generate a compressed zip file that will be saved on the specific Download folder in your computer. Once decompressed, the folder will contain several files and eight folders, one for each Part of the study. We'll need the .rda files from folders DS0002 and DS0004. These folders will also contain the codebook for each part of the study as well. 

## R and RStudio:

If you are using your own computer, you'll need to install R and RStudio. A short tutorial on how to do that is available [here](https://rstudio-education.github.io/hopr/starting.html). Alternatively, the Windows computers available at the Mitchell Memorial Library Computer Commons have both installed and ready to use. 

After installation, you'll need to install a few packages that we are using the tutorial. They can be installed by typing the following command lines in the Console window on RStudio:

```
install.packages("ggplot2")
install.packages("formattable")
install.packages("stargazer")
install.packages("AICcmodavg")
```
You should be set to start now!

## Scripts:

There is a [short](https://github.com/carol-siniscalchi/R_demonstration/blob/main/r_demo_v2.2.R) and [long](https://github.com/carol-siniscalchi/R_demonstration/blob/main/R_demo_v.2.0_longversion.R) version of the script. The long version contains some extra analyses. 

