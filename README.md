# Cook County Residential Property Tax Appeals
This repo contains an analysis of residential property tax appeals in Cook County. This repo builds on [work](https://harris.uchicago.edu/files/inline-files/Ross%20-%20Vertical%20Equity%20in%20Cook%20County%20-%20CMF%20-%20Final.pdf) done in 2017 that identified issues with property tax appeals.  

## Repo structure

The structure of this repo is as follows:
- `master.r` can call each individual file
- Each file in the code folder executes a specific task, exporting material for use in the report
- The main report is in the root folder and knits outputs created from the files in the code folder

## Primary questions

We want to know whether appeals, as administered by the Cook County Assessors' Office and Board of Review, improve the property tax system as a whole. Appeals can do this by:

- Increasing uniformity such that properties with similar market values are assessed similarly
- Increasing accuracy such that properties' assessed values more accurately reflect their true market values.
- Increasing fairness such that properties of different market values are assessed proportionately. 

In addition, we want to identify gaps in transparency that frustrate this analysis. 
