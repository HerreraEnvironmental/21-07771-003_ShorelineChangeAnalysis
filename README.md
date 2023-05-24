21-07771-003: Shoreline Change Analysis
================
Regina Lionheart
2023-05-24

## R Markdown

Tags quartiles

> Herrera Environmental Consultants

## Table of Contents

- [Project Description](#-Project-Description)
  - [Data Discrepancies](#-Data-Discrepancies)
  - [Metadata](#-Metadata)
- [Location in Herrera Database](#-Location-in-Herrera-Database)  
- [Requirements](#-Requirements)
- [Usage](#-Usage)
  - [Layout of Directory and Data](#-Layout-of-Directory-and-Data)
- [Visualizations](#-Visualization)

------------------------------------------------------------------------

### Description

**Important:** *Link to VantagePoint somehow here*

This analysis aims to characterize approximately 25 years of shoreline
profile transect data, provided by NANOOS (Northwest Association of
Networked Ocean Observing Systems). Using this data combined with area
expertise, the relevant coastal areas have been delineated into reaches.

The raw data consists of approximately 4500 individual profiles across
54 coastal sites in Washington and Oregon. The profiles are in x y z
format (Easting Northing elevation).The naming convention is
prof_X\_ssYY.out where prof is short for profile, X is the profile
number, ss is a season code (e.g. f = fall) and YY is the year, in the
format of the last two digits (e.g. “98” is 1998”, “00” is 2000, “08” is
2008, etc).

#### Data discrepancies

Quite a few profiles are null (empty) and have been removed and noted.
See the “Explore Profiles” section for more information.

A small portion of the profiles had non-conforming filenames: “BigE06”,
“beachface”, etc. These files do not represent a large percentage of the
files and have not been included in the analysis.

Profiles 42 - 47 are in Oregon and have been excluded from most of the
analysis.

*Geographically, profiles do not proceed sequentially.*

#### Metadata

The vertical datum is NAVD88. The horizontal is WA State Plane South.
All of the units are in meters.

### 📦 Requirements

Below is a list of packages and external sofwares that this project
utilizes. Please ensure you have the package(s) installed and have
access to the tools listed below.

| Name                                                                                       | Description                                                                                                                                       |
|:-------------------------------------------------------------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------|
| [`R`](https://www.r-project.org/)                                                          | Programming language used for this project.                                                                                                       |
| [`Example Data`](https://drive.google.com/drive/folders/1E-B4dyDTkwOfupHJK0uL8BgKj586OU31) | Pretidied data from the Ingalls Lab for learning purposes. If you do not have access to this drive, please contact the owners of this repository. |

### Usage

The first few chunks of the Control.Rmd script will create empty folders
for different types of data (raw, processed, extra, intermediate). Some
of this data will be produced by the script (processed, intermediate)
and some needs to be moved into those folders before proceeding. Because
this is a template script, the data has been modified for easy input,
which will NEVER be the case in a real run.

All the necessary example data can be downloaded here:
<https://drive.google.com/drive/folders/1E-B4dyDTkwOfupHJK0uL8BgKj586OU31>

If you do not have access to this drive, please contact the owners of
this repository.

> #### User-Defined QC Parameters

These parameters will change depending on your data and level of
stringency. Below are the “default” values.

**area.min**: The minimum area of an integrated peak to be considered a
real peak. HILIC - 1000, Reverse Phase - 5000.

**RT.flex**: Flexibility of the deviation of the sample retention time
from the appropriate standard. HILIC +/- 0.4 min, Reverse Phase +/- 0.2
min.

**blk.thresh**: Cutoff ratio value of sample area to a blank reference
sample. HILIC +/-0.3, Reverse Phase +/- 0.2.

**SN.min**: Cutoff ratio value for Signal to Noise. HILIC +/- 4, Reverse
Phase +/- 4.

#### Additional QC parameters for Skyline

**height.min**: Minimum height for a peak. 1000, HILIC and Reverse
Phase.

**height.max**: Maximum height for a peak. 1.0e8, HILIC and Reverse
Phase.

#### Additional QC parameter for TQS

**IR.flex**: The TQS produces quantitative and secondary trace values.
Find the Ion Ratio by dividing the area of the quantitative trace by the
area of the secondary trace for standards, and use these ratios to
create a reference table. Deviations from this reference table that are
larger than the user-defined values will be flagged. 0.3, HILIC and
Reverse Phase.

## Visualization

![Click on me to see a visual layout of the Targeted
Pipeline!](visual/Targeted_Pipeline_Visualization.pdf)

------------------------------------------------------------------------

## 🔧 Pull Requests

Pull requests are welcome. For major changes, please open an issue
first.

## 💬 Contributors + Contact Information

- [Regina Lionheart](https://github.com/R-Lionheart)

------------------------------------------------------------------------

![dumbbell_plot](https://img.shields.io/badge/plot-dumbbell-%231D455C)
![HCA_clustering](https://img.shields.io/badge/stats-HCA%20clustering-%233ECDA3)
![linear_model](https://img.shields.io/badge/stats-linear%20model-%231D455C)
![boxplot](https://img.shields.io/badge/plot-boxplot-%231D455C)
![3D_regression](https://img.shields.io/badge/plot-3D%20regression-%231D455C)
![quartiles](https://img.shields.io/badge/stats-quartiles-%233ECDA3)
