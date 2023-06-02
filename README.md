21-07771-003: Shoreline Change Analysis
================
Regina Lionheart
2023-06-02

![dumbbell_plot](https://img.shields.io/badge/plot-dumbbell-%231D455C)
![HCA_clustering](https://img.shields.io/badge/stats-HCA%20clustering-%233ECDA3)
![linear_model](https://img.shields.io/badge/stats-linear%20model-%231D455C)
![boxplot](https://img.shields.io/badge/plot-boxplot-%231D455C)
![3D_regression](https://img.shields.io/badge/plot-3D%20regression-%231D455C)
![quartiles](https://img.shields.io/badge/stats-quartiles-%233ECDA3)

![code_style](https://img.shields.io/badge/style-needs%20checking-red)

------------------------------------------------------------------------

TODO: Marketing needs to go over this to ensure it looks good!

> Herrera Environmental Consultants

## Table of Contents

- [Project Description](#-Project-Description)
  - [Data Discrepancies](#-Data-Discrepancies)
  - [Metadata](#-Metadata)
- [Location in Herrera Database](#-Location-in-Herrera-Database)  
- [Requirements and Dependencies](#-Requirements-and-Dependencies)
- [Installation and Usage](#-Installation-and-Usage)
  - [Layout of Directory and Data](#-Layout-of-Directory-and-Data)
- [Visualizations](#-Visualization)

------------------------------------------------------------------------

# Project 21-07771 - Task 003

Seashore Conservation Area

**SharePoint Site: [SharePoint
Site](https://herrerainc.sharepoint.com/teams/21-07771-002)**

**Vantagepoint Site: TODO: what’s the best way to link here?**

------------------------------------------------------------------------

### Project Description

This analysis aims to characterize approximately 25 years of shoreline
profile transect data, provided by NANOOS (Northwest Association of
Networked Ocean Observing Systems). Using this data combined with area
expertise, the relevant coastal areas have been delineated into reaches.

### :droplet: Location in Herrera Database

The raw data used in this project is located in the “data_raw” folder.
All of the information in the data_raw folder is located TODO: Fill this
in. Best to use O drive? Or SharePoint?

If you do not have access to the data, please contact the emails listed
at the bottom of the repository.

### 📦 Requirements and Dependencies

TODO: Installation tips will be helpful for libraries/packages, but less
so for workflows, and for PMs. How should we handle this section? It was
suggested from Paul/Clark to include a “typical imports” section.

Install from <library repository> (repeat as necessary) Install from
source Install from source for development (including add’l
dependencies)

TODO: This was something I involved in my previous READMEs, but might be
more trouble than it’s worth. Thoughts?

Below is a list of packages and external softwares that this project
utilizes. Please ensure you have the package(s) installed and have
access to the tools listed below.

| Name                                                         | Description                                                                                                      |
|:-------------------------------------------------------------|:-----------------------------------------------------------------------------------------------------------------|
| [`R`](https://www.r-project.org/)                            | Programming language used for this project.                                                                      |
| [`NANOOS Profile Data`](https://nvs.nanoos.org/BeachMapping) | The raw data for analysis. If you do not have access to this data, please contact the owners of this repository. |

The raw data consists of approximately 4500 individual profiles across
54 coastal sites in Washington and Oregon. The profiles are in x y z
format (Easting Northing elevation).The naming convention is
prof_X\_ssYY.out where prof is short for profile, X is the profile
number, ss is a season code (e.g. f = fall) and YY is the year, in the
format of the last two digits (e.g. “98” is 1998”, “00” is 2000, “08” is
2008, etc).

#### :exclamation: Data discrepancies

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

------------------------------------------------------------------------

### Installation and Usage

Please ensure you have R and Rstudio installed on your computer.

#### Layout of directory and data

This analysis is run by the Control.Rmd script. The folders included in
this repository are data_raw, data_secondary, figures, scripts, and
archive. The data_raw and archive folders are backed up to TODO add
location here.

## Visualization

TODO: Put any helpful images here. Should maybe go at the top?

------------------------------------------------------------------------

## 🔧 Pull Requests

Pull requests are welcome. For major changes, please open an issue
first.

All functioning code is located on the main branch. Dev branches are to
be named with

## 💬 Contributors + Contact Information

- [Regina Lionheart](https://github.com/R-Lionheart)
- [Andrea
  MacLennan](https://www.herrerainc.com/team-member/andrea-maclennan/)
