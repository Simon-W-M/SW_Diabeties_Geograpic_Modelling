# Geographic Modelling for Optical Diabeties Screening
## NHS England South West Intelligence and Insights

### About the Project

In development
[![status: experimental](https://github.com/GIScience/badges/raw/master/status/experimental.svg)](https://github.com/GIScience/badges#experimental)

This repository holds code for the Geographic Modelling for Optical Diabeties Screening  project.  This is a project to explore a modelled diabeties population and applies travel times to sites to explore coverage in SW ICBs. Further optimisaion is checked against proposed sites to determine possible optimal sites. 

_**Note:** Only public or fake data are shared in this repository._

### Project Stucture

- The main code is found in the root of the repository (see Usage below for more information)
- The accompanying [report](./reports/report.pdf) is also available in the `reports` folder
- More information about the code usage can be found in the [model card](./model_card.md)
- {OTHER REPO STRUCTURE}

### Built With

[R Studio](http://www.rstudio.com/.)  
[R Statistical Software](https://www.R-project.org/.)  
[SQL SSMS](https://learn.microsoft.com/en-us/sql/ssms/download-sql-server-management-studio-ssms?view=sql-server-ver16)  

Main packages utilsed
library('tidyverse')     # data wrangling
library('janitor')       # data tidying
library('cli')           # for process messages
library('leaflet')       # main mapping utility
library('sf')            # sf (simple features) - handling spatial data
library('readxl')        # read in excel sheet 
library('curl')          # read data from web
library('htmltools')     # additional leaflet tweaks
library('tidygeocoder')  # converts postcodes to long / lat
library('fontawesome')   # makes funky icons
library('leaf.magic')    # makes pretty markers with icons
library('gt')            # makes pretty tables


### Getting Started

#### Installation

To get a local copy up and running follow these simple steps.

To clone the repo:

`git clone https://github.com/Simon-W-M/SW_Diabeties_Geograpic_Modelling`

Additional datasets are drawn from web
These are the LSOA shapefile and LSOA - ICB attribution file.


### Usage
{DESCRIPTION OF CODE}
{DESCRIPTION OF PROCESS AND TECHNIQUES UTILISED}
{METHODOLOGY USED}

#### Outputs
{LIST AND DESCRIPTION OF OUTPUTS}

{NOTES ON REPRODUCIBILITY OF RESULTS}

#### Datasets
{DESCRIPTION AND LINKS TO DATASETS}

{LINK TO FAKE DATA TO SUPPORT INITAIL CODE RUNS}

### Roadmap

See the {LINK TO REPO ISSUES} for a list of proposed features (and known issues).

### Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**.

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

_See [CONTRIBUTING.md](./CONTRIBUTING.md) for detailed guidance._

### License

Unless stated otherwise, the codebase is released under [the MIT Licence][mit].
This covers both the codebase and any sample code in the documentation.

_See [LICENSE](./LICENSE) for more information._

The documentation is [© Crown copyright][copyright] and available under the terms
of the [Open Government 3.0][ogl] licence.

[mit]: LICENCE
[copyright]: http://www.nationalarchives.gov.uk/information-management/re-using-public-sector-information/uk-government-licensing-framework/crown-copyright/
[ogl]: http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/

### Contact

To find out more about the South West Intelligence and Insights Team visit our [South West Intelligence and Insights Team Futures Page](https://future.nhs.uk/SouthWestAnalytics)) or get in touch at [england.southwestanalytics@nhs.net](mailto:england.southwestanalytics@nhs.net).

<!-- ### Acknowledgements -->

