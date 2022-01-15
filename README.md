# Data Science for the Public Good

The Data Science for the Public Good (DSPG) Young Scholars program is an immersive summer program that engages students from across Iowa to work together on projects that address local and state government challenges around critical social issues relevant in the world today. Learn more about the program [here](https://dspg.iastate.edu/).

<br>

## I2D2 Dashboard 2021
The goal of the I2D2 Dashboard project was to build tools for data collection to produce indicators of childhood wellness as identified by [Early Childhood Iowa](https://earlychildhood.iowa.gov/document/indicators-and-data-dictionary) and structure this data within an accessible [**dashboard**](https://i2d2.shinyapps.io/DSPG_2021_Dashboard/).

This dashboard should serve a wide array of purposes, including providing data for early childhood agencies writing grant applications, aiding advocacy work, and informing policy-makers and legislators about the state of early childhood wellness.

<br>

### **Objectives**

- Identified data sources for 22 indicators
- Collected data from different repositories/websites using API-based and web scraping tools
- Worked within RStudio environment
- Constructed scripts of code to semi-automate the collection, download, and processing of new data
- Fed data to build a shiny web application to develop a fully functional, interactive dashboard with visualizations and the ability to download raw data

<br>

### **Indicators**
22 indicators have been identified by the project team to include in the dashboard. The indicators have been grouped into 5 categories. Some indicators were cross-listed among multiple categories (_italicized_).

**Physical and Mental Health**  

- Low Birth Weight
- Percent of Immunized Children
- % of Students Entering Kindergarten with No Obvious Dental Problems
- _Incidence of Child Abuse/1,000 Children_
- _Domestic Violence Rate_

**Community**  

- Serious Crimes/100,000 Population
- Juvenile Arrests/100,000 Population
- _Unemployment Rate_
- _Incidence of Child Abuse/1,000 Children_
- _Domestic Violence Rate_
- Teen Births
- Housing (availability, affordability, quality)
- Homelessness

**Education**

- % of Kindergarten Students Proficient by Kindergarten Literacy Assessment
- Educational Attainment of Mothers
- _Quality Early Learning Environments, QRS Rating, # of Programs in a Quality Initiative_
- _Availability of Childcare, Cost, # of Childcare Providers, # of Childcare Spaces_

**Employment and Income**

- % of Children Under Age 6 Living in Poverty
- % of Children Under Age 6 with All Parents in the Workforce
- _Unemployment Rate_

**Services**
- % of Accredited Family Support Programs in the State
- _Quality Early Learning Environments, QRS Rating, # of Programs in a Quality Initiative_
- _Availability of Childcare, Cost, # of Childcare Providers, # of Childcare Spaces_
- Childcare Spots Lost Due to COVID-19
- Availability of Doctors (pediatricians in particular) Who Accept Medicaid
- Availability of Dentists Who Accept Medicaid

<br>

### **Data**
The indicators were computed using public data-sets obtained from different sources/agencies. Number of available years and granularity of the data varied across sources. Data sources for few indicators were not identified, therefore the project team could not compute the corresponding indicators. Detailed information about sources used can be found [here](https://iastate.app.box.com/file/844461079162?s=wsb68re1hs0qgnfba6r5lupx59hdvwip).

|  No. | Indicator      | Source   | Year* |
| -    | -------------- | -------- | :--: |
|  1   |  Low birth weight |  Iowa Department of Public Health | 2015-2019 |
|  2   |  Percent of immunized children |  Iowa Department of Public Health  | 2020 |
|  3   |  K assessment data |  FastBridge | 2020-2021 |
|  4   |  % of students entering K with no obvious dental problems |  Iowa Department of Public Health | 2016-2019 |
|  5   |  Educational attainment of mothers |  ACS | 2010-2019 |
|  6   |  Serious crime/100,000 population |  Uniform Crime Report | 2019 |
|  7   |  Juvenile arrests/100,000 population |  Iowa Division of Criminal and Juvenile Justice Planning | 2015-2018 |
|  8   |  Unemployment rate |  Iowa Workforce Development  | 2001-2021 |
|  9   |  % of children under age 6 living in poverty (recommended to break down by education, race, marital status, and employment)  |  ACS | 2009-2019 and 2013-2019 |
|  10  |  % of children under age 6 with all parents in the workforce |   ACS | 2009-2019 |
|  11  |  Incidence of child abuse/1,000 children |   Iowa Department of Human Services | 2004-2020 |
|  13  |  Domestic violence rate |    |  |
|  14  |  Teen births |   Iowa Department of Public Health  | 2000-2019 |
|  15  |  % of accredited family support programs in the state |   Iowa Department of Human Services | 2020 |
|  16  |  Quality early learning environments, QRS rating, # of programs in a quality initiative |   Iowa Department of Human Services | 2020 |
|  17  |  Availability of child care, cost, # of childcare providers, # of childcare spaces |   Iowa Department of Human Services | 2020 |
|  18  |  Possibly specifically the number of slots lost during COVID-19? |   Iowa Department of Human Services |  |
|  19  |  Availability of pediatrics who accept medicaid by county |   Iowa Department of Human Services | Data updates daily |
|  20  |  Availability of dentists who accept medicaid by county |   Iowa Department of Human Services | Data updates daily |
|  22  |  Homelessness |   Institute for Community Alliances | 2019-2021 |

\* _Number of available years does not always correspond to the number of data-years obtained by the project team._

<br>

### **Code**
Both data scraping and dashboard development was implemented in R. Corresponding code is available on:

- [GitHub Repository](https://github.com/DSPG2021/i2d2) (limited access, owned by Adisak Sukul)
- [Iowa IDS CyBox](https://iastate.app.box.com/folder/152647528114) under `Iowa IDS` > `Projects` > `2021 Dashboard` > `Code` > `DSPG_2021` (private)

The final version of the dashboard was deployed on shinyapps.io by [Giorgi Chighladze](https://github.com/giorgichi) at [https://i2d2.shinyapps.io/DSPG_2021_Dashboard/](https://i2d2.shinyapps.io/DSPG_2021_Dashboard/).

<br>

### **Other Documents**
The I2D2 Dashboard project team made three presentation during the project period which could be found along with the recording of the presentations on [Iowa IDS CyBox](https://iastate.app.box.com/folder/145226631787) under `Iowa IDS` > `Projects` > `2021 Dashboard` > `DSPG 2021 work` > `DSPG 2021 Presentations` (private).


<br>

### **Team**
The I2D2 Dashboard project team  brought together backgrounds in Computer Science, Systems Engineering, Economics, and Statistics, with interests in applications of technical skills to achieve meaningful impacts for decision making processes related to policy problems in local communities.

- **Todd Abraham** - Team Leader, Iowa State University, Psychology, Statistics
- **Tiancheng Zhou** - Graduate fellow, Iowa State University Computer Science
- **Avery Schoen** - Intern ‘24, University of Chicago, Statistics
- **Dylan Mack** - Intern ‘24, Washington University, Systems Engineering
- **Sonyta Ung** - Intern ‘22, Iowa State University, Computer Science

<br>

