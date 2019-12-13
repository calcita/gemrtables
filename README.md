# gemrtables
A package for generating the Global Education Monitoring (GEM)
Report statistical tables.

The Global Education Monitoring Report (GEM Report), is an editorially independent, authoritative, and evidence-based annual report that monitors progress towards the global education goal and targets adopted at the UN General Assembly in September 2015. The gemrtables package provides an easy way to obtain data from the UNESCO Institute for Statistics ([UIS](https://apiportal.uis.unesco.org/)) by accessing its API and several others sources.

## Installation

`devtools::install_github("northeastloon/gemrtables")`

## Usage

gemrtables() is the primary function for this package, generating the statistical tables in a 'wide' data format in excel, or a 'long' format.

In addition, the gemrtables package provides three categories of functions: import, clean, and summarise. The import functions read data from SDMX url queries, the GEM cedar SQL database, and other sources.

Some indices are calculated at the country level and then are aggregated at the regional level or the income level. Also, it is possible to calculate those indices directly at the regional or the income level.


## Drake plan
