[![Build Status](https://travis-ci.org/bpbond/cosore.svg?branch=master)](https://travis-ci.org/bpbond/cosore) 

[![codecov](https://codecov.io/gh/bpbond/cosore/branch/master/graph/badge.svg)](https://codecov.io/gh/bpbond/cosore)


# cosore

The `cosore` package consists of data, metadata, and software tools for COSORE, a reproducibility-oriented 
community database for continuous soil respiration data.

**To use the database from within R**, install this `cosore` package by for example `devtools::install_github("bpbond/cosore")`.

**To download the COSORE database in a flat-file format**, click on the
[Releases](https://github.com/bpbond/cosore/releases) tab above.

**A step-by-step guide to using COSORE** is available [here](). [not available yet]

**To contribute to the database**, fill out the [metadata form](https://forms.gle/xRSY7WwmWKTL6iCv5).

## Principles and general information

Only free use data accepted.

The package, and the process of contributing data, should be as focused and 
simple as possible (but no simpler).

All contributors will be included on an introductory database paper planned for spring 2020.

The database is completely open for reuse, and licensed under the [CC BY 4](https://creativecommons.org/licenses/by/4.0/) license. We request that users cite the 
database definition paper, and strongly encourage them to (i) cite all dataset primary
publications, and (ii) involve data contributors as co-authors when possible.

**COSORE is not designed to be, and should not be treated as, a permanent
data repository.** It is a community database, but not an institutionally-backed repository like Figshare, DataONE, ESS-DIVE, etc. We recommend depositing your data in one of these first, and providing its DOI in your COSORE dataset metadata.

## Database design

This database is comprised of a collection of datasets, each converted to a standard format and units.
A _dataset_ is one or more files of continuous (automated) soil respiration data,
with accompanying metadata, with all measurements taken at a single _site_ and with
constant _treatment_ assignments (i.e. they may vary between chambers but not over time).

As much as possible, metadata are kept to a minimum. There are five metadata files, but only two of them absolutely need to be filled out:

### `DESCRIPTION.txt`

* Site name
* Longitude, latitude, and elevation
* Site [timezone name](https://en.wikipedia.org/wiki/List_of_tz_database_time_zones)
* [IGBP cover type](http://www.eomf.ou.edu/static/IGBP.pdf)
* Site network affiliation and network code [optional]
* Measurement instrument and length
* File and timestamp format and timezone
* Primary publication DOI or URL [optional]
* Other publications DOI or URL [optional]
* Data DOI or URL [optional]
* Acknowledgment text [optional]

### `CONTRIBUTORS.txt`

Information on arbitrary number (>=1) of contributors. The first contributor listed is assumed to be the point of contact for the dataset.

* First name, family name, email
* ORCID (https://orcid.org) [optional]
* Role (https://www.casrai.org/credit.html) [optional]

### `PORTS.txt`

Continuous systems typically, but not always, are comprised of a single analyzer plumbed to multiple chambers through a multiplexer. In COSORE, for each multiplexer port, we can define:

* Measurement variable (typically Rs, Rh, or NEE)
* Treatment ("None" by default)
* Chamber area [optional]
* Collar depth [optional]
* Species [optional]
* Sensor depths [optional, only for gradient method]

### `COLUMNS.txt`

This file is used during the import of raw (contributed) data, and maps
between the raw _dataset_ fields and standardized COSORE fields.
It include an optional compute-on-columns capability (e.g. to change units or combine columns). See `?map_columns` for more information.

### `ANCILLARY.txt`

This file contains arbitrary ancillary data: stand structure, carbon cycle, disturbance, etc. [all optional]

## Operation

When asked (via `csr_build()` to build the synthesis dataset, the `cosore` R package:
* Scans for and parses metadata on all installed datasets
* If standardized (already imported data) is available for a dataset, loads that
* If not, parses, QA/QCs, and transforms raw contributed data into a standardized form
* This is done via a [drake](https://github.com/ropensci/drake) pipeline, so we only 
rebuild datasets when needed
* Currently a `list` is returned, in which each entry is an individual dataset, itself
a list comprised of dataset objects (the description, contributors, etc., tables)
* User-side convenience functions generate data frames ready (or readier) for analysis
* Reports are generated for the overall database and each individual dataset.

## Data access

* The easiest way to get the data is on the [Releases](https://github.com/bpbond/cosore/releases) page.

## Priorities

* Structured/standardized continuous IRGA data
* Raw LI-8100A data
* Other data, e.g. long term survey measurements
