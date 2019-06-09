# cosore

R package: continuous soil respiration database

## Principles and general information

Only free use data accepted.

The package, and the process of contributing data, should be as focused and 
simple as possible (but no simpler).

All contributors will be included on a database definition paper.

The database is completely open for reuse. We request that users cite the 
database definition paper, and strongly encourage them to (i) cite all dataset primary
publications, and (ii) involve data contributors as co-authors when possible.

**This database is not designed for, and should not be treated as, a permanent
data repository.** COSORE is a community database, but not an institutionally-backed repository like Figshare, DataONE, ESS-DIVE, etc. We recommend depositing your data in one of these first, and providing its DOI in metadata.

## Database design

This database is comprised of a collection of datasets, each converted to a standard format and units.
A _dataset_ is one or more files of continuous (automated) soil respiration data,
with accompanying metadata, with all measurements taken at a single _site_ and with
constant _treatment_ assignments.

As much as possible, metadata are kept to a minimum. There are five metadata files, but only two of them absolutely need to be filled out:

### `DESCRIPTION.txt`

* Site name
* Longitude
* Latitude
* Elevation [optional]
* Site timezone
* Site [timezone name](https://en.wikipedia.org/wiki/List_of_tz_database_time_zones)
* [IGBP cover type](http://www.eomf.ou.edu/static/IGBP.pdf)
* Instrument name
* File format
* Timestamp format and timezone
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

For each multiplexer port, can define:

* Treatment ("None" by default)
* Species [optional]
* Chamber area [optional]
* Collar depth [optional]

### `COLUMNS.txt`

This maps between _dataset_ fields and standardized _database_ fields.
It include an optional compute-on-columns capability (e.g. to change units or combine columns). See `?map_columns` for more information.

### `ANCILLARY.txt`

This file contains ancillary data: stand structure, carbon cycle, disturbance, etc. [all optional]

## Operation

When asked (via `csr_build()` to build the synthesis dataset, the `cosore` R package
* Scans its `inst/extdata` folder for metadata on all installed datasets
* Parses the metadata, and then based on `File_format` calls 
the appropriate function to parse the raw data (which for size reasons are not currently included in the repository)
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
