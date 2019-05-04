# cosore

R package: continuous soil respiration database

## Principles and general information

Only free use data accepted.

The package, and the process of contributing data, should be as focused and 
simple as possible (but no simpler).

All contributors will be included on database definition paper.

The database is completely open for reuse. We request that users cite the 
database definition paper, and encourage them to (i) cite all dataset primary
publications, and (ii) involve data contributors as co-authors when possible.

_This database is not designed for, and should not be treated as, a permanent
data repository._ Best practice is to deposit the data and then link to it
from the contributed dataset here.

## Design: what would _minimal_ (core) metadata look like?

A _dataset_ is one or more files of continuous (automated) soil respiration data,
with accompanying metadata, with all measurements taken at a single _site_ and with
constant _treatment_ assignments. A dataset lives within a single folder, and the name
of the folder is the dataset's unique _ID_.

See above about simplicity...there are five metadata files, but only two of them
(ten pieces of information in total) absolutely need to be filled out:

### `DESCRIPTION.txt`

* Site name
* Longitude
* Latitude
* Elevation
* UTC_offset
* IGBP cover type
* Instrument name (this is used to parse the data files)
* Primary publication DOI or URL [optional]
* Other publications DOI or URL [optional]
* Data DOI or URL [optional]
* Acknowledgemnt [optional]

### `CONTRIBUTORS.txt`

Information on arbitrary number (>=1) of contributors. The first contributor listed is assumed to be the point of contact for the dataset.

* First name, family name, email
* ORCID (https://orcid.org) [optional]
* Role (https://www.casrai.org/credit.html) [optional]

### `PORTS.txt`

For each multiplexer port, can define:

* Treatment ("None" by default)
* Species [optional]
* Area [optional], area of chamber (cm2)
* Depth [optional], depth of collar insertion (cm)

### `COLUMNS.txt`

* Mapping between _dataset_ fields and standardized _database_ fields.
* Optional compute-on-columns capability (e.g. to change units)

### `ANCILLARY.txt`

* Stand structure, carbon cycle, disturbance, etc. [all optional]

## Operation

When asked to build the synthesis dataset, the R package will
* Scan the `inst/extdata` folder for installed datasets
* For each dataset, parse the metadata, and then based on `Instrument_name` call 
the appropriate function to parse the raw data (which for size reasons are not located in the repository)
* This is done via a [drake](https://github.com/ropensci/drake) pipeline, so we only 
rebuild datasets when needed
* Currently a `list` of lists is returned, where each entry is a list of the relevant
dataset objects (description, contributors, etc); convenience functions generate
 data frames ready (or readier) for analysis
* Reports are generated for the overall database and each individual dataset.

## Priorities

* KISS: start with LI-8150 (multiplexer) data parser only 
(probably want to split this out into separate package actually)
