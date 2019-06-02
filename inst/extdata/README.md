# README file for COSORE - continuous soil respiration database

*********************************************************************************
This is a pre-release of the database.

Structure, names, formatting: **anything may change** in future versions, 
EXCEPT for the data use conditions.
*********************************************************************************

Version: %VERSION

Git commit: %GIT_SHA

Release date: %DATE


## Table of contents

1. Data use policy, citations, and co-authorship   **<– read this please**
2. Files included in this release
3. How to access the data
4. Feedback and problems


## Data use policy, citations, and co-authorship

The database is intended as a community and scientific resource and is
completely open for reuse. It's published under the permissive
[MIT License](https://en.wikipedia.org/wiki/MIT_License). No co-authorship 
or citations are required for papers using these data for analysis.

**But, scientists need incentives and rewards to contribute data and maintain the database.** So, we
* **_strongly request_** that you cite the main database paper [when it is published].
* _request_ that you cite all "primary publication" studies found in the `DESCRIPTION` table (see below).
* _encourage_ you to involve data contributors as co-authors whenever possible. 

In summary, please do your part to balance open data and open science with ensuring that 
data contributors receive tangible benefits from participating in COSORE. Your synthesis
depends on the labor of many, many people; recognize them.

Finally: for reproducibility please _always_ include the version (%VERSION) and, ideally, 
the commit number (%GIT_SHA) in your methods section.


## Files included in this release:

%FILELIST


## How to access the data

If you use R, just

```
cosore <- readRDS("cosore_data.RDS")
```

This will load the entire database (currently %DATABASE_SIZE) into memory. It's
structured as a list of lists: the first level is broken up by dataset, and the
second level by the `description`, `contributors`, `ports`, etc., tables. See the
[vignette](TODO) (in R, type `vignette("cosore-data")`) for a detailed example.

All the data are also written out as `csv` files (see list above),
which can be read by almost any data analysis tool. Most tables
are combined into a single file for ease of access; the `data` tables, which are large,
are written individually into the `datasets/` directory.


## Feedback and problems

If you discover a code or data problem, please 
[open an issue on GithHub](https://github.com/bpbond/cosore/issues/new) (preferred), 
or email the maintainer, Ben Bond-Lamberty, <bondlamberty@pnnl.gov>.
