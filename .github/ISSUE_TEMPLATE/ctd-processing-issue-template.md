---
name: CTD processing issue template
about: Template for outlining the procedure for processing a CTD data set.
title: CTD Processing for Mission
labels: ''
assignees: ''

---

# CTD Processing Issue Template

## Background

A new CTD data set now requires processing. Use the following workflow to properly process the CTD data set. Normally the data set is associated with a mission. The data set might only be a single profile but could be hundreds of profiles; which will impact the length of time required and the possible steps required.

Normally the CTD data is acquired using Sea-Bird Scientific instruments but might also be acquired using RBR instruments or instruments from another manufacturer.

## Prep Work

- [ ] Secured a copy of the cruise report from ARC or chief scientist (if available)
- [ ] Copied raw CTD data from SRC
- [ ] Copied available Autosal salinity data and/or Winkler dissolved oxygen data from SRC
- [ ] Obtained copy of electronic event log and/or CTD log sheets
- [ ] Obtained copies of Dry and/or Wet log sheets (if available)
- [ ] Latest version of CTDDAP software installed; along with supported version of Sea-Bird data processing software
- [ ] Latest versions of MATLAB®, R, RStudio, and Python available on Software Center installed


## Processing Steps

- ### Step 0 - Calibrate data
  - [ ] Obtained R script templates used for calibration
  - [ ] Computed new linear correction for CTD conductivity sensor(s)
  - [ ] Computed new Soc correction for CTD dissolved oxygen sensor(s)
  - [ ] Applied pH linear correction (if provided by tracer group)
- ### Step 1 - Inspect ODF files
- ### Step 2 - Update metadata
- ### Step 3 - Consult with (obtain feedback from) physical scientist on CTD data quality
- ### Step 4 - Add quality flag fields and run data through automated checks
- ### Step 5 - Visually inspect data while referring to physical scientist feedback and report produced from automated checking
- ### Step 6 - Perform any required final updates to ODF files
- ### Step 7 - Archive data and email notification
  - [ ] Copied files to SRC
  - [ ] Loaded files into ODF_ARCHIVE database
  - [ ] Notified data archivist and respective data manager
