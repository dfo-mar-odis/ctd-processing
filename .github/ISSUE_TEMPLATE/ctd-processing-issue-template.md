---
name: New Dataset to Process
about: Template outlining the procedure for processing a new CTD data set.
title: <Enter Mission Name e.g. NED2015017>
labels: ''
assignees: ''

---

# Processing Steps

## Background

A new CTD data set now requires processing. Use the following workflow to properly process the CTD data set. Normally the data set is associated with a mission. The data set might only be a single profile but could be hundreds of profiles; which will impact the length of time required and the possible steps required.

Normally the CTD data is acquired using Sea-Bird Scientific instruments but might also be acquired using RBR instruments or instruments from another manufacturer.

## Prep Work

- [ ] Secured a copy of the cruise report from ARC or chief scientist (if available)
- [ ] Copied raw CTD data from SRC
- [ ] Copied available Autosal salinity data from SRC
- [ ] Copied available Winkler dissolved oxygen data from SRC
- [ ] Obtained copy of electronic event log and/or CTD log sheets
- [ ] Obtained copies of Dry and/or Wet log sheets (if available)
- [ ] Latest version of CTDDAP software installed; along with supported version of Sea-Bird data processing software
- [ ] Latest versions of MATLAB®, R, RStudio, and Python available on Software Center installed

## Processing Steps

- ### Step 1 - **Compute Calibrations**
  - [ ] Obtained R script templates used for calibrations
  - [ ] Computed new linear correction for CTD conductivity sensor(s)
  - [ ] Computed new Soc correction for CTD dissolved oxygen sensor(s)
  - [ ] ??? Computed new linear correction for CTD fluorometer (to be adopted)
- ### Step 2 - **Apply Calibrations**
  - [ ] Copied old CTD configuration files (CON or XMLCON) and updated with revised sensor calibrations
  - [ ] Reprocessed CTD data using CTDDAP and new CON files
- ### Step 3 - **Update Metadata**
  - [ ] Ran the MATLAB® script _output_ctd_header_info_ on calibrated ODF files
  - [ ] Opened header.txt in Microsoft Excel
  - [ ] Inspected metadata for required updates
  - [ ] Copied and edited MATLAB® template for updating metadata
  - [ ] Applied pH linear correction (if provided by tracer group)
- ### Step 4 - **Consult with Physical Scientist** 
  - [ ] Physical scientist given copy of calibrated ODF files
  - [ ] Physical scientist evaluated data quality (e.g. using Temperature-Salinity plots)
  - [ ] Physical scientist provided feedback on data quality
- ### Step 5 - **Add Quality Flag Fields and Run Automated Checks**
  - [ ] Executed the MATLAB® script _add_qfs_to_odf_ to add quality flag fields to the ODF files and run automated quality control checks
- ### Step 6 - **Visually Inspect Data**
  - [ ] Copied RMarkdown template and updated it for current mission
  - [ ] Knitted RMarkdown file to produce HTML file 
  - [ ] Opened HTML file containing profile plots to do a quick visual inspection
  - [ ] Ran the MATLAB® script _odf_qc_ to visual inspect CTD profiles one at a time and assign flags to suspect data points
  Note: Operator should refer to physical scientist feedback and report produced from automated checking to help make judgements on suspect data points
- ### Step 7 - **Make Final Metadata Updates**
  - [ ] (if required) Executed a MATLAB® script to do some final updates (e.g. documenting that the secondary sensor data should be used instead of the primary sensor data) to the ODF file metadata
- ### Step 8 - **Archive Data and Email Notification**
  - [ ] Copied files to SRC
  - [ ] Loaded files into ODF_ARCHIVE database
  - [ ] Notified data archivist and respective data manager
