StepsForRunningReport
================


Overview of Document
====================

This document is intended to outline the steps in the process for producing a weekly QC report, as well as describe the documents used.

**UPDATE THIS FILE AS WE UPDATE THE PROCESS!**

Process
=======

-   Perform weekly
    -   Idealy on run Mondays, but generalized so that could be run later in the week and produce a report the same as if run on Monday
-   Steps
    1.  Run the file `Update_Sensor_Data.R`, which loads *all* sensor data, loads location data from Access database, adds site ID to the sensor data, and writes to `sensor_data.csv` saved in X:/Data/rawdata/.  
        -  **Only do this step to update files once a week (unless have a specific reason) since it takes time.**
    2.  (After the data has been updated) Run the file `Compile_Sensor_QC_Report.R` located in X:/Production\_code/sensor\_QC/.  This will compile the R markdown file `Sensor_QC_Report.Rmd`, and output an html report `Sensor_QC_Report_reportdate.html`, where *reportdate* is the Monday after the monitoring interval of the report (Monday was chosen because that is the day the report should usually be compiled).  The html report is saved in its own folder "X:/SensorQC_Reports" away from the other code.
        -  We do this step rather than compile `Sensor_QC_Report.Rmd` so we avoid having to manually rename the report or risk overwriting an older report.
        -  Note that a folder `Sensor_QC_Report_reportdate_files` is also produced and is connected with the main report document.  This is done because of an Rmarkdown bug (see below).  **The folder can be ignored (but not deleted!).**
        
**Rmarkdown note:** At the time this document was created, there was a bug compiling Rmarkdown files on the DEOHS computers using Windows.  If you are having issues compiling Rmarkdown documents on the department computers you may have to open the `Command Prompt` and run the following code: 
```
H:
cd \My Documents\R\win-library\3.4\rmarkdown\rmd\h
copy default.html D0CS9J~9.html
```
 Then make sure that the header of the R markdown file includes 
 ```
output:
    html_document:
        self_contained: no
```
 as can be seen in `Sensor_QC_Report.Rmd`.
    
Description of Files
====================

<table>
<colgroup>
<col width="25%" />
<col width="35%" />
<col width="38%" />
</colgroup>
<thead>
<tr class="header">
<th>File Name</th>
<th>Location</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Update_Sensor_Data.R</td>
<td>X:/Production_Code/sensor_QC/</td>
<td>Download all sensor and location data, merge, and write to csv</td>
</tr>
<tr class="even">
<td>GetAccessTables.R</td>
<td>X:/Production_Code/sensor_QC/</td>
<td>Script to get location data from Access Database (required to run "Update_Sensor_Data.R")</td>
</tr>
<tr class="odd">
<td>Compile_Sensor_QC_Report.R</td>
<td>X:/Production_Code/sensor_QC/</td>
<td>Runs "Sensor_QC_Report.Rmd" but outputs a file with the report date in the filename (to avoid manually renaming each week when compiling the report). </td>
</tr>
<tr class="even">
<td>Sensor_QC_Report.Rmd</td>
<td>X:/Production_Code/sensor_QC/</td>
<td>Produces the html report</td>
</tr>
<tr class="odd">
<td>custom.css</td>
<td>X:/Production_Code/sensor_QC/</td>
<td>css file that adds a floating table of contents to the report</td>
</tr>
<tr class="even">
<td>completenessCode.R</td>
<td>X:/Production_Code/sensor_QC/</td>
<td>Contains functions for completeness used in report</td>
</tr>
</tbody>
</table>
