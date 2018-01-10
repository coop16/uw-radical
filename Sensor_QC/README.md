StepsForRunningReport
================
Updated 01-09-2018

Overview of Document
====================

This document is intended to outline the steps in the process for producing a weekly QC report, as well as describe the documents used.

**UPDATE THIS FILE AS WE UPDATE THE PROCESS!**

Process
=======

-   Perform weekly
    -   Idealy on run Mondays, but generalized so that could be run later in the week and produce a report the same as if run on Monday
-   Steps
    1.  Run the file `Update_Sensor_Data.R` to download *all* sensor data and write to `sensor_data.csv` saved in X:/Data/rawdata/.  
        -  So `sensor_data.csv` will be updated once a week and keep the same file name.  
        -  **Only do this step once a week since it takes time.**
        -  (*Still need to add code to merge the location data with sensor data*)
    2.  (After the data has been updated) Compile the report using `Sensor_QC_Report.Rmd` located in X:/Production\_code/sensor\_QC/, which will output an html report `Sensor_QC_Report_dateofreport.html`, where *dateofreport* is automated to be the Monday after the report interval (i.e. the day in which the report should usually be compiled). 
        -  At the time this document was created, there was a bug compiling Rmarkdown files on the DEOHS computers using Windows.  If you get an error you may have to open the `Command Prompt` and run the following code:    
`H:`
`cd \My Documents\R\win-library\3.4\rmarkdown\rmd\h`
`copy default.html D0CS9J~9.html`
          

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
<td>Download sensor data and write to csv</td>
</tr>
<tr class="even">
<td>Sensor_QC_Report.Rmd</td>
<td>X:/Production_Code/sensor_QC/</td>
<td>Outputs and html report</td>
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
