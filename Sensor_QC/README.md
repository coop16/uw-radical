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
    1.  Run the file `Update_Sensor_Data.R` to download *all* sensor data and write to csv: `sensor_data.csv` located in X:/Data/rawdata/.
    2.  (After the data has been updated) Compile the report using `Sensor_QC_Report.Rmd` located in X:/Production\_code/sensor\_QC/, which will output an html report `Sensor_QC_Report.Rmd`
        -  At the time this document was created, there was a bug compiling Rmarkdown files on the DEOHS computers using Windows.  You may have to open the `Command Prompt` and run the following code:    
`H:`
`cd \My Documents\R\win-library\3.4\rmarkdown\rmd\h`
`copy default.html D0CS9J~9.html`
          
    3.  Rename the html output document by adding on the report date at the end (for example `Sensor_QC_Report_01_08_18.Rmd`)
        -   Use the date of the Monday the report was run (the day after the range of dates in the report)
        -   I hope to make this renaming step automatic eventually

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
