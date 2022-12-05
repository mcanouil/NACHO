Each individual lane scanned on an nCounter system is divided into a few hundred imaging sections, called Fields of View (**FOV**), the exact number of which will depend on the system being used (*i.e.*, **MAX/FLEX** or **SPRINT**), and the scanner settings selected by the user.  
The system images these **FOV**s separately, and sums the barcode counts of all **FOV**s from a single lane to form the final raw data count for each unique barcode target.  
Finally, the system reports the number of **FOV**s successfully imaged as **FOV** Counted.

Significant discrepancy between the number of **FOV** for which imaging was attempted (**FOV Count**) and for which imaging was successful (**FOV Counted**) may indicate an issue with imaging performance.  
Recommended percentage of registered FOVs (*i.e.*, **FOV Counted** over **FOV Count**) is `75 %`.
