The limit of detection (**LoD**) is determined by measuring the ability to detect **POS_E**, the `0.5 fM` positive control probe, which corresponds to about 10,000 copies of this target within each sample tube.  
On a **FLEX**/**MAX** system, the standard input of `100 ng` of total RNA will roughly correspond to about 10,000 cell equivalents (assuming one cell contains `10 pg` total RNA on average).  
An nCounter assay run on the **FLEX**/**MAX** system should thus conservatively be able to detect roughly one transcript copy per cell for each target (or 10,000 total transcript copies).  
In most assays, you will observe that even the **POS_F** probe (equivalent to 0.25 copies per cell) is detectable above background.

<!--
To determine whether **POS_E** is detectable, it can be compared to the counts for the negative control probes.  
Every nCounter Gene Expression assay is manufactured with eight negative control probes that should not hybridize to any targets within the sample.  
Counts from these will approximate general non-specific binding of probes within the samples being run.  
The counts of **POS_E** should be higher than two times the standard deviation above the mean of the negative control. 
-->
