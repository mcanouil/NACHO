The imaging unit only counts the codes that are unambiguously distinguishable.  
It simply will not count codes that overlap within an image.  
This provides increased confidence that the molecular counts you receive are from truly recognisable codes.  
Under most conditions, forgoing the few barcodes that do overlap will not impact your data.  
Too many overlapping codes in the image, however, will create a condition called image saturation in which significant data loss could occur (critical data loss from saturation is uncommon).

To determine the level of image saturation, the nCounter instrument calculates the number of optical features per square micron for each lane as it processes the images.  
This is called the **Binding Density** (**BD**).  
The **Binding Density** is useful for determining whether data collection has been compromised due to image saturation.
The acceptable range for **Binding Density** is:

* `0.1 - 2.25` for **MAX**/**FLEX** instruments
* `0.1 - 1.8` for **SPRINT** instruments

Within these ranges, relatively few reporters on the slide surface will overlap, enabling the instrument to accurately tabulate counts for each reporter species.  
A **Binding Density** significantly greater than the upper limit in either range is indicative of overlapping reporters on the slide surface.  
The counts observed in lanes with a **Binding Density** at this level may have had significant numbers of codes ignored, which could potentially affect quantification and linearity of the assay.
