The imaging unit only counts codes that it can tell apart.
It does not count codes that overlap within an image.
This gives you more confidence that the molecular counts come from clearly recognizable codes.
Dropping the few barcodes that overlap rarely changes your data.
Too many overlapping codes cause image saturation, and then data loss is possible, although serious loss from saturation is uncommon.

The nCounter instrument calculates the number of optical features per square micron for each lane while it processes the images.
This is the **Binding Density** (**BD**).
Use it to check whether image saturation compromised the data collection.

NACHO flags a lane when its **Binding Density** is outside the range of the preset:

* `0.05 - 2.25` for **MAX**, **FLEX** and **PRO** instruments (the legacy preset uses `0.1 - 2.25`).
* `0.1 - 1.8` for **SPRINT** instruments (the legacy preset uses `0.1 - 2.25`).

Within these ranges, few reporters on the slide surface overlap, so the instrument can count each reporter species accurately.
A **Binding Density** above the upper limit means that reporters overlap on the slide surface.
The instrument may have ignored many codes in such a lane, which can affect the quantification and the linearity of the assay.
