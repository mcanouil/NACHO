The instrument divides each lane into a few hundred imaging sections called Fields of View (**FOV**).
The exact number depends on the system (**MAX**/**FLEX** or **SPRINT**) and on the scanner settings.
The system images each **FOV** separately.
It sums the barcode counts of all **FOV**s of a lane to get the raw count of each barcode target.
It then reports the number of **FOV**s that it imaged successfully as **FOV Counted**.

A large gap between the **FOV** that the instrument tried to image (**FOV Count**) and the **FOV** that it imaged successfully (**FOV Counted**) can point to a problem with imaging.
The recommended share of registered **FOV**s (**FOV Counted** over **FOV Count**) is `75 %`.
