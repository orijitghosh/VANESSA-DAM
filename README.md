# VANESSA-DAM&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img src="VANESSA-DAM-CRA/VANESSA hex.png" alt="VANESSA hex" width="150" />

Visual ANalysis of timE SerieS dAta - Drosophila Activity Monitors (VANESSA-DAM) is a collection of useful tools to visualize and analyze Time series data obtained from Drosophila Activity Monitors (https://www.trikinetics.com/). The first in the series of tools is a shiny app for circadian rhythm analysis and visualization - VANESSA-DAM for circadian rhythm analysis (**VANESSA-DAM-CRA**). For any suggestions, questions, troubleshooting or customization, please contact arijitghosh2009@gmail.com.

## **VANESSA-DAM-CRA**

VANESSA-DAM-CRA is dependent on [Quentin Geissmann](https://github.com/qgeissmann)'s [rethomics](https://github.com/rethomics) family of packages - [behavr](https://github.com/rethomics/behavr), [damr](https://github.com/rethomics/damr), [ggetho](https://github.com/rethomics/ggetho), [zeitgebr](https://github.com/rethomics/zeitgebr), for some analysis and visualization options. It offers several advantages over existing tools for circadian rhythm analysis from DAM systems, some mentionable ones are - 

1. Analysis and visualization of multiple monitor files, genotypes, replicates together in a high-throughput manner.

2. Automatic period power detection through a range of periodogram methods.

3. Producing high-resolution publication-quality figures with a plethora of customization.

4. Data curation - automatic user-defined parameter based deletion of dead and arrhythmic individuals.

5. Individual wise CWT spectrograms.

6. Visual comparison among genotypes, replicates.

7. Timeseries filtering with kernel smoothing and Butterworth filters.

8. Reproducible code report so that you can generate the figures and analysis without the shiny app from RStudio directly.

A short tutorial is provided (*Easy tutorial to start using VANESSA-DAM-CRA.pdf*) which is self explanatory and should help beginners start using the app right away.
