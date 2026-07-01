# ghg-processor

[Ben Bond-Lamberty](https://scholar.google.com/citations?user=9ODDdTAAAAAJ) and [Stephanie J. Wilson](https://scholar.google.com/citations?hl=en&user=s27RJ-oAAAAJ), 2023

Processing code for LI-7810 gas concentration data

Basic use: put data and metadata into the "data/" folder, open `ghg-processor.qmd`
file and click "Render" in RStudio; this assumes you have the [Quarto](https://quarto.org) package
installed. Outputs will be saved into the "output/" folder:

* results_<date>.csv - flux calculations and QA/QC flags
* results_metadata_<date>.csv - metadata for the results file
* observations_<date>.csv - observed gas concentration data
* PDF plots for each observation

More advanced uses: change the defaults given in the `ghg-processor.qmd`
header, or call it from other script; see `examples.R`.
