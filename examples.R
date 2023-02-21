# Example of how to call the ghg-processor.qmd file to do
# initial data processing, before proceeding with analysis
# BBL 2023-02-15

library(quarto)

# Run the processor file as if you click the "render" button
quarto_render("ghg-processor.qmd")
# This script could now read in the results from the output/ folder

# Run but don't generate individual graphs (which is a slow step), overriding
# the default ("SAVE_RESULTS_FIGURES: true"; see line 12 of ghg-processor.qmd)
quarto_render("ghg-processor.qmd",
              execute_params = list(SAVE_RESULTS_FIGURES = 0))

# Run with a different dead band default
quarto_render("ghg-processor.qmd",
              execute_params = list(DEAD_BAND_SEC = 30))

# If your data or metadata are someplace else, you can override the
# Quarto file location defaults
# This will not work unless you have this folder on your computer!
quarto_render("ghg-processor.qmd",
              execute_params = list(data_folder = "/Users/bpbond/mydata"))
