
# Analysis map ---- 
library(tidyverse)

# To Start from scratch, but keep datasets which would take long to load, a separate .Rdata
# exists which can be provided on request which contains the big datasets obtained
# through the loading scripts: #vision_dataset and full_dataset

# Loading

# Visual test ----
#source(file = "vision.R") # <- only run if re-loading data
# Data selection ----
#source(file = "load.R") # <- only run if re-loading data

# Processing

# Data Wrangling ----
source(file = "processing.R")
# Prepare ----
source(file = "prepare.R")
# Inferential analysis ----
source(file = "bayesian.R")
# Plots ----
source(file = "plotting.R")

# Save plots again
source(file = "plot_saving.R")


# Appendices
source(file = "appendices.R")

