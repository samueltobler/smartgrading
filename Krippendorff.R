## © 2023 Samuel Tobler

# This script allows you to determine Krippendorff's alpha inter-rater reliability

# Load necessary packages
require(icr)

# Load data
manual.evaluation <- read.csv(file = "../ManualRatings.csv", sep = ",", header = T)
smart.evalation <- read.csv(file = "../SmartRatings.csv", sep = ",", header = T)

# Data preparation
## potentially, you need to specify the column. 
## this can be done by writing instead: manual.evaluation$COLUMNNAME

df <- data.frame(Manual = as.numeric(manual.evaluation), AIbased = as.numeric(smart.evalation))

# Transformation
df.matrix <- t(as.matrix(df))

# additionally compute bootstrapped uncertainty estimates for alpha
alpha <- krippalpha(df.matrix, metric = "ordinal", bootstrap = TRUE, bootnp = F, nboot = 10000)
alpha

