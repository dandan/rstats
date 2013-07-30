# ----------------------------------------------------------------------------
# Sample of what we can do with R
# 
# Run this file 1 line at a time fro within RStudio.
# 
# AIMS:
# - Load a companies CSV file
# - Inspect some data
# - Plot a histogram of scores
# - Plot a heatmap of emissions/sales VS score
# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# Load a companies CSV file
# ----------------------------------------------------------------------------
# Load companies
companies = read.csv("data/sample_companies_10000.csv", header=TRUE)

# Load only the columns we want from companies
col_classes = rep("NULL", 152)
col_classes[36] = NA
col_classes[140] = NA
col_classes[141] = NA
companies = read.csv("data/sample_companies_10000.csv", header=TRUE, colClasses=col_classes)

# ----------------------------------------------------------------------------
# Inspect some data
# ----------------------------------------------------------------------------
# "companies" object is a "data.frame"
class(companies)

# Print the first few rows of the loaded companies
companies

# Can also view as a table in RStudio
View(companies)

# Get the dimensions of the companies data.frame
print(dim(companies))


# ----------------------------------------------------------------------------
# - Plot a histogram of scores
# ----------------------------------------------------------------------------

# Plot a basic histogram
h = hist(companies$score_d)

# Make it red
h = hist(companies$score_d, col="red")

# Inspect the histogram data
h

# The "breaks" indicate how the graph is grouped.  i.e. One bar = 5 scores

# Set the bucket sizes
h = hist(companies$score_d, breaks=c(0:100), col="red")

# Now we can see the breaks makes more sense
h

# We can extract the counts if we want
h$counts

# Fix the axis labels - draw the histogram without labels
h = hist(companies$score_d, breaks=c(0:100), col="red", axes=FALSE)

# Add the X axis labels - 1 label every 5 scores
axis(1, at=seq(0, 100, 5))

# Add the Y axis labels
axis(2)

# Rotate the Y axis labels
axis(2, las=2)

# ----------------------------------------------------------------------------
# - Plot a heatmap of emissions/sales VS score
# ----------------------------------------------------------------------------
h = hist(raw_data, breaks=buckets, plot=FALSE)

# Remove rows with missing data
complete_companies = companies[complete.cases(companies),]
dim(complete_companies)

# Calculate financial entensity
complete_companies$financial_intensities = complete_companies$est_mtco2e / complete_companies$turnover * 1000
complete_companies$financial_intensities_log10 = log10(complete_companies$financial_intensities)
View(complete_companies)

# Draw heatmap
ss = smoothScatter(complete_companies$financial_intensities_log10, complete_companies$score_d, nbin = 128)
