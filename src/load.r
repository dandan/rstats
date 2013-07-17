#!/usr/bin/env Rscript



# ---------------------------------------------------------------------------
# Functions
# ---------------------------------------------------------------------------

# Example score histogram
plot_example_score_histogram -< function {
  data = floor(runif(2000000, 1, 101))
  png("~/expected_score_histogram.png",width=1200, height=1000,units="px",bg = "white")
  h = hist(data, breaks=c(0:100), col="red", axes=FALSE)
  axis(1, at=seq(0, 100, 5))
  axis(2, at=seq(0, round(max(h$counts)/10000)*10000, 20000), las=2)
  garbage <- dev.off()
  
}

  
  
# Score distribution
plot_score_histogram <- function(output_to_file=FALSE) {
  if (output_to_file != FALSE){
    png(output_to_file,width=1200, height=1000,units="px",bg = "white")
  }
  h = hist(companies$amee_industry_score, breaks=c(0:100), col="red", axes=FALSE)
  axis(1, at=seq(0, 100, 5))
  axis(2, at=seq(0, round(max(h$counts)/10000)*10000, 20000), las=2)
  if (output_to_file != FALSE){
    garbage <- dev.off()
  }
}
  
# Fuction - Draw log10 x log10 histogram
histogram_log10 <- function(raw_data, title="Histogram", xlab='Count', ylab='Amount', col='yellow', output_to_file='histogram.png'){
  if (output_to_file != FALSE){
    png(output_to_file, width=1200,height=1000, units="px", bg="white")
  }
  par(mar = c(6.5, 6.5, 5, 0.5), mgp = c(5, 1, 0))

  potential_buckets = c(0, 0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 10000000000, 100000000000, 1000000000000, 10000000000000)
  potential_labels = c('missing', '0', '0-10', '10-100', '100-1K', '1K-10K', '10K-100K', '100K-1M', '1M-10M', '10M-100M', '100M-1B', '1B-10B', '10B-100B', '100B-1T', '1T-10T')
  
  x_max_exp = floor(log10(max(raw_data, na.rm=TRUE)))+3
  x_max_exp
  buckets = potential_buckets[1:x_max_exp]
  labels = potential_labels[1:x_max_exp]

  h = hist(raw_data, breaks=buckets, plot=FALSE)
  counts = c(sum(is.na(raw_data)), h$counts)
  counts_no_zero = counts
  counts_no_zero[counts_no_zero==0] <- 1

  bp = barplot(counts_no_zero, log="y", axes=FALSE, col=col, main=title, xlab=xlab, ylab=ylab)
    
  axis(1, at=bp, labels=labels, las=2)  
  axis(2, at=10^seq(0, 10), las=2)
  text(bp, counts, format(counts), xpd = TRUE, col="black", pos=3)
  
  if (output_to_file != FALSE){
    garbage <- dev.off()
  }
}



# Total Emissions distribution (log10)
plot_emissions_histogram <- function(output_to_file=FALSE) {
  
  histogram_log10(companies$emissions_total, 
                  title="Histogram of companies$emissions_total", 
                  xlab='Emissions Total',
                  ylab='Number of Companies',
                  col='red',
                  output_to_file=output_to_file)
}


# Annual Sales distribution (log10)
plot_annual_sales_histogram <- function(output_to_file=FALSE) {
  histogram_log10(companies$annual_sales_local, 
                title="Histogram of companies$annual_sales_local", 
                xlab='Annual Sales Local',
                ylab='Number of Companies',
                col='green',
                output_to_file=output_to_file)
}

# Total Assets distribution (log10)
plot_total_assets_histogram <- function(output_to_file=FALSE) {
  histogram_log10(companies$total_assets_local, 
                title="Histogram of companies$total_assets_local", 
                xlab='Total Assets Local',
                ylab='Number of Companies',
                col='blue',
                output_to_file=output_to_file)
}

# Employees Total distribution (log10)
plot_employees_total_histogram <- function(output_to_file=FALSE) {
  histogram_log10(companies$employees_total, 
                title="Histogram of companies$employees_total", 
                xlab='Employees Total',
                ylab='Number of Companies',
                col='lightblue',
                #output_to_file=FALSE)
                output_to_file=output_to_file)
}


# Annual Sales vs Emissions - Heatmap
plot_revenue_vs_emissions_heatmap <- function(output_to_file=FALSE) {
  library(MASS)
  library(hexbin)
  
  if (output_to_file != FALSE){
    png(output_to_file, width=1200, height=1000, units="px", bg="white")
  }
  
  keep_emissions = companies$emissions_total >= 1
  emissions = companies$emissions_total[keep_emissions]
  annual_sales = companies$annual_sales_local[keep_emissions]
  
  keep_sales =annual_sales >= 1
  emissions = emissions[keep_sales]
  annual_sales = annual_sales[keep_sales]
  
  labels = c('1', '10', '100', '1K', '10K', '100K', '1M', '10M', '100M', '1B', '10B', '100B', '1T', '10T')
  
  log10_emissions = log10(emissions)
  log10_annual_sales = log10(annual_sales)
    
  palette <- colorRampPalette(c("white", "green", "yellow", "orange", "red"), space = "Lab", bias=2, interpolate='linear')
  
  ss = smoothScatter(log10_annual_sales, log10_emissions, nbin = 128, colramp=palette, axes=FALSE)
  
  axis(1, at=seq(0, length(labels)-1), labels=labels)
  axis(2, at=seq(0, length(labels)-1), labels=labels, las=2)
  
  max(annual_sales, na.rm=TRUE)
  
  if (output_to_file != FALSE){
    garbage <- dev.off()
  }
}

# Annual Sales vs Score - Heatmap
plot_revenue_vs_score_heatmap <- function(output_to_file=FALSE) {
  library(MASS)
  library(hexbin)
  
  if (output_to_file != FALSE){
    png(output_to_file, width=1200,height=1000, units="px", bg="white")
  }
  
  keep_scores = companies$amee_industry_score >= 1
  scores = companies$amee_industry_score[keep_scores]
  annual_sales = companies$annual_sales_local[keep_scores]
  
  keep_sales =annual_sales >= 1
  scores = scores[keep_sales]
  annual_sales = annual_sales[keep_sales]
  
  labels = c('1', '10', '100', '1K', '10K', '100K', '1M', '10M', '100M', '1B', '10B', '100B', '1T', '10T')
  
  log10_annual_sales = log10(annual_sales)
  
  palette <- colorRampPalette(c("white", "green", "yellow", "orange", "red"), space = "Lab", bias=2, interpolate='linear')
  
  ss = smoothScatter(log10_annual_sales, scores, nbin = 128, colramp=palette, axes=FALSE)
  axis(1, at=seq(0, length(labels)-1), labels=labels, las=2)
  axis(2, at=seq(0,100))
    
  if (output_to_file != FALSE){
    garbage <- dev.off()
  }
}


# Emissions vs Score - Heatmap
plot_emissions_vs_score_heatmap <- function(output_to_file=FALSE) {
  library(MASS)
  library(hexbin)
  
  if (output_to_file != FALSE){
    png(output_to_file, width=1200,height=1000, units="px", bg="white")
  }
  
  keep_emissions = companies$emissions_total >= 1
  emissions = companies$emissions_total[keep_emissions]
  scores = companies$amee_industry_score[keep_emissions]
  
  keep_scores = scores >= 1
  emissions = emissions[keep_scores]
  scores = scores[keep_scores]
  
  labels = c('1', '10', '100', '1K', '10K', '100K', '1M', '10M', '100M', '1B', '10B', '100B', '1T', '10T')
  
  log10_emissions = log10(emissions)
  
  palette <- colorRampPalette(c("white", "green", "yellow", "orange", "red"), space = "Lab", bias=2, interpolate='linear')
  
  ss = smoothScatter(log10_emissions, scores, nbin = 128, colramp=palette, axes=FALSE)
  axis(1, at=seq(0, length(labels)-1), labels=labels, las=2)
  axis(2, at=seq(0,100))
  
  if (output_to_file != FALSE){
    garbage <- dev.off()
  }
}

# Emissions vs Score - Heatmap
plot_financial_intensity_vs_score_heatmap <- function(output_to_file=FALSE) {
  library(MASS)
  library(hexbin)
  
  if (output_to_file != FALSE){
    png(output_to_file, width=1200,height=1000, units="px", bg="white")
  }
  
  keep_emissions = (companies$emissions_total >= 1 & !is.na(companies$emissions_total))
  emissions = companies$emissions_total[keep_emissions]
  scores = companies$amee_industry_score[keep_emissions]
  annual_sales = companies$annual_sales_local[keep_emissions]
  
  keep_scores = (scores >= 1 & !is.na(scores))
  emissions = emissions[keep_scores]
  scores = scores[keep_scores]
  annual_sales = annual_sales[keep_scores]
  
  keep_sales = (annual_sales >= 1 & !is.na(annual_sales))
  emissions = emissions[keep_sales]
  scores = scores[keep_sales]
  annual_sales = annual_sales[keep_sales]
  
  financial_intensities = (emissions / annual_sales) * 1000
  log10_financial_intensities = log10(financial_intensities)
  
  range = seq(floor(min(log10_financial_intensities)), ceiling(max(log10_financial_intensities)))
  labels = c('1/10T', '1/1T', '1/100B', '1/10B', '1/1B', '1/100M', '1/10M', '1/1M', '1/100K', '1/10K', '1/1K', '1/100', '1/10', '1', '10', '100', '1K', '10K', '100K', '1M', '10M', '100M', '1B', '10B', '100B', '1T', '10T')
  labels = labels[range+14]
  
  
  #log10_emissions = log10(emissions)

  
  palette <- colorRampPalette(c("white", "green", "yellow", "orange", "red"), space = "Lab", bias=2, interpolate='linear')
  
  ss = smoothScatter(log10_financial_intensities, scores, nbin = 128, colramp=palette, axes=FALSE)
  axis(1, at=range, labels=labels, las=2)
  axis(2, at=seq(0,100))
  
  if (output_to_file != FALSE){
    garbage <- dev.off()
  }
}

# plot_financial_intensity_histogram()
plot_financial_intensity_histogram <- function(output_to_file=FALSE) {
  library(MASS)
  library(hexbin)
  
  if (output_to_file != FALSE){
    png(output_to_file, width=1200,height=1000, units="px", bg="white")
  }
  
  keep_emissions = (companies$emissions_total >= 1 & !is.na(companies$emissions_total))
  emissions = companies$emissions_total[keep_emissions]
  scores = companies$amee_industry_score[keep_emissions]
  annual_sales = companies$annual_sales_local[keep_emissions]
  
  keep_scores = (scores >= 1 & !is.na(scores))
  emissions = emissions[keep_scores]
  scores = scores[keep_scores]
  annual_sales = annual_sales[keep_scores]
  
  keep_sales = (annual_sales >= 1 & !is.na(annual_sales))
  emissions = emissions[keep_sales]
  scores = scores[keep_sales]
  annual_sales = annual_sales[keep_sales]
  
  financial_intensities = (emissions / annual_sales) * 1000
  log_financial_intensities = log10(financial_intensities)
    
  h = hist(log_financial_intensities, breaks=100, col="red", 
           main='Histogram of financial intensity (Co2e kg / revenue GBP)', axes=FALSE)
  x_breaks = h$breaks
  x_breaks[(h$breaks %% 1) < 0.999 & (h$breaks %% 1 > 0.001)] = 0
  x_breaks = 10^x_breaks
  x_breaks = round(x_breaks, digits=6)
  x_breaks[x_breaks==1] = ''
  axis(1, at=h$breaks, labels=x_breaks, las=2)
  axis(2, at=seq(0, round(max(h$counts)/10000)*10000, 20000), las=2)

  # Financial intensity vs Score - Scatter
  
  if (output_to_file != FALSE){
    garbage <- dev.off()
  }
}


# ---------------------------------------------------------------------------
# MAIN
# ---------------------------------------------------------------------------


args <- commandArgs(TRUE)

if(FALSE) {
  #small_companies_csv_path = '~/local/amee/data/companies-20130226.small.csv'
  #small_companies_csv_path = '~/local/amee/data/companies-20130531.small.csv'
  #scompanies = read.csv(small_companies_csv_path, header=TRUE)
  #print(dim(scompanies))
  #companies_csv_path = '~/local/amee/companies-20130226.csv'
  #companies_csv_path = '~/local/amee/companies-20130531.csv'
  #companies_csv_path = '~/local/amee/au_score_data-20130620.csv'
  # companies_csv_path = args[1]
  companies = read.csv(companies_csv_path, header=TRUE)
  save(list = ls(all = TRUE), file = "~/local/amee/companies-20130531.RData")
} else if(FALSE) {
  companies_csv_path = '/home/daniel/local/amee/au_score_data_final_20130701.csv'
  data = read.csv(companies_csv_path, header=TRUE)
  names(data)[names(data) == 'emissions_modelled_total'] = 'emissions_total'
  names(data)[names(data) == 'turnover_modelled'] = 'annual_sales_local'
  names(data)[names(data) == 'industry_score'] = 'amee_industry_score'
  names(data)[names(data) == 'amee_id'] = 'amee_company_id'
  names(data)
  companies = data
} else {
  load("~/local/amee/companies-20130531.RData")
}

print(dim(companies))
options("scipen"=100, "digits"=4)

if (TRUE) {
  # Plot to file
  plot_score_histogram(output_to_file="01_distribution_amee_industry_score.png")
  plot_emissions_histogram(output_to_file="02_distribution_emissions_total.png")
  plot_annual_sales_histogram(output_to_file="03_distribution_annual_sales_local.png")
  plot_total_assets_histogram(output_to_file="04_distribution_total_assets_local.png")
  plot_employees_total_histogram(output_to_file="05_distribution_employees_total.png")
  plot_revenue_vs_emissions_heatmap(output_to_file="06_heatmap_revenue_vs_emissions.png")
  plot_revenue_vs_score_heatmap(output_to_file="07_heatmap_revenue_vs_score.png")
  plot_emissions_vs_score_heatmap(output_to_file="08_heatmap_emissions_vs_score.png")
  plot_financial_intensity_vs_score_heatmap(output_to_file="09_heatmap_financial_intensity_vs_score.png")
  plot_financial_intensity_histogram(output_to_file="10_distribution_financial_intensity.png")
} else {
  # Plot to screen
  plot_score_histogram()
  plot_emissions_histogram()
  plot_annual_sales_histogram()
  plot_total_assets_histogram()
  plot_employees_total_histogram()
  plot_revenue_vs_emissions_heatmap()
  plot_revenue_vs_score_heatmap()
  plot_emissions_vs_score_heatmap()
  plot_financial_intensity_vs_score_heatmap()
  plot_financial_intensity_histogram()  
}


# 
# png('histogram_score_diff_v19_v21_raw.png', width=1200,height=1000, units="px", bg="white")
# 
# companies2 = data
# merged = merge(companies, companies2, by="amee_company_id")
# diff = merged['amee_industry_score.y'] - merged['amee_industry_score.x']
# 
# score_diff <- diff[!is.na(diff)]
# 
# h = hist(score_diff, breaks=c(-100:100), col="red", axes=FALSE)
# axis(1, at=seq(-100, 100, 5))
# axis(2, at=seq(0, round(max(h$counts)/10000)*10000, 20000), las=2)
# garbage <- dev.off()
# 
# 
# png('histogram_score_diff_random.png', width=1200,height=1000, units="px", bg="white")
# random1 = sample(1:100,length(score_diff), replace=T)
# random2 = sample(1:100,length(score_diff), replace=T)
# rdiff = random1 - random2
# hr = hist(rdiff, breaks=c(-100:100), col="red", axes=TRUE)
# garbage <- dev.off()
# 
# diff_fixed = h$counts - hr$counts
# 
# bp = barplot(diff_fixed, col="blue")
