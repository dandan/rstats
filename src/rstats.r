#!/usr/bin/env Rscript
# Load from Megafile


library(optparse)
source("plot_charts.r")
options("scipen"=100, "digits"=4)

options(error = function() traceback(2))

get_options <- function() {
	option_list <- list(
		make_option("--input", help = "Companies CSV file to load"),
		make_option("--config", help= "Configuration file")
	)
	options = parse_args(OptionParser(option_list = option_list))
	if(is.null(options$config)){
		stop("You must set the option --config <path_to_config.rb>")
	}
	
	source(options$config)
	config$input = options$input
	return(config)
}

# Load and tranform the data into normalized form
load_companies_csv <- function(companies_csv_path, options) {
	print("=============================================================================")
	print(sprintf("Loading: '%s'", companies_csv_path))
	print("=============================================================================")
	col_classes = load_col_classes(companies_csv_path, options)
	benchmark = system.time(companies <- read.csv(companies_csv_path, header=TRUE, colClasses=col_classes))
	
	print(sprintf("Finished loading %d row(s) with %d col(s) in %ds", dim(companies)[1], dim(companies)[2], round(benchmark[3])))

	print("Sample of raw data loaded:")
	print("--------------------------------------------------------")
	print(head(companies,4))
	print("--------------------------------------------------------")
	
	print("=============================================================================")
	print("Renaming columns")
	print("=============================================================================")
	for (old_name in names(options$rename_columns)) {
		new_name = options$rename_columns[old_name]
		print(sprintf("Renaming column: %s ====> %s", old_name, new_name))
		names(companies)[names(companies) == old_name] = new_name
	}
	
	print("=============================================================================")
	print("Processing custom transformations")
	print("=============================================================================")
	for (code in options$transform_data) {
		print(sprintf("Executing: %s", code))
		eval(parse(text=code))
	}
	print("Data after transformations:")
	print("--------------------------------------------------------")
	print(head(companies,4))
	print("--------------------------------------------------------")
	
	return(companies)
}


# Figure out which column indexes to load
load_col_classes <- function(companies_csv_path, options) {
	col_names <- colnames(read.csv(sprintf("%s", companies_csv_path), header=TRUE, nrows=1))
	load_column_indexes = which(col_names %in% options$load_columns)
		
	print("Found the following columns:")
	print(col_names[load_column_indexes])
	print("With indexes:")
	print(load_column_indexes)
	
	col_classes = rep("NULL", length(col_names))
	col_classes[load_column_indexes] = NA
	
	return(col_classes)
}

run <- function() {
	options = get_options()
	companies = load_companies_csv(options$input, options)
	
	plot_score_histogram(companies, output_to_file="../output/01_distribution_amee_industry_score.png")
	plot_emissions_histogram(companies, output_to_file="../output/02_distribution_emissions_total.png")
	plot_annual_sales_histogram(companies, output_to_file="../output/03_distribution_annual_sales_local.png")
	#plot_total_assets_histogram(companies, output_to_file="../output/04_distribution_total_assets_local.png")
	#plot_employees_total_histogram(companies, output_to_file="../output/05_distribution_employees_total.png")
	plot_revenue_vs_emissions_heatmap(companies, output_to_file="../output/06_heatmap_revenue_vs_emissions.png")
	plot_revenue_vs_score_heatmap(companies, output_to_file="../output/07_heatmap_revenue_vs_score.png")
	plot_emissions_vs_score_heatmap(companies, output_to_file="../output/08_heatmap_emissions_vs_score.png")
	plot_financial_intensity_vs_score_heatmap(companies, output_to_file="../output/09_heatmap_financial_intensity_vs_score.png")
	plot_financial_intensity_histogram(companies, output_to_file="../output/10_distribution_financial_intensity.png")	
}

run()

