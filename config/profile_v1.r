
config = list(
	# Load only selected columns (Keep memory footprint reasonable)
	load_columns = list(
		"amee_company_id",
		"emissions_total",
		"amee_industry_score",
		"annual_sales_local"
	),
	
	# Rename columns to standard names
	rename_columns = list(),

	# Execute raw code
	transform_data = list()
)