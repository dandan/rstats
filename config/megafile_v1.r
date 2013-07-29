
config = list(
	# Load only selected columns (Keep memory footprint reasonable)
	load_columns = list(
		"amee_id",
		"est_mtco2e",
		"score_d",
		"turnover"
	),
	
	# Rename columns to standard names
	rename_columns = list(
		est_mtco2e="emissions_total",
		score_d="amee_industry_score",
		amee_id="amee_company_id",
		turnover="annual_sales_local"
	),

	# Execute raw code
	transform_data = list(
	)
)
