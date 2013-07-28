
config = list(
	# Load only selected columns (Keep memory footprint reasonable)
	load_columns = list(
		"amee_id",
		"est_mtco2e",
		"score_d",
		"uk_modsal",
		"au_modsal"
	),
	
	# Rename columns to standard names
	rename_columns = list(
		est_mtco2e="emissions_total",
		score_d="amee_industry_score",
		amee_id="amee_company_id",
		uk_modsal="annual_sales_local"
	),

	# Execute raw code
	transform_data = list(
		# If uk_modsal is missing, then use the value from au_modsal
		'missing_sales_indexes = (companies$annual_sales_local==0 | is.na(companies$annual_sales_local))',
		'companies$annual_sales_local[missing_sales_indexes] = companies$au_modsal[missing_sales_indexes]'
	)
)