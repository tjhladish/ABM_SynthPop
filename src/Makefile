# This is not really a good makefile but I'm focusing on making it work only right now.

unique_sch_catalog: unique_sch_catalog.R ../raw_data/SEGEY_school_data_2017/catalogo_escuelas.csv
	Rscript unique_sch_catalog.R
	
google_result_filter: unique_sch_catalog google_result_filter.R ../derived_data/sch_google_json_parsed.out ../raw_data/shapefiles_INEGI_SCINCE_2010/municipal.shp ../raw_data/shapefiles_INEGI_SCINCE_2010/loc_urb.shp ../raw_data/shapefiles_INEGI_SCINCE_2010/loc_rur.shp
	Rscript google_result_filter.R
	
augment_ntl: augment_ntl_data.R ../raw_data/nighttime_light_raster/SVDNB_npp_20150101-20151231_75N180W_vcm-orm-ntl_v10_c201701311200.avg_rade9.tif ../raw_data/shapefiles_INEGI_SCINCE_2010/municipal.shp ../raw_data/shapefiles_INEGI_SCINCE_2010/loc_urb.shp ../raw_data/shapefiles_INEGI_SCINCE_2010/loc_rur.shp ../raw_data/shapefiles_INEGI_SCINCE_2010/estatal.shp
	Rscript augment_ntl_data.R
	
impute_sch_dat: google_result_filter augment_ntl loc_imputation.R
	Rscript loc_imputation.R
	
final_sch_coords: impute_sch_dat merge_google_impute.R
	Rscript merge_google_impute.R
	
workplace_coords: noising_workplace_coords.R ../raw_data/denue_workplace/denue_inegi_31_.csv
	Rscript noising_workplace_coords.R
	
assign_people: assign_people_to_work_and_school.cpp
	g++ -O2 --std=c++17 assign_people_to_work_and_school.cpp -o assign_people

swap: determine_immunity_swap_probs.cpp
	g++ -g determine_immunity_swap_probs.cpp -o generate_swap_file

swap_omp: determine_immunity_swap_probs.cpp
	g++ -O2 -fopenmp determine_immunity_swap_probs.cpp -o generate_swap_file_omp
