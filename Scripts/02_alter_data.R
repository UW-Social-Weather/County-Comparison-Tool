# Alter data frames 


vaccine_trends <- readRDS(here(data_dir, "aim_1/New/01_vaccine_trends.RDS"))
sdi <- readRDS(here(data_dir, "aim_1/New/02_sdi.RDS"))
sdi$sdi[sdi$year_id == '2020'] <- NA

#raw_extracted_dhs <- readRDS("aim_1/New/03_raw_extracted_dhs.RDS")
#prepped_dhs_for_mov <- readRDS("aim_1/New/04_prepped_dhs_for_mov.RDS")
disease_trends <- readRDS(here(data_dir, "aim_1/New/05_disease_trends.RDS"))
codebook <- read.csv(here(data_dir, "aim_2/vaccine_index_variable_codebook_for_website.csv"))
merged_data_for_visuals <- readRDS(here(data_dir,"aim_1/New/06_merged_data_for_visuals.RDS"))
merged_data_for_visuals$sdi[merged_data_for_visuals$year_id == '2020'] <- NA

vaccine_preventable_diseases <- read_excel(here(data_dir, "aim_1/vaccine_preventable_diseases.xlsx"))

#available disease data for cause name
merged_data_for_vac_dis <- dplyr::left_join(vaccine_preventable_diseases,disease_trends, "cause_name", "cause_name")
# available vaccine data with description and cause name
preventable_vac_trend <- vaccine_trends

#aim_2
#index_results <- readRDS("aim_2/10_index_results.RDS")
# index_results <- readRDS(here(data_dir,"aim_2/11_index_results.RDS"))
index_results <- readRDS(here(data_dir,"aim_2/19_index_results_third_version.RDS")) #from 11/03/2022
index_results$sdi[index_results$year == '2020'] <- NA

sdi_dup <- sdi
colnames(sdi_dup)[2] <- "location"
colnames(sdi_dup)[4] <- "year"
merged_data_for_vacii_sdi <- dplyr::left_join(index_results,sdi_dup[,-c("sdi")], by=c("location","year"))
