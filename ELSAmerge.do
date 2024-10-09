// Define base path
global base_path "/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA"

// Read datasets
use "$base_path/ELSA/UKDA-5050-stata/output/cluster_assignments/mergeelsa_w2.dta", clear
save "$base_path/ELSA/UKDA-5050-stata/output/temp_elsa_w2.dta", replace

use "$base_path/ELSA/UKDA-5050-stata/output/elsa_w2.dta", clear
merge 1:1 idauniq using "$base_path/ELSA/UKDA-5050-stata/output/temp_elsa_w2.dta"
save "$base_path/ELSA/UKDA-5050-stata/output/cluster_assignments/merged_data2.dta", replace

use "$base_path/ELSA/UKDA-5050-stata/output/cluster_assignments/mergeelsa_w8.dta", clear
save "$base_path/ELSA/UKDA-5050-stata/output/temp_elsa_w8.dta", replace

use "$base_path/ELSA/UKDA-5050-stata/output/elsa_w8.dta", clear
merge 1:1 idauniq using "$base_path/ELSA/UKDA-5050-stata/output/temp_elsa_w8.dta"
save "$base_path/ELSA/UKDA-5050-stata/output/cluster_assignments/merged_data8.dta", replace

// Clean up temporary files
erase "$base_path/ELSA/UKDA-5050-stata/output/temp_elsa_w2.dta"
erase "$base_path/ELSA/UKDA-5050-stata/output/temp_elsa_w8.dta"


// Wave 2
use "$base_path/ELSA/UKDA-5050-stata/output/h_elsa_g3.dta", clear
save "$base_path/ELSA/UKDA-5050-stata/output/temp_OGELSA.dta", replace

use "$base_path/ELSA/UKDA-5050-stata/output/cluster_assignments/merged_data2.dta", clear
drop _merge
merge 1:1 idauniq using "$base_path/ELSA/UKDA-5050-stata/output/temp_OGELSA.dta"
svyset r2clust [pw=r2cwtresp], strata(r2strat) vce(linearized) // Include the stratification variable
keep if _merge == 3
keep if r2cohort_e == 1
save "$base_path/ELSA/UKDA-5050-stata/output/weightedELSAW2.dta", replace
erase "$base_path/ELSA/UKDA-5050-stata/output/temp_OGELSA.dta"

// Wave 8
use "$base_path/ELSA/UKDA-5050-stata/output/h_elsa_g3.dta", clear
save "$base_path/ELSA/UKDA-5050-stata/output/temp_OGELSA.dta", replace

use "$base_path/ELSA/UKDA-5050-stata/output/cluster_assignments/merged_data8.dta", clear
drop _merge
merge 1:1 idauniq using "$base_path/ELSA/UKDA-5050-stata/output/temp_OGELSA.dta"
svyset r8clust [pw=r8cwtresp], strata(r8strat) vce(linearized) // Include the stratification variable
keep if _merge == 3
keep if r8cohort_e == 1
save "$base_path/ELSA/UKDA-5050-stata/output/weightedELSAW8.dta", replace
erase "$base_path/ELSA/UKDA-5050-stata/output/temp_OGELSA.dta"	
