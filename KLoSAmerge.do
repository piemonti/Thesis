// Define base path
global base_path "/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA"

// Read datasets
use "$base_path/KLoSA/output/cluster_assignments/mergeklosa_w3.dta", clear
save "$base_path/KLoSA/output/temp_klosa_w3.dta", replace

use "$base_path/KLoSA/output/klosa_w3.dta", clear
merge 1:1 pid using "$base_path/KLoSA/output/temp_klosa_w3.dta"
save "$base_path/KLoSA/output/cluster_assignments/merged_data3.dta", replace

use "$base_path/KLoSA/output/cluster_assignments/mergeklosa_w8.dta", clear
save "$base_path/KLoSA/output/temp_klosa_w8.dta", replace

use "$base_path/KLoSA/output/klosa_w8.dta", clear
merge 1:1 pid using "$base_path/KLoSA/output/temp_klosa_w8.dta"
save "$base_path/KLoSA/output/cluster_assignments/merged_data8.dta", replace

// Clean up temporary files
erase "$base_path/KLoSA/output/temp_klosa_w3.dta"
erase "$base_path/KLoSA/output/temp_klosa_w8.dta"

// Wave 3
use "$base_path/KLoSA/output/H_KLoSA_e2.dta", clear
save "$base_path/KLoSA/output/temp_OGKLoSA.dta", replace

use "$base_path/KLoSA/output/cluster_assignments/merged_data3.dta", clear
drop _merge
merge 1:1 pid using "$base_path/KLoSA/output/temp_OGKLoSA.dta"
svyset [pw=r3wtresp], vce(linearized)
keep if _merge == 3
save "$base_path/KLoSA/output/weightedKLoSAW3.dta", replace
erase "$base_path/KLoSA/output/temp_OGKLoSA.dta"

// Wave 8
use "$base_path/KLoSA/output/H_KLoSA_e2.dta", clear
save "$base_path/KLoSA/output/temp_OGKLoSA.dta", replace

use "$base_path/KLoSA/output/cluster_assignments/merged_data8.dta", clear
drop _merge
merge 1:1 pid using "$base_path/KLoSA/output/temp_OGKLoSA.dta"
svyset [pw=r8wtresp], vce(linearized)
keep if _merge == 3
save "$base_path/KLoSA/output/weightedKLoSAW8.dta", replace
erase "$base_path/KLoSA/output/temp_OGKLoSA.dta"
