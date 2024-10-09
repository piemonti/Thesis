// Define base path
global base_path "/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA"

// Read datasets
use "$base_path/CHARLS/output/cluster_assignments/mergecharls_w1.dta", clear
save "$base_path/CHARLS/output/temp_charls_w1.dta", replace

use "$base_path/CHARLS/output/charls_w1.dta", clear
merge 1:1 ID using "$base_path/CHARLS/output/temp_charls_w1.dta"
save "$base_path/CHARLS/output/cluster_assignments/merged_data1.dta", replace

use "$base_path/CHARLS/output/cluster_assignments/mergecharls_w4.dta", clear
save "$base_path/CHARLS/output/temp_charls_w4.dta", replace

use "$base_path/CHARLS/output/charls_w4.dta", clear
merge 1:1 ID using "$base_path/CHARLS/output/temp_charls_w4.dta"
save "$base_path/CHARLS/output/cluster_assignments/merged_data4.dta", replace

// Clean up temporary files
erase "$base_path/CHARLS/output/temp_charls_w1.dta"
erase "$base_path/CHARLS/output/temp_charls_w4.dta"


// Wave 3
use "$base_path/CHARLS/output/H_CHARLS.dta", clear
save "$base_path/CHARLS/output/temp_OGCHARLS.dta", replace

use "$base_path/CHARLS/output/cluster_assignments/merged_data1.dta", clear
drop _merge
merge 1:1 ID using "$base_path/CHARLS/output/temp_OGCHARLS.dta"
svyset [pw=r1wtresp], vce(linearized)
keep if _merge == 3
save "$base_path/CHARLS/output/weightedCHARLSW1.dta", replace
erase "$base_path/CHARLS/output/temp_OGCHARLS.dta"

// Wave 8
use "$base_path/CHARLS/output/H_CHARLS.dta", clear
save "$base_path/CHARLS/output/temp_OGCHARLS.dta", replace

use "$base_path/CHARLS/output/cluster_assignments/merged_data4.dta", clear
drop _merge
merge 1:1 ID using "$base_path/CHARLS/output/temp_OGCHARLS.dta"
svyset [pw=r4wtresp], vce(linearized)
keep if _merge == 3
save "$base_path/CHARLS/output/weightedCHARLSW4.dta", replace
erase "$base_path/CHARLS/output/temp_OGCHARLS.dta"
