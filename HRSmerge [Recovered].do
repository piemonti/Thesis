// Define base path
global base_path "/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA"

// Read datasets
use "$base_path/HRS/output/cluster_assignments/mergehrs_w7.dta", clear
save "$base_path/HRS/output/temp_hrs_w7.dta", replace

use "$base_path/HRS/output/hrs_w7.dta", clear
merge 1:1 hhidpn using "$base_path/HRS/output/temp_hrs_w7.dta"
save "$base_path/HRS/output/cluster_assignments/merged_data7.dta", replace

use "$base_path/HRS/output/cluster_assignments/mergehrs_w12.dta", clear
save "$base_path/HRS/output/temp_hrs_w12.dta", replace

use "$base_path/HRS/output/hrs_w12.dta", clear
merge 1:1 hhidpn using "$base_path/HRS/output/temp_hrs_w12.dta"
save "$base_path/HRS/output/cluster_assignments/merged_data12.dta", replace

erase "$base_path/HRS/output/temp_hrs_w7.dta"
erase "$base_path/HRS/output/temp_hrs_w12.dta"

// Weigting 
set maxvar 50000
// Wave 7
use "$base_path/HRS/output/HRS_harmonised.dta", clear
drop _merge
save "$base_path/HRS/output/temp_OGHRS.dta", replace

use "$base_path/HRS/output/cluster_assignments/merged_data7.dta", clear
drop _merge
merge 1:1 hhidpn using "$base_path/HRS/output/temp_OGHRS.dta"
svyset [pw=r7wtresp], vce(linearized)
keep if _merge == 3
save "$base_path/HRS/output/weightedHRSW7.dta", replace
erase "$base_path/HRS/output/temp_OGHRS.dta"

// Wave 12
use "$base_path/HRS/output/HRS_harmonised.dta", clear
drop _merge
save "$base_path/HRS/output/temp_OGHRS.dta", replace

use "$base_path/HRS/output/cluster_assignments/merged_data12.dta", clear
drop _merge
merge 1:1 hhidpn using "$base_path/HRS/output/temp_OGHRS.dta"
svyset [pw=r7wtresp], vce(linearized)
keep if _merge == 3
save "$base_path/HRS/output/weightedHRSW12.dta", replace
erase "$base_path/HRS/output/temp_OGHRS.dta"


