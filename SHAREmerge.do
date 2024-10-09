// Define base path
global base_path "/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA"

// Read datasets
use "$base_path/SHARE/output/cluster_assignments/mergeshare_w1.dta", clear
save "$base_path/SHARE/output/temp_share_w1.dta", replace

use "$base_path/SHARE/output/share_w1.dta", clear
merge 1:1 mergeid using "$base_path/SHARE/output/temp_share_w1.dta"
save "$base_path/SHARE/output/merged_data1.dta", replace

use "$base_path/SHARE/output/cluster_assignments/mergeshare_w6.dta", clear
save "$base_path/SHARE/output/temp_share_w6.dta", replace

use "$base_path/SHARE/output/share_w6.dta", clear
merge 1:1 mergeid using "$base_path/SHARE/output/temp_share_w6.dta"
save "$base_path/SHARE/output/merged_data6.dta", replace

// Clean up temporary files
erase "$base_path/SHARE/output/temp_share_w1.dta"
erase "$base_path/SHARE/output/temp_share_w6.dta"

// Weigting 
set maxvar 50000
// Wave 1
use "$base_path/SHARE/output/H_SHARE_f.dta", clear
save "$base_path/SHARE/output/temp_OGSHARE.dta", replace

use "$base_path/SHARE/output/merged_data1.dta", clear
drop _merge
merge 1:1 mergeid using "$base_path/SHARE/output/temp_OGSHARE.dta"
svyset [pw=r1wtsamp], vce(linearized)
keep if _merge == 3
save "$base_path/SHARE/output/weightedSHAREW1.dta", replace
erase "$base_path/SHARE/output/temp_OGSHARE.dta"

// Wave 6
use "$base_path/SHARE/output/H_SHARE_f.dta", clear
save "$base_path/SHARE/output/temp_OGSHARE.dta", replace

use "$base_path/SHARE/output/merged_data6.dta", clear
drop _merge
merge 1:1 mergeid using "$base_path/SHARE/output/temp_OGSHARE.dta"
svyset [pw=r6wtsamp], vce(linearized)
keep if _merge == 3
save "$base_path/SHARE/output/weightedSHAREW6.dta", replace
erase "$base_path/SHARE/output/temp_OGSHARE.dta"



