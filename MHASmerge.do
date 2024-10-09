// Define base path
global base_path "/Users/pietromonti/Library/CloudStorage/OneDrive-Universita'degliStudidiRomaTorVergata/RA"

// Read datasets
use "$base_path/MHAS/output/cluster_assignments/mergemhas_w2.dta", clear
save "$base_path/MHAS/output/temp_mhas_w2.dta", replace

use "$base_path/MHAS/output/mhas_w2.dta", clear
merge 1:1 unhhidnp using "$base_path/MHAS/output/temp_mhas_w2.dta"
save "$base_path/MHAS/output/cluster_assignments/merged_data2.dta", replace

use "$base_path/MHAS/output/cluster_assignments/mergemhas_w4.dta", clear
save "$base_path/MHAS/output/temp_mhas_w4.dta", replace

use "$base_path/MHAS/output/mhas_w4.dta", clear
merge 1:1 unhhidnp using "$base_path/MHAS/output/temp_mhas_w4.dta"
save "$base_path/MHAS/output/cluster_assignments/merged_data4.dta", replace

// Clean up temporary files
erase "$base_path/MHAS/output/temp_mhas_w2.dta"
erase "$base_path/MHAS/output/temp_mhas_w4.dta"


// Wave 2
use "$base_path/MHAS/output/H_MHAS_c2.dta", clear
save "$base_path/MHAS/output/temp_OGMHAS.dta", replace

use "$base_path/MHAS/output/cluster_assignments/merged_data2.dta", clear
drop _merge
merge 1:1 unhhidnp using "$base_path/MHAS/output/temp_OGMHAS.dta"
svyset [pw=r2wtresp], vce(linearized)
keep if _merge == 3
save "$base_path/MHAS/output/weightedMHASW2.dta", replace
erase "$base_path/MHAS/output/temp_OGMHAS.dta"

// Wave 4
use "$base_path/MHAS/output/H_MHAS_c2.dta", clear
save "$base_path/MHAS/output/temp_OGMHAS.dta", replace

use "$base_path/MHAS/output/cluster_assignments/merged_data4.dta", clear
drop _merge
merge 1:1 unhhidnp using "$base_path/MHAS/output/temp_OGMHAS.dta"
svyset [pw=r4wtresp], vce(linearized)
keep if _merge == 3
save "$base_path/MHAS/output/weightedMHASW4.dta", replace
erase "$base_path/MHAS/output/temp_OGMHAS.dta"
