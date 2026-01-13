* ====================================================================================================
* Data Cleaning Midline
* Author: Arianna Cima
* PART 1 OF DATASET
* ====================================================================================================

clear all
set more off
set maxvar 20000

* ===========================================================================================
* USER SETTINGS (CHANGE ONLY THIS LINE)
* ===========================================================================================
global USER "annai"

* ===========================================================================================
* PROJECT PATHS
* ===========================================================================================
global PROJECT "C:/Users/$USER/Documents/Mozambique_pro"
global DATA    "$PROJECT/data"

* ===========================================================================================
* IMPORT DATA (PART 1)
* ===========================================================================================
import excel ///
    "$DATA/MOZ Midline FINAL_December 2, 2025_16.33.xlsx", ///
    sheet("Sheet0") ///
    firstrow ///
    cellrange(A1:JKC894) ///
    clear

save "$DATA/moz_mid_part1.dta", replace

* ===========================================================================================
* LOAD DATA
* ===========================================================================================
use "$DATA/moz_mid_part1.dta", clear

* ---------------------------------------------------------------------------
* Narrow all string variables
* ---------------------------------------------------------------------------
ds, has(type string)
foreach v in `r(varlist)' {
    format `v' %12s
}

* Narrow all numeric variables
ds, has(type numeric)
foreach v in `r(varlist)' {
    format `v' %9.0g
}

* ---------------------------------------------------------------------------
* USE FIRST OBSERVATION AS VARIABLE LABELS
* ---------------------------------------------------------------------------
foreach var of varlist _all {
    capture confirm string variable `var'
    if !_rc {
        label var `var' "`=`var'[1]'"
    }
}
drop if _n == 1

* ---------------------------------------------------------------------------
* Drop empty metadata variables (remove if not needed)
* ---------------------------------------------------------------------------
foreach v in RecipientLastName RecipientFirstName RecipientEmail ///
            ExternalReference DistributionChannel {
    capture confirm variable `v'
    if !_rc {
        quietly count if !missing(`v')
        if r(N) == 0 drop `v'
    }
}

* ===============================================================================================================
* A Section - DATA Cleaning for respondent CORE VARIABLES
* ===============================================================================================================

* ===========================================================================================
* Duration cleaning
* ===========================================================================================

count if trim(Durationinseconds) == "0"

destring Durationinseconds, gen(duration_sec) force
label var duration_sec "Duration (seconds)"

gen double duration_h_frac = duration_sec / 3600
label var duration_h_frac "Duration (hours, fractional)"

quietly summarize duration_h_frac
scalar MEAN_H = r(mean)
scalar MIN_H  = r(min)
scalar MAX_H  = r(max)

* ===========================================================================================
* Day of interview cleaning
* ===========================================================================================

gen double rec_dt = clock(RecordedDate, "MDYhm")
format rec_dt %tc

gen rec_date = dofc(rec_dt)
format rec_date %td
label var rec_date "Interview date"

egen n_days = nvals(rec_date)
scalar N_DAYS = n_days[1]

gen rec_month = mofd(rec_date)
format rec_month %tm
label var rec_month "Interview month"

* ===========================================================================================
* Finished interview
* ===========================================================================================

gen byte finished_num = (trim(Finished) == "1")
label var finished_num "Finished interview (1=yes)"

quietly count if finished_num == 1
scalar N_FIN = r(N)

capture confirm numeric variable Finished
if _rc {
    destring Finished, replace force
}

* --- Apply Yes/No value label ---
label define Finished_lbl 1 "Yes" 2 "No", replace
label values Finished Finished_lbl
label var Finished "Survey finished"

* ===========================================================================================
* Overall counts
* ===========================================================================================

quietly count
scalar N_OBS = r(N)

* ===========================================================================================
* GPS
* ===========================================================================================

count if missing(LocationLatitude) | missing(LocationLongitude)
scalar N_no_gps = r(N)

* ===========================================================================================
* CORE SUMMARY TABLES (summary_overall.tex)
* ===========================================================================================

tempname fh
file open `fh' using "$DATA/summary_overall.tex", write replace

file write `fh' "\begin{tabular}{lr}" _n
file write `fh' "\toprule" _n
file write `fh' "Statistic & Value \\" _n
file write `fh' "\midrule" _n

file write `fh' "Observations & " %9.0f (N_OBS) " \\" _n
file write `fh' "Finished interviews & " %9.0f (N_FIN) " \\" _n
file write `fh' "Interview days & " %9.0f (N_DAYS) " \\" _n
file write `fh' "Min duration (hours) & " %9.2f (MIN_H) " \\" _n
file write `fh' "Mean duration (hours) & " %9.2f (MEAN_H) " \\" _n
file write `fh' "Max duration (hours) & " %9.2f (MAX_H) " \\" _n

file write `fh' "\bottomrule" _n
file write `fh' "\end{tabular}" _n
file close `fh'

* ===========================================================================================
* Summary by month (summary_by_month.tex)
* ===========================================================================================

preserve
collapse (count) N_obs = rec_date, by(rec_month)
sort rec_month

tempname fm
file open `fm' using "$DATA/summary_by_month.tex", write replace

file write `fm' "\begin{tabular}{lr}" _n
file write `fm' "\toprule" _n
file write `fm' "Month & Observations \\" _n
file write `fm' "\midrule" _n

forvalues i = 1/`=_N' {
    local mstr : display %tm rec_month[`i']
    file write `fm' "`mstr' & " %9.0f (N_obs[`i']) " \\" _n
}

file write `fm' "\bottomrule" _n
file write `fm' "\end{tabular}" _n
file close `fm'
restore

* ===========================================================================================
* Interviews by enumerator
* ===========================================================================================

destring INT_NAME, replace ignore(" ")

label define lbl_INT_NAME ///
 1  "Adérito Manjate" ///
 2  "Adil Sadeia" ///
 3  "António Amane" ///
 4  "Arminda Moçambique" ///
 5  "Auneta Nhanombe" ///
 8  "Camila Langa" ///
 11 "César Matsinha" ///
 12 "Chigule Sadique" ///
 13 "Dilsa Fumane" ///
 14 "Dinaria Hilário" ///
 15 "Edson Mondlane" ///
 17 "Egídio Vilanculos" ///
 18 "Etelvina Chambule" ///
 19 "Filomena Massolonga" ///
 20 "Hélio Mutombene" ///
 22 "Idelvilton Mapulaciane" ///
 23 "Jeremias Machanisse" ///
 24 "José Matonse" ///
 25 "Júlio Avanco" ///
 49 "Lequicio Tsamba" ///
 51 "Lurdes Cumbane" ///
 54 "Mara Tovele" ///
 57 "Maraya Cumbane" ///
 59 "Micas Sarmento" ///
 62 "Nelson Maulele" ///
 64 "Noa Vonbe" ///
 65 "Odete Júlio" ///
 67 "Pedro Bararano" ///
 70 "Sara Chabana" ///
 73 "Sérgio Ouane" ///
 82 "Valquíria Nhampossa" ///
 83 "Vircelio Miambo" ///
 84 "Virgina Sitoe" ///
 85 "Witor Ponzole" ///
 86 "Outro", replace

label values INT_NAME lbl_INT_NAME

decode INT_NAME, gen(INT_NAME_str)
preserve

collapse (count) N_interviews = rec_date, by(INT_NAME_str)
gsort -N_interviews INT_NAME_str

gen long idx = _n
local half = ceil(_N/2)

gen str40 INT_NAME_R = ""
gen double N_interviews_R = .

forvalues i = 1/`half' {
    local j = `i' + `half'
    if `j' <= _N {
        replace INT_NAME_R       = INT_NAME_str[`j'] in `i'
        replace N_interviews_R   = N_interviews[`j'] in `i'
    }
}

keep if idx <= `half'

* 5. Write LaTeX table
tempname fe
file open `fe' using "$DATA/summary_by_enumerator.tex", write replace

file write `fe' "\begin{tabular}{lr lr}" _n
file write `fe' "\toprule" _n
file write `fe' "Enumerator & Interviews & Enumerator & Interviews \\" _n
file write `fe' "\midrule" _n

forvalues i = 1/`=_N' {
    local L  = INT_NAME_str[`i']
    local Li = N_interviews[`i']
    local R  = INT_NAME_R[`i']
    local Ri = N_interviews_R[`i']

    if "`R'" == "" {
        file write `fe' "`L' & " %9.0f (`Li') " &  &  \\" _n
    }
    else {
        file write `fe' "`L' & " %9.0f (`Li') " & `R' & " %9.0f (`Ri') " \\" _n
    }
}

file write `fe' "\bottomrule" _n
file write `fe' "\end{tabular}" _n
file close `fe'

restore

rename Q2328 INT_NAME_oth

* ...existing code...
