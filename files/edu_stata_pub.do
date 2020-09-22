 import delimited "C:\Users\Fabio M\Documents\Research\AUS_edu\AU_edu_data\all_edu_stata.csv", clear

********************************************************************************
****************** FURTHER TIDY UP PRE ANALYSIS ********************************
********************************************************************************

*drop row I added to prevent stata from automatically 
drop if year==0

*due to issues with cross-institutional programmes 
drop if year <= 2005

drop if disc_code=="NA"

*create numerical ID for the fields
egen  disc_num = group(disc_code)
*informative labels for the field IDs
labmask disc_num, values(disc)

*uselss variable
drop v1

*perfect duplicates: identify and drop
duplicates report _all
duplicates tag _all, gen(dup)
drop if dup==1


*check if there are more than one unit observations per each year
duplicates report disc_code year   //there aren't!

*set dataset as panel
xtset disc_num year

***Identify gaps in the panel
sort disc_code year
gen gap = ((year[_n]!=year[_n-1]+1) & (disc_code[_n]==disc_code[_n-1]))

*one gap is in "education", weird narrow field within the education broad field 
*   that is recorded for only 3 years and then disappears
drop if disc_code=="079999"


*weird inequalities instead of sigle values. <10 recoded as 5, <5 recoded as 3
replace bachelor="5" if disc_code=="030599" & year==2010
replace bachelor="5" if disc_code=="050501" & year==2010
replace bachelor="3" if disc_code=="030599" & year==2012

*values "np" replaced with missing observations in bachelor
replace bachelor="." if bachelor=="np"

*bachelor numerical to string
destring bachelor, replace

*fix missing observations and destring stud_contrib
replace stud_contrib = "." if stud_contrib=="NA"
destring stud_contrib, replace

*another gap is in "personal services": it was intermittentl;y merged with 
*   "food and hospitality" ==> make them a single field. Luckily bachelor students 
*   for personal services are always 0 but for 3 in 2017 and 2 in 2018. So I am
*   adding these units to "food and hospitality manually".
replace bachelor=bachelor + 3  if year==2017 & disc=="foodandhospitality"
replace bachelor=bachelor + 2  if year==2018 & disc=="foodandhospitality"
drop if disc=="personalservices"

sort disc_code year
drop gap
gen gap = ((year[_n]!=year[_n-1]+1) & (disc_code[_n]==disc_code[_n-1]))

*look for units that are making the panel unbalanced
tsset disc_num year
tsspell, f(L.year == .)  

egen length = max(_seq), by(disc_code _spell) 

*identify short spells
gen short = (length < 13)
tab short

*drop these fields, almost all empty and 3-years long. When non-empty, the 
*   values are added to the appropriate field
drop if disc=="naturalandphysicalsciences"

drop if disc=="informationtechnology"

drop if disc=="engeneeringandrelatedtechnologies"

drop if disc=="health"

drop if disc=="managementandcommerce"

drop if disc=="societyandculture"
replace bachelor=bachelor + 4  if year==2009 & disc=="othersocietyandculture"

drop if disc=="creativearts"
replace bachelor=bachelor + 7  if year==2008 & disc=="othercreativearts"
replace bachelor=bachelor + 27 if year==2009 & disc=="othercreativearts"

drop if disc == "engineeringandrelatedtechnologies"

*this field is short and has a couple of missing observations=>drop
drop if disc_code=="129900"

*no remaining short fields
tab short

*generate linear time trend
gen t = year - 2006

*log transformation
gen ln_bachelor = ln(bachelor)
gen ln_stud_contrib = ln(stud_contrib)

*demeaned bachelor
//ssc install center
//help center
bysort disc_num (year) : center bachelor, gen(dem_bachelor)
bysort disc_num (year) : center ln_bachelor, gen(ln_dem_bachelor)

*construct dummy variable =1 after a field gets out of the NP program and 0 otherwise
gen de_treat = (((regexm(disc_code, "^07") == 1) | (regexm(disc_code, "^0603") == 1)) & (year>=2010)) | ///
			   ((regexm(disc_code, "^01") == 1) & (year>=2013))

*generate variables to identify discipline part of the first and second national 
* priority programme
gen np1 = ((regexm(disc_code, "^07") == 1) | (regexm(disc_code, "^0603") == 1))
gen np2 = regexm(disc_code, "^01") == 1 

*time-to-event for each NP programme and the two combined  
gen event_time1 = year - 2009 if np1 == 1
gen event_time2 = year - 2012 if np2 == 1
gen event_time = event_time1 if np1 == 1
replace event_time = event_time2 if np2 == 1

*label variables (for graphs)
label var bachelor "Commencing students"
label var ln_bachelor "ln(Commencing students)"
label var ln_dem_bachelor "ln(Commencing students)"
label var t "Time trend"
label var de_treat "Contribution increase"
label var event_time "Event time"
	   
cd "C:\Users\Fabio M\Documents\Research\AUS_edu\AU_edu_data"
save edu_stata_clean, replace

********************************************************************************
****************************** ANALYSIS ****************************************
********************************************************************************

********************************************************************************
cd "C:\Users\Fabio M\Documents\Research\AUS_edu\AU_edu_data"
use edu_stata_clean, clear

*keep only treated fields and check for presence of kink in enorolment growth
keep if treat_group==1 //keeps only evertreated fields

*check whether bachelor has values <1, undefined in logs
sum ln_bachelor bachelor

*export csv file
export delimited "C:\Users\Fabio M\Documents\Research\AUS_edu\AU_edu_data\just_NP.csv"

*FINAL PLOTS - COMBINED IN LATEX************************************************
cd "C:\Users\Fabio M\Documents\Research\AUS_edu"

*get nice palette
colorpalette9 plottig, hue nograph   //     "255 103 164" "0 188 216" "107 177 0" "229 135 0" "253 97 209"
return list 

*plot bachelor over time, national priority #1
list disc_code disc if (regexm(disc_code, "^06") == 1 | regexm(disc_code, "^07") == 1) & year == 2006
tw  ( line bachelor year if disc_code == "070199", lcolor(black) ) ///
	( line bachelor year if disc_code == "070300", lcolor("97 156 255") ) ///
	( line bachelor year if disc_code == "079900", lcolor("201 152 0") ) ///
	( line bachelor year if disc_code == "060399", lcolor("185 56 255") ) ///
	,scheme(fabcolor) ///
	ytitle("commencing students", height(+5)) ///
	xtitle("year", height(+5)) ///
	xscale(range(2006 2018)) xlabel(2006(2)2018) ///
	yscale(range(0 18000)) ylabel(0(4000)18000) ///ytick(0(2000)18000) ///
	xline(2009) ///
	legend(order(1 "Teacher Education" 2 "Curriculum Education Studies" 3 "Other Education" 4 "Nursing"))
graph export "np09.pdf", replace	
graph save np09s, replace

*plot bachelor over time, national priority #2	
list disc_code disc if regexm(disc_code, "^01") == 1 & year == 2006
tw  ( line bachelor year if disc_code == "010199", lcolor("231 107 243") ) ///
	( line bachelor year if disc_code == "010300", lcolor("248 118 109") ) ///
	( line bachelor year if disc_code == "010799", lcolor("0 176 246") ) ///
	( line bachelor year if disc_code == "010999", lcolor("0 186 56") ) ///
	( line bachelor year if disc_code == "019900", lcolor("163 165 0") ) ///
	,scheme(fabcolor) ///
	ytitle("", height(+5)) ///
	xtitle("year", height(+5)) ///
	yscale(range(0 18000)) ylabel(0(4000)18000, nolabel) ///ytick(0(2000)18000) ///
	xscale(range(2006 2018)) xlabel(2006(2)2018) ///
	xline(2012) ///
	legend(order(1 "Mathematical Sciences" 2 "Physics and Astronomy" 3 "Earth Sciences" 4 "Biological Sciences")) // 5 "Other Natural and Physical Sciences"))
graph export "np12.pdf", replace	
graph save np12s, replace
********************************************************************************

********************************************************************************
*EVENT STUDY ANALYSIS
cd "C:\Users\Fabio M\Documents\Research\AUS_edu\AU_edu_data"
use edu_stata_clean, clear

*restrict sample
keep if (np1 == 1) | (np2 == 1)
keep if event_time>=-3 & event_time<=5

xtset disc_num event_time

/*
*quick plot
xtline ln_dem_bachelor if  (np1 == 1) | (np2 == 1), ///
	xline(0) overlay scheme(fabcolor)  ///
	ytitle("Centered ln(Commencing students)", height(+5)) ///
	xtitle("Event time", height(+5)) ///	
	xscale(range(-3 5)) xlabel(-3(1)5) //legend(off)
*/

*proper plot (same colour-coding used before)
tw  ( line ln_dem_bachelor event_time if disc_code == "070199", lcolor(black) ) ///
	( line ln_dem_bachelor event_time if disc_code == "070300", lcolor("97 156 255") ) ///
	( line ln_dem_bachelor event_time if disc_code == "079900", lcolor("201 152 0") ) ///
	( line ln_dem_bachelor event_time if disc_code == "060399", lcolor("185 56 255") ) ///
    ( line ln_dem_bachelor event_time if disc_code == "010199", lcolor("231 107 243") ) ///
	( line ln_dem_bachelor event_time if disc_code == "010300", lcolor("248 118 109") ) ///
	( line ln_dem_bachelor event_time if disc_code == "010799", lcolor("0 176 246") ) ///
	( line ln_dem_bachelor event_time if disc_code == "010999", lcolor("0 186 56") ) ///
	( line ln_dem_bachelor event_time if disc_code == "019900", lcolor("163 165 0") ) ///	
	,scheme(fabcolor) ///
	xline(0) ///
	ytitle("Centered ln(Commencing students)", height(+5)) ///
	xtitle("Event time", height(+5)) ///	
	xscale(range(-3 5)) xlabel(-3(1)5)	legend(off)
graph export "C:\Users\Fabio M\Documents\Research\AUS_edu\au_event.pdf", replace	


*regressions and latex table
eststo clear
eststo: xtreg ln_bachelor c.event_time##i.de_treat, fe robust
eststo: xtreg ln_bachelor event_time c.event_time#i.de_treat, fe robust
eststo: xtreg ln_bachelor c.event_time##i.de_treat if disc!="othereducation", fe robust
eststo: xtreg ln_bachelor event_time c.event_time#i.de_treat if disc!="othereducation", fe robust
esttab using "C:\Users\Fabio M\Documents\Research\AUS_edu\edu_reg1.tex", ///
	label nobaselevel booktabs replace numbers nomtitles


