
capture log close

* Data processing
clear

//Directory with code and data
cd "/Users/blocke/Box Sync/Residency Personal Files/Scholarly Work/Locke Research Projects/VR in ICU/Data"
//cd ""

capture mkdir "Results and Figures"
capture mkdir "Results and Figures/$S_DATE/" //make new folder for figure output if needed
capture mkdir "Results and Figures/$S_DATE/Logs/" //new folder for stata logs
local a1=substr(c(current_time),1,2)
local a2=substr(c(current_time),4,2)
local a3=substr(c(current_time),7,2)
local b = "VR_ICU.do" // do file name to copy logs
copy "`b'" "Results and Figures/$S_DATE/Logs/(`a1'_`a2'_`a3')`b'"

set scheme cleanplots
graph set window fontface "Times New Roman" 
capture log close
log using "Results and Figures/$S_DATE/Logs/temp.log", append


/* -----
DATA CLEANING 
-------*/ 

import excel "ICU_patient_database.xlsx", sheet("data") firstrow case(lower)

drop if missing(patient_id)
generate id = _n

save patient_db, replace

clear
import excel "ICU_selection_database.xlsx", sheet("data") firstrow case(lower)
drop if missing(consented)
generate id = _n

//replace patient_id = "ICU_M_"+str(id) if missing(patient_id)
replace patient_id = "ICU_M_" + string(id, "%12.0g") if patient_id == ""

merge 1:1 patient_id using "patient_db", update generate(_merge_data)


label variable id "Patient ID"

generate female = 1 if gender == "f" 
replace female = 0 if gender == "m"
label variable female "Female Gender"
label define female_lab 0 "Male" 1 "Female"
label values female female_lab
drop gender


generate consent = 1 if consented == "yes"
replace consent = 0 if consented == "no" 
label variable consent "Consented?"
drop consented


/* TODO: fix this if study date used for anything in the analysis; just string for now.
generate study_date = date(date, "DMY")
format study_date %td
*/ 

replace race = "white" if race == "wite"

rename race nih_race
rename ethnicity nih_ethnicity
encode nih_race, gen(race)
label variable race "Race"
drop nih_race
encode nih_ethnicity, gen(ethnicity)
label variable ethnicity "Ethnicity" 
drop nih_ethnicity 

generate prior_vr = 1 if vr_history == "yes"
replace prior_vr = 0 if vr_history == "no"
label variable prior_vr "Prior VR Use?"
label define vr_hist_lab 0 "No Prior VR Use" 1 "Prior VR Use" 
label values prior_vr vr_hist_lab
drop vr_history

//Medical history variables: 
//DM
generate dm = 0 
replace dm = 1 if strpos(medical_history, "t2dm")
replace dm = 1 if strpos(medical_history, "diabetes")
label variable dm "Type 2 Diabetes"

//AF
generate afib = 0 
replace afib = 1 if strpos(medical_history, "atrial")
replace afib = 1 if strpos(medical_history, "afib")
label variable afib "Atrial Fibrillation"

//COPD
generate copd = 0 
replace copd = 1 if strpos(medical_history, "chronic obstructive")
replace copd = 1 if strpos(medical_history, "COPD")
label variable copd "COPD"

//Heart Failure 
generate chf = 0 
replace chf = 1 if strpos(medical_history, "heart failure")
replace chf = 1 if strpos(medical_history, "CHF")
label variable chf "CHF"

//OSA
generate osa = 0 
replace osa = 1 if strpos(medical_history, "obstructive sleep apnea")
label variable osa "OSA"

//CKD
generate ckd = 0 
replace ckd = 1 if strpos(medical_history, "chronic kidney disease")
label variable ckd "CKD"

//DVT/PE
generate dvt = 0 
replace dvt = 1 if strpos(medical_history, "DVT")
label variable dvt "DVT"

//cirrhosis
generate cirrhosis = 0
replace cirrhosis = 1 if strpos(medical_history, "cirrhosis")
label variable cirrhosis "Cirrhosis"

//cancer 
generate malig = 0
replace malig = 1 if strpos(medical_history, "AML")
replace malig = 1 if strpos(medical_history, "renal cell")
replace malig = 1 if strpos(medical_history, "leukemia")
replace malig = 1 if strpos(medical_history, "bone marrow")
replace malig = 1 if strpos(medical_history, "lymphoma")
label variable malig "Malignancy"


//Categorize ICU admission by type
replace icu_admission = "hemorrhagic shock" if icu_admission == "hemoperitoneum"
replace icu_admission = "hemorrhagic shock" if icu_admission == "gastrointestinal bleed, metastatic duodenal cancer"
replace icu_admission = "hemorrhagic shock" if icu_admission == "gastrointestinal bleed, metastatic duodenal cancer"
replace icu_admission = "respiratory failure" if icu_admission == "hypoxemic respiratory failure"
replace icu_admission = "respiratory failure" if icu_admission == "hypoxemia"
replace icu_admission = "respiratory failure" if icu_admission == "intestitial lung disease"
replace icu_admission = "respiratory failure" if icu_admission == "respiratory failure,shock" // per chart review
replace icu_admission = "respiratory failure" if icu_admission == "pneumonia" //per chart review
replace icu_admission = "respiratory failure" if icu_admission == "hemoptysis"
replace icu_admission = "septic shock" if icu_admission == "septic knee,bacteremia"
replace icu_admission = "septic shock" if icu_admission == "septic shock"
replace icu_admission = "heart failure" if icu_admission == "pulmonary embolism,heart failure" //per chart review
replace icu_admission = "heart failure" if icu_admission == "complete heart block"
replace icu_admission = "venous thromboembolism" if icu_admission == "pulmonary embolism"
replace icu_admission = "venous thromboembolism" if icu_admission == "renal vein thrombosis"
replace icu_admission = "venous thromboembolism" if icu_admission == "splenic thrombosis"
replace icu_admission = "post-procedure" if icu_admission == "post-operative(hepatic shunt)"
replace icu_admission = "post-procedure" if icu_admission == "post-operative"
replace icu_admission = "glucose / electrolytes" if icu_admission == "diabetic ketoacidosis"
replace icu_admission = "glucose / electrolytes" if icu_admission == "hyperkalemia"
replace icu_admission = "glucose / electrolytes" if icu_admission == "hypoglycemia"
replace icu_admission = "other" if icu_admission == "drug reaction,eosinophilia"
replace icu_admission = "other" if icu_admission == "sickle cell crisis"
replace icu_admission = "other" if icu_admission == "leukemia"
replace icu_admission = "other" if icu_admission == "hypotension" 
replace icu_admission = "other" if icu_admission == "stroke,dysphagia"
replace icu_admission = "other" if icu_admission == "shock"
tab icu_admission, plot

encode icu_admission, generate(reason_admit)
label variable reason_admit "Reason for ICU Admission"
drop icu_admission

encode resp_support, generate(respiratory_supp)
label variable respiratory_supp "Respiratory Support"
drop resp_support

/* Hearing/Vision impairments. Note: blindness/deafness excluded */ 
replace glasses = "yes" if glasses == "eyeglasses"
generate eyeglasses = 1 if glasses == "yes"
replace eyeglasses = 0 if glasses == "no" 
label variable eyeglasses "Eyeglasses?"
label define glasses_lab 0 "No Eyeglasses" 1 "Eyeglasses"
label values eyeglasses glasses_lab
drop glasses

encode hearing_issues, generate(hearing)
label variable hearing "Hearing Impairment?"
drop hearing_issues

/* VR_cat */
gen vr_cat = "Nature Scene" if vr_selection == "Nature Trek" | vr_selection == "Nature Trek,YouTube_Taiwan"
replace vr_cat = "Travel" if vr_selection == "YouTube_Budapest" | vr_selection == "YouTube_Croatia" | vr_selection == "YouTube_Taiwan"
replace vr_cat = "Synthetic Environment" if vr_selection == "Tripp"
encode vr_cat, generate(vr_category)
label variable vr_category "VR Category"
drop vr_cat

//encode original choice
encode vr_selection, generate(vr_sel)
label variable vr_sel "VR Selection"
drop vr_selection

label variable vr_minutes "Minutes of VR"

rename pre_vas_q1 vas_q1_1
rename pre_vas_q2 vas_q2_1
rename pre_vas_q3 vas_q3_1

rename post_vas_q1 vas_q1_2
rename post_vas_q2 vas_q2_2
rename post_vas_q3 vas_q3_2

/* Data quality summaries */ 
encode pre_eda_av, generate(eda_av_1)
label variable eda_av_1 "Pre EDA Data Quality"
drop pre_eda_av 

replace post_ecg_av = strtrim(post_ecg_av)

encode pre_ecg_av, generate(ecg_av_1)
label variable ecg_av_1 "Pre ECG Data Quality"
drop pre_ecg_av

encode pre_ppg_av, generate(ppg_av_1)
label variable ppg_av_1 "Pre PPG Data Quality"
drop pre_ppg_av

encode post_eda_av, generate(eda_av_2)
label variable eda_av_2 "Post EDA Data Quality"
drop post_eda_av

encode post_ecg_av, generate(ecg_av_2)
label variable ecg_av_2 "Post ECG Data Quality"
drop post_ecg_av

encode post_ppg_av, generate(ppg_av_2)
label variable ppg_av_2 "Post ECG Data Quality"
drop post_ppg_av


//Manual Data Quality Fixes: 
replace reason_to_refuse = "not related to VR" if comments == "did not want to participate in research"
// Replace `reason_to_refuse` with the same string but with the first character capitalized
replace reason_to_refuse = substr(upper(reason_to_refuse), 1, 1) + substr(reason_to_refuse, 2, .)
encode reason_to_refuse, generate(decline_reason)
label variable decline_reason "Reason given for declining"

//String version 
rename reason_to_refuse reason_declined
label variable reason_declined "Reason for declining to participate"
tab reason_declined

gen participate = 0 if missing(vas_q1_1)
replace participate = 1 if !missing(vas_q1_1)
label variable participate "Agreed to Participate?"
label define participate_lab 0 "Did not agree to participate" 1 "Agreed to participate" 
label values participate participate_lab

//Tags for consort diagram
//Flag for if excluded by non-participation
gen didnt_participate = "Did not agree to participate" if missing(vas_q1_1)

//Flag for incomplete if pre is present but post isn't
gen didnt_finish = "Too much pain" if !missing(vas_q1_1) & missing(vas_q1_2)


/* 
Data Summarization
*/ 

//Demographics

table1_mc, by(participate) ///   
vars( ///
age contn %4.0f \ ///
female bin %4.0f \ ///
race cat %4.0f \ ///
ethnicity cat %4.0f \ ///
hearing cat %4.0f \ ///
eyeglasses cat %4.0f \ /// 
respiratory_supp cat %4.0f \ ///
prior_vr bin %4.0f \ ///
) ///
percent_n percsign("%") iqrmiddle(",") sdleft(" (±") sdright(")") onecol saving("Results and Figures/$S_DATE/All patients Demographics.xlsx", replace)


table1_mc, by(participate) ///    
vars( ///
age contn %4.0f \ ///
female bin %4.0f \ ///
dm bin %4.0f \ ///
afib bin %4.0f \ ///
copd bin %4.0f \ ///
chf bin %4.0f \ ///
osa bin %4.0f \ ///
ckd bin %4.0f \ ///
dvt bin %4.0f \ ///
cirrhosis bin %4.0f \ ///
malig bin %4.0f \ ///
reason_admit cat %4.0f \ ///
) ///
percent_n percsign("%") iqrmiddle(",") sdleft(" (±") sdright(")") onecol total(before) saving("Results and Figures/$S_DATE/All patients comorbs.xlsx", replace)


table1_mc, by(participate) ///    // Make this consented once those ones are in
vars( ///
eyeglasses bin %4.0f \ ///
prior_vr bin %4.0f \ ///
vr_category cat %4.0f \ ///
vr_minutes contn %4.0f \ ///
decline_reason cat %4.0f \ ///
) ///
percent_n percsign("%") iqrmiddle(",") sdleft(" (±") sdright(")") onecol saving("Results and Figures/$S_DATE/All patients VR chars.xlsx", replace)

save "db_for_consort", replace


//Baseline and Follow-up Visual-analog scores.
sum vas_q1_1, detail
sum vas_q1_2, detail
sum vas_q2_1, detail
sum vas_q2_2, detail
sum vas_q3_1, detail
sum vas_q3_2, detail

//Visualize VAR Responses: 
//n=19 with complete data
ttest vas_q1_1 == vas_q1_2 // P = .0023 for improvement; mean 1.8 (0.65 - 3.0)
ttest vas_q2_1 == vas_q2_2 // P = .001 for improvement; mean 1.7 (0.8 - 2.7)
ttest vas_q3_1 == vas_q3_2 // P = .003 for improvement; mean 1.3 (0.53 - 2.1)

//Is there any interaction by choice of VR scenario? no
gen change_vas_q1 = vas_q1_1 - vas_q1_2
gen change_vas_q2 = vas_q2_1 - vas_q2_2
gen change_vas_q3 = vas_q3_1 - vas_q3_2

hist change_vas_q1
summ change_vas_q1, detail
hist change_vas_q2
summ change_vas_q2, detail
hist change_vas_q3
summ change_vas_q3, detail

regress change_vas_q1 //replicates paired t-test
regress change_vas_q1 i.vr_category // no significant interaction
regress change_vas_q2 //replicates paired t-test
regress change_vas_q2 i.vr_category // no significant interaction
regress change_vas_q3 //replicates paired t-test
regress change_vas_q3 i.vr_category // no significant interaction

gen pre = 1
gen post = 2 


//Individual Patient Visualizations

/* Overall mood */
//title("How you feel at this moment?", size(vlarge)) subtitle("Individual Patient Ratings; Higher = Better", size(large))
//ytitle("0-10 Visual Analog Scale Rating", size(large))
twoway pcspike vas_q1_1 pre vas_q1_2 post, /// 
	lpattern(longdash) lcolor(edkblue%70) lwidth(medthick) ///
	ylabel(1(1)10, labsize(medlarge)) ///
	xlabel(1 "Pre" 2 "Post", labsize(large)) ///
	title("") ///
	ytitle("Individual Patients", size(large)) ///
	xtitle("") ///
	text(1.6 1.05 "Worse", size(medlarge) color(gs8) placement(east)) ///
	text(9.6 1.05 "Better", size(medlarge) color(gs8) placement(east))
graph save "VAS_q1_pre-post_indiv.gph", replace
graph export "Results and Figures/$S_DATE/VAS q1 pre-post indiv.png", as(png) name("Graph") replace

/* Anxiety */ 
//title("How worried/anxious you feel at this moment?", size(vlarge)) subtitle("Individual Patient Ratings: Lower = Less Worried", size(large))
//ytitle("0-10 Visual Analog Scale Rating", size(large))
twoway pcspike vas_q2_1 pre vas_q2_2 post, ///
	lpattern(longdash) lcolor(dkorange%70) lwidth(medthick) ///
	ylabel(1(1)10, labsize(medlarge)) ///
	xlabel(1 "Pre" 2 "Post", labsize(medlarge)) ///
	title("") ytitle("Individual Patients", size(large)) ///
	xtitle("") ///
	text(1.6 1.05 "Less Anxious", size(medlarge) color(gs8) placement(east)) ///
	text(9.6 1.05 "More Anxious", size(medlarge) color(gs8) placement(east))
graph save "VAS_q2_pre-post_indiv.gph", replace
graph export "Results and Figures/$S_DATE/VAS q2 pre-post indiv.png", as(png) name("Graph") replace

/* Pain */ 
//title("HHow much Pain do you have at the moment?", size(vlarge)) subtitle("Individual Patient Ratings: Lower = Less Pain", size(large))
//ytitle("0-10 Visual Analog Scale Rating", size(large))
twoway pcspike vas_q3_1 pre vas_q3_2 post, ///
	lpattern(longdash) lcolor(cranberry%70) lwidth(medthick) ///
	ylabel(1(1)10, labsize(medlarge)) ///
	xlabel(1 "Pre" 2 "Post", labsize(medlarge)) ///
	title("") ytitle("Individual Patients", size(large)) ///
	xtitle("") ///
	text(1.6 1.05 "Less Pain", size(medlarge) color(gs8) placement(east)) ///
	text(9.6 1.05 "More Pain", size(medlarge) color(gs8) placement(east))
graph save "VAS_q3_pre-post_indiv.gph", replace
graph export "Results and Figures/$S_DATE/VAS q3 pre-post indiv.png", as(png) name("Graph") replace

//Reshape to long data for pre-post means
reshape long vas_q1_ vas_q2_ vas_q3_ eda_av_ ecg_av_ ppg_av_, i(patient_id) j(time)

label variable time "Pre vs Post"
label define time_lab 1 "Pre" 2 "Post" 
label values time time_lab

label variable eda_av_ "EDA Data Quality"
label variable ecg_av_ "ECG Data Quality"
label variable ppg_av_ "PPG Data Quality"

table1_mc, by(time) ///    // Make this consented once those ones are in
vars( ///
eda_av cat %4.0f \ ///
ecg_av cat %4.0f \ ///
ppg_av cat %4.0f \ ///
) ///
percent_n percsign("%") iqrmiddle(",") sdleft(" (±") sdright(")") onecol saving("Results and Figures/$S_DATE/All patients data quality.xlsx", replace)

//VAS 
label variable vas_q1_ "How you feel at this moment? (1-10)"
label variable vas_q2_ "How worried/anxious you feel at this moment? (1-10)" 
label variable vas_q3_ "How much pain you have at this moment? (1-10)"

hist vas_q1_, width(1) xscale(range(1 10)) by(time)
hist vas_q2_, width(1) xscale(range(1 10)) by(time)
hist vas_q3_, width(1) xscale(range(1 10)) by(time)

/* Overall */ 
preserve
statsby vas_q1_mean=r(mean) vas_q1_ub=r(ub) vas_q1_lb=r(lb) vas_q1_N=r(N) , by(time ) clear: ci means vas_q1_
//title("How you feel at this moment?", size(vlarge))
//ytitle("0-10 Visual Analog Scale Rating", size(large))
graph twoway ///
	rcap vas_q1_ub vas_q1_lb time, lcolor(edkblue%70) lwidth(thick)|| ///
		connected vas_q1_mean time, lcolor(edkblue%70) mcolor(edkblue%70) lwidth(thick) ||, ///
	ylabel(1(1)10, labsize(medlarge)) xlabel(1 "Pre" 2 "Post", labsize(medlarge)) xtitle("") ///
	ytitle("Mean and 95 CI", size(large)) ///
	xtitle("")  legend(off) ///
	text(4 1.5 "{it:P}=.002; mean improvement" "of 1.8 (0.65 - 3.0) points", size(large)) /// 
	text(1.6 1.05 "Worse", size(medlarge) color(gs8) placement(east)) ///
	text(9.6 1.05 "Better", size(medlarge) color(gs8) placement(east))
graph save "VAS_q1_pre-post_mean.gph", replace
graph export "Results and Figures/$S_DATE/VAS q1 pre-post mean.png", as(png) name("Graph") replace

graph combine VAS_q1_pre-post_indiv.gph VAS_q1_pre-post_mean.gph, ///
	xcommon col(1) ///
	title("Overall", size(huge)) ///
	subtitle("How do you feel at the moment?") ///
	xsize(5) ysize(7)
graph save "VAS_q1_combined.gph", replace
graph export "Results and Figures/$S_DATE/VAS q1 pre-post combined.png", as(png) name("Graph") replace
restore	
	
	
/* Anxiety */ 
preserve
statsby vas_q2_mean=r(mean) vas_q2_ub=r(ub) vas_q2_lb=r(lb) vas_q2_N=r(N) , by(time ) clear: ci means vas_q2_
graph twoway ///
	rcap vas_q2_ub vas_q2_lb time, lcolor(dkorange%70) lwidth(thick)|| ///
		connected vas_q2_mean time, lcolor(dkorange%70) mcolor(dkorange%70) lwidth(thick) ||, ///
	ylabel(1(1)10, labsize(medlarge)) xlabel(1 "Pre" 2 "Post", labsize(medlarge)) ///
	title("") ytitle("Mean and 95 CI", size(large)) xtitle("") ///
	legend(off) ///
	text(6.5 1.5 "{it:P}=.001; mean improvement" "of 1.7 (0.8 - 2.7) points", size(large)) ///
	text(1.6 1.05 "Less Anxious", size(medlarge) color(gs8) placement(east)) ///
	text(9.6 1.05 "More Anxious", size(medlarge) color(gs8) placement(east))
graph save "VAS_q2_pre-post_mean.gph", replace
graph export "Results and Figures/$S_DATE/VAS q2 pre-post mean.png", as(png) name("Graph") replace

graph combine VAS_q2_pre-post_indiv.gph VAS_q2_pre-post_mean.gph, ///
	xcommon col(1) ///
	title("Anxiety", size(huge)) ///
	subtitle("How worried/anxious do you feel at the moment?") ///
	xsize(5) ysize(7)
graph save "VAS_q2_combined.gph", replace
graph export "Results and Figures/$S_DATE/VAS q2 pre-post combined.png", as(png) name("Graph") replace
restore	
		
/* Pain */ 
preserve
statsby vas_q3_mean=r(mean) vas_q3_ub=r(ub) vas_q3_lb=r(lb) vas_q3_N=r(N) , by(time ) clear: ci means vas_q3_
graph twoway ///
	rcap vas_q3_ub vas_q3_lb time, lcolor(cranberry%70) lwidth(thick)|| ///
		connected vas_q3_mean time, lcolor(cranberry%70) mcolor(cranberry%70) lwidth(thick) ||, ///
	ylabel(1(1)10, labsize(medlarge)) xlabel(1 "Pre" 2 "Post", labsize(medlarge)) ///
	title("") ytitle("Mean and 95 CI", size(large)) xtitle("") ///
	legend(off) ///
	text(6.5 1.5 "{it:P}=.003; mean improvement" "of 1.3 (0.53 - 2.1) points", size(medlarge)) ///
	text(1.6 1.05 "Less Pain", size(medlarge) color(gs8) placement(east)) ///
	text(9.6 1.05 "More Pain", size(medlarge) color(gs8) placement(east))
graph save "VAS_q3_pre-post_mean.gph", replace
graph export "Results and Figures/$S_DATE/VAS q3 pre-post mean.png", as(png) name("Graph") replace

graph combine VAS_q3_pre-post_indiv.gph VAS_q3_pre-post_mean.gph, ///
	xcommon col(1) ///
	title("Pain", size(huge)) ///
	subtitle("How much pain do you feel at the moment?") ///
	xsize(5) ysize(7)
graph save "VAS_q3_combined.gph", replace
graph export "Results and Figures/$S_DATE/VAS q3 pre-post combined.png", as(png) name("Graph") replace
restore	


graph combine VAS_q1_combined.gph VAS_q2_combined.gph VAS_q3_combined.gph, ///
	cols(3) /// 
	title("") ///
	xsize(12) ysize(7)
graph export "Results and Figures/$S_DATE/VAS q1-3 combined.png", as(png) width(3600) name("Graph") replace
	
	
   
