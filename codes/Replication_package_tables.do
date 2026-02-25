cd "/Users/antalertl/Desktop/r_projects/2023_loss_aversion_classroom_experiment_BCE/new_stata_codes"

* Read the data
import delimited using "/Users/antalertl/Desktop/r_projects/2023_loss_aversion_classroom_experiment_BCE/data_clean.csv", clear


* Read alternative data - for robustness
* import delimited using reg_data_no_outliers.csv, clear



/*#------------
data cleaning after reading in the data
#------------*/

foreach v in test1 test2 test3 test4 final_test semester_tests ///
            total_score total_score_without_hw final_score {

    replace `v' = subinstr(`v', ",", ".", .)
    destring `v', replace
}

* percentage recoding

replace test1 = (test1 / 16) * 100
replace test2 = (test2 / 16) * 100
replace test3 = (test3 / 16) * 100
replace test4 = (test4 / 16) * 100
replace semester_tests = (semester_tests / 48) * 100

replace final_test = (final_test / 40) * 100

rename group_cat Group_cat



* ##### FIRST REGRESSION TABLE

regress test1 treatment_loss treatment_hybrid d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm1

regress test2 treatment_loss treatment_hybrid d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm2


regress test3 treatment_loss treatment_hybrid d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm3


regress test4 treatment_loss treatment_hybrid d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm4

regress semester_tests treatment_loss treatment_hybrid d_no d_mothereduc_uni d_derivation d_nowork time_slot_thursday time_slot_friday d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar, cluster(Group_cat)
eststo lm5

regress final_test treatment_loss treatment_hybrid d_no d_mothereduc_uni d_derivation d_nowork time_slot_thursday time_slot_friday d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar, cluster(Group_cat)
eststo lm6


regress final_test treatment_loss treatment_hybrid semester_tests d_no d_mothereduc_uni d_derivation d_nowork time_slot_thursday time_slot_friday d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar, cluster(Group_cat)
eststo lm7
test treatment_loss treatment_hybrid

esttab lm1 lm2 lm3 lm4 lm5 lm6 lm7, ///
    label nodepvars mti("Regression Results") ///
    collabels(none) nonumbers booktabs ///
    star(* 0.10 ** 0.05 *** 0.01) se replace ///
    stats(N r2 rmse, fmt(3) labels("Observations" "R-squared" "Residual Std. Error"))
	



	
//// Corresponding Multiple Hypothesis Testing (Appendix)

rwolf2 (regress test1 treatment_loss treatment_hybrid d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)), indepvars(treatment_loss,treatment_hybrid, treatment_loss treatment_hybrid)

		(regress test2 treatment_loss treatment_hybrid d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat))
		(regress test3 treatment_loss treatment_hybrid d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat))
		(regress test4 treatment_loss treatment_hybrid d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat))
		(regress semester_tests treatment_loss treatment_hybrid d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat))
		(regress final_test treatment_loss treatment_hybrid d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat))
		(regress final_test treatment_loss treatment_hybrid d_no semester_tests d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)),

	


rwolf2 (reg semester_tests treatment_loss treatment_hybrid d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)), indepvars(treatment_loss treatment_hybrid) reps(10000) seed(1) nodots

rwolf2 (reg final_test treatment_loss treatment_hybrid d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)), indepvars(treatment_loss treatment_hybrid) reps(10000) seed(1) nodots

rwolf2 (reg final_test treatment_loss treatment_hybrid semester_tests d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)), indepvars(treatment_loss treatment_hybrid) reps(10000) seed(1) nodots
	
	



////-----------------------------------------------------------------------------------------------------------------------
////-----------------------------------------------------------------------------------------------------------------------
////-----------------------------------------------------------------------------------------------------------------------
////-----------------------------------------------------------------------------------------------------------------------
////-----------------------------------------------------------------------------------------------------------------------



* TABLE 4: Gender heterogeneity tests:
	
regress test1 treatment_loss treatment_hybrid d_no female_loss female_hybrid d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)
eststo lm_test1_gender

regress test2 treatment_loss treatment_hybrid d_no female_loss female_hybrid d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)
eststo lm_test2_gender

regress test3 treatment_loss treatment_hybrid d_no female_loss female_hybrid d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)
eststo lm_test3_gender

regress test4 treatment_loss treatment_hybrid d_no female_loss female_hybrid d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)
eststo lm_test4_gender

regress semester_tests treatment_loss treatment_hybrid d_no female_loss female_hybrid d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)
eststo lm_semester_test_gender
	
regress final_test treatment_loss treatment_hybrid d_no female_loss female_hybrid d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)
eststo lm_exam_score_gender

regress final_test treatment_loss treatment_hybrid d_no female_loss female_hybrid d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday semester_tests, cluster(Group_cat)
eststo lm_exam_score_gender2

*TABLE 4: Export the regression results to LaTeX -- gender heterogeneity tests


esttab lm_test1_gender lm_test2_gender lm_test3_gender lm_test4_gender lm_semester_test_gender lm_exam_score_gender lm_exam_score_gender2, ///
    label nodepvars mti("Regression Results") ///
    collabels(none) nonumbers booktabs ///
    star(* 0.10 ** 0.05 *** 0.01) se replace ///
    stats(N r2 rmse, fmt(3) labels("Observations" "R-squared" "Residual Std. Error"))



	
	
	
// Corresponding Multiple Hypothesis test: Gender heterogeneity (Appendix)

// Individual rwolf2 commands:
rwolf2 (reg semester_tests treatment_loss treatment_hybrid d_no female_loss female_hybrid d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)), indepvars(female_loss female_hybrid) reps(10000) seed(1) nodots

rwolf2 (reg final_test treatment_loss treatment_hybrid d_no female_loss female_hybrid d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)), indepvars(female_loss female_hybrid) reps(10000) seed(1) nodots

rwolf2 (reg final_test treatment_loss treatment_hybrid d_no female_loss female_hybrid semester_tests d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)), indepvars(female_loss female_hybrid) reps(10000) seed(1) nodots

	
	
	
	
	
	

////-----------------------------------------------------------------------------------------------------------------------
////-----------------------------------------------------------------------------------------------------------------------
////-----------------------------------------------------------------------------------------------------------------------
////-----------------------------------------------------------------------------------------------------------------------
////-----------------------------------------------------------------------------------------------------------------------


// # Heterogeneity by Student Performance
	
	
clear all
import delimited using "/Users/antalertl/Desktop/r_projects/2023_loss_aversion_classroom_experiment_BCE/data_clean.csv", clear



/*------------
data cleaning after reading in the data
#------------*/

foreach v in test1 test2 test3 test4 final_test semester_tests ///
            total_score total_score_without_hw final_score {

    replace `v' = subinstr(`v', ",", ".", .)
    destring `v', replace
}

* percentage recoding

replace test1 = (test1 / 16) * 100
replace test2 = (test2 / 16) * 100
replace test3 = (test3 / 16) * 100
replace test4 = (test4 / 16) * 100
replace semester_tests = (semester_tests / 48) * 100

replace final_test = (final_test / 40) * 100

rename group_cat Group_cat

/*set seed 42
sqreg final_test loss_final d_no semester_tests d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, q(0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.55 0.6 0.65 0.7 0.75, 0.8 0.85 0.9)


bootstrap, cluster(Group_cat): sqreg final_test loss_final d_no semester_tests d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork,q( 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.55 0.6 0.65 0.7 0.75, 0.8 0.85 0.9)
eststo points



*/

* FIGURE 5: plot:
global x d_no semester_tests d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork
global z d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork
quietly bootstrap, cluster(Group_cat) reps(100): rqr final_test loss_final, quantile(.15(.05).9) controls($x)
rqrplot




// APPENDIX -- corresponding regression table

quietly eststo: bootstrap, cluster(Group_cat) reps(100): rqr final_test loss_final, quantile(.15(.05).9) controls($x)
quietly eststo: bootstrap, cluster(Group_cat) reps(100): rqr final_test loss_final, quantile(.15(.05).9) controls($z)


	
gen dropout = 0
replace dropout = 1 if final_test == 0

keep if dropout == 0	
	
quietly eststo: bootstrap, cluster(Group_cat) reps(100): rqr final_test loss_final, quantile(.15(.05).9) controls($x)
quietly eststo: bootstrap, cluster(Group_cat) reps(100): rqr final_test loss_final, quantile(.15(.05).9) controls($z)	

esttab, b(4) se(4) keep(loss_final) nomtitles tex
	



//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	

	
* TABLE 5 -- Split  KNOWS DERAVATIVES OR NOT
	
	
import delimited using "/Users/antalertl/Desktop/r_projects/2023_loss_aversion_classroom_experiment_BCE/data_clean.csv", clear

/*------------
data cleaning after reading in the data
#------------*/

foreach v in test1 test2 test3 test4 final_test semester_tests ///
            total_score total_score_without_hw final_score {

    replace `v' = subinstr(`v', ",", ".", .)
    destring `v', replace
}

* percentage recoding

replace test1 = (test1 / 16) * 100
replace test2 = (test2 / 16) * 100
replace test3 = (test3 / 16) * 100
replace test4 = (test4 / 16) * 100
replace semester_tests = (semester_tests / 48) * 100

replace final_test = (final_test / 40) * 100

rename group_cat Group_cat


keep if d_derivation == 0


regress final_test loss_final d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_exam_noderivates1


regress final_test loss_final semester_tests d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_exam_noderivates2

* Read the data another time
	
import delimited using "/Users/antalertl/Desktop/r_projects/2023_loss_aversion_classroom_experiment_BCE/data_clean.csv", clear
/*------------
data cleaning after reading in the data
------------*/

foreach v in test1 test2 test3 test4 final_test semester_tests ///
            total_score total_score_without_hw final_score {

    replace `v' = subinstr(`v', ",", ".", .)
    destring `v', replace
}

* percentage recoding

replace test1 = (test1 / 16) * 100
replace test2 = (test2 / 16) * 100
replace test3 = (test3 / 16) * 100
replace test4 = (test4 / 16) * 100
replace semester_tests = (semester_tests / 48) * 100

replace final_test = (final_test / 40) * 100

rename group_cat Group_cat


keep if d_derivation == 1



regress final_test loss_final  d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_exam_derivates1

regress final_test loss_final semester_tests d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_exam_derivates2


// TABLE 5 LATEX OUTPUT:

esttab lm_exam_noderivates1  lm_exam_noderivates2 lm_exam_derivates1 lm_exam_derivates2, ///
    label nodepvars mti("Regression Results") ///
    collabels(none) nonumbers booktabs ///
    star(* 0.10 ** 0.05 *** 0.01) se replace ///
    stats(N r2 rmse, fmt(3) labels("Observations" "R-squared" "Residual Std. Error"))
	
		
	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	



//---------------- #
//---------------- #
// APPENDIX 
//---------------- #
//---------------- #

import delimited using "/Users/antalertl/Desktop/r_projects/2023_loss_aversion_classroom_experiment_BCE/data_clean.csv", clear

/*------------
data cleaning after reading in the data
#------------*/

foreach v in test1 test2 test3 test4 final_test semester_tests ///
            total_score total_score_without_hw final_score {

    replace `v' = subinstr(`v', ",", ".", .)
    destring `v', replace
}

* percentage recoding

replace test1 = (test1 / 16) * 100
replace test2 = (test2 / 16) * 100
replace test3 = (test3 / 16) * 100
replace test4 = (test4 / 16) * 100
replace semester_tests = (semester_tests / 48) * 100

replace final_test = (final_test / 40) * 100

rename group_cat Group_cat	
	
	

	
	
	
regress test1 loss_semester d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test1

regress test2 loss_semester d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test2

regress test3 loss_semester d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test3

regress test4 loss_semester d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test4

regress semester_tests loss_semester d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_semester_test


regress final_test loss_final d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_exam_score


* controlling for semester_tests on final_test:
regress final_test loss_final d_no semester_tests d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_exam_semesterctrl


* TABLE 7 LaTex file

	
// NOTE: loss_semester stands for losing points throughout the semester, while
// loss_final is losing points during the final test. In the paper, we show these in a single line,
// as "Losing Points". This comment also stands for all further (and relevant) analyses.

	
esttab lm_test1 lm_test2 lm_test3 lm_test4 lm_semester_test lm_exam_score lm_exam_semesterctrl, ///
    label nodepvars mti("Regression Results") ///
    collabels(none) nonumbers booktabs ///
    star(* 0.10 ** 0.05 *** 0.01) se replace ///
    stats(N r2 rmse, fmt(3) labels("Observations" "R-squared" "Residual Std. Error"))
	
	
	
// Corresponding gender heterogeneity: -- TABLE 10


gen female_loss_final = d_no * loss_final
gen female_loss_semester = d_no * loss_semester	
	
* Gender heterogeneity tests:
	
regress test1 loss_semester d_no female_loss_semester d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test1_gender

regress test2 loss_semester d_no female_loss_semester  d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test2_gender

regress test3 loss_semester d_no female_loss_semester  d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test3_gender

regress test4 loss_semester d_no female_loss_semester  d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test4_gender

regress semester_tests loss_semester d_no female_loss_semester  d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_semester_test_gender
	
regress final_test loss_final d_no female_loss_final  d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_exam_score_gender	

regress final_test loss_final d_no semester_tests female_loss_final  d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_exam_score_genders

* Export the regression results to LaTeX -- Table 5 -- gender heterogeneity tests

* Regr_3 in LaTex file

esttab lm_test1_gender lm_test2_gender lm_test3_gender lm_test4_gender lm_semester_test_gender lm_exam_score_gender lm_exam_score_genders, ///
    label nodepvars mti("Regression Results") ///
    collabels(none) nonumbers booktabs ///
    star(* 0.10 ** 0.05 *** 0.01) se replace ///
    stats(N r2 rmse, fmt(3) labels("Observations" "R-squared" "Residual Std. Error"))	
	
	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	


// # TABLE 8 - Gain vs. Loss Treatment 

import delimited using "/Users/antalertl/Desktop/r_projects/2023_loss_aversion_classroom_experiment_BCE/data_clean.csv", clear

/*------------
data cleaning after reading in the data
#------------*/

foreach v in test1 test2 test3 test4 final_test semester_tests ///
            total_score total_score_without_hw final_score {

    replace `v' = subinstr(`v', ",", ".", .)
    destring `v', replace
}

* percentage recoding

replace test1 = (test1 / 16) * 100
replace test2 = (test2 / 16) * 100
replace test3 = (test3 / 16) * 100
replace test4 = (test4 / 16) * 100
replace semester_tests = (semester_tests / 48) * 100

replace final_test = (final_test / 40) * 100

rename group_cat Group_cat	
	
keep if treatment_loss == 1 | treatment_gain == 1

* Run the regressions with clustered standard errors
regress test1 treatment_loss d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test1

regress test2 treatment_loss d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test2

regress test3 treatment_loss d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test3

regress test4 treatment_loss d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test4

regress semester_tests treatment_loss d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_semester_test

regress final_test treatment_loss d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_exam_score

* controlling for semester_tests on final_test:
regress final_test treatment_loss d_no semester_tests d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_exam_semesterctrl

* Export the regression results to LaTeX  -- TABLE 8:
	
esttab lm_test1 lm_test2 lm_test3 lm_test4 lm_semester_test lm_exam_score lm_exam_semesterctrl, ///
    label nodepvars mti("Regression Results") ///
    collabels(none) nonumbers booktabs ///
    star(* 0.10 ** 0.05 *** 0.01) se replace ///
    stats(N r2 rmse, fmt(3) labels("Observations" "R-squared" "Residual Std. Error"))
	
	
	
	
	
//--- Corresponding gender-heterogeneity: Loss vs. Gain -- TABLE 11	
	
	
* Gender heterogeneity tests:
	
regress test1 treatment_loss  d_no female_loss  d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test1_gender

regress test2 treatment_loss  d_no female_loss  d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test2_gender

regress test3 treatment_loss  d_no female_loss  d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test3_gender

regress test4 treatment_loss  d_no female_loss  d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test4_gender

regress semester_tests treatment_loss  d_no female_loss  d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_semester_test_gender
	
regress final_test treatment_loss  d_no female_loss  d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_exam_score_gender	

regress final_test treatment_loss semester_tests d_no female_loss  d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_exam_score_gender_s

* Export the regression results to LaTeX -- Table 11 -- gender heterogeneity tests



esttab lm_test1_gender lm_test2_gender lm_test3_gender lm_test4_gender lm_semester_test_gender lm_exam_score_gender lm_exam_score_gender_s, ///
    label nodepvars mti("Regression Results") ///
    collabels(none) nonumbers booktabs ///
    star(* 0.10 ** 0.05 *** 0.01) se replace ///
    stats(N r2 rmse, fmt(3) labels("Observations" "R-squared" "Residual Std. Error"))

	
	
	
	
	
	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
	

// # TABLE 9 - Loss vs. Hybrid Treatment 

import delimited using "/Users/antalertl/Desktop/r_projects/2023_loss_aversion_classroom_experiment_BCE/data_clean.csv", clear

/*------------
data cleaning after reading in the data
#------------*/

foreach v in test1 test2 test3 test4 final_test semester_tests ///
            total_score total_score_without_hw final_score {

    replace `v' = subinstr(`v', ",", ".", .)
    destring `v', replace
}

* percentage recoding

replace test1 = (test1 / 16) * 100
replace test2 = (test2 / 16) * 100
replace test3 = (test3 / 16) * 100
replace test4 = (test4 / 16) * 100
replace semester_tests = (semester_tests / 48) * 100

replace final_test = (final_test / 40) * 100

rename group_cat Group_cat	
	
keep if treatment_loss == 1 | treatment_hybrid == 1

* Run the regressions with clustered standard errors
regress test1 treatment_loss d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test1

regress test2 treatment_loss d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test2

regress test3 treatment_loss d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test3

regress test4 treatment_loss d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test4

regress semester_tests treatment_loss d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_semester_test

regress final_test treatment_loss d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_exam_score

* controlling for semester_tests on final_test:
regress final_test treatment_loss d_no semester_tests d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_exam_semesterctrl

* Export the regression results to LaTeX  -- TABLE 9:
	
esttab lm_test1 lm_test2 lm_test3 lm_test4 lm_semester_test lm_exam_score lm_exam_semesterctrl, ///
    label nodepvars mti("Regression Results") ///
    collabels(none) nonumbers booktabs ///
    star(* 0.10 ** 0.05 *** 0.01) se replace ///
    stats(N r2 rmse, fmt(3) labels("Observations" "R-squared" "Residual Std. Error"))
		

	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
		
// REGRESSIONS EXCLUDING DROPOUTS	
	
import delimited using "/Users/antalertl/Desktop/r_projects/2023_loss_aversion_classroom_experiment_BCE/data_clean.csv", clear

/*------------
data cleaning after reading in the data
#------------*/

foreach v in test1 test2 test3 test4 final_test semester_tests ///
            total_score total_score_without_hw final_score {

    replace `v' = subinstr(`v', ",", ".", .)
    destring `v', replace
}

* percentage recoding

replace test1 = (test1 / 16) * 100
replace test2 = (test2 / 16) * 100
replace test3 = (test3 / 16) * 100
replace test4 = (test4 / 16) * 100
replace semester_tests = (semester_tests / 48) * 100

replace final_test = (final_test / 40) * 100

rename group_cat Group_cat	




gen dropout = 0
replace dropout = 1 if final_test == 0



* Run the regressions with clustered standard errors

regress dropout d_no loss_semester, cluster(Group_cat)
eststo dropout1

regress dropout d_no loss_semester d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo dropout11

regress dropout d_no loss_semester semester_tests d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo dropout12


regress dropout treatment_loss treatment_hybrid d_no, cluster(Group_cat)
eststo dropout2

regress dropout treatment_loss treatment_hybrid d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo dropout21


regress dropout treatment_loss treatment_hybrid  semester_tests d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo dropout22

* TABLE 14: Linear Probability Models of dropouts

esttab dropout1 dropout11  dropout2 dropout21 , ///
    label nodepvars mti("Regression Results") ///
    collabels(none) nonumbers booktabs ///
    star(* 0.10 ** 0.05 *** 0.01) se replace ///
    stats(N r2 rmse, fmt(3) labels("Observations" "R-squared" "Residual Std. Error"))


//--------------------	
// EXCLUSION OF DROPOUTS
drop if dropout == 1	
// # Table 15 regressions: 	
	
regress test1 loss_semester d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test1

regress test2 loss_semester d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test2

regress test3 loss_semester d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test3

regress test4 loss_semester d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test4

regress semester_tests loss_semester d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_semester_test


regress final_test loss_final d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_exam_score


* controlling for semester_tests on final_test:
regress final_test loss_final d_no semester_tests d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_exam_semesterctrl



* TABLE 15:
	
esttab lm_test1 lm_test2 lm_test3 lm_test4 lm_semester_test lm_exam_score lm_exam_semesterctrl, ///
    label nodepvars mti("Regression Results") ///
    collabels(none) nonumbers booktabs ///
    star(* 0.10 ** 0.05 *** 0.01) se replace ///
    stats(N r2 rmse, fmt(3) labels("Observations" "R-squared" "Residual Std. Error"))
	
	
	
// Corresponding gender heterogeneity: -- TABLE 16


gen female_loss_final = d_no * loss_final
gen female_loss_semester = d_no * loss_semester	
	
* Gender heterogeneity tests:
	
regress test1 loss_semester d_no female_loss_semester d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test1_gender

regress test2 loss_semester d_no female_loss_semester  d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test2_gender

regress test3 loss_semester d_no female_loss_semester  d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test3_gender

regress test4 loss_semester d_no female_loss_semester  d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_test4_gender

regress semester_tests loss_semester d_no female_loss_semester  d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_semester_test_gender
	
regress final_test loss_final d_no female_loss_final  d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_exam_score_gender	

regress final_test loss_final d_no semester_tests female_loss_final  d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm_exam_score_genders

* Export the regression results to LaTeX -- Table 16 -- gender heterogeneity tests

esttab lm_test1_gender lm_test2_gender lm_test3_gender lm_test4_gender lm_semester_test_gender lm_exam_score_gender lm_exam_score_genders, ///
    label nodepvars mti("Regression Results") ///
    collabels(none) nonumbers booktabs ///
    star(* 0.10 ** 0.05 *** 0.01) se replace ///
    stats(N r2 rmse, fmt(3) labels("Observations" "R-squared" "Residual Std. Error"))	
	
	
	
	
// WITH ALL THREE TREATMENTS -- TABLES 17 AND 18	
	
regress test1 treatment_loss treatment_hybrid d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm1

regress test2 treatment_loss treatment_hybrid d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm2


regress test3 treatment_loss treatment_hybrid d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm3


regress test4 treatment_loss treatment_hybrid d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm4

regress semester_tests treatment_loss treatment_hybrid d_no d_mothereduc_uni d_derivation d_nowork time_slot_thursday time_slot_friday d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar, cluster(Group_cat)
eststo lm5

regress final_test treatment_loss treatment_hybrid d_no d_mothereduc_uni d_derivation d_nowork time_slot_thursday time_slot_friday d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar, cluster(Group_cat)
eststo lm6


regress final_test treatment_loss treatment_hybrid semester_tests d_no d_mothereduc_uni d_derivation d_nowork time_slot_thursday time_slot_friday d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar, cluster(Group_cat)
eststo lm7

// # TABLE 17

esttab lm1 lm2 lm3 lm4 lm5 lm6 lm7, ///
    label nodepvars mti("Regression Results") ///
    collabels(none) nonumbers booktabs ///
    star(* 0.10 ** 0.05 *** 0.01) se replace ///
    stats(N r2 rmse, fmt(3) labels("Observations" "R-squared" "Residual Std. Error"))
	
	
	
* TABLE 18: gender heterogeneity
	
regress test1 treatment_loss treatment_hybrid d_no female_loss female_hybrid d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)
eststo lm_test1_gender

regress test2 treatment_loss treatment_hybrid d_no female_loss female_hybrid d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)
eststo lm_test2_gender

regress test3 treatment_loss treatment_hybrid d_no female_loss female_hybrid d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)
eststo lm_test3_gender

regress test4 treatment_loss treatment_hybrid d_no female_loss female_hybrid d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)
eststo lm_test4_gender

regress semester_tests treatment_loss treatment_hybrid d_no female_loss female_hybrid d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)
eststo lm_semester_test_gender
	
regress final_test treatment_loss treatment_hybrid d_no female_loss female_hybrid d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)
eststo lm_exam_score_gender

regress final_test treatment_loss treatment_hybrid d_no female_loss female_hybrid d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday semester_tests, cluster(Group_cat)
eststo lm_exam_score_gender2

*TABLE 18: Export the regression results to LaTeX -- gender heterogeneity tests


esttab lm_test1_gender lm_test2_gender lm_test3_gender lm_test4_gender lm_semester_test_gender lm_exam_score_gender lm_exam_score_gender2, ///
    label nodepvars mti("Regression Results") ///
    collabels(none) nonumbers booktabs ///
    star(* 0.10 ** 0.05 *** 0.01) se replace ///
    stats(N r2 rmse, fmt(3) labels("Observations" "R-squared" "Residual Std. Error"))


	
	
	

//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
	
	// Regression Results using all students: In separate R code
	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------	
//-------------------------------------------		
	
// EARLY FEEDBACK ON POOR PERFORMANCE AND EFFECT SIZE


	
import delimited using "/Users/antalertl/Desktop/r_projects/2023_loss_aversion_classroom_experiment_BCE/data_clean.csv", clear

/*------------
data cleaning after reading in the data
#------------*/

foreach v in test1 test2 test3 test4 final_test semester_tests ///
            total_score total_score_without_hw final_score {

    replace `v' = subinstr(`v', ",", ".", .)
    destring `v', replace
}


rename group_cat Group_cat	


* Find the minimum score across the four tests for each student
egen min_score = rowmin(test1 test2 test3 test4)

* Create the "labourus" variable (1 if test4 is the worst test, 0 otherwise)
gen worst_first_score = (test1 == min_score & test1 < test3 & test1 < test2 & test1 < test4)


table worst_first_score


gen min_score_1 = 0
gen min_score_2 = 0 
gen min_score_3 = 0 
gen min_score_4 = 0 
gen min_score_multiple = 0 



gen min_score_str = "multiple worst tests"
replace min_score_str = "test4" if test4 == min_score & test4 < test3 & test4 < test3 & test4 < test1
replace min_score_str = "test3" if test3 == min_score & test3 < test4 & test3 < test2 & test3 < test1

replace min_score_str = "test2" if test2 == min_score & test2 < test4 & test2 < test3 & test2 < test1
replace min_score_str = "test1" if test1 == min_score & test1 < test4 & test1 < test3 & test1 < test2

replace min_score_1 = 1 if min_score_str == "test1"
replace min_score_2 = 1 if min_score_str == "test2"
replace min_score_3 = 1 if min_score_str == "test3"
replace min_score_4 = 1 if min_score_str == "test4"
replace min_score_multiple = 1 if min_score_str == "multiple worst tests"




replace test1 = (test1/16)*100
replace test2 = (test2/16)*100
replace test3 = (test3/16)*100
replace test4 = (test4/16)*100
replace semester_tests = (semester_tests/48)*100
replace final_test = (final_test/40)*100


gen loss_mins_score1 = treatment_loss * min_score_1

gen worst_score = min(test1, test2, test3, test4)

	
	
regress semester_tests treatment_loss treatment_hybrid min_score_1 worst_score d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)
eststo lm_semester_test0

regress semester_tests treatment_loss treatment_hybrid  min_score_1 loss_mins_score1 worst_score d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)
eststo lm_semester_test1


regress final_test treatment_loss treatment_hybrid  min_score_1 worst_score  d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)
eststo lm_exam_score0

regress final_test treatment_loss treatment_hybrid  min_score_1 worst_score loss_mins_score1 d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)
eststo lm_exam_score1


regress final_test treatment_loss treatment_hybrid  min_score_1 worst_score d_no semester_tests d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)
eststo lm_exam_semesterctrl0
	
regress final_test treatment_loss treatment_hybrid  min_score_1 worst_score loss_mins_score1  d_no semester_tests d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday, cluster(Group_cat)
eststo lm_exam_semesterctrl1
		
	
	
esttab lm_semester_test0  lm_semester_test1  lm_exam_score0 lm_exam_score1 lm_exam_semesterctrl0 lm_exam_semesterctrl1, ///
    label nodepvars mti("Regression Results") ///
    collabels(none) nonumbers booktabs ///
    star(* 0.10 ** 0.05 *** 0.01) se replace ///
    stats(N r2 rmse, fmt(3) labels("Observations" "R-squared" "Residual Std. Error"))	
	
	
	
	
	
	
// --- Second Revision: 
//----------------------	


* Read the data
import delimited using "/Users/antalertl/Desktop/r_projects/2023_loss_aversion_classroom_experiment_BCE/data_clean.csv", clear


* Read alternative data - for robustness
* import delimited using reg_data_no_outliers.csv, clear



/*#------------
data cleaning after reading in the data
#------------*/

foreach v in test1 test2 test3 test4 final_test semester_tests ///
            total_score total_score_without_hw final_score {

    replace `v' = subinstr(`v', ",", ".", .)
    destring `v', replace
}

* percentage recoding

replace test1 = (test1 / 16) * 100
replace test2 = (test2 / 16) * 100
replace test3 = (test3 / 16) * 100
replace test4 = (test4 / 16) * 100
replace semester_tests = (semester_tests / 48) * 100

replace final_test = (final_test / 40) * 100

rename group_cat Group_cat


* Run the regressions with clustered standard errors
regress semester_tests treatment_loss, cluster(Group_cat)
eststo lm1

regress semester_tests treatment_loss treatment_hybrid , cluster(Group_cat)
eststo lm2

regress semester_tests treatment_loss treatment_hybrid d_no, cluster(Group_cat)
eststo lm3

regress semester_tests treatment_loss treatment_hybrid d_no d_mothereduc_uni d_derivation d_nowork, cluster(Group_cat)
eststo lm4

regress semester_tests treatment_loss treatment_hybrid d_no d_mothereduc_uni d_derivation d_nowork time_slot_thursday time_slot_friday, cluster(Group_cat)
eststo lm5

regress semester_tests treatment_loss treatment_hybrid d_no d_mothereduc_uni d_derivation d_nowork time_slot_thursday time_slot_friday d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar, cluster(Group_cat)
eststo lm6


esttab  lm2 lm3 lm4 lm5 lm6, ///
    label nodepvars mti("Regression Results") ///
    collabels(none) nonumbers booktabs ///
    star(* 0.10 ** 0.05 *** 0.01) se replace ///
    stats(N r2 rmse, fmt(3) labels("Observations" "R-squared" "Residual Std. Error"))
	

	
	
// 	
keep if d_okt3 == 1 | d_okt5 == 1 | d_okt6 ==1	
	
	
	
	
regress test1 treatment_loss treatment_hybrid d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm1

regress test2 treatment_loss treatment_hybrid d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm2


regress test3 treatment_loss treatment_hybrid d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm3


regress test4 treatment_loss treatment_hybrid d_no d_mothereduc_uni d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar d_derivation time_slot_thursday time_slot_friday d_nowork, cluster(Group_cat)
eststo lm4

regress semester_tests treatment_loss treatment_hybrid d_no d_mothereduc_uni d_derivation d_nowork time_slot_thursday time_slot_friday d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar, cluster(Group_cat)
eststo lm5

regress final_test treatment_loss treatment_hybrid d_no d_mothereduc_uni d_derivation d_nowork time_slot_thursday time_slot_friday d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar, cluster(Group_cat)
eststo lm6


regress final_test treatment_loss treatment_hybrid semester_tests d_no d_mothereduc_uni d_derivation d_nowork time_slot_thursday time_slot_friday d_okt3 d_okt4 d_okt5 d_okt6 d_okt7 d_szfvar, cluster(Group_cat)
eststo lm7


esttab lm1 lm2 lm3 lm4 lm5 lm6 lm7, ///
    label nodepvars mti("Regression Results") ///
    collabels(none) nonumbers booktabs ///
    star(* 0.10 ** 0.05 *** 0.01) se replace ///
    stats(N r2 rmse, fmt(3) labels("Observations" "R-squared" "Residual Std. Error"))
	








