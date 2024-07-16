********************************************************************************
** Preamble **
********************************************************************************
/*
Project: Final Assignment for PADM-GP.4502.001
Created by: Monica Millay
Created on: 10/30/2023

Description:
Final Assignment (option #4) for Using Large Datasets in Policy Research (Fall 2023).

Prompt:
The City is interested in learning if self-employed (not incorporated) workers are at higher risk of poverty than wage workers in NYC. Use the Current Population Survey to examine whether poverty rates differ depending on whether a worker is self-employed or a wage worker and include an explanation for why differences (if any) exist. 
Results from the study will be used to inform services for independent contractors and other self-employed workers. To make sure that resources are optimally allocated, look into underlying demographic trends (e.g., younger v. older residents).

Notes:
Estimates from the 2023 Current Population Survey (CPS), Annual Social and Economic Supplement (ASEC).
(Data extracted from IPUMS https://cps.ipums.org/cps/index.shtml)

WARNING:
There were only 76 self-employed NYC residents interviewed as a part of the survey, so I used the ASEC-provided weights to create the estimates in this memo.
*/

********************************************************************************
** Setup **
********************************************************************************
cd "/Users/monicamillay/OneDrive - nyu.edu/Fall 2023/Using Large Data Sets in Policy Research/Final assignment/"

use "cps_asec_2023.dta", clear

set more off

log using "final.txt", text replace

********************************************************************************
** Create sample **
********************************************************************************

*dataset includes all of NYS, narrow down to just NYC
recode county (36005 = 1) (36047 = 2) (36061 = 3)  (36081 = 4) (36085 = 5) (0 36055 36059 36067/36071 36087/36119 = 6), gen(county_recode)
label variable county_recode "County Recoded"
label define county_label 1 "Bronx" 2 "Brooklyn" 3 "Manhattan" 4 "Queens" 5 "SI" 6 "Other"
label value county_recode county_label
tab county_recode
drop if county_recode==6

*create self-employed dummy
tab classwkr
tab classwly
*there's a slight difference between the classwkr (class of worker, from CPS) and classwly (class of worker last year, from ASEC) variables. I'll use classwly, since the income and poverty variables are also from ASEC
tab classwly, nolabel
recode classwly (22/28 14 = 0) (13 = 1) (0 = 3), gen(self_emp)
label variable self_emp "Self employed, not incorporated"
label define self_emp_label 0 "Wage worker" 1 "Self-employed" 3 "NA"
label value self_emp self_emp_label
tab self_emp

*simplify race variable
tab race
recode race (100 = 1) (200 = 2) (300 = 3) (651/652 = 4) (700 = 5) (801/830 = 6), gen(race_recode)
label variable race_recode "Race recoded"
label define race_label 1 "White" 2 "Black/African American" ///
3 "American Indian or Alaska Native" 4 "Asian or Pacific Islander" ///
5 "Other Race" 6 "Two or More Races"
label value race_recode race_label
tab race_recode

*simplify ethnicity/hispanic variables
tab hispan
recode hispan (0 = 0) (100/612 = 1), gen(hispan_recode)
label variable hispan_recode "Hispanic recoded"
label define hispan_label 0 "Not Hispanic" 1 "Hispanic"
label value hispan_recode hispan_label
tab hispan_recode

*bin education
tab educ
codebook educ
recode educ (0/71 = 1) (73 = 2) (80/90 = 3) (91/92 = 4)(111/125 = 5) , gen(eduattainment)
label variable eduattainment "Educational Attainment"
label define attainment 1 "No High School Diploma" 2 "High School Diploma" ///
3 "Some College" 4 "Associate's Degree" 5 "Bachelor's Degree or Higher"
label value eduattainment attainment
tab eduattainment

*created employed/unemployed dummy
tab empstat
recode empstat (0 32 34 36 = 0) (10 12 = 1) (21/22 = 2), gen(empstat_recode)
label variable empstat_recode "Simplified employment status"
label define empstat_label 0 "NA" 1 "Employed" 2 "Unemployed"
label value empstat_recode empstat_label
tab empstat_recode

tab offpov // whether last year's income was above or below poverty line. IPUMS corrected version of this variable.
tab offpov, nolabel
*I'd prefer a dummy where 1 = below poverty line and 0 = not below/above poverty line, that way I can do regressions later on.
recode offpov (2 = 0)
*I'm also going to drop the one missing/not in universe (99) observation. Poverty is the main outcome variable, so I don't want to include an observation where it wasn't measured.
drop if offpov == 99

tab fullpart // whether someone worked full or part-time last year.
sum uhrsworkly, d // # hours that someone usually worked last year. 999 is probably for those who didn't work.
sum wkswork1, d // # weeks worked last year. range is 0-52

********************************************************************************
** Weights **
********************************************************************************
/*ASEC uses a special weight (asecwt) which is based on the inverse probability of selection into the sample and adjustments for the following factors: failure to obtain an interview; sampling within large sample units; the known distribution of the entire population according to age, sex, and race; over-sampling Hispanic persons; to give husbands and wives the same weight; and an additional step to provide consistency with labor force estimates from the basic survey.*/
tab offpov [iw= asecwt]
tab offpov self_emp if self_emp !=3 [iw= asecwt], column

/*I can use an iw (importance weight) with asecwt, but then I will get some nonsensical frequencies like in the tabulations above that include decimals for counts of people. It might make more sense to round asecwt to the nearest integer so that I can use it with fw (frequency weight).*/
gen asecwt_rounded = round(asecwt)
tab offpov [fw= asecwt_rounded]
tab offpov self_emp if self_emp !=3 [fw= asecwt_rounded], column
*the column % remain the same, but the frequencies are reported in whole numbers which makes more sense when we are counting people.

********************************************************************************
** Analysis **
********************************************************************************
*/I'm interested in comparing self-employed, not incorporated to wage workers, so I'm going to drop everyone who doesn't fit in those catergories*/
drop if self_emp ==3
tab self_emp [fw= asecwt_rounded]

*what do most self-employed workers do for work?
tab indly if self_emp == 1 [fw= asecwt_rounded], sort

sum age if self_emp == 1 [fw= asecwt_rounded], d // median=51
sum age if self_emp == 0 [fw= asecwt_rounded], d // median=41

*create age bins
recode age (0/17 = 1) (18/24 = 2) (25/39 = 3) (40/54 = 4) (55/66 = 5) (67/85 = 6), gen(age_recode)
label variable age_recode "Age bins"
label define age_label 1 "Under 18" 2 "18-24" 3 "25-39" 4 "40-54" 5 "55-66" 6 "67 and over"
label value age_recode age_label
tab age_recode
tab age_recode self_emp [fw= asecwt_rounded], column

*see if any trends emerge based on other characteristics
tab race_recode self_emp [fw= asecwt_rounded], column
tab hispan_recode self_emp [fw= asecwt_rounded], column
tab sex self_emp [fw= asecwt_rounded], column
tab county_recode self_emp [fw= asecwt_rounded], column
tab eduattainment self_emp [fw= asecwt_rounded], column

*now look at employment trends
*unemployment
tab empstat_recode self_emp [fw= asecwt_rounded], column
tab fullpart self_emp [fw= asecwt_rounded], column

*usual hours worked per week
sum uhrsworkly if self_emp == 1 [fw= asecwt_rounded], d
sum uhrsworkly if self_emp == 0 [fw= asecwt_rounded], d
*both have a median of 40, but the SD for self-employed workers is larger than for wage workers
*let's see if there's a difference if we only look at part-time workers
sum uhrsworkly if self_emp == 1 & fullpart==2 [fw= asecwt_rounded], d
sum uhrsworkly if self_emp == 0 & fullpart==2 [fw= asecwt_rounded], d
*usual hours doesn't seem to be that different between self-employed and wage workers, even if they're part-time

*weeks worked last year
sum wkswork1 if self_emp == 1 [fw= asecwt_rounded], d
sum wkswork1 if self_emp == 0 [fw= asecwt_rounded], d
*similar in terms of mean, median, and SD
*look at only part-time workers
sum wkswork1 if self_emp == 1 & fullpart==2 [fw= asecwt_rounded], d
sum wkswork1 if self_emp == 0 & fullpart==2 [fw= asecwt_rounded], d
*weeks worked doesn't seem to be that different between self-employed and wage workers, even if they're part-time

*maybe the driver of the different poverty rates for self-employed and wage workers is how much they are earning (not how much they are working)
*compare total personal income
sum inctot if self_emp == 1 [fw= asecwt_rounded], d
sum inctot if self_emp == 0 [fw= asecwt_rounded], d

*finish with a regression analysis
reg inctot self_emp [fw= asecwt_rounded]
reg offpov self_emp [fw= asecwt_rounded] 
reg offpov self_emp i.race_recode i.hispan_recode i.county_recode i.eduattainment i.age_recode[fw= asecwt_rounded]
reg offpov self_emp i.race_recode i.hispan_recode i.county_recode i.eduattainment i.age_recode i.fullpart[fw= asecwt_rounded]

********************************************************************************
** Graphics **
********************************************************************************
graph pie [fw=asecwt_rounded], over(self_emp) plabel(1 percent, size(*1.5)) plabel(2 percent, size(*1.5)) plabel(3 percent, size(*1.5)) plabel(4 percent, size(*1.5)) plabel(5 percent, size(*1.5)) plabel(6 percent, size(*1.5)) plabel(7 percent, size(*1.5))

graph pie [fw=asecwt_rounded], over(offpov) by(self_emp)

graph bar offpov [fw = asecwt_rounded], over(self_emp)

graph bar [fw=asecwt_rounded], over(offpov) by(self_emp)

graph pie [fw=asecwt_rounded], over(fullpart) by(self_emp) plabel(1 percent, size(*1.5)) plabel(2 percent, size(*1.5)) plabel(3 percent, size(*1.5)) plabel(4 percent, size(*1.5)) plabel(5 percent, size(*1.5)) plabel(6 percent, size(*1.5)) plabel(7 percent, size(*1.5))

log close
