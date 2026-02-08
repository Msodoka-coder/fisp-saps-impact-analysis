   * ******************************************************************** *
   * ******************************************************************** *
   *                                                                      *
   *****************************************************
	* Project: FISP and SAPS Impact Analysis
	* Author: Issa Thabiso
	* Date: 2026
	* Description:
					* This script performs descriptive statistics,
					* multinomial probit, and MESR impact evaluation.
	****************************************************
							  *
   * ******************************************************************** *
   * ******************************************************************** *

	/*
	** OUTLINE:     
		PART 0: Details and unique identifiers
		PART 1: Globals, Macros, Paths, and Ados
		Part 2: Data Cleaning and Management
		Part 3: Descriptives
		PART 4: Objective 1 Models : Multinomial Probit Models
		PART 5: Robust Checks : bivariate probit regression
		PART 6: Objective 2 :  Impact Evaluation through MESR Models
		PART 7: Treatment Effects
	*/		

********************************************************************************
**# PART 0: Details and unique identifiers
********************************************************************************
/*
	   
       ** NOTES: 		This do file manages data as well as estimates and produces word outputs for an 
						Endogenous Switching Regression Models to quantify the impact of adopting CSAs on 
						productivity and food security.
						
						The data is sourced from the World Bank Living Standards Measurement Study (LSMS) 
						household program.
						
						The do-file uses a loop to efficiently process two separately collected 
						cross-sectional datasets:IHSV (2019/20) and IHSIV (2016/19).
						
       ** WRITTEN BY:   Issah Msodoka (issahmsodoka07@gmail.com) +265 99 209 41 70
	   ** IDS VAR:      case_id

       ** Last date modified: 7 Feb 2026  
*/


********************************************************************************
**# PART 1: Globals, Macros, Paths, and Ados
********************************************************************************

*Ados

 	local user_commands winsor mkdir  table collect rbiprobit  ietoolkit iefieldkit movestay asdoc  codebookout confirmdir zscore06
	  
	foreach command of local user_commands {
		   cap which `command'
		   if _rc == 111 {
			   ssc install `command'
		   }
		   else disp "`command' already installed, moving to the next command line"
	 }
	

	*Set this value to the user currently using this file
   global user  1
   
   
* Project folder : Note ! - make sure the replaced path has / AND not \
	  
	
	if $user == 1 {
       global projectpath "C:/Users/Issa Msodoka/Documents/Applications/IHS MW data cleaning"
   }

   if $user == 2 {
       global projectfolder "C:/Users/KHUNDI/Documents/Commercialization"  // Enter the file path to the project folder for the next user here
   }

	
	cd "$projectpath"

	
*Creating project folders

    local mainfolder `""Data and Do file""'
	
    foreach dir in `mainfolder' {
        confirmdir "`dir'"
        if `r(confirmdir)'==170 {
            mkdir "`dir'"
            display in yellow "Project directory named: `dir' created"
            }
        else disp as error "`dir' already exists. Skipped to next command."
        cd "${projectpath}/`dir'"
    }

	
*Creating subfolders
    local subfolders `" "IHSV" "Working files"  "Do" "IHSIV" "'
	

    foreach dir in `subfolders' {
        confirmdir "`dir'"
        if `r(confirmdir)'==170 {
            mkdir "`dir'"
            disp in yellow "`dir' successfully created."
        }
        else disp as error "`dir' already exists. Skipped to next command."
    }

****NOTE : You need to Manually  Copy the IHSIV data to the "IHSIV" folder 	
**************************************************************************
	
* Path globals
	global IHSV				"${projectpath}/Data and Do file/IHSV"
	global IHSIV			"${projectpath}/Data and Do file/IHSIV"
	global workingfiles		"${projectpath}/Data and Do file/Working files"
	global do 				"${projectpath}/Data and Do file/do"
	
	
* Equation globals
	global socio 		age 		gender 		educ 					hh_Size
	global insitutional extension 	credit	
	global farmlevel 	type2 		type3 		quality2 				quality3
	global weather  	l_avr_yearly_tmp2019   	l_avr_yearly_pre2019
	global selection	extension 	drought	 
	global eq			$socio		$insitutional		$farmlevel		$weather 


	global categorical 	gender 		FGT0 		extension				credit  ///
	type1  type2 		type3 		quality1 	quality2 				quality3 /// 		
	drought 		box_ridges 				minimum_tillage 					///
	erosion_control_bunds 			vetiver 	plating_pits 			traditional_tilage ///
	Terraces Water_harvest_bunds 	agro_forestry  						none

			
			
	global continous 	age 		educ 		hh_Size 				Plot_Area 	///
	output l_avr_yearly_tmp2019 	l_avr_yearly_pre2019  				IV_fisp 	///
	IV_saps IV_fisp_saps
	 
	global ihsivlock IHSIV // a lock to apply for IHS4 only, 
///in instances where there are different variable or datafile names
	 
********************************************************************************
**#Part 2: Data Cleaning and Management
********************************************************************************

foreach IHSnumber in IHSIV IHSV  {
		
use "${`IHSnumber'}/hh_mod_b.dta", clear 

		
		*merging with pertinent modules
		merge m:1 	case_id  		using 	"${`IHSnumber'}/hh_mod_a_filt.dta", 	nogen 
		merge m:1 	case_id	 	  	using 	"${`IHSnumber'}/hh_mod_f.dta", 			nogen 
		merge m:1 	case_id  	 	using 	"${workingfiles}/asset.dta", 			nogen 
		merge m:1 	case_id  	 	using 	"${`IHSnumber'}/hh_mod_a_filt.dta", 	nogen

		
		if "`IHSnumber'"=="$ihsivlock" {
			
			foreach module in c v e {	
				merge 1:1 case_id pid using "${`IHSnumber'}/hh_mod_`module'.dta", nogen
				}
				
			rename pid	PID
		}
		
			else {
						foreach module in c v e {	
								merge 1:1 case_id PID using "${`IHSnumber'}/hh_mod_`module'.dta", nogen
						}
			}

		
*Renaming variables that are labeled differently between the two datasets.

		if "`IHSnumber'"=="$ihsivlock" {
			rename  (hhid) (HHID)
		}
		
		
* HH demo vars
		********************************************************************************
		
		*Age
		bysort 		case_id: g  	age=hh_b05a 		if 	hh_b04==1 

		*Dependancy ratio		
		bysort 	case_id: 	egen 	a=count(PID) 		if  hh_b05a<=14  | hh_b05a>64
		bysort 	case_id: 	egen	dependants=max(a) 	
		
		bysort 	case_id: 	egen 	b=count(PID) 		if 	hh_b05a>14	 & hh_b05a<=64
		bysort 	case_id: 	egen	working=max(b) 
		
		recode	 dependants (.=0)	
		recode 	 working	(.=1)
		
		g	 	 dependency=(dependants/working)
		
		*HH_Size
		bysort 	 case_id: 	 egen 	hh_size=count(PID)

		*Alduts
		bysort  case_id: 	 egen   alduts=count(PID) 	if hh_b05a>=15
		
		*Maritial 
		bysort case_id: 	 g  	marital=hh_b24 		if hh_b04==1
		
		
		*Gender
		bysort case_id: 	 g  	gender=hh_b03 		if hh_b04==1
		
		*Years in the Village
		bysort case_id: 	 g  	years_village=hh_b12 if hh_b04==1
		replace years_village=age 	if  missing( hh_b12 )
		
		*Education
		bysort case_id: g  educ=hh_c08 if hh_b04==1
		
	
* HH MPI Indictors 
		********************************************************************************
		
*Education
			
			/*A household is deprived if all members aged 15+ have
			*less than 8 years of schooling OR cannot read or write
			English or Chichewa
			
			There is no variable asking the exact years spend on education
			Therefore, I make the strong assumption that getting to STD 8 
			is above the above defined assumption*/
			
			
			g 		lessstd8=1		if		((hh_b05a>=15)		& 	(hh_c08<8))
			
			if "`IHSnumber'"=="$ihsivlock" {
				g 		cannotread=1	if 		((hh_b05a>=15)		& 	(hh_c05a==2 & hh_c05b==2 ))
			}
			else {
					g 		cannotread=1	if 		((hh_b05a>=15)		& 	(hh_c05_1==2 ))
			}
			
			g 		deped=1 		if 		lessstd8==1			| 	cannotread==1
			
			
			// counting the above created conditions per HH
			bysort		case_id:	egen hh_15_counta=count(PID)		if 	hh_b05a>=15
			bysort		case_id:	egen depedcounta=count(PID)			if 	deped==1 	
			
			foreach 	var 	in 	hh_15_count depedcount {
				bysort	case_id:	egen `var'=count(`var'a)
			}
			
			bysort 		case_id:	g	 dep_educ_1=1 	if		hh_15_count==depedcount 
		
		
			/*A household is deprived if at least one child aged 6–14 is
			not attending school*/
			g 		attendance=1 	if 	((inrange(hh_b05a, 6, 14)) & hh_c13==2)
			bysort 		case_id:	egen	 dep_educ_2=max(attendance) 
			
			
*Health and Population

			/*A household is deprived if the sanitation facility is not
			flush or a VIP latrine or a latrine with a roof OR if it is
			shared with other households*/
			
			if "`IHSnumber'"=="$ihsivlock" {
					g		  latrine=1 	if 	(inrange(hh_f41, 8, 13)) | hh_f42==2	
				  }
				else 	g		  latrine=1 	if 	(inrange(hh_f41, 8, 13)) | hh_f41_4==1
			
			bysort 		case_id:	egen	 dep_helth_1=max(latrine) 
			
			
			/*A household is deprived if there is at least one child
			under 5 who is either underweight, stunted, or wasted*/
			
			gen 	 agemonths =hh_b05b     if hh_b05a ==0
			replace  agemonths =hh_b05b +12 if hh_b05a ==1
			replace  agemonths =hh_b05b +24 if hh_b05a ==2
			replace  agemonths =hh_b05b +36 if hh_b05a ==3
			replace  agemonths =hh_b05b +48 if hh_b05a ==4

						
			*** Create child age-group dummies

			* 0-23 months
			gen 	age_0_23m = (agemonths <= 23)        if hh_b05a <18

			* 24-59 months
			gen 	age_24_59m= inrange(agemonths,24,59) if hh_b05a <18
			
			* Generate z-scores for stunting, underweight and wasting for children under 5
			* using 'zscore06' the 2006 WHO child growth standards 
		

			* Age of child in months, a()
			sum      agemonths

			* Sex of child, s() where 1 is male and 2 is female
			tab      hh_b03

			* Height/length of child, h() in cm 
			*	Recumbent length is assumed for children <24months 
			*	While standing height for children >24 months. If not, measure() must be 
			*   specified 1 for recumbent length and 2 for standing height 

	
			tab      hh_v10 if hh_b05a <5, m
			gen 	 measure = cond(hh_v10<2,2,1) if hh_b05a <5 & !missing(hh_v10)

			* Treating missing 'measure', assuming recumbent length for 0-23 months and
			*    standing height for 24-59 months
			tab      measure if hh_b05a <5, m
			replace  measure = 1 if age_0_23m == 1 & missing(hh_v10) 
			replace  measure = 2 if age_24_59m== 1 & missing(hh_v10)
			tab      measure	

			* Check the variables for missing observations
			count if (hh_b03==. | hh_v09==. | hh_v08==. | measure==.) & hh_b05a<5

			//Expecting to have some cases with missing zscores

			zscore06, a(agemonths) s(hh_b03) h(hh_v09) w(hh_v08) measure(measure)

			* Check missing zscores 
			count if haz06==. & hh_b05a <5
			count if waz06==. & hh_b05a <5
			count if whz06==. & hh_b05a <5
			   
			* ------------------------------------------------------------------------------
			*** Indicator i) Stunted

			* Deprived if height for age < -2 standard deviations (s.d.) (24-59 months)
			* ------------------------------------------------------------------------------

			gen 	ind_stunted =   (haz06 <-2) if age_24_59m ==1
			replace ind_stunted = . if haz06==. &  age_24_59m ==1

			* ------------------------------------------------------------------------------
			*** Indicator ii) Underweight

			* Deprived if Weight for age <-2 s.d. (0-23 months)
			* ------------------------------------------------------------------------------

			gen     ind_underweight =   (waz06 <-2) if age_0_23m ==1
			replace ind_underweight = . if waz06==. &  age_0_23m ==1

			* ------------------------------------------------------------------------------
			*** Indicator iii) Wasting

			* Deprived if weight for height <-2 s.d. (0-23 months)
			* ------------------------------------------------------------------------------

			gen      ind_wasting =   (whz06 <-2) if age_0_23m ==1
			replace  ind_wasting = . if whz06==. &  age_0_23m ==1
			* ------------------------------------------------------------------------------
						
			foreach ind in stunted underweight wasting {
				bysort 		case_id:	egen	 	`ind'=max(ind_`ind') 
			}
			
			gen 	dep_helth_2=1 	if  stunted==1  | underweight==1  |  wasting==1
			
			/*A household is deprived if their main source of water is
			unimproved OR it takes 30 minutes or more (round trip)
			to collect it*/
			
			gen      	watersource= 		inlist(hh_f36,4,5,10,11,12,16,18) 
			
			foreach  string in PIPED TAP PROTECTED {
				replace watersource=. if regexm(hh_f36_oth,"`string'")
			}
			 
			replace 	watersource=1 	if 	hh_f38a>30 	& hh_f38b==1
			replace 	watersource=1 	if 	hh_f38a>1 	& hh_f38b==2
			
			bysort 		case_id:	egen	 	dep_helth_3=max(watersource)
			
			
			/*A household is deprived if, in the past 12 months, they
			were hungry but did not eat AND went without eating for
			a whole day because there was not enough money or
			other resources for food*/
			
			
			/*A household is deprived if they do not have access to
			electricity*/
			
			bysort 		case_id:	egen 		electricity=max(hh_f19) 	
			bysort 		case_id:	g			dep_env1=1 		if inlist(electricity,1)
			
			/*A household is deprived if rubbish is disposed of on a
			public heap, is burnt, disposed of by other means, or there
			is no disposal*/
			
			gen      	dep_env2= 		inlist(hh_f43,3,4,5,6) 
			
			foreach string in DISPOS SERVICE {
				replace dep_env2=. 	if	regexm(hh_f43_oth,"`string'")
			}
			
			
			/* A household is deprived if at least two of the following
			dwelling structural components are of poor quality:
				•Walls (grass, mud, compacted earth, unfired mud
				bricks, wood, iron sheets, or other materials)
				•Roof (grass, plastic sheeting, or other materials)
				•Floor (sand, smoothed mud, wood, or other materials)*/
			
			gen      	wall= 		inlist(hh_f07,1,2,3,4,7,8,9) 
			
			foreach string in CONC GLA {
				replace wall=. 	if	regexm(hh_f07_oth,"`string'")
			}
			
			
			gen      	roof= 		inlist(hh_f08,1,5,6) 
			
			foreach string in LEEDS TILES  {
				replace roof=. 	if	regexm(hh_f08_oth,"`string'")
			}
			
			gen      	floor= 		inlist(hh_f09,1,2,4,6)
			
			foreach string in CEME CONC  {
				replace floor=. 	if	regexm(hh_f09_oth,"`string'")
			}
			
			egen		agrigate=rowtotal(wall roof floor )
			
			bysort 		case_id:	g			dep_env3=1 		if agrigate>=2 
			
			
			g 		asset_1dummy=1 	if 	hh_b04a==1
			egen	asset_1=max(asset_1dummy),by(case_id)
			
			
*Employment (1/4)		

			/*A household is deprived if at least one member aged
			18–64 has not been working but has been looking for a
			job during the past 4 weeks*/
			
				
			if "`IHSnumber'"=="$ihsivlock" {
								g 		unempllookingfor=1		if 	(hh_e06_2==2  	| 	hh_e06_4==2	)	& 	hh_e16==1 & inrange(hh_b05a,18,64)	
				  }
				else 			g 		unempllookingfor=1		if 	(hh_e06_2==2  	| 	hh_e06_4==2	)	& 	hh_e17_1==1 & inrange(hh_b05a,18,64)
				
				


			egen 	dep_empl_1=max(unempllookingfor), 	by(case_id)
			
			/*A household is deprived if all working members are only
			engaged in farm activities, household livestock activities,
			or casual part-time work (ganyu)*/
			
			g 		ganyu=1 	if	(hh_e06_6==1 	| 	hh_e06_1a==1  |	hh_e06_1b==1 | inlist(hh_e06_8a,3,5))	& inrange(hh_b05a,18,64)
			replace ganyu=. 	if 	hh_e06_2==1  	| 	hh_e06_4==1	  | inlist(hh_e06_8a,1,2)
			
			egen 	workinga=count(PID)		if 		inrange(hh_b05a,18,64), 	by(case_id)
			egen 	wcount=	max(workinga)	, 	by(case_id)
			egen 	gcount=count(ganyu)		, 	by(case_id)
			
			g 		equal=1 	if 	wcount==gcount & !missing(wcount)
			
			egen  	dep_empl_2=max(equal)	, by(case_id)
			
			/*A household is deprived if any child aged 5–17 is
			engaged in any economic activities in or outside of the
			household*/
			g		ylabor=1 if (inlist(hh_e06_8a,1,2,5)	|	hh_e06_2==1	|	hh_e06_4==1	|	hh_e06_6==1	)	 & inrange(hh_b05a,5,17)
			
			egen  	dep_empl_3=max(ylabor)	, by(case_id)
			
			

			/*A household is deprived if they do not own more than
			two of the following basic livelihood items: radio,
			television, telephone, computer, animal cart, bicycle,
			motorbike, or refrigerator AND do not own a car or truck*/
			
			egen assets=rowtotal(assest_507 assest_5081 assest_509 assest_529 assest_516 assest_517 assest_514  assest_609 assest_610 assest_613 asset_1)
			
			g	 dummy=1	if	 assets>2 | car==1

			bysort 		case_id:	egen	 	dep_helth_4=max(dummy)

			collapse (firstnm) ea_id HHID (max) age hh_size alduts educ years_village marital dependency gender dep_* reside region district  hh_wgt hh_a02a, by (case_id)
		
save  "${workingfiles}/`IHSnumber'a.dta", replace 

		
		*Shock 

use "${`IHSnumber'}/hh_mod_u.dta", clear

		g drought=1 if hh_u0a==101 & hh_u01==1
		g floods=1 if hh_u0a==102 & hh_u01==1
		g irregularrains=1 if hh_u0a==1101 & hh_u01==1	
		keep case_id  drought floods irregularrains
		collapse (max) drought floods irregularrains, by (case_id)
		merge 1:1 case_id using "${workingfiles}/`IHSnumber'a.dta", nogen
	
save  "${workingfiles}/`IHSnumber'c_b.dta", replace 			
		
	
		*Temp 	
		if "`IHSnumber'"=="$ihsivlock" {
			use "${`IHSnumber'}/householdgeovariablesihs4.dta", clear 
			rename (af_bio_1 af_bio_12) ( af_bio_1_x af_bio_12_x )
		}
		
		if "`IHSnumber'"!="$ihsivlock" {
			use "${`IHSnumber'}/householdgeovariables_ihs5.dta", clear 
		}

		keep case_id af_bio_1_x af_bio_12_x
		replace af_bio_1_x=af_bio_1_x/10
		merge 1:1 case_id using "${workingfiles}/`IHSnumber'c_b.dta", nogen
save  "${workingfiles}/`IHSnumber'ob.dta", replace 


**# 	Credit
use "${`IHSnumber'}/hh_mod_s1.dta", clear				
		keep if hh_s01==1
		duplicates drop case_id, force
		keep hh_s01 case_id
		rename hh_s01 credit
		merge 1:1 case_id using "${workingfiles}/`IHSnumber'ob.dta", nogen	
save  "${workingfiles}/`IHSnumber'hh_data1.dta", replace 		
	
	
**# 	Extension
use "${`IHSnumber'}/ag_mod_t1.dta" , clear 
		keep if ag_t01==1
		keep case_id ag_t01
		collapse (max) ag_t01 , by (case_id)
		rename ag_t01 extension
		merge 1:1 case_id using "${workingfiles}/`IHSnumber'hh_data1.dta", nogen	
save  "${workingfiles}/`IHSnumber'hh_data2.dta", replace 		


**# 	input_subsidy
use "${`IHSnumber'}/ag_mod_e2.dta" , clear 
		keep if ag_e01==1
		keep case_id ag_e01
		collapse (max) ag_e01 , by (case_id)
		rename ag_e01 input_subsidy
		merge 1:1 case_id using "${workingfiles}/`IHSnumber'hh_data2.dta", nogen	
save  "${workingfiles}/`IHSnumber'hh_data.dta", replace 		




**#		Land Size
use "${`IHSnumber'}/ag_mod_c.dta" , clear
		keep case_id gardenid plotid ag_c04a ag_c04b ag_c04c ag_c04b_oth
		replace ag_c04a=ag_c04a*2.4711 if ag_c04b==2
		replace ag_c04a=ag_c04a*000024710538146717 if ag_c04b==3
		replace ag_c04a=ag_c04a*0.0002066115903 if ag_c04b_oth=="YARDS"
		replace ag_c04a=ag_c04a*000024710538146717 if ag_c04b_oth=="METERS"
		replace ag_c04c=ag_c04b if ag_c04c==. | ag_c04c==0
		rename ag_c04c Plot_Area
		keep case_id gardenid plotid Plot_Area
save  "${workingfiles}/`IHSnumber'p.dta", replace 
		
		
**# 	Plot details 
use "${`IHSnumber'}/ag_mod_d.dta" , clear
		g maize=.
		foreach v of varlist ag_d20a ag_d20b ag_d20c ag_d20d ag_d20e {
		replace maize=1 if `v'<=4
		}
		g type= ag_d21 if maize==1
		g quality= ag_d22 if maize==1
		rename ag_d36  organic_fertilizer
		rename ag_d38  inorganic_fertilizer
		rename ag_d55_1  agro_forestry
		g box_ridges=1 if ag_d62==2
		g minimum_tillage=1 if ag_d62==6
		g traditional_tilage=1 if ag_d62==1
		g plating_pits=1 if ag_d62==3
		
		
		foreach i in erosion_control_bunds vetiver Terraces Water_harvest_bunds  {
			g `i'=.
		}
		
		foreach i in a b  {
			replace erosion_control_bunds=1 if ag_d25`i'==3
			replace vetiver=1 if ag_d25`i'==5
			replace Terraces=1 if ag_d25`i'==2
			replace Water_harvest_bunds=1 if ag_d25`i'==7
			
		}
		
		
		keep case_id gardenid plotid maize type quality inorganic_fertilizer organic_fertilizer agro_forestry erosion_control_bunds vetiver box_ridges minimum_tillage plating_pits traditional_tilage Terraces Water_harvest_bunds
		merge 1:1 case_id gardenid plotid using "${workingfiles}/`IHSnumber'p.dta", nogen
save  "${workingfiles}/`IHSnumber'q.dta", replace 	




**# 	Dry season
use "${`IHSnumber'}/ag_mod_k.dta" , clear
		
		g dry_season=.
		
		foreach v of varlist ag_k21a ag_k21b ag_k21c ag_k21d ag_k21e {
		replace dry_season=1 if `v'<=4
		}
		
		keep case_id gardenid plotid dry_season 
		merge 1:1 case_id gardenid plotid using "${workingfiles}/`IHSnumber'q.dta", nogen
save  "${workingfiles}/`IHSnumber'ra.dta", replace 		
	
	
**#output
use  "${`IHSnumber'}/ag_mod_g.dta" , clear 	
	
	merge m:1 case_id  using "${`IHSnumber'}/hh_mod_a_filt.dta" 
	
	keep if _merge ==3
	rename ag_g13a output
	rename ag_g13b unit 
	rename ag_g13c condition
	
	
	*keep if crop_code<=4
	
	replace output=output*51.6    if region==1  & crop_code==1  & unit==2   & condition==1
	replace output=output*46.07   if region==1  & crop_code==1  & unit==2   & condition>=2
	replace output=output*99.9    if region==1  & crop_code==1  & unit==3   & condition==1
	replace output=output*82.92   if region==1  & crop_code==1  & unit==3   & condition>=2
	replace output=output*4.45    if region==1  & crop_code==1  & unit==4   & condition==1
	replace output=output*18.7    if region==1  & crop_code==1  & unit==5   & condition==1
	replace output=output*14.98   if region==1  & crop_code==1  & unit==5   & condition>=2
	replace output=output*468     if region==1  & crop_code==1  & unit==12  & condition==1
	replace output=output*388.44  if region==1  & crop_code==1  & unit==12  & condition>=2
	replace output=output*10      if region==1  & crop_code==1  & unit==14  & condition==1
	replace output=output*51.6    if region==1  & crop_code==2  & unit==2   & condition==1
	replace output=output*46.07   if region==1  & crop_code==2  & unit==2   & condition>=2
	replace output=output*99.9    if region==1  & crop_code==2  & unit==3   & condition==1
	replace output=output*82.92   if region==1  & crop_code==2  & unit==3   & condition>=2
	replace output=output*4.45    if region==1  & crop_code==2  & unit==4   & condition==1
	replace output=output*18.7    if region==1  & crop_code==2  & unit==5   & condition==1
	replace output=output*14.98   if region==1  & crop_code==2  & unit==5   & condition>=2
	replace output=output*468     if region==1  & crop_code==2  & unit==12  & condition==1
	replace output=output*388.44  if region==1  & crop_code==2  & unit==12  & condition>=2
	replace output=output*10      if region==1  & crop_code==2  & unit==14  & condition==1
	replace output=output*51.6    if region==1  & crop_code==3  & unit==2   & condition==1
	replace output=output*46.07   if region==1  & crop_code==3  & unit==2   & condition>=2
	replace output=output*99.9    if region==1  & crop_code==3  & unit==3   & condition==1
	replace output=output*82.92   if region==1  & crop_code==3  & unit==3   & condition>=2
	replace output=output*4.45    if region==1  & crop_code==3  & unit==4   & condition==1
	replace output=output*18.7    if region==1  & crop_code==3  & unit==5   & condition==1
	replace output=output*14.98   if region==1  & crop_code==3  & unit==5   & condition>=2
	replace output=output*468     if region==1  & crop_code==3  & unit==12  & condition==1
	replace output=output*388.44  if region==1  & crop_code==3  & unit==12  & condition>=2
	replace output=output*10      if region==1  & crop_code==3  & unit==14  & condition==1
	replace output=output*51.6    if region==1  & crop_code==4  & unit==2   & condition==1
	replace output=output*46.07   if region==1  & crop_code==4  & unit==2   & condition>=2
	replace output=output*99.9    if region==1  & crop_code==4  & unit==3   & condition==1
	replace output=output*82.92   if region==1  & crop_code==4  & unit==3   & condition>=2
	replace output=output*4.45    if region==1  & crop_code==4  & unit==4   & condition==1
	replace output=output*18.7    if region==1  & crop_code==4  & unit==5   & condition==1
	replace output=output*14.98   if region==1  & crop_code==4  & unit==5   & condition>=2
	replace output=output*468     if region==1  & crop_code==4  & unit==12  & condition==1
	replace output=output*388.44  if region==1  & crop_code==4  & unit==12  & condition>=2
	replace output=output*10      if region==1  & crop_code==4  & unit==14  & condition==1
	replace output=output*50      if region==2  & crop_code==1  & unit==2   & condition==1
	replace output=output*44.23   if region==2  & crop_code==1  & unit==2   & condition>=2
	replace output=output*95.93   if region==2  & crop_code==1  & unit==3   & condition==1
	replace output=output*79.62   if region==2  & crop_code==1  & unit==3   & condition>=2
	replace output=output*5.29    if region==2  & crop_code==1  & unit==4   & condition==1
	replace output=output*3.83    if region==2  & crop_code==1  & unit==4   & condition>=2
	replace output=output*16.69   if region==2  & crop_code==1  & unit==5   & condition==1
	replace output=output*14.39   if region==2  & crop_code==1  & unit==5   & condition>=2
	replace output=output*468     if region==2  & crop_code==1  & unit==12  & condition==1
	replace output=output*682     if region==2  & crop_code==1  & unit==12  & condition>=2
	replace output=output*8.05    if region==2  & crop_code==1  & unit==14  & condition==1
	replace output=output*50      if region==2  & crop_code==2  & unit==2   & condition==1
	replace output=output*44.23   if region==2  & crop_code==2  & unit==2   & condition>=2
	replace output=output*95.93   if region==2  & crop_code==2  & unit==3   & condition==1
	replace output=output*79.62   if region==2  & crop_code==2  & unit==3   & condition>=2
	replace output=output*5.29    if region==2  & crop_code==2  & unit==4   & condition==1
	replace output=output*3.83    if region==2  & crop_code==2  & unit==4   & condition>=2
	replace output=output*16.69   if region==2  & crop_code==2  & unit==5   & condition==1
	replace output=output*14.39   if region==2  & crop_code==2  & unit==5   & condition>=2
	replace output=output*468     if region==2  & crop_code==2  & unit==12  & condition==1
	replace output=output*682     if region==2  & crop_code==2  & unit==12  & condition>=2
	replace output=output*8.05    if region==2  & crop_code==2  & unit==14  & condition==1
	replace output=output*50      if region==2  & crop_code==3  & unit==2   & condition==1
	replace output=output*44.23   if region==2  & crop_code==3  & unit==2   & condition>=2
	replace output=output*95.93   if region==2  & crop_code==3  & unit==3   & condition==1
	replace output=output*79.62   if region==2  & crop_code==3  & unit==3   & condition>=2
	replace output=output*5.29    if region==2  & crop_code==3  & unit==4   & condition==1
	replace output=output*3.83    if region==2  & crop_code==3  & unit==4   & condition>=2
	replace output=output*16.69   if region==2  & crop_code==3  & unit==5   & condition==1
	replace output=output*14.39   if region==2  & crop_code==3  & unit==5   & condition>=2
	replace output=output*468     if region==2  & crop_code==3  & unit==12  & condition==1
	replace output=output*682     if region==2  & crop_code==3  & unit==12  & condition>=2
	replace output=output*8.05    if region==2  & crop_code==3  & unit==14  & condition==1
	replace output=output*50      if region==2  & crop_code==4  & unit==2   & condition==1
	replace output=output*44.23   if region==2  & crop_code==4  & unit==2   & condition>=2
	replace output=output*95.93   if region==2  & crop_code==4  & unit==3   & condition==1
	replace output=output*79.62   if region==2  & crop_code==4  & unit==3   & condition>=2
	replace output=output*5.29    if region==2  & crop_code==4  & unit==4   & condition==1
	replace output=output*3.83    if region==2  & crop_code==4  & unit==4   & condition>=2
	replace output=output*16.69   if region==2  & crop_code==4  & unit==5   & condition==1
	replace output=output*14.39   if region==2  & crop_code==4  & unit==5   & condition>=2
	replace output=output*468     if region==2  & crop_code==4  & unit==12  & condition==1
	replace output=output*682     if region==2  & crop_code==4  & unit==12  & condition>=2
	replace output=output*8.05    if region==2  & crop_code==4  & unit==14  & condition==1
	replace output=output*51      if region==3  & crop_code==1  & unit==2   & condition==1
	replace output=output*44.27   if region==3  & crop_code==1  & unit==2   & condition>=2
	replace output=output*96.02   if region==3  & crop_code==1  & unit==3   & condition==1
	replace output=output*79.69   if region==3  & crop_code==1  & unit==3   & condition>=2
	replace output=output*5.25    if region==3  & crop_code==1  & unit==4   & condition==1
	replace output=output*18      if region==3  & crop_code==1  & unit==5   & condition==1
	replace output=output*14.73   if region==3  & crop_code==1  & unit==5   & condition>=2
	replace output=output*468     if region==3  & crop_code==1  & unit==12  & condition==1
	replace output=output*388.44  if region==3  & crop_code==1  & unit==12  & condition>=2
	replace output=output*8.94    if region==3  & crop_code==1  & unit==14  & condition==1
	replace output=output*51      if region==3  & crop_code==2  & unit==2   & condition==1
	replace output=output*44.27   if region==3  & crop_code==2  & unit==2   & condition>=2
	replace output=output*96.02   if region==3  & crop_code==2  & unit==3   & condition==1
	replace output=output*79.69   if region==3  & crop_code==2  & unit==3   & condition>=2
	replace output=output*5.25    if region==3  & crop_code==2  & unit==4   & condition==1
	replace output=output*18      if region==3  & crop_code==2  & unit==5   & condition==1
	replace output=output*14.73   if region==3  & crop_code==2  & unit==5   & condition>=2
	replace output=output*468     if region==3  & crop_code==2  & unit==12  & condition==1
	replace output=output*388.44  if region==3  & crop_code==2  & unit==12  & condition>=2
	replace output=output*8.94    if region==3  & crop_code==2  & unit==14  & condition==1
	replace output=output*51      if region==3  & crop_code==3  & unit==2   & condition==1
	replace output=output*44.27   if region==3  & crop_code==3  & unit==2   & condition>=2
	replace output=output*96.02   if region==3  & crop_code==3  & unit==3   & condition==1
	replace output=output*79.69   if region==3  & crop_code==3  & unit==3   & condition>=2
	replace output=output*5.25    if region==3  & crop_code==3  & unit==4   & condition==1
	replace output=output*18      if region==3  & crop_code==3  & unit==5   & condition==1
	replace output=output*14.73   if region==3  & crop_code==3  & unit==5   & condition>=2
	replace output=output*468     if region==3  & crop_code==3  & unit==12  & condition==1
	replace output=output*388.44  if region==3  & crop_code==3  & unit==12  & condition>=2
	replace output=output*8.94    if region==3  & crop_code==3  & unit==14  & condition==1
	replace output=output*51      if region==3  & crop_code==4  & unit==2   & condition==1
	replace output=output*44.27   if region==3  & crop_code==4  & unit==2   & condition>=2
	replace output=output*96.02   if region==3  & crop_code==4  & unit==3   & condition==1
	replace output=output*79.69   if region==3  & crop_code==4  & unit==3   & condition>=2
	replace output=output*5.25    if region==3  & crop_code==4  & unit==4   & condition==1
	replace output=output*18      if region==3  & crop_code==4  & unit==5   & condition==1
	replace output=output*14.73   if region==3  & crop_code==4  & unit==5   & condition>=2
	replace output=output*468     if region==3  & crop_code==4  & unit==12  & condition==1
	replace output=output*388.44  if region==3  & crop_code==4  & unit==12  & condition>=2
	replace output=output*8.94    if region==3  & crop_code==4  & unit==14  & condition==1

		keep case_id output gardenid plotid
		duplicates drop  case_id gardenid plotid, force 
		merge 1:1 case_id gardenid plotid using "${workingfiles}/`IHSnumber'ra.dta", nogen
save  "${workingfiles}/`IHSnumber'rb.dta", replace 

**# 	Harvest
use "${`IHSnumber'}/ag_mod_g.dta" , clear
		keep if crop_code<=4
		foreach i in plotid gardenid {
			drop if `i'==""
		}
		
		if "`IHSnumber'"=="$ihsivlock" {
			rename (ag_g13a) ( ag_g13_1)
		
		}
		
		rename ag_g13_1 harvest 
		collapse (sum) harvest, by (case_id gardenid plotid)
		keep case_id gardenid plotid  harvest
		
		merge 1:1 case_id gardenid plotid using "${workingfiles}/`IHSnumber'rb.dta", nogen	
		
		foreach i in  dry_season organic_fertilizer inorganic_fertilizer maize type quality  agro_forestry{ 
			recode `i' (.=0)
		}
			
		collapse (sum) harvest Plot_Area output (max) plating_pits traditional_tilage Terraces Water_harvest_bunds dry_season organic_fertilizer agro_forestry inorganic_fertilizer maize type quality erosion_control_bunds vetiver box_ridges minimum_tillage, by (case_id)
		
save  "${workingfiles}/`IHSnumber's.dta", replace





		
		

**# 	Sales
use "${`IHSnumber'}/ag_mod_i.dta" , clear 		
		keep if crop_code<=4
		keep if ag_i01==1
		rename ag_i01 sold
		rename ag_i03 sales
		keep case_id crop_code sales sold 
		collapse (sum) sales (max) sold, by(case_id)
		merge 1:1 case_id  using "${workingfiles}/`IHSnumber's.dta", nogen	
		merge 1:1 case_id  using "${workingfiles}/`IHSnumber'hh_data.dta", nogen	
		keep if maize==1
		
		
		


save  "${workingfiles}/`IHSnumber'combined.dta", replace		
	

**# 	Data Construction 
		
*ooutliers
		foreach v of varlist  output {
			zscore `v'
			egen a_`v'=mean(`v') if z_`v'<3
			egen m_`v'=mean(a_`v')
			replace `v'=m_`v' if `v'<50
			replace `v'=m_`v' if z_`v'>3
			replace `v'=m_`v' if z_`v'<-0.02
}
		drop z_* a_* m_*
		
		
foreach v of varlist  output {
			zscore `v'
			egen a_`v'=mean(`v') if z_`v'<3
			egen m_`v'=mean(a_`v')
			replace `v'=m_`v' if `v'<50
			replace `v'=m_`v' if z_`v'>3
			*replace `v'=m_`v' if z_`v'<-0.1
}
		drop z_* a_* m_*
		

			g productivity=output/Plot_Area
	
	foreach v of varlist input_subsidy organic_fertilizer reside gender  inorganic_fertilizer sold  maize dry_season credit extension agro_forestry erosion_control_bunds vetiver box_ridges minimum_tillage drought floods irregularrains plating_pits traditional_tilage Terraces Water_harvest_bunds {
		recode `v' (2=0)
		recode `v' (.=0)
		la val `v' YN
	}
	
	
**# Value labels
	
	la define gender 1 "Male" 0 "Female"
	la define YN 1 "Yes" 0 "No"
	la define reside1 1 "urban" 0 "rural"
	la define marital 1 "Married" 2 "Separated/Divorced" 3 "Widowed" 4 "Never"
	la define type 1 "Sandy" 2 "Between" 3 "Clay"
	la define quality 1 "Good" 2 "Fair" 3 "Poor"

	
	foreach v of varlist input_subsidy organic_fertilizer reside gender  inorganic_fertilizer sold  maize dry_season credit extension agro_forestry erosion_control_bunds vetiver box_ridges minimum_tillage drought floods irregularrains plating_pits traditional_tilage Terraces Water_harvest_bunds {
		recode `v' (2=0)
		recode `v' (.=0)
		la val `v' YN
	}
	
	recode marital (2=1)
	recode marital (3=2)
	recode marital (4=2)
	recode marital (5=3)
	recode marital (6=4)
	
	recode type (4=2)
	recode educ (.=0)
	
	
	la val marital marital
	la val quality quality
	la val type ag_d21
	la val gender gender
	la val reside reside1
	
	foreach i in marital type quality region {
		ta `i', g(`i')
	}
	
	global saps box_ridges erosion_control_bunds vetiver plating_pits Terraces Water_harvest_bunds agro_forestry
	
	g saps=.
	foreach i in $saps {
		replace saps=1 if `i'==1
	}

	g fisp_saps=1 if input_subsidy==1 | saps==1
	
	recode saps fisp_saps (.=0) 
	
	global `IHSnumber'categorical gender credit extension type1 type2 type3 quality1 quality2 quality3 input_subsidy drought floods irregularrains box_ridges minimum_tillage erosion_control_bunds vetiver plating_pits traditional_tilage Terraces Water_harvest_bunds agro_forestry saps
	
		if "`IHSnumber'"=="$ihsivlock" {
				global `IHSnumber'categorical gender credit extension type2 type3 type4 quality2 quality3 quality4 input_subsidy drought floods irregularrains box_ridges minimum_tillage erosion_control_bunds vetiver plating_pits traditional_tilage Terraces Water_harvest_bunds agro_forestry saps
		
		}
	

		
	foreach var of varlist productivity  HDDS FCS  {
		g l_`var'=ln(`var')
	}
	
	g 		combined=2 	if 		input_subsidy==1
	replace combined=3 	if 		saps ==1
	replace combined=1 	if 		saps ==1 			& 	input_subsidy==1
	recode 	combined 	(.=0)
	
	
	g 		combined_fisp_saps=4 if combined==0 

replace combined_fisp_saps=1 if combined==2
replace combined_fisp_saps=2 if combined==3
replace combined_fisp_saps=3 if combined==1




	global `IHSnumber'continous age educ hh_Size Plot_Area output af_bio_1_x af_bio_12_x 
	
		foreach dependent in productivity FCS HDDS  {
		foreach class in  1 2 3 4 {
			g		`dependent'`class'=`dependent' 		if `class'==combined_fisp_saps
			replace `dependent'`class'=. 				if `class'!=combined_fisp_saps
			g		l_`dependent'`class'=l_`dependent' 	if `class'==combined_fisp_saps
			replace l_`dependent'`class'=. 				if `class'!=combined_fisp_saps
		}
	}

	
	
	save "${workingfiles}/`IHSnumber'Proposal_Data.dta",replace
	
	}
	
	
*The analysis was only for IHSV given following instructions from reviewers	
	
	*Svy Set 
	svyset 	case_id 	[pweight= hh_wgt ], 	strata( ea_id ) 	singleunit(centered)
	

	do "${Do}/Var lebels.do" 	

**#PART 3: Descriptives

	*Pooled
	foreach i in continous {
			table  ( var) (), statistic(mean $`i') statistic(sd $`i')  nformat(%9.2fc mean sd )  sformat("(%s)" sd)  
			collect preview
			collect style header result, level(hide)
			collect style showbase off
			collect style putdocx, layout(autofitcontents)
			collect export pooled`i'.docx, as(docx) replace
	}


	
	foreach i in categorical  {
			table  ( var) (), statistic(fvfrequency $`i') statistic(fvpercent $`i')  nformat(%9.0fc fvfrequency  ) ///
			nformat(%9.2fc fvpercent )sformat("(%s)" sd)   sformat("%s%%" fvpercent)
			collect preview
			collect style showbase off
			collect style header result, level(hide)
			collect style putdocx, layout(autofitcontents)
			collect export pooled`i'.docx, as(docx) replace
	}

	*Foreach regime
	
		foreach category in  4 1 2 3 {
		
			foreach i in continous {
					table  ( var) () if  combined_fisp_saps==`category', statistic(mean $`i') statistic(sd $`i')  nformat(%9.2fc mean sd )  sformat("(%s)" sd)   
					collect preview
					collect style header result, level(hide)
					collect style showbase off
					collect style putdocx, layout(autofitcontents)
					collect export `category'_continous.docx, as(docx) replace
			}


		foreach i in categorical  {
					table  ( var) () if  combined_fisp_saps==`category', statistic(fvfrequency $`i') statistic(fvpercent $`i')  nformat(%9.0fc fvfrequency  ) ///
			nformat(%9.2fc fvpercent )sformat("(%s)" sd)   sformat("%s%%" fvpercent)
					collect preview
					collect style showbase off
					collect style header result, level(hide)
					collect style showbase off
					collect preview
					collect style putdocx, layout(autofitcontents)
					collect export `category'_categorical.docx, as(docx) replace
			}
	 	}
	
	
	
**#PART 4: Objective 1 Models : Multinomial Probit Models 
	collect 	clear
		
	collect clear 
	*Collection of the _r_b _r_se _r_z
	collect get _r_b _r_se _r_z _r_p, tag(model[(all)]): mprobit combined_fisp_saps  $socio FGT0 $insitutional $farmlevel  $weather IV_fisp IV_saps , baseoutcome(4)
	etable,  stars(.05 "*" .01 "**", attach(_r_b))
	collect style cell result[_r_se], nformat(%6.3f) 	halign(center)
	collect style cell result[_r_b ], nformat(%10.4f) 	halign(center)
	collect style cell result[_r_z ], nformat(%5.2f) 	halign(center)
	collect stars 	_r_p 0.01 "***" 0.05 "**" 0.1 "*" 1  " ", attach(_r_b) 
	collect composite  define meansd =  _r_b _r_se
	collect layout   (colname[]) (coleq#etable_depvar#stars[value]#result[meansd _r_z])
	collect style      showbase  off
	collect style 	   cell 	border_block, 	border(right, pattern(nil))  
	collect levelsof 	cell_type
	collect style		cell 	cell_type[item column-header], halign(center)
	collect style 		header 	result, level(hide)
	collect style 		row 	stack, spacer delimiter(" x ")
	collect style 		putdocx, layout(autofitcontents)
	collect export 		Multinomialprobitfirst.docx, as(docx) replace

	
		collect clear 
	*Collection of the _r_b _r_se _r_z
	collect get _r_b _r_se _r_z _r_p, tag(model[(all)]): mprobit combined_fisp_saps  $socio FGT0 $insitutional $farmlevel  $weather IV_fisp IV_saps, baseoutcome(4)
	etable
	etable, replay showstars showstarsnote  stars(.05 "*" .01 "**" .001 "***", attach(_r_b))
	collect export 		Multinomialprobitfirststars.docx, as(docx) replace
	
	
	collect clear
	foreach 	dependent	in	1 2 3 {
		mprobit combined_fisp_saps  $socio FGT0 $insitutional $farmlevel  $weather IV_fisp IV_saps, baseoutcome(4)
		collect get _r_b _r_p , tag(model[(`dependent')]):  margins, predict(pr outcome(`dependent')) dydx(*) force post
	}

		*Exporting 	margins into word document
		collect style cell result[_r_b ], nformat(%10.4f) halign(center)
		collect style cell result[_r_se], sformat("(%s)")
		collect stars 	_r_p 0.01 "***" 0.05 "**" 0.1 "*" 1  " ", attach(_r_b) 
		collect layout   (colname) (cmdset#result[_r_b])
		collect style      showbase  off
		collect style 	   cell 	border_block, 	border(right, pattern(nil))  
		collect levelsof 	cell_type
		collect style		cell 	cell_type[item column-header], halign(center)
		collect style 		header 	result, level(hide)
		collect style 		row 	stack, spacer delimiter(" x ")
		collect style 		putdocx, layout(autofitcontents)
		collect export 		Multinomialdydx.docx, as(docx) replace
		
	
	
	
**#PART 5: Robust Checks : bivariate probit regression		
		est clear
		biprobit (fisp $socio FGT0 $insitutional $farmlevel  $weather IV_fisp IV_saps) (saps $socio FGT0 $insitutional $farmlevel  $weather IV_fisp IV_saps),  vce(robust)
		est sto biprobit
		
		esttab biprobit using Recursivefinal.rtf,   stats(chi2 p , labels("Wald chi" "Prob > chi2" ))  star(* 0.10 ** 0.05 *** 0.01) unstack r b(3) se(2)
		
		
*/	

**#PART 5: Objective 2 :  Impact Evaluation through MESR Models
		
	*A suser written selmlog ado file
	local runningscript_firsttime "No" //ARE YOU RUNNING THIS SCRIPT FOR THE FIRST TIME (After reopening Stata?)  Yes/No : Input the answer in the local below
	
	if  "`runningscript_firsttime'"=="Yes" {
		
		do "${Do}/selmlogdo.do" 	
		
	}
  

		svyset 	case_id 	[pweight= hh_wgt ], 	strata( ea_id ) 	singleunit(centered)
		
		collect clear
		
	*Productivity Model
		
		foreach 	number in 4 1 2 3    { //a loop for efficiency
			
			local IV_4			 IV_fisp_saps
			local IV_3			 IV_fisp_saps
			local IV_1			 IV_fisp
			local IV_2			 IV_saps
			
		*Selmlog estimation	
		*The if are applying due to changes in the IV
		
			collect _r_b _r_se, tag(model[(`number')]): selmlog productivity`number' $eq , select(combined_fisp_saps = $selection `IV_`number'') boot(300) dmf(0) gen(m`number')	
	
			
			// Overall significance and Number of observetions 
			test 		$eq
			local		chi2				`r(chi2)'
			collect 	chi2 = r(value),	tag(model[(`number')]):  echo	`chi2'
			
			test 		$eq
			local		p				 	`r(p)'
			collect 	p= r(value),		tag(model[(`number')]):  echo	`p'
			
			count 		if 					combined_fisp_saps==`number'
			local 		N			 		`r(N)'
			collect 	N= r(value),		tag(model[(`number')]):  echo	`N'
		
		
			//predicitons for average estimations
			if `number'==4 {
					rename 		m`number'3 		_m3
					rename 		m`number'1 		_m1
					rename 		m`number'2 		_m2
					predict		y_bar_p_`number', xb
					
					ge Emz_p_`number'= 	( y_bar_p_`number' )
					ge Emz_p_`number'_3= 	Emz_p_`number' 	if 	combined_fisp_saps  ==3
					ge Emz_p_`number'_2= 	Emz_p_`number' 	if 	combined_fisp_saps  ==2
					ge Emz_p_`number'_1= 	Emz_p_`number'	if 	combined_fisp_saps  ==1
					ge Emz_p_`number'_4=	Emz_p_`number'  if 	combined_fisp_saps  ==4
			
			}
			
			if `number'==3 {
					rename 		m`number'4 		_m4
					rename 		m`number'1 		_m1
					rename 		m`number'2 		_m2
					predict		y_bar_p_`number', xb
					ge Emz_p_`number'= 	( y_bar_p_`number' )
					ge Emz_p_`number'_3= 	Emz_p_`number' 	if 	combined_fisp_saps  ==3
					ge Emz_p_`number'_2= 	Emz_p_`number' 	if 	combined_fisp_saps  ==2
					ge Emz_p_`number'_1= 	Emz_p_`number'	if 	combined_fisp_saps  ==1
					ge Emz_p_`number'_4=	Emz_p_`number'  if 	combined_fisp_saps  ==4
			}
			
			if `number'==1 {
					rename 		m`number'4  	_m4
					rename 		m`number'3 		_m3
					rename 		m`number'2		_m2
					predict		y_bar_p_`number', xb
					ge Emz_p_`number'= 	( y_bar_p_`number' )
					ge Emz_p_`number'_3= 	Emz_p_`number' 	if 	combined_fisp_saps  ==3
					ge Emz_p_`number'_2= 	Emz_p_`number' 	if 	combined_fisp_saps  ==2
					ge Emz_p_`number'_1= 	Emz_p_`number'	if 	combined_fisp_saps  ==1
					ge Emz_p_`number'_4=	Emz_p_`number'  if 	combined_fisp_saps  ==4
			}
			
			if `number'==2 {
					rename 		m`number'4  	_m4
					rename 		m`number'3 		_m3
					rename 		m`number'1 		_m1
					predict		y_bar_p_`number', xb
					ge Emz_p_`number'= 	( y_bar_p_`number' )
					ge Emz_p_`number'_3= 	Emz_p_`number' 	if 	combined_fisp_saps  ==3
					ge Emz_p_`number'_2= 	Emz_p_`number' 	if 	combined_fisp_saps  ==2
					ge Emz_p_`number'_1= 	Emz_p_`number'	if 	combined_fisp_saps  ==1
					ge Emz_p_`number'_4=	Emz_p_`number'  if 	combined_fisp_saps  ==4
			}
			
		}
		
		
		
		collect style cell result[_r_se], nformat(%4.3f) 		halign(center)
		collect style cell result[_r_b ], nformat(%10.4f) 		halign(center)
		collect style cell result[_r_z ], nformat(%5.2f) 		halign(center)
		collect style cell result[_r_se], sformat("(%s)") 		halign(center)
		collect stars _r_p 0.01 "***" 0.05 "**" 0.1 "*" 1  " ", attach(_r_b) 
		collect composite  define meansd =  _r_b _r_se		
		collect style showbase off

		*Removing vertor line
		collect style cell border_block, border(right, pattern(nil))  
		collect levelsof cell_type
		collect style cell cell_type[item column-header], halign(center)
		collect style header result, level(hide)
		collect style row stack, spacer delimiter(" x ")
		collect stars _r_p  0.01 "***" 0.05 "** " 0.1 "* " 1 " ", attach(_r_b) 
		collect layout   (colname) (model#result[meansd _r_z])
		collect style cell result[chi2 p N], nformat(%8.2f)
		collect style header result[chi2 p N], level(label)
		collect style putdocx, layout(autofitcontents)
		collect export productivity.docx, as(docx) replace


		
		
		collect clear
*FCS Models
		foreach 	number in 4 1 2 3   { //a loop for efficiency
			
				
			local IV_4			 IV_fisp_saps
			local IV_3			 IV_fisp_saps
			local IV_1			 IV_fisp
			local IV_2			 IV_saps
			
		*Selmlog estimation	
		*The if are applying due to changes in the IV
		
				 collect _r_b _r_se, tag(model[(`number')]): selmlog FCS`number' $eq, select(combined_fisp_saps = $selection `IV_`number'') boot(300) dmf(0) gen(m`number')	
	
			
			// Overall significance and Number of observetions 
				// Overall significance and Number of observetions 
			test 		$eq
			local		chi2				`r(chi2)'
			collect 	chi2 = r(value),	tag(model[(`number')]):  echo	`chi2'
			
			test 		$eq
			local		p				 	`r(p)'
			collect 	p= r(value),		tag(model[(`number')]):  echo	`p'
			
			count 		if 					combined_fisp_saps==`number'
			local 		N			 		`r(N)'
			collect 	N= r(value),		tag(model[(`number')]):  echo	`N'
		
			//predicitons for average estimations
			if `number'==4 {
					rename 		m`number'3 		_m3
					rename 		m`number'1 		_m1
					rename 		m`number'2 		_m2
					predict		y_bar_f_`number', xb
					
					ge Emz_f_`number'= 	( y_bar_f_`number' )
					ge Emz_f_`number'_3= 	Emz_f_`number' 	if 	combined_fisp_saps  ==3
					ge Emz_f_`number'_2= 	Emz_f_`number' 	if 	combined_fisp_saps  ==2
					ge Emz_f_`number'_1= 	Emz_f_`number'	if 	combined_fisp_saps  ==1
					ge Emz_f_`number'_4=	Emz_f_`number'  if 	combined_fisp_saps  ==4
			}

			if `number'==3 {
					rename 		m`number'4 		_m4
					rename 		m`number'1 		_m1
					rename 		m`number'2 		_m2
					predict		y_bar_f_`number', xb
					ge Emz_f_`number'= 	( y_bar_f_`number' )
					ge Emz_f_`number'_3= 	Emz_f_`number' 	if 	combined_fisp_saps  ==3
					ge Emz_f_`number'_2= 	Emz_f_`number' 	if 	combined_fisp_saps  ==2
					ge Emz_f_`number'_1= 	Emz_f_`number'	if 	combined_fisp_saps  ==1
					ge Emz_f_`number'_4=	Emz_f_`number'  if 	combined_fisp_saps  ==4
			}
			
			if `number'==1 {
					rename 		m`number'4  	_m4
					rename 		m`number'3 		_m3
					rename 		m`number'2		_m2
					predict		y_bar_f_`number', xb
					ge Emz_f_`number'= 	( y_bar_f_`number' )
					ge Emz_f_`number'_3= 	Emz_f_`number' 	if 	combined_fisp_saps  ==3
					ge Emz_f_`number'_2= 	Emz_f_`number' 	if 	combined_fisp_saps  ==2
					ge Emz_f_`number'_1= 	Emz_f_`number'	if 	combined_fisp_saps  ==1
					ge Emz_f_`number'_4=	Emz_f_`number'  if 	combined_fisp_saps  ==4
			}
			
			if `number'==2 {
					rename 		m`number'4  	_m4
					rename 		m`number'3 		_m3
					rename 		m`number'1 		_m1
					predict		y_bar_f_`number', xb
					ge Emz_f_`number'= 	( y_bar_f_`number' )
					ge Emz_f_`number'_3= 	Emz_f_`number' 	if 	combined_fisp_saps  ==3
					ge Emz_f_`number'_2= 	Emz_f_`number' 	if 	combined_fisp_saps  ==2
					ge Emz_f_`number'_1= 	Emz_f_`number'	if 	combined_fisp_saps  ==1
					ge Emz_f_`number'_4=	Emz_f_`number'  if 	combined_fisp_saps  ==4
			}
			
		}
		
	
		collect style cell result[_r_se], nformat(%4.3f) 		halign(center)
		collect style cell result[_r_b ], nformat(%10.4f) 		halign(center)
		collect style cell result[_r_z ], nformat(%5.2f) 		halign(center)
		collect style cell result[_r_se], sformat("(%s)") 		halign(center)
		collect stars _r_p 0.01 "***" 0.05 "**" 0.1 "*" 1  " ", attach(_r_b) 
		collect composite  define meansd =  _r_b _r_se		
		collect style showbase off

		*Removing vertor line
		collect style cell border_block, border(right, pattern(nil))  
		collect levelsof cell_type
		collect style cell cell_type[item column-header], halign(center)
		collect style header result, level(hide)
		collect style row stack, spacer delimiter(" x ")
		collect stars _r_p  0.01 "***" 0.05 "** " 0.1 "* " 1 " ", attach(_r_b) 
		collect layout   (colname) (model#result[meansd _r_z])
		collect style cell result[chi2 p N], nformat(%8.2f)
		collect style header result[chi2 p N], level(label)
		collect style putdocx, layout(autofitcontents)
		collect export FCS.docx, as(docx) replace




		collect clear
*HDDS Models
		foreach 	number in 4 1 2 3   { //a loop for efficiency
			
				
		    local IV_4			 IV_fisp_saps
			local IV_3			 IV_fisp_saps
			local IV_1			 IV_fisp
			local IV_2			 IV_saps
			
		*Selmlog estimation	
		*The if are applying due to changes in the IV
		
				 collect _r_b _r_se, tag(model[(`number')]): selmlog HDDS`number' $eq, select(combined_fisp_saps = $selection `IV_`number'') dmf(0) gen(m`number')	
	
			
			// Overall significance and Number of observetions 
	// Overall significance and Number of observetions 
			test 		$eq
			local		chi2				`r(chi2)'
			collect 	chi2 = r(value),	tag(model[(`number')]):  echo	`chi2'
			
			test 		$eq
			local		p				 	`r(p)'
			collect 	p= r(value),		tag(model[(`number')]):  echo	`p'
			
			count 		if 					combined_fisp_saps==`number'
			local 		N			 		`r(N)'
			collect 	N= r(value),		tag(model[(`number')]):  echo	`N'
	
		
			//predicitons for average treatment estimations
			if `number'==4 {
					rename 		m`number'3 		_m3
					rename 		m`number'1 		_m1
					rename 		m`number'2 		_m2
					predict		y_bar_h_`number', xb
					
					ge Emz_h_`number'= 	( y_bar_h_`number' )
					ge Emz_h_`number'_3= 	Emz_h_`number' 	if 	combined_fisp_saps  ==3
					ge Emz_h_`number'_2= 	Emz_h_`number' 	if 	combined_fisp_saps  ==2
					ge Emz_h_`number'_1= 	Emz_h_`number'	if 	combined_fisp_saps  ==1
					ge Emz_h_`number'_4=	Emz_h_`number'  if 	combined_fisp_saps  ==4
			}

			if `number'==3 {
					rename 		m`number'4 		_m4
					rename 		m`number'1 		_m1
					rename 		m`number'2 		_m2
					predict		y_bar_h_`number', xb
					ge Emz_h_`number'= 	( y_bar_h_`number' )
					ge Emz_h_`number'_3= 	Emz_h_`number' 	if 	combined_fisp_saps  ==3
					ge Emz_h_`number'_2= 	Emz_h_`number' 	if 	combined_fisp_saps  ==2
					ge Emz_h_`number'_1= 	Emz_h_`number'	if 	combined_fisp_saps  ==1
					ge Emz_h_`number'_4=	Emz_h_`number'  if 	combined_fisp_saps  ==4
			}
			
			if `number'==1 {
					rename 		m`number'4  	_m4
					rename 		m`number'3 		_m3
					rename 		m`number'2		_m2
					predict		y_bar_h_`number', xb
					ge Emz_h_`number'= 	( y_bar_h_`number' )
					ge Emz_h_`number'_3= 	Emz_h_`number' 	if 	combined_fisp_saps  ==3
					ge Emz_h_`number'_2= 	Emz_h_`number' 	if 	combined_fisp_saps  ==2
					ge Emz_h_`number'_1= 	Emz_h_`number'	if 	combined_fisp_saps  ==1
					ge Emz_h_`number'_4=	Emz_h_`number'  if 	combined_fisp_saps  ==4
			}
			
			if `number'==2 {
					rename 		m`number'4  	_m4
					rename 		m`number'3 		_m3
					rename 		m`number'1 		_m1
					predict		y_bar_h_`number', xb
					ge Emz_h_`number'= 	( y_bar_h_`number' )
					ge Emz_h_`number'_3= 	Emz_h_`number' 	if 	combined_fisp_saps  ==3
					ge Emz_h_`number'_2= 	Emz_h_`number' 	if 	combined_fisp_saps  ==2
					ge Emz_h_`number'_1= 	Emz_h_`number'	if 	combined_fisp_saps  ==1
					ge Emz_h_`number'_4=	Emz_h_`number'  if 	combined_fisp_saps  ==4
			}
			
		}
		
	
		
			collect style cell result[_r_se], nformat(%4.3f) 		halign(center)
		collect style cell result[_r_b ], nformat(%10.4f) 		halign(center)
		collect style cell result[_r_z ], nformat(%5.2f) 		halign(center)
		collect style cell result[_r_se], sformat("(%s)") 		halign(center)
		collect stars _r_p 0.01 "***" 0.05 "**" 0.1 "*" 1  " ", attach(_r_b) 
		collect composite  define meansd =  _r_b _r_se		
		collect style showbase off

		*Removing vertor line
		collect style cell border_block, border(right, pattern(nil))  
		collect levelsof cell_type
		collect style cell cell_type[item column-header], halign(center)
		collect style header result, level(hide)
		collect style row stack, spacer delimiter(" x ")
		collect stars _r_p  0.01 "***" 0.05 "** " 0.1 "* " 1 " ", attach(_r_b) 
		collect layout   (colname) (model#result[meansd _r_z])
		collect style cell result[chi2 p N], nformat(%8.2f)
		collect style header result[chi2 p N], level(label)
		collect style putdocx, layout(autofitcontents)
		collect export HDDS1.docx, as(docx) replace

		
**#PART 7: Treatment Effects
		
		foreach outcome in p f h {
			
		local command  "command( Use= r(mu_1)  Non_Use= r(mu_2) (Difference =r(mu_1)-r(mu_2)) pvalue = r(p)"
		table (command) (result), `command'   : ttest Emz_`outcome'_1_1==Emz_`outcome'_4_1 ) `command'   : ttest Emz_`outcome'_1==Emz_`outcome'_4 ) `command'   : ttest Emz_`outcome'_2_2==Emz_`outcome'_4_2 ) ///
		`command'   : ttest Emz_`outcome'_2==Emz_`outcome'_4 )  `command'   : ttest Emz_`outcome'_3_3==Emz_`outcome'_4_3 ) `command'   : ttest Emz_`outcome'_3==Emz_`outcome'_4 )
		
		collect style cell result[ Non_Use Use Difference ], nformat(%10.2f)
		collect stars pvalue 0.01 "***" 0.05 "** " 0.1 "* " 1 " ", attach(Difference) 
		collect style putdocx, layout(autofitcontents)
		collect export `outcome'_Effects.xlsx, as(xlsx) replace
		
		}

