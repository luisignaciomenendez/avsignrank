


/*        __^__                                      __^__
         ( ___ )------------------------------------( ___ )
          | / |                                      | \ |
          | / |          BEGINING PROGRAM            | \ |
          |___|                                      |___|
         (_____)------------------------------------(_____) 


		 
*inputs: 1 T, 2 B, 3 B (Bpair) 4 levels 5 types 6 condns 7 vars 8 filename
*input1: Treatment
*input2: Block1
*input3: Block2
*input4: levels
*input5: types012
*input6: condns
*input7: vars
*input8: file name to export the stata table	 
		 
		 
*/



**#
qui{
*** signranktest program---------------------------------------------------------
capture program drop avsignrank
program define avsignrank

args T b1 b2 levels types012 conds vars filename

qui{
if `5' ==0     local lab5 "L" // mu is same as NC for "No Condition other than mu" 
if `5' ==1     local lab5 "H" 
if `5' ==2     local lab5 "Both" 


//prior/ 
if "`6'"!="mu" & "`6'"!="nmu"  local lab6 `6' 
if "`6'"=="muH"|"`6'"=="muL"    local lab6 ""   //cd omit line i think

}


local fixedstatement "T==`1' & `6' ==1 & dblock==`2'"

log on 

if "`4'"=="id"   local appro_ex  "Exact(id)"
if "`4'"=="mg"   local appro_ex  "Exact"




di  as text "****** T(`1')-n(`2'v`3')-`4'- `appro_ex' Signed ranks ***********************************"
// n or B here; active n only except n0 means B0 and is 4 or 2
log off
foreach v7 in `7' {

local `var' strlower(`v7')
*local ut : lower "`v7'"	
if "`lab6'"=="" {
	
	
	local text "`ut'`lab5'"   
}
else if substr("`lab6'",1,3)=="muH" | substr("`lab6'",1,3)=="muL" {
    local temp=  substr("`lab6'",5,.) 
	local text "`ut'`lab5'\_`temp'"
}
else {
    local text "`ut'`lab5'\_`lab6'" 
}   	
	
	


log on
qui{ 	
 bysort T `4' `6': egen `v7'_B`2'for_vs`3' = mean(`v7') if dblock==`2'  
     bysort T `4' `6': egen tt = max(`v7'_B`2'for_vs`3') 
	 replace `v7'_B`2'for_vs`3' = tt if `v7'_B`2'for_vs`3'==.
	 drop tt
 bysort T `4' `6': egen `v7'_B`3'for_vs`2' = mean(`v7') if dblock==`3'
     bysort T `4' `6': egen tt = max(`v7'_B`3'for_vs`2')
     replace `v7'_B`3'for_vs`2' = tt if `v7'_B`3'for_vs`2'==.
	 drop tt
     } //qui-end 
qui signrank `v7'_B`2'for_vs`3'=`v7'_B`3'for_vs`2' if  `fixedstatement' 


 
 local n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rp_ex= r(p_exact) 
 local n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rp_l_ex= r(p_l_exact)
 local n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rp_u_ex= r(p_u_exact)
 local n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rN= r(N)
 local n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rNpos= r(N_pos)
 *local rz   = round(r(z),.01)
 local rz : di %3.2f r(z)
	sum `v7' if T==`1'  & `6'==1 & dblock ==`2', meanonly
	gen av_`v7'`lab5'`lab6'_T`1'B`2'for_vs`3' = r(mean)
	* report the total ut divided by 5 maybe??
	
	sum `v7' if T==`1'  & `6'==1 & dblock ==`3', meanonly
	local lower : di %3.2f r(mean)
	* before local lower= r(mean)...
	gen av_`v7'`lab5'`lab6'_T`1'B`3'for_vs`2' = r(mean)
	sum av_`v7'`lab5'`lab6'_T`1'B`3'for_vs`2' , meanonly
	local upper  : di %3.2f r(mean)

if av_`v7'`lab5'`lab6'_T`1'B`2'for_vs`3' > av_`v7'`lab5'`lab6'_T`1'B`3'for_vs`2'  ///
						local directionT`1'B`2'for_vs`3' "higher" 
if av_`v7'`lab5'`lab6'_T`1'B`2'for_vs`3' ==   av_`v7'`lab5'`lab6'_T`1'B`3'for_vs`2' /// 
						local directionT`1'B`2'for_vs`3' "EQ avs" 
if av_`v7'`lab5'`lab6'_T`1'B`2'for_vs`3'<av_`v7'`lab5'`lab6'_T`1'B`3'for_vs`2' ///
						local directionT`1'B`2'for_vs`3' "lower "
if `n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rp_ex' <= 0.1  local  significanceis "**"   // at 10% 2-sided)"


if `n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rp_ex' <= 0.05  local  significanceis "***"   // at 5% 2-sided)"

if `n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rp_ex' > 0.2   local significanceis "" //not even @20%2s)"
if `n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rp_ex' < 0.2 & `n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rp_ex' > 0.1 ///
local significanceis "*"

										
di _continue as text %6.3f "`text': ", _col(14) as result "`directionT`1'B`2'for_vs`3''", 
di _cont _col(22) as text " `significanceis' " 
di _cont _col(33)" p2si:"        as result %6.3f `n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rp_ex', 
di _cont as result _col(44) " (" ///
%4.3f av_`v7'`lab5'`lab6'_T`1'B`2'for_vs`3', %6.3f av_`v7'`lab5'`lab6'_T`1'B`3'for_vs`2' ")",
di _cont _col(63) as text "1-si-lower:"   as result %6.3f `n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rp_l_ex',  
di _cont as text "1-si-upper:"   as result %6.3f `n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rp_u_ex',   
di _cont as text "  N:"            as result `n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rN',   
di _cont as text "Npos:"         as result `n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rNpos',   
di _continue as text _col(115)  "z-stat:", `rz', 	

* Display warning: whenever mg N=6


if "`4'"=="mg" & `n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rN'!=6 {
        display "Warning: N must be equal to 6 for mg."
    }



log off	 

*log close


local OutputFileName "`filename'.txt"

    // Open the output file for writing
    file open myfile using "`OutputFileName'", write text replace
  
 
		
		
		
  file write myfile "\makecell{$ `text' $ \\ T(`1')\\ Dblocks:  `2'v`3' }  & `n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rp_ex' & ( `n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rp_l_ex', `n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rp_u_ex' )`significanceis' & `n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rNpos'   & `rz' \\ & {(`lower',`upper')} & & &  \\" _n
  
  
  /*
	file write myfile "$ `text', T(`1')-n(`2'v`3')-`4'- `appro_ex'  and Dblock:  `2'  $ & `n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rp_ex' & ( `n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rp_l_ex', `n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rp_u_ex' ) `significanceis' & `n`2'T`1'vsn`3'T`1'_`v7'`lab5'`lab6'_rNpos'   & `rz' \\
   & {(`lower',`upper')} & & &  \\" _n

*/


    // Close the output file
    file close myfile






}
end
} // end-qui 


