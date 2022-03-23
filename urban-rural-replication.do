clear
use "urban-rural-replication.dta"
set matsize 11000

** Ordered Logit (Table 2 in appendix)
version 14.2: ologit pid5 black hispanic incomeLt20 income20t30 income30t50 income50t75 income100t150 incomeMt150 senior under30 hsOrLess baDegree postGrad relAttOnceAWeek relAttAlmostEveryWeek relAttOnceAMonth relAttNever religionCatholic religionJewish religionNone religionProtestant married male cityDistanceLogged  PopDens101000s year2-year16 statedummy2-statedummy51, cluster(zipcodeRecode)
outreg2 using "ologitTable" , tex(frag) label keep (pid5 black hispanic incomeLt20 income20t30 income30t50 income50t75 income100t150 incomeMt150 senior under30 hsOrLess baDegree postGrad relAttOnceAWeek relAttAlmostEveryWeek relAttOnceAMonth relAttNever religionCatholic religionJewish religionNone religionProtestant married male cityDistanceLogged  PopDens101000s) dec(3) ctitle(" ")  replace   
** Generate predicted probabilities for Figure 9 in paper
prgen PopDens101000s, from(0) to(26.77164) generate(densOP) ncases(10) ci noisily
export delimited densOP* using "OLdenSims", replace
prgen cityDistanceLogged, from(0) to(6.570883) generate(cityOP) ncases(10) ci noisily
export delimited cityOP* using "OLcitySims", replace

** M Logit (Table 3 in appendix)
mlogit pid5 black hispanic incomeLt20 income20t30 income30t50 income50t75 income100t150 incomeMt150 senior under30 hsOrLess baDegree postGrad relAttOnceAWeek relAttAlmostEveryWeek relAttOnceAMonth relAttNever religionCatholic religionJewish religionNone religionProtestant married male cityDistanceLogged PopDens101000s year2-year16 statedummy2-statedummy51, cluster(zipcodeRecode)
outreg2 using "mlogitTable", tex(frag) label keep (pid5 black hispanic incomeLt20 income20t30 income30t50 income50t75 income100t150 incomeMt150 senior under30 hsOrLess baDegree postGrad relAttOnceAWeek relAttAlmostEveryWeek relAttOnceAMonth relAttNever religionCatholic religionJewish religionNone religionProtestant married male cityDistanceLogged  PopDens101000s) dec(3) ctitle(" ")  replace     

** Linear Model (Table 4 in appendix)
reg pid5 black hispanic incomeLt20 income20t30 income30t50 income50t75 income100t150 incomeMt150 senior under30 hsOrLess baDegree postGrad relAttOnceAWeek relAttAlmostEveryWeek relAttOnceAMonth relAttNever religionCatholic religionJewish religionNone religionProtestant married male cityDistanceLogged  PopDens101000s i.State year2-year16, cluster(zipcodeRecode)
outreg2 using "olsTable" , tex(frag) label keep (pid5 black hispanic incomeLt20 income20t30 income30t50 income50t75 income100t150 incomeMt150 senior under30 hsOrLess baDegree postGrad relAttOnceAWeek relAttAlmostEveryWeek relAttOnceAMonth relAttNever religionCatholic religionJewish religionNone religionProtestant married male cityDistanceLogged  PopDens101000s) dec(3) ctitle(" ")  replace   




