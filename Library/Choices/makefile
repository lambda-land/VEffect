helpers = ../HelperFunctions.ml ../Context.ml ../V.ml
eff = ~/VEffect/eff_working/eff $(helpers) 

Chc: 
	$(eff) ChcHelperFunctions.ml  Chc.ml 

ChcCount: 
	$(eff) ../StateInt.ml ChcHelperFunctions.ml  Chc.ml ChcCount.ml

ChcExamples: 
	$(eff) ChcHelperFunctions.ml  ../StateInt.ml Chc.ml  ChcExamples.ml
