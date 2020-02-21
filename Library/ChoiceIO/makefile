helpers = ../HelperFunctions.ml ../Context.ml ../V.ml
chc = ../Choices/ChcHelperFunctions.ml ../Choices/Chc.ml
eff = ~/research/eff_working/eff $(helpers) 
ifd =  ../StateCtxList.ml ../StateQueue.ml ../Ifdef.ml
opt_read_file = ../Queue/Opt.ml ../Queue/OptQueue.ml ../Parser/ReadHelperFunctions.ml ../Parser/OptReadFile.ml



ChcWrite: 
	$(eff) $(chc)  $(ifd) ChcWrite.ml


ChcWriteEx: 
	$(eff) $(chc)  $(ifd) ChcWrite.ml ChcWriteEx.ml

ChcRead: 
	$(eff) $(opt_read_file) $(ifd) $(chc) ChcRead.ml

ReadFile: 
	$(eff) $(opt_read_file) $(ifd) $(chc) ReadFile.ml

ChcReadEx: 
	$(eff) $(opt_read_file) $(ifd) $(chc) ChcRead.ml ChcReadEx.ml
