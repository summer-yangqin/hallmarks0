


signatures : lists/modellist 
	bash  bin/buildSignatures.sh
	Rscript bin/processSigs.R Models/*.signature

lists/modellist : step1 
	python bin/ParseSamples.py

step1 ReferenceData/NETWORK ReferenceData/GTEX.RSEM ReferenceData/GTEX.phenotype ReferenceData/TCGA.RSEM ReferenceData/TCGA.phenotype :
	(cd ReferenceData; make)
	touch step1
