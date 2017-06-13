

signatures : step4
	Rscript bin/processSigs.R Models/*.signature

step4 : step3 lists/GTEX.acute_myeloid_leukemia lists/GTEX.glioblastoma_multiforme lists/TCGA.acute_myeloid_leukemia lists/TCGA.glioblastoma_multiforme GTEX.colon adenocarcinoma lists/GTEX.lung_adenocarcinoma lists/TCGA.colon_adenocarcinoma lists/TCGA.lung_adenocarcinoma 
	bash  bin/buildSignatures.sh
	touch step4

step3 : step2 lists/GTEX.acute_myeloid_leukemia lists/GTEX.glioblastoma_multiforme lists/TCGA.acute_myeloid_leukemia lists/TCGA.glioblastoma_multiforme GTEX.colon adenocarcinoma lists/GTEX.lung_adenocarcinoma lists/TCGA.colon_adenocarcinoma lists/TCGA.lung_adenocarcinoma  MapControlToDiseaseTissue
	python bin/ParseSamples.py
	touch step3

step2 lists/GTEX.acute_myeloid_leukemia lists/GTEX.glioblastoma_multiforme lists/TCGA.acute_myeloid_leukemia lists/TCGA.glioblastoma_multiforme GTEX.colon adenocarcinoma lists/GTEX.lung_adenocarcinoma lists/TCGA.colon_adenocarcinoma lists/TCGA.lung_adenocarcinoma : step1  MapControlToDiseaseTissue ReferenceData/NETWORK ReferenceData/GTEX.RSEM ReferenceData/GTEX.phenotype ReferenceData/NETWORK ReferenceData/TCGA.RSEM ReferenceData/TCGA.phenotype
	python2 bin/ParseSamples.py
	touch step2

step1 ReferenceData/NETWORK ReferenceData/GTEX.RSEM ReferenceData/GTEX.phenotype ReferenceData/TCGA.RSEM ReferenceData/TCGA.phenotype :
	(cd ReferenceData; make)
	touch step1
