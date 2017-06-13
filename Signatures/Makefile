

signatures : step2
	Rscript bin/processSigs.R Models/*.signature

step2 : lists/GTEX.acute_myeloid_leukemia lists/GTEX.glioblastoma_multiforme lists/TCGA.acute_myeloid_leukemia lists/TCGA.glioblastoma_multiforme GTEX.colon adenocarcinoma lists/GTEX.lung_adenocarcinoma lists/TCGA.colon_adenocarcinoma lists/TCGA.lung_adenocarcinoma 
	bash  bin/buildSignatures.sh
	touch step2

<<<<<<< HEAD
# lists/GTEX.acute_myeloid_leukemia lists/GTEX.glioblastoma_multiforme lists/TCGA.acute_myeloid_leukemia lists/TCGA.glioblastoma_multiforme GTEX.colon adenocarcinoma lists/GTEX.lung_adenocarcinoma lists/TCGA.colon_adenocarcinoma lists/TCGA.lung_adenocarcinoma : MapControlToDiseaseTissue
step1 :ReferenceData
	python bin/ParseSamples.py
	touch step1

ReferenceData: ReferenceData/NETWORK ReferenceData/GTEX.RSEM ReferenceData/GTEX.phenotype ReferenceData/NETWORK ReferenceData/TCGA.RSEM ReferenceData/TCGA.phenotype
	(cd ReferenceData; make)
=======
lists/GTEX.acute_myeloid_leukemia lists/GTEX.glioblastoma_multiforme lists/TCGA.acute_myeloid_leukemia lists/TCGA.glioblastoma_multiforme GTEX.colon adenocarcinoma lists/GTEX.lung_adenocarcinoma lists/TCGA.colon_adenocarcinoma lists/TCGA.lung_adenocarcinoma : MapControlToDiseaseTissue
	python2 bin/ParseSamples.py
>>>>>>> b06070954f5bb56f56dbbfbc0d65ce32de5fa2c6
