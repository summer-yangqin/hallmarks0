signatures : step2
	Rscript bin/processSigs.R Models/*.signature

step2 : lists/GTEX.acute_myeloid_leukemia lists/GTEX.glioblastoma_multiforme lists/TCGA.acute_myeloid_leukemia lists/TCGA.glioblastoma_multiforme GTEX.colon adenocarcinoma lists/GTEX.lung_adenocarcinoma lists/TCGA.colon_adenocarcinoma lists/TCGA.lung_adenocarcinoma 
	bash  bin/buildSignatures.sh
	touch step2

lists/GTEX.acute_myeloid_leukemia lists/GTEX.glioblastoma_multiforme lists/TCGA.acute_myeloid_leukemia lists/TCGA.glioblastoma_multiforme GTEX.colon adenocarcinoma lists/GTEX.lung_adenocarcinoma lists/TCGA.colon_adenocarcinoma lists/TCGA.lung_adenocarcinoma : MapControlToDiseaseTissue
	python2 bin/ParseSamples.py
