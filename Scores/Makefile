
all: acute_myeloid_leukemia.scores  colon_adenocarcinoma.scores  lung_adenocarcinoma.scores 

acute_myeloid_leukemia.scores : tpm_matrix_AML_superOrtho_meta.tsv.log_norm ../Signatures/R/run_sigs.R 
	Rscript run_sigs.R Whole_Blood-acute_myeloid_leukemia tpm_matrix_AML_superOrtho_meta.tsv.log_norm  $@

colon_adenocarcinoma.scores : tpm_matrix_colon_superOrtho_meta.tsv.log_norm ../Signatures/R/run_sigs.R 
	Rscript run_sigs.R Colon-colon_adenocarcinoma tpm_matrix_colon_superOrtho_meta.tsv.log_norm  $@

lung_adenocarcinoma.scores : tpm_matrix_lung_superOrtho_meta.tsv.log_norm  ../Signatures/R/run_sigs.R 
	Rscript run_sigs.R Lung-lung_adenocarcinoma tpm_matrix_lung_superOrtho_meta.tsv.log_norm  $@

