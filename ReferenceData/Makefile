got.data: TCGA.Z GTEX.Z NETWORK

GTEX.Z : GTEX.RSEM
	../bin/rescale.R $< $@

TCGA.Z : TCGA.RSEM
	../bin/rescale.R $< $@

TCGA.RSEM: get
	./get "https://toil.xenahubs.net/download/tcga_RSEM_Hugo_norm_count.gz" TCGA.RSEM

GTEX.RSEM: get
	./get "https://toil.xenahubs.net/download/gtex_RSEM_Hugo_norm_count.gz" GTEX.RSEM

NETWORK: get
	./get "http://www.pathwaycommons.org/archives/PC2/v8/PathwayCommons.8.All.BINARY.SIF.hgnc.txt.sif.gz" NETWORK
