#!/bin/bash
s='Models'
t='tmp'

i='../genelists/GO/data/hallmarks.genes'

mkdir -p $s $t
for o in  \
    "acute_myeloid_leukemia" \
    "colon_adenocarcinoma" \
    "glioblastoma_multiforme" \
    "lung_adenocarcinoma" 
do
    for h in `ls $i/`
    do
        F="$s/$o.GTEX.TCGA.$h.signature"
        if [ -f $F ]; then
                echo $F exists > /dev/null
        else
                echo "File $F does not exist."
                echo Processing $h in tissue of origin $o 

                python bin/matrix_slice.py $i/$h "lists/TCGA.$o"  ReferenceData/TCGA.RSEM > "$t/$o.TCGA.$h.data"
                python bin/matrix_slice.py $i/$h "lists/GTEX.$o"  ReferenceData/GTEX.RSEM > "$t/$o.GTEX.$h.data"
                python bin/matrix_join_common.py "$t/$o.GTEX.$h.data" "$t/$o.TCGA.$h.data" > "$t/$o.GTEX.TCGA.$h.data"

                Rscript bin/thresholdColumns.R "$t/$o.GTEX.TCGA.$h.data"

                head -1 "$t/$o.GTEX.TCGA.$h.data" | tr '\t' '\n' | sed -e '1d;s/GTEX.*/0/;s/TCGA.*/1/' > "$t/$o.GTEX.TCGA.$h.phen"
                cut -f1 "$t/$o.GTEX.TCGA.$h.data" | sed -e '1d' > "$t/$o.GTEX.TCGA.$h.genes"
                python bin/adjacency.py  ReferenceData/NETWORK "$t/$o.GTEX.TCGA.$h.genes" | sed -e 's/ //g' > "$t/$o.GTEX.TCGA.$h.network"

                Rscript bin/gelNet.R "$t/$o.GTEX.TCGA.$h.data" "$t/$o.GTEX.TCGA.$h.phen" "$t/$o.GTEX.TCGA.$h.network" "$s/$o.GTEX.TCGA.$h.signature" "$h" "$o" 

        fi

    done
done
