HOT=null;

function asv() {
    debugger;
}
Handsontable.hooks.add('afterScrollHorizontally', asv);




OMFScell = function(instance, td, row, col, prop, value, cellProperties) {
    if (HOT == null) {
    debugger
        HOT = instance
        HOT.addHook("afterScrollVertically", asv);
    }
    var pre  = '<div class="table-cell-wrapper"> <div class="table-cell">';
    var post = '</div> </div>'



    if (value == true || value == false) 
        Handsontable.CheckboxRenderer.apply(this, arguments)
    else {
        value = pre + value + post;
        Handsontable.HtmlRenderer.apply(this, arguments)
    }


    if (instance.params && (col == 0 || col >= 10)) {
        var ids = instance.params.BioSampleID;
        td.classList = 'all-sample-info sample-' + ids[row];
        if (document.ROWCOLORSHACK && ids[row] in document.ROWCOLORSHACK)
            td.style.backgroundColor = document.ROWCOLORSHACK[ ids[row] ]
    }
    // if (instance.params && hcols.includes(col)) td.style.background = 'red';
    // if (instance.params && hrows.includes(row)) td.style.background = 'yellow';
}
