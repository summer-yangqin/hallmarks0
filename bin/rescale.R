#!/usr/local/bin/Rscript
# rescale.R input output

args <- commandArgs(trailingOnly = TRUE)

rank.normalize <- function(x, FUN=qnorm, ties.method = "average", na.action) {
    if (missing(na.action)) {
        na.action <- get(getOption("na.action"))
    }
    if(! is.function(na.action)) {
        stop("'na.action' must be a function")
    }
    x <- na.action(x)
    FUN(rank(x, ties.method = ties.method)/(length(x)+1))
}

rescale= function(b) {
  maxb = max(b)
  minb = min(b)
  c = 6/(maxb-minb)*(b-minb)-3
  return(c);
}

input = file(args[1], open="r");
topLine <- readLines(input, n = 1, warn = FALSE)
X = read.table(input, row.names=1, header=FALSE);
close(input)

Y = apply(X, 2, function(orig) {
   col = orig
   col = col[order(col)]

   values = col[col >= 1.0];
   below = col[col < 1.0];

   nana = rep(0.0, length(below))

   ranked = rank.normalize(values);
   scaled = rescale(ranked)
   names(scaled) = names(values)
   result = c(nana, scaled)
   names(result) = c(names(below), names(values));
   result = result[order(names(result))];
   return(result);
}
)
browser()
output = file(args[2], open="w")
writeLines(topLine, output)
write.table(Y, file=output, sep="\t", col.names = F, quote=FALSE)
close(output)
