# gelNet.R input_data input_dichotomy input_network output_model


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
  cat("\nmaxb=")
  cat(maxb);
  cat("\nminb=")
  cat(minb);
  cat("\n\n")
  c = 6/(maxb-minb)*(b-minb)-3
  return(c);
}

X = read.table(args[1],row.names=1,header=TRUE);

Y = apply(X, 2, function(orig) {
   col = orig
   col = col[order(col)]
   col0 = col[col == 0];

   nana = rep(NA, length(col0))
   names(nana) = names(col0)
   
   values = col[col > 0];
   ranked = rank.normalize(values);
   scaled = rescale(ranked)
   names(scaled) = names(values)
   result = c(nana, scaled)
   result = result[order(names(result))];
   return(result);
}
)
write.table(Y, file=args[2], sep="\t", quote=FALSE)
