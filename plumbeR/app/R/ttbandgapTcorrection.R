library(Rcpp)

Rcpp::sourceCpp( file = "/app/R/tt.cpp")

ttnorm_analog_t = function(table){
  t_norm = apply(table, 1, function(x) Normalize(x[1],x[2]))
  return(t_norm)
}