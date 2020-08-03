one_cmpt_pk = function(mtime, dose, V, hl){
  rate = log(0.5)/hl
  dose/V * exp(mtime * rate)
}
