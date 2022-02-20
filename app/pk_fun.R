one_cmpt_pk = function(mtime, dose, V, hl, SC = F, ka, Fbio = 1){
  rate = log(0.5)/hl
  if(SC){
    return(Fbio * dose/V * ka/(ka + rate) * (
      exp(mtime * rate) - exp(-ka * mtime)
      ))
  }
  dose/V * exp(mtime * rate)
}