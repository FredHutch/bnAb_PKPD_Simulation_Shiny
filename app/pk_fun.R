one_cmpt_pk = function(mtime, dose, V, hl, SC = F, ka, Fbio = 1){
  rate = log(0.5)/hl
  if(SC){
    return(Fbio * dose/V * ka/(ka + rate) * (
      exp(mtime * rate) - exp(-ka * mtime)
      ))
  }
  dose/V * exp(mtime * rate)
}

# ------- two-cmpt setup --------


calc_twocmpt_hl = function (Cl, V, Q, Vp) {

  kel <- Cl/V
  k12 <- Q/V
  k21 <- Q/Vp
  beta <- ((1/2) * ((k12 + k21 + kel) - sqrt((k12 + k21 + kel)^2 - 
                                               (4 * k21 * kel))))
  log(2)/beta

}

CL_opt_fun = function(logCl, V, Q, Vp, hl){
  hl_out = calc_twocmpt_hl(Cl = exp(logCl), V = V, Q = Q, Vp = Vp)
  abs(hl_out - hl)
}

calc_CL = function(V, Q, Vp, hl){
  opt = optimise(CL_opt_fun, c(-8, 2), V = V, Q = Q, Vp = Vp, hl = hl)
  exp(opt$minimum)
} 

convert_twocmpt_parms = function(V, Cl, Q, Vp){
  k <- Cl/V
  terms <- (Q/V) + (Q/Vp) + (Cl/V)
  beta <- 0.5 * (terms - sqrt(terms^2 - 4 * (Q/Vp) * (Cl/V)))
  alpha <- ((Q/Vp) * (Cl/V))/beta
  A <- (alpha - (Q/Vp))/(alpha - beta)
  B <- ((beta - Q/Vp)/(beta - alpha))
  tibble(
    beta = beta,
    alpha = alpha,
    A = A,
    B = B
  )
}

two_cmpt_pk = function(mtime, dose, V, Cl, Q, Vp, SC = F, ka, Fbio = 1){
  
  rate_parms = convert_twocmpt_parms(V = V, Cl = Cl, Q = Q, Vp = Vp)
  
  if(SC){
    Aabs = rate_parms$A * ka /(ka - rate_parms$alpha)
    Babs = rate_parms$B * ka /(ka - rate_parms$beta)
    return(Fbio * dose/V * (Aabs*exp(-rate_parms$alpha * mtime) +
               Babs*exp(-rate_parms$beta * mtime) -
                (Aabs+Babs) * exp(-ka * mtime)
                )
    )
  }
  dose/V * (rate_parms$A*exp(-rate_parms$alpha * mtime) +
              rate_parms$B*exp(-rate_parms$beta * mtime)  
            )
}
