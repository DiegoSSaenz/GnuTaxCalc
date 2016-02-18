#helpers.R for Roth vs. Traditional

#Federal Tax Brackets
Tax_Fed_m <- matrix(c(0,18150,73800,148850,226850,405100,0,1815,10162.5,
                   28925,50765,109587.50,.10,.15,.25,.28,.33,.35)
                   , nrow=6, ncol=3)
#States with no income tax: AK,FL,NV,NH,SD,TN,TX,WA,WY
Tax_NT_m <- matrix(c(0, 0, 0), nrow =1, ncol =3)
#Colorado Sate Income Tax
Tax_CO_m <- matrix(c(0, 0, 0.0463), nrow =1, ncol =3)
#Illinois Sate Income Tax
Tax_IL_m <- matrix(c(0, 0, 0.05), nrow =1, ncol =3)
#Indiana State Income Tax (Need to add Counties)
Tax_IN_m <- matrix(c(0, 0, 0.034), nrow =1, ncol =3)
#Maryland State Income Tax Including Local Tax
Tax_MD_m <- matrix(c(0,1000,2000,3000,150000,175000,225000,0,20,50,90,
                     7072.50,8322.50,10947.50,.02,.03,.04,.0475,.05,
                     .0525,.055), nrow = 7, ncol=3)

#fica <- 0.062 + .0145 #Social Security and Disability Insurance + Medicare

state_tax <- function(income, state){
  if(state == "NT"){
    agi_s <- 0
    Tax_S_m <- Tax_NT_m
  }
  if(state == "CO"){
    agi_s <- income
    Tax_S_m <- Tax_CO_m
  }
  if(state == "IL"){
    agi_s <- income - 4000
    Tax_S_m <- Tax_IL_m
  }
  if(state == "IN"){
    agi_s <- income 
    Tax_S_m <- Tax_IN_m
  }
  if(state == "MD"){
    agi_s <- income - (1200+4000+6400)
    Tax_S_m <- Tax_MD_m
  }
  i <- dim(Tax_S_m)[1]
  while(i>0){
    if(agi_s >= Tax_S_m[i,1]){
      return(Tax_S_m[i,2] + (agi_s - Tax_S_m[i,1]) * Tax_S_m[i,3] +
             agi_s * 0.032)
    }
    i <- i - 1
  }
  return(0)
}

fed_tax <- function(income){
  agi <- income - (7800 + 12400) 
  i <- dim(Tax_Fed_m)[1]
  while(i>0){
    if(agi >= Tax_Fed_m[i,1]){
      return(Tax_Fed_m[i,2] + (agi - Tax_Fed_m[i,1]) * Tax_Fed_m[i,3])
    }
    i <- i - 1
  }
  return(0)
}

fica <- function(income){
  #Social Security and Disability Insurance + Medicare
  return(income * (0.062 + .0145))
}
total_tax <- function(income, state_tax){
  return(fed_tax(income)+state_tax+fica(income))
}