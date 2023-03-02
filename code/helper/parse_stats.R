# considered using the effectsize package but afaik we don't care about estimating CI's here and there were optim problems with that

parse_t <- function(tval, within_between) {
  df=str_extract(tval, "\\(.*\\)") |> str_sub(2,-2) |> as.numeric()
  val=str_extract(tval, "=.*") |> str_sub(2,-1) |> as.numeric() |> abs()
  pval=pt(q=val, df=df, lower.tail=FALSE)*2
  d_calc=NA
  N_from_df=NA
  if(within_between=="between"){
    d_calc=2*val/(sqrt(df+2)) # this is a simplification for equal groups
    N_from_df=df+2 # between so add 2
  }
  else{
    d_calc=val/sqrt(df+1)
    N_from_df=df+1
  }
  se=4/N_from_df+d_calc**2/(2*N_from_df)
  return(data.frame("df_1"=NA, "df_2"=df,"tstat"=val, "fstat"=NA, "p_calc"=pval, "d_calc"=d_calc, "N_calc"=N_from_df, "ES"=d_calc, "SE"=se))
}

parse_f <- function(fval, within_between){
  df_1 =str_extract(fval, "\\(.*,") |> str_sub(2,-2) |> as.numeric()
  df_2 =str_extract(fval, ",.*\\)") |> str_sub(2,-2) |> as.numeric()
  val=str_extract(fval, "=.*") |> str_sub(2,-1) |> as.numeric() |> abs()
  pval=pf(q=val, df1=df_1, df2=df_2, lower.tail=FALSE)
  d_calc=NA
  N_from_df=df_1+df_2+1
  if(!is.na(df_1)&&df_1==1){
    if(within_between=="between"){
      d_calc=2*sqrt(val)/sqrt(N_from_df) # if there's just two groups, then F=t**2 and we can use the t-test thingy
    }
    else{
      d_calc=sqrt(val)/sqrt(df_2)
    }
    se=4/N_from_df+d_calc**2/(2*N_from_df)
  }
  else if(!is.na(df_1)){
    d_calc=NA
    se=NA
  }
  
  return(data.frame("df_1"=df_1,"df_2"=df_2,"tstat"=NA, "fstat"=val, "p_calc"=pval, "d_calc"=d_calc, "N_calc"=N_from_df, "ES"=d_calc, "SE"=se))
}


# for comparison between proportions 
#ex <- "prob: 13 / 14 , 2 / 14"
parse_prop <- function(propval){
  val <- str_extract_all(propval, "[0-9]+")[[1]]
  num1 <- val[1] |> as.numeric()
  den1 <- val[2] |> as.numeric()
  num2 <- val[3] |> as.numeric()
  den2 <- val[4] |> as.numeric()
  est1 <- num1/den1
  est1_var <- est1*(1-est1)/den1
  est2 <- num2/den2
  est2_var <- est2*(1-est2)/den2
  diff <- est1-est2
  diff_var <- est1_var+est2_var
  return(data.frame("df_1"=NA, "df_2"=NA,"tstat"=NA, "fstat"=NA, "p_calc"=NA, "d_calc"=NA, "N_calc"=NA, "ES"=diff, "SE"=sqrt(diff_var)))
}
#parse_prop(ex)


# for one proportion
#ex <- "raw prop : 10 / 18"
parse_raw_prop <- function(propval){
  val <- str_extract_all(propval, "[0-9]+")[[1]]
  num1 <- val[1] |> as.numeric()
  den1 <- val[2] |> as.numeric()
  est1 <- num1/den1
  est1_var <- est1*(1-est1)/den1
  return(data.frame("df_1"=NA, "df_2"=NA,"tstat"=NA, "fstat"=NA, "p_calc"=NA, "d_calc"=NA, "N_calc"=NA, "ES"=est1, "SE"=sqrt(est1_var)))
}
#parse_raw_prop(ex)


parse_mean_ci <- function(mci,within_between,n){
  # parsing 
  cond1=str_extract(mci, "m1.*\\],")
  cond2=str_extract(mci, "m2.*\\]")
  m1=str_extract(cond1, "m1=.*\\[") |> str_sub(4,-2) |> as.numeric()
  m2=str_extract(cond2, "m2=.*\\[") |> str_sub(4,-2) |> as.numeric()
  low1=str_extract(cond1, "\\[.*,.") |> str_sub(2,-3) |> as.numeric()
  low2=str_extract(cond2, "\\[.*,") |> str_sub(2,-2) |> as.numeric()
  high1=str_extract(cond1, ",.*\\]") |> str_sub(2,-2) |> as.numeric()
  high2=str_extract(cond2, ",.*\\]") |> str_sub(2,-2) |> as.numeric()
  # calculations 
  se1=high1-low1/(2*1.96)
  se2=high2-low2/(2*1.96)
  per_group_n=ifelse(within_between=="within",n, n/2)
  sd1=se1*sqrt(per_group_n-1) # assume equal groups
  sd2=se2*sqrt(per_group_n-1)
  sd_pool=sqrt((sd1**2+sd2**2)/2) # assume equal groups
  se_pool=sd_pool/sqrt(n-1)
  d_calc=abs(m1-m2/sd_pool) # note we force all positive and then fix later 
  tval=(m1-m2)/se_pool |> abs()
  df=ifelse(within_between=="within", n-1, n-2)
  pval=pt(q=tval, df=df, lower.tail=FALSE)*2
  return(data.frame("df_1"=df,"df_2"=NA,"tstat"=tval, "fstat"=NA, "p_calc"=pval, "d_calc"=d_calc, "N_calc"=NA, "ES"=m1-m2, "SE"=se_pool))
}


parse_mean_sd <- function(msd,within_between,n){
  cond1=str_extract(msd, "m1.*\\),")
  cond2=str_extract(msd, "m2.*\\)")
  m1=str_extract(cond1, "m1=.*\\(") |> str_sub(4,-2) |> as.numeric()
  m2=str_extract(cond2, "m2=.*\\(") |> str_sub(4,-2) |> as.numeric()
  sd1=str_extract(cond1, "\\(.*\\),") |> str_sub(2,-3) |> as.numeric()
  sd2=str_extract(cond2, "\\(.*\\)") |> str_sub(2,-2) |> as.numeric()
  #
  sd_pool=sqrt((sd1**2+sd2**2)/2) # assume equal groups
  se_pool=sd_pool/sqrt(n-1)
  d_calc=abs(m1-m2/sd_pool) # note we force all positive and then fix later 
  tval=(m1-m2)/se_pool |> abs()
  df=ifelse(within_between=="within", n-1, n-2)
  pval=pt(q=tval, df=df, lower.tail=FALSE)*2
  return(data.frame("df_1"=df,"df_2"=NA,"tstat"=tval, "fstat"=NA, "p_calc"=pval, "d_calc"=d_calc, "N_calc"=NA, "ES"=m1-m2, "SE"=se_pool))
}


parse_mean_se <- function(mse,within_between,n){
  cond1=str_extract(mse, "m1.*\\),")
  cond2=str_extract(mse, "m2.*\\)")
  m1=str_extract(cond1, "m1=.*\\(") |> str_sub(4,-2) |> as.numeric()
  m2=str_extract(cond2, "m2=.*\\(") |> str_sub(4,-2) |> as.numeric()
  se1=str_extract(cond1, "\\(.*\\),") |> str_sub(2,-3) |> as.numeric()
  se2=str_extract(cond2, "\\(.*\\)") |> str_sub(2,-2) |> as.numeric()
  
  per_group_n=ifelse(within_between=="within",n, n/2)
  sd1=se1*sqrt(per_group_n-1) # assume equal groups
  sd2=se2*sqrt(per_group_n-1)
  sd_pool=sqrt((sd1**2+sd2**2)/2) # assume equal groups
  se_pool=sd_pool/sqrt(n-1)
  d_calc=abs(m1-m2/sd_pool) # note we force all positive and then fix later 
  tval=(m1-m2)/se_pool |> abs()
  df=ifelse(within_between=="within", n-1, n-2)
  pval=pt(q=tval, df=df, lower.tail=FALSE)*2
  return(data.frame("df_1"=df,"df_2"=NA,"tstat"=tval, "fstat"=NA, "p_calc"=pval, "d_calc"=d_calc, "N_calc"=NA, "ES"=m1-m2, "SE"=se_pool))
}


parse_beta_se <- function(raw_stat){
  beta=str_extract(raw_stat, "b=.*\\(") |> str_sub(3,-2) |> as.numeric()
  se=str_extract(raw_stat, "\\(.*\\)") |> str_sub(2,-2) |> as.numeric()
  #zval=beta/se
  #pval=2*pnorm(zval, lower.tail=F)
  return(data.frame("df_1"=NA,"df_2"=NA,"tstat"=NA, "fstat"=NA, "p_calc"=NA, "d_calc"=NA, "N_calc"=NA, "ES"=beta, "SE"=se))
}


parse_beta_ci <- function(raw_stat){
  beta=str_extract(raw_stat, "b=.*\\[") |> str_sub(3,-2) |> as.numeric()
  low=str_extract(raw_stat, "\\[.*,") |> str_sub(2,-2) |> as.numeric()
  high=str_extract(raw_stat, ",.*\\]") |> str_sub(2,-2) |> as.numeric()
  se=high-low/(2*1.96)
  #zval=beta/se
  #pval=2*pnorm(zval, lower.tail=F)
  return(data.frame("df_1"=NA,"df_2"=NA,"tstat"=NA, "fstat"=NA, "p_calc"=NA, "d_calc"=NA, "N_calc"=NA, "ES"=beta, "SE"=se))
}

#this is just to deal with yang2018 where we have d's given and bci
#ex <- "d: 1.40 ; bci: b=1.72 [1.44, 2.00]"
parse_dbci <- function(raw_stat){
  bci = str_extract(raw_stat, ";.*") |> str_sub(2,-1)
  beta=str_extract(bci, "b=.*\\[") |> str_sub(3,-2) |> as.numeric()
  low=str_extract(bci, "\\[.*,") |> str_sub(2,-2) |> as.numeric()
  high=str_extract(bci, ",.*\\]") |> str_sub(2,-2) |> as.numeric()
  se=high-low/(2*1.96)
  
  dval = str_extract(raw_stat, ".*;") |> str_sub(3,-2) |> as.numeric()
  return(data.frame("df_1"=NA,"df_2"=NA,"tstat"=NA, "fstat"=NA, "p_calc"=NA, "d_calc"=dval, "N_calc"=NA, "ES"=beta, "SE"=se))
}
#parse_dbci(ex)


do_blanks <- function(){
  return(data.frame("df_1"=NA,"df_2"=NA,"tstat"=NA, "fstat"=NA, "p_calc"=NA, "d_calc"=NA, "N_calc"=NA, "ES"=NA, "SE"=NA))
}
do_parsing=function(raw_stat, within_between,n){
  if (is.na(raw_stat)) {return (do_blanks())}
  if (str_sub(raw_stat,1,1)=="t"){return(parse_t(raw_stat, within_between))}
  if (str_sub(raw_stat,1,1)=="F"){return(parse_f(raw_stat, within_between))}
  if (str_sub(raw_stat,1,4)=="prop"){return(parse_prop(raw_stat))}
  if (str_sub(raw_stat,1,7)=="rawprop"){return(parse_raw_prop(raw_stat))}
  if (str_sub(raw_stat, 1,3)=="MCI"){return(parse_mean_ci(raw_stat, within_between,n))}
  if (str_sub(raw_stat, 1,3)=="MSD"){return(parse_mean_sd(raw_stat, within_between,n))}
  if (str_sub(raw_stat, 1,3)=="MSE"){return(parse_mean_se(raw_stat, within_between,n))}
  if (str_sub(raw_stat, 1,8)=="wilcoxon"){return(parse_wilcoxon(raw_stat))}
  if (str_sub(raw_stat, 1,3)=="bse"){return(parse_beta_se(raw_stat))}
  if (str_sub(raw_stat, 1,3)=="bci"){return(parse_beta_ci(raw_stat))}
  if (str_sub(raw_stat, 1,1)=="d"){return(parse_dbci(raw_stat))}
  
  
  return (do_blanks())
}

