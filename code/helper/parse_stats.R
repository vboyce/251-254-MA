# considered using the effectsize package but afaik we don't care about estimating CI's here and there were optim problems with that
# https://training.cochrane.org/handbook/current/chapter-06 may be relevant

# Standard errors of d, assuming equal groups (Borenstein et al., 2009, ch. 4).
# The parsers return d_calc_se as an SE, not a variance: stats.R squares it before
# passing it to Replicate::pred_int and Replicate::p_orig, which take variances.
# (Before the 2026 correction the parsers returned the variance here, so it was squared twice.)
#
# Between participants, d_s for two equal groups with N people in total:
#   Var(d_s) = 4/N + d^2/(2N)
se_of_d_between <- function(d, n) {
  sqrt(4/n + d**2/(2*n))
}
# Within participants or one sample, d_z = t/sqrt(N) with N people:
#   Var(d_z) = 1/N + d^2/(2N)
# Using the between formula here would make the SE about twice too large.
se_of_d_within <- function(d, n) {
  sqrt(1/n + d**2/(2*n))
}

# Standard error of a difference between condition means, from the pooled SD of the cells.
# Between participants, with n people split equally over n_cells independent cells
# (n/n_cells per cell) and contrast weights of +1 or -1 on each cell:
#   Var(difference) = n_cells * sd^2 / (n/n_cells) = n_cells^2 * sd^2 / n
#   so SE = n_cells * sd / sqrt(n): 2*sd/sqrt(n) for two groups,
#   4*sd/sqrt(n) for the difference of differences in a 2x2.
# Within and mixed designs keep sd/sqrt(n-1). The correlation between conditions is not
# reported, and when it is about .5 the SE of a paired difference is about sd/sqrt(n),
# so this is a reasonable approximation.
se_of_mean_difference <- function(sd_pool, n, n_cells, within_between) {
  if (within_between == "between") {
    return(n_cells * sd_pool / sqrt(n))
  }
  sd_pool / sqrt(n - 1)
}

parse_t <- function(tval, within_between) {
  df=str_extract(tval, "\\(.*\\)") |> str_sub(2,-2) |> as.numeric()
  val=str_extract(tval, "=.*") |> str_sub(2,-1) |> as.numeric() |> abs()
  pval=pt(q=abs(val), df=df, lower.tail=FALSE)*2
  d_calc=NA
  N_from_df=NA
  if(within_between=="between"){
    d_calc=2*val/(sqrt(df+2)) # this is a simplification for equal groups
    N_from_df=df+2 # between so add 2
    se=se_of_d_between(d_calc, N_from_df)
  }
  else{
    d_calc=val/sqrt(df+1) # d_z, so it gets the within SE (mixed designs included)
    N_from_df=df+1
    se=se_of_d_within(d_calc, N_from_df)
  }
  return(data.frame("df_1"=NA, "df_2"=df,"tstat"=val, "fstat"=NA, "p_calc"=pval, "d_calc"=d_calc, "d_calc_se"=se, "N_calc"=N_from_df, "ES"=NA, "SE"=NA))
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
      se=se_of_d_between(d_calc, N_from_df)
    }
    else{
      d_calc=sqrt(val)/sqrt(df_2) # d_z, so it gets the within SE (mixed designs included)
      se=se_of_d_within(d_calc, N_from_df)
    }
  }
  else if(!is.na(df_1)){
    d_calc=NA
    se=NA
  }
  
  return(data.frame("df_1"=df_1,"df_2"=df_2,"tstat"=NA, "fstat"=val, "p_calc"=pval, "d_calc"=d_calc, "d_calc_se"=se, "N_calc"=N_from_df, "ES"=NA, "SE"=NA))
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
  return(data.frame("df_1"=NA, "df_2"=NA,"tstat"=NA, "fstat"=NA, "p_calc"=NA, "d_calc"=NA, "d_calc_se"=NA, "N_calc"=NA, "ES"=diff, "SE"=sqrt(diff_var)))
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
  return(data.frame("df_1"=NA, "df_2"=NA,"tstat"=NA, "fstat"=NA, "p_calc"=NA, "d_calc"=NA, "d_calc_se"=NA,"N_calc"=NA, "ES"=est1, "SE"=sqrt(est1_var)))
}
#parse_raw_prop(ex)

#foo = "MSD1:m1=49.9(11),comp=50"
parse_mean_sd1 <- function(msd,within_between,n){
  cond1=str_extract(msd, "m1.*\\),")
  comp=str_extract(msd, "comp.*")|> str_sub(6,-1) |> as.numeric()
  m1=str_extract(cond1, "m1=.*\\(") |> str_sub(4,-2) |> as.numeric()
  sd1=str_extract(cond1, "\\(.*\\),") |> str_sub(2,-3) |> as.numeric()
  #
  se=sd1/sqrt(n-1)
  d_calc=abs(m1-comp)/sd1 # note we force all positive and then fix later 
  tval=(m1-comp)/se |> abs()
  df=n-1
  pval=pt(q=abs(tval), df=df, lower.tail=FALSE)*2
  d_se=se_of_d_within(d_calc, n) # one-sample comparison, so d is a d_z

  return(data.frame("df_1"=df,"df_2"=NA,"tstat"=tval, "fstat"=NA, "p_calc"=pval, "d_calc"=d_calc, "d_calc_se"=d_se,"N_calc"=NA, "ES"=m1-comp, "SE"=se))
}

#parse_mean_sd1(foo, "within", 5)


parse_mean_se1 <- function(mse,within_between,n){
  cond1=str_extract(mse, "m1.*\\),")
  comp=str_extract(mse, "comp.*")|> str_sub(6,-1) |> as.numeric()
  m1=str_extract(cond1, "m1=.*\\(") |> str_sub(4,-2) |> as.numeric()
  se1=str_extract(cond1, "\\(.*\\),") |> str_sub(2,-3) |> as.numeric()
  #
  sd1=se1*sqrt(n-1)
  d_calc=abs(m1-comp)/sd1 # note we force all positive and then fix later 
  tval=(m1-comp)/se1 |> abs()
  df=n-1
  pval=pt(q=abs(tval), df=df, lower.tail=FALSE)*2
  d_se=se_of_d_within(d_calc, n) # one-sample comparison, so d is a d_z

  return(data.frame("df_1"=df,"df_2"=NA,"tstat"=tval, "fstat"=NA, "p_calc"=pval, "d_calc"=d_calc, "d_calc_se"=d_se, "N_calc"=NA, "ES"=m1-comp, "SE"=se1))
}

parse_mean_ci2 <- function(mci,within_between,n){
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
  se1=(high1-low1)/(2*1.96)
  se2=(high2-low2)/(2*1.96)
  per_group_n=n/2 #this is conservative to treat within as if it's between
  sd1=se1*sqrt(per_group_n) # assume equal groups; SE of a mean is sd/sqrt(n per group)
  sd2=se2*sqrt(per_group_n)
  sd_pool=sqrt((sd1**2+sd2**2)/2) # assume equal groups
  se_pool=se_of_mean_difference(sd_pool, n, n_cells=2, within_between)

  d_calc=abs(m1-m2)/sd_pool # note we force all positive and then fix later
  tval=(m1-m2)/se_pool |> abs()
  df=ifelse(within_between=="within", n-1, n-2)
  pval=pt(q=abs(tval), df=df, lower.tail=FALSE)*2
  d_se=se_of_d_between(d_calc, n) # d is scaled by the SD of the cells, not of the differences
  
  return(data.frame("df_1"=df,"df_2"=NA,"tstat"=tval, "fstat"=NA, "p_calc"=pval, "d_calc"=d_calc, "d_calc_se"=d_se, "N_calc"=NA, "ES"=m1-m2, "SE"=se_pool))
}

parse_mean_se2 <- function(mse,within_between,n){
  cond1=str_extract(mse, "m1.*\\),")
  cond2=str_extract(mse, "m2.*\\)")
  m1=str_extract(cond1, "m1=.*\\(") |> str_sub(4,-2) |> as.numeric()
  m2=str_extract(cond2, "m2=.*\\(") |> str_sub(4,-2) |> as.numeric()
  se1=str_extract(cond1, "\\(.*\\),") |> str_sub(2,-3) |> as.numeric()
  se2=str_extract(cond2, "\\(.*\\)") |> str_sub(2,-2) |> as.numeric()
  
  per_group_n=n/2 # conservative
  sd1=se1*sqrt(per_group_n) # assume equal groups; SE of a mean is sd/sqrt(n per group)
  sd2=se2*sqrt(per_group_n)
  sd_pool=sqrt((sd1**2+sd2**2)/2) # assume equal groups
  se_pool=se_of_mean_difference(sd_pool, n, n_cells=2, within_between)
  d_calc=abs(m1-m2)/sd_pool # note we force all positive and then fix later
  tval=(m1-m2)/se_pool |> abs()
  df=ifelse(within_between=="within", n-1, n-2)
  pval=pt(q=abs(tval), df=df, lower.tail=FALSE)*2
  d_se=se_of_d_between(d_calc, n) # d is scaled by the SD of the cells, not of the differences
  
  return(data.frame("df_1"=df,"df_2"=NA,"tstat"=tval, "fstat"=NA, "p_calc"=pval, "d_calc"=d_calc, "d_calc_se"=d_se, "N_calc"=NA, "ES"=m1-m2, "SE"=se_pool))
}

parse_mean_sd2 <- function(msd,within_between,n){
  cond1=str_extract(msd, "m1.*\\),")
  cond2=str_extract(msd, "m2.*\\)")
  m1=str_extract(cond1, "m1=.*\\(") |> str_sub(4,-2) |> as.numeric()
  m2=str_extract(cond2, "m2=.*\\(") |> str_sub(4,-2) |> as.numeric()
  sd1=str_extract(cond1, "\\(.*\\),") |> str_sub(2,-3) |> as.numeric()
  sd2=str_extract(cond2, "\\(.*\\)") |> str_sub(2,-2) |> as.numeric()
  #
  sd_pool=sqrt((sd1**2+sd2**2)/2) # assume equal groups
  se_pool=se_of_mean_difference(sd_pool, n, n_cells=2, within_between)
  d_calc=abs(m1-m2)/sd_pool # note we force all positive and then fix later
  tval=(m1-m2)/se_pool |> abs()
  df=ifelse(within_between=="within", n-1, n-2)
  pval=pt(q=abs(tval), df=df, lower.tail=FALSE)*2
  d_se=se_of_d_between(d_calc, n) # d is scaled by the SD of the cells, not of the differences
  
  return(data.frame("df_1"=df,"df_2"=NA,"tstat"=tval, "fstat"=NA, "p_calc"=pval, "d_calc"=d_calc, "d_calc_se"=d_se, "N_calc"=NA, "ES"=m1-m2, "SE"=se_pool))
}
#foo <- "MSD4:m1=3.26(1.91),m2=5.40(1.59),m3=3.67(2.00),m4=4.67(2.06)"

parse_mean_sd4 <- function(msd,within_between,n){
  cond1=str_extract(msd, "m1.*\\),m2")
  cond2=str_extract(msd, "m2.*\\),m3")
  cond3=str_extract(msd, "m3.*\\),")
  cond4=str_extract(msd, "m4.*\\)")
  
  m1=str_extract(cond1, "m1=.*\\(") |> str_sub(4,-2) |> as.numeric()
  m2=str_extract(cond2, "m2=.*\\(") |> str_sub(4,-2) |> as.numeric()
  m3=str_extract(cond3, "m3=.*\\(") |> str_sub(4,-2) |> as.numeric()
  m4=str_extract(cond4, "m4=.*\\(") |> str_sub(4,-2) |> as.numeric()
  
  sd1=str_extract(cond1, "\\(.*\\),") |> str_sub(2,-3) |> as.numeric()
  # cond2 and cond3 extract to "(sd)" with no trailing comma, so trim one character per side
  # (trimming -3 here used to drop the SD's last digit: 1.59 was read as 1.5)
  sd2=str_extract(cond2, "\\(.*\\)") |> str_sub(2,-2) |> as.numeric()
  sd3=str_extract(cond3, "\\(.*\\)") |> str_sub(2,-2) |> as.numeric()
  sd4=str_extract(cond4, "\\(.*\\)") |> str_sub(2,-2) |> as.numeric()
  
  #diff in diff
  m_diff=m1-m2-m3+m4
  sd_pool=sqrt((sd1**2+sd2**2+sd3**2+sd4**2)/4)
  se_pool=se_of_mean_difference(sd_pool, n, n_cells=4, within_between)
  d_calc=abs(m_diff/sd_pool) # note we force all positive and then fix later 
  tval=(m_diff)/se_pool |> abs()
  df=ifelse(within_between=="within", n-1, n-2)
  pval=pt(q=abs(tval), df=df, lower.tail=FALSE)*2
  d_se=se_of_d_between(d_calc, n) # d is scaled by the SD of the cells, not of the differences
  
  return(data.frame("df_1"=df,"df_2"=NA,"tstat"=tval, "fstat"=NA, "p_calc"=pval, "d_calc"=d_calc, "d_calc_se"=d_se, "N_calc"=NA, "ES"=m_diff, "SE"=se_pool))
}

#parse_mean_sd4(foo, "between", 10)

#foo="MSE4:m1=4.8(.6),m2=5.5(.6),m3=3.8(.5),m4=3.7(.5)"
parse_mean_se4 <- function(msd,within_between,n){
  cond1=str_extract(msd, "m1.*\\),m2")
  cond2=str_extract(msd, "m2.*\\),m3")
  cond3=str_extract(msd, "m3.*\\),")
  cond4=str_extract(msd, "m4.*\\)")
  
  m1=str_extract(cond1, "m1=.*\\(") |> str_sub(4,-2) |> as.numeric()
  m2=str_extract(cond2, "m2=.*\\(") |> str_sub(4,-2) |> as.numeric()
  m3=str_extract(cond3, "m3=.*\\(") |> str_sub(4,-2) |> as.numeric()
  m4=str_extract(cond4, "m4=.*\\(") |> str_sub(4,-2) |> as.numeric()
  
  se1=str_extract(cond1, "\\(.*\\),") |> str_sub(2,-3) |> as.numeric()
  se2=str_extract(cond2, "\\(.*\\),") |> str_sub(2,-3) |> as.numeric()
  se3=str_extract(cond3, "\\(.*\\),") |> str_sub(2,-3) |> as.numeric()
  se4=str_extract(cond4, "\\(.*\\)") |> str_sub(2,-2) |> as.numeric()
  
  per_group_n=n/4 #conservative 
  #diff in diff
  m_diff=m1-m2-m3+m4
  sd1=se1*sqrt(per_group_n) # assume equal groups; SE of a mean is sd/sqrt(n per group)
  sd2=se2*sqrt(per_group_n)
  sd3=se3*sqrt(per_group_n)
  sd4=se4*sqrt(per_group_n)
  sd_pool=sqrt((sd1**2+sd2**2+sd3**2+sd4**2)/4)
  se_pool=se_of_mean_difference(sd_pool, n, n_cells=4, within_between)
  d_calc=abs(m_diff/sd_pool) # note we force all positive and then fix later 
  tval=(m_diff)/se_pool |> abs()
  df=ifelse(within_between=="within", n-1, n-2)
  pval=pt(q=abs(tval), df=df, lower.tail=FALSE)*2
  d_se=se_of_d_between(d_calc, n) # d is scaled by the SD of the cells, not of the differences
  
  return(data.frame("df_1"=df,"df_2"=NA,"tstat"=tval, "fstat"=NA, "p_calc"=pval, "d_calc"=d_calc, "d_calc_se"=d_se, "N_calc"=NA, "ES"=m_diff, "SE"=se_pool))
}

#parse_mean_se4(foo, "between", 100)




parse_beta_se <- function(raw_stat){
  beta=str_extract(raw_stat, "b=.*\\(") |> str_sub(3,-2) |> as.numeric()
  se=str_extract(raw_stat, "\\(.*\\)") |> str_sub(2,-2) |> as.numeric()
  #zval=beta/se
  #pval=2*pnorm(zval, lower.tail=F)
  return(data.frame("df_1"=NA,"df_2"=NA,"tstat"=NA, "fstat"=NA, "p_calc"=NA, "d_calc"=NA, "d_calc_se"=NA, "N_calc"=NA, "ES"=beta, "SE"=se))
}


parse_beta_ci <- function(raw_stat){
  beta=str_extract(raw_stat, "b=.*\\[") |> str_sub(3,-2) |> as.numeric()
  low=str_extract(raw_stat, "\\[.*,") |> str_sub(2,-2) |> as.numeric()
  high=str_extract(raw_stat, ",.*\\]") |> str_sub(2,-2) |> as.numeric()
  se=(high-low)/(2*1.96)
  #zval=beta/se
  #pval=2*pnorm(zval, lower.tail=F)
  return(data.frame("df_1"=NA,"df_2"=NA,"tstat"=NA, "fstat"=NA, "p_calc"=NA, "d_calc"=NA, "d_calc_se"=NA, "N_calc"=NA, "ES"=beta, "SE"=se))
}

#this is just to deal with yang2018 where we have d's given and bci
#ex <- "d: 1.40 ; bci: b=1.72 [1.44, 2.00]"
parse_dbci <- function(raw_stat,n){
  bci = str_extract(raw_stat, ";.*") |> str_sub(2,-1)
  beta=str_extract(bci, "b=.*\\[") |> str_sub(3,-2) |> as.numeric()
  low=str_extract(bci, "\\[.*,") |> str_sub(2,-2) |> as.numeric()
  high=str_extract(bci, ",.*\\]") |> str_sub(2,-2) |> as.numeric()
  se=(high-low)/(2*1.96)
  t=beta/se
  df=n-1
  pval=pt(q=abs(t), df=df, lower.tail=FALSE)*2
  dval = str_extract(raw_stat, ".*;") |> str_sub(3,-2) |> as.numeric()
  d_se=se_of_d_between(dval, n)
  
  return(data.frame("df_1"=NA,"df_2"=NA,"tstat"=NA, "fstat"=NA, "p_calc"=pval, "d_calc"=dval, "d_calc_se"=d_se, "N_calc"=NA, "ES"=beta, "SE"=se))
}
#parse_dbci(ex)

foo <- "pear: r=-.75, n=505"
parse_pearson <- function(raw_stat){
  r=str_extract(raw_stat, "r=.*,") |> str_sub(3,-2) |> as.numeric()
  n=str_extract(raw_stat, "n=.*") |> str_sub(3,-1) |> as.numeric()
  d=2*r/sqrt(1-r**2) # using an approximation
  print(d)
  se=se_of_d_between(d, n) # ES is d here, so its SE is the SE of d
  t=d*sqrt(n)/2
  pval=pt(q=abs(t), df=n-2, lower.tail=FALSE)*2
  d_se=se
  
return(data.frame("df_1"=NA,"df_2"=NA,"tstat"=NA, "fstat"=NA, "p_calc"=pval, "d_calc"=d, "d_calc_se"=d_se, "N_calc"=NA, "ES"=d, "SE"=se))
  
}

parse_pearson(foo)


do_blanks <- function(){
  return(data.frame("df_1"=NA,"df_2"=NA,"tstat"=NA, "fstat"=NA, "p_calc"=NA, "d_calc"=NA, "d_calc_se"=NA, "N_calc"=NA, "ES"=NA, "SE"=NA))
}
do_parsing=function(raw_stat, within_between,n){
  if (is.na(raw_stat)) {return (do_blanks())}
  if (str_sub(raw_stat,1,1)=="t"){return(parse_t(raw_stat, within_between))}
  if (str_sub(raw_stat,1,1)=="F"){return(parse_f(raw_stat, within_between))}
  
  if (str_sub(raw_stat,1,4)=="prop"){return(parse_prop(raw_stat))}
  if (str_sub(raw_stat,1,7)=="rawprop"){return(parse_raw_prop(raw_stat))}
  
  if (str_sub(raw_stat, 1,4)=="MSD1"){return(parse_mean_sd1(raw_stat, within_between,n))} #comparision with 0 or chance as specified 
  if (str_sub(raw_stat, 1,4)=="MSE1"){return(parse_mean_se1(raw_stat, within_between,n))}
  
  if (str_sub(raw_stat, 1,4)=="MCI2"){return(parse_mean_ci2(raw_stat, within_between,n))}
  if (str_sub(raw_stat, 1,4)=="MSD2"){return(parse_mean_sd2(raw_stat, within_between,n))}
  if (str_sub(raw_stat, 1,4)=="MSE2"){return(parse_mean_se2(raw_stat, within_between,n))}
  
  if (str_sub(raw_stat, 1,4)=="MSD4"){return(parse_mean_sd4(raw_stat, within_between,n))}#for diff in diff
  if (str_sub(raw_stat, 1,4)=="MSE4"){return(parse_mean_se4(raw_stat, within_between,n))}

  if (str_sub(raw_stat, 1,3)=="bse"){return(parse_beta_se(raw_stat))}
  if (str_sub(raw_stat, 1,3)=="bci"){return(parse_beta_ci(raw_stat))}
  if (str_sub(raw_stat, 1,1)=="d"){return(parse_dbci(raw_stat,n))}
  if (str_sub(raw_stat, 1,4)=="pear"){return(parse_pearson(raw_stat))}
  
  
  return (do_blanks())
}

