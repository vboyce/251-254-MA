#helper function for pooling when needed
pool_m_sd <- function(m1, sd1, n1, m2, sd2, n2){
  m=(m1*n1+m2*n2)/(n1+n2)
  var=(n1*(sd1**2+m1**2)+n2*(sd2**2+m2**2))/(n1+n2)-m**2
  sd=sqrt(var)
  return(c(m,sd))
  
}

d_es <-  test |> 
  mutate(target_d_calc=abs(target_d_calc),
         target_ES=abs(target_ES),
         rep_d_calc=case_when(
           same_dir=="yes" ~ abs(rep_d_calc),
           same_dir=="no" ~-abs(rep_d_calc),
           T ~ as.numeric(NA)
         ),
         rep_ES=case_when(
           same_dir=="yes" ~ abs(rep_ES),
           same_dir=="no" ~-abs(rep_ES),
           T ~ as.numeric(NA)
         ))


do_pred_int <- function(target_ES, target_SE, rep_ES, rep_SE){
  if(!is.na(target_ES)&!is.na(rep_ES)&!is.na(target_SE)&!is.na(rep_SE)){
    return(Replicate::pred_int(target_ES, target_SE**2, rep_ES, rep_SE**2)$rep.inside)
  }
  return(NA)
}

do_p_orig <- function(target_ES, target_SE, rep_ES, rep_SE){
  if(!is.na(target_ES)&!is.na(rep_ES)){
    return(Replicate::p_orig(target_ES, target_SE**2,rep_ES, t2=0, rep_SE**2))
  }
  return(NA)
}

do_subjective_rep <- function(replicated_instructor_code, replicated_report_code, adjudicated_replication_code){
  if (is.na(replicated_instructor_code)){return(as.numeric(NA))}
  if (is.na(replicated_report_code)){return(as.numeric(NA))}
  if (replicated_instructor_code==replicated_report_code){ return(replicated_instructor_code)}
  if (!is.na(adjudicated_replication_code)){return (adjudicated_replication_code)}
  return(as.numeric(NA))
}