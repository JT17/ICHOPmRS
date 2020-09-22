# add the fields needed to create appropriate categorical/binary patient groups

combine_similar_comorbidities <- function(input_df, age_cutoff = 65, bmi_cutoff = 30,
                                          extra_combined=F,race_ethnicity=F){
  input_df$home_o2[input_df$home_o2==1] = 0
  input_df$home_o2[input_df$home_o2==2] = 1
  end_id <- ncol(input_df)
  input_df$bmi_numeric <- input_df$bmi
  input_df$bmi_threshold<- ifelse(input_df$bmi_numeric>=bmi_cutoff,1,0)
  input_df$sex_category <- input_df$sex
  input_df$readmit <- ifelse(input_df$n.visits==1, 0,1) # readmitted at any point?
  input_df$smoking_status <- input_df$smoking 
  input_df$chf <- ifelse(input_df$hf==0,0,1) # if any form of hf
  input_df$pulm_dz <- ifelse(input_df$copd | input_df$asthma | input_df$ild | input_df$osa |input_df$other_pulm_dz, 1,0) #any pulmonary disease
  input_df$renal_dz <- ifelse(input_df$ckd |input_df$esrd, 1,0) # all renal disease
  input_df$viral_hepatitis<-ifelse(input_df$hep_b |input_df$hep_c,1,0) # viral hepatitis
  input_df$cancer <- ifelse(input_df$liquid_cancer | input_df$solid_cancer,1,0) # cancer
  input_df$transplant_hx <- ifelse(input_df$kidney_transplant | input_df$liver_transplant | input_df$bmt_transplant | input_df$ot_transplant,1,0) # transplant
  input_df$rheum_dz <- ifelse(input_df$ot_rheum | input_df$sle| input_df$ra,1,0) # prior rheum conditions
  input_df$non_oral_steroids<-ifelse(input_df$inhaled_steroids | input_df$nasal_steroids,1,0) # divide between inhaled/oral steroids
  input_df$immunosuppressed_state<-ifelse(input_df$chemo | input_df$radiation | input_df$inherited_immunodeficiency ,1,0) # divide between inhaled/oral steroids
  input_df$no_comorbs<-ifelse(input_df$cad | input_df$cirrhosis | input_df$cva | input_df$dm | input_df$hiv | input_df$htn | input_df$ibd | input_df$copd|input_df$asthma|input_df$ild|input_df$osa|input_df$chf|input_df$pulm_dz|input_df$renal_dz|input_df$viral_hepatitis|input_df$cancer|input_df$transplant_hx|input_df$rheum_dz|input_df$immunosuppressed_state|input_df$bmi_threshold,0,1)
  input_df$no_comorbs[is.na(input_df$no_comorbs)]=0
  
  if(extra_combined){
    input_df$immunosupression_drug <- ifelse(input_df$prednisone_under_20mg| input_df$prednisone_over_20mg | input_df$tnf_alpha_inhibitor |input_df$tacrolimus | input_df$cyclosporine | input_df$mtor_inhibitor | input_df$mycophenolate | input_df$azathioprine | input_df$methotrexate | input_df$other_immunosuppression_drug,1,0)
    input_df$cv_dz <- ifelse(input_df$cad | input_df$cva | input_df$htn | input_df$chf, 1,0)
    if(race_ethnicity){
      input_df$asian = ifelse(input_df$race_ethnicity == "Asian",1,0)
      input_df$black = ifelse(input_df$race_ethnicity == "Black or African American",1,0)
      input_df$hispanic = ifelse(input_df$race_ethnicity == "Hispanic or Latino",1,0)
      input_df$white = ifelse(input_df$race_ethnicity == "White",1,0)
    }
  }
  
  # Stratify age groups
  input_df$numeric_age = input_df$age
  for(i in 1:length(input_df$numeric_age)) {
    if(input_df$numeric_age[i]>65) {
      input_df$char_age[i]<-"age_65_plus"} else if (input_df$numeric_age[i]>54) {
        input_df$char_age[i]<-"age_55_64"} else if (input_df$numeric_age[i]>34) {
          input_df$char_age[i]<-"age_35_54"} else if (input_df$numeric_age[i]>18) {
            input_df$char_age[i]<-"age_18_34"} else {
              input_df$char_age[i]<-"age_0_17"}
  }
  
  input_df$char_age <- as.factor(input_df$char_age)
  return (input_df)
}

#as input, need to input dataframe, path to the file for mapping, and the original length of the dataframe
#i might move that code to the above function but basically in order to add the new variable names, you
#need to figure out what was added. orig_length is that input. It's a hack, we can figure this out as a group
generate_additional_mappings <- function(input_df, map_file_path, orig_length,bmi_cutoff = 30) {
  new_vars <- names(input_df[,(orig_length+1):ncol(input_df)])
  mapping <- read.csv(paste0(map_file_path),row.names=2)
  mapping$NewFields <- rownames(mapping)
  mapping$Local <- F
  # Attach additional variables created for characterization tables and attach tags and english dictionary for a local mapping key
  extra_maps<-as.data.frame(matrix(nrow=length(new_vars),ncol=5))
  rownames(extra_maps)<-new_vars
  colnames(extra_maps)<-names(mapping)
  extra_maps$NewFields<-new_vars
  
  # This part is unfortunately still hard coded to align the new tags and translations manually.  Right now if you need to add an additional field in order to perform analysis, it needs to be appended to the end of these two lists.

  extra_maps$Tag<-c("Demographic","Comorbidity","Demographic","Demographic","Demographic","Comorbidity","Comorbidity","Comorbidity","Comorbidity","Comorbidity","Comorbidity","Comorbidity","Medication","Cormobidity","Comorbidity","Demographic","Demographic")
  extra_maps$English<-c("BMI",paste0("BMI > ", bmi_cutoff), "Sex","Readmission","Smoking Status","Congestive Heart Failure","Pulmonary Disease","CKD","Hepatitis B or C","Active Malignancy","Prior Transplant", "Rheumatological Disease","Inhaled/Nasal Steroids","Immunosuppressed State","No Major Comorbidities","Age","Age Category")

  extra_maps$Local <- T
  
  # Add tags and english translations
  local_mapping <- rbind.data.frame(mapping,extra_maps)[,c("NewFields","Tag","English")]
  demos <- local_mapping$NewFields[which(local_mapping$Tag=="Demographic")]
  comorbs <- local_mapping$NewFields[which(local_mapping$Tag=="Comorbidity")]
  meds <- local_mapping$NewFields[which(local_mapping$Tag=="Medication")]
  comps <- local_mapping$NewFields[which(local_mapping$Tag=="Complication")]
  symptoms <- local_mapping$NewFields[which(local_mapping$Tag=="Symptom")]
  hosp_meds <- local_mapping$NewFields[which(local_mapping$Tag=="Hospital Med")]
  
  #R doesn't provide a great way to return multiple objects, so I'll return it as a list
  return (list(demos = demos, comorbs = comorbs, meds = meds, comps=comps, symptoms=symptoms,hosp_meds=hosp_meds, local_mapping = local_mapping))
}
