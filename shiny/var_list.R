continuous_vars <- c("hba1c_mmol","hba1c_pct","bmi","age_diag","homa_b","homa_ir",
                     "hdlc","ldlc","totalc","tgl","egfr")
names(continuous_vars) <- c("HbA1c (mmol/mol)","HbA1c (%)","BMI","Age in years","HOMA-B","HOMA-IR",
                            "HDLc","LDLc","Totalc","Triglycerides","eGFR")

categorical_vars <- c("family_history","male","insulin","metformin","sulfonylureas",
                      "tzd","dpp4","sglt2","glp1ra","agi")
names(categorical_vars) <- c("Family history","Male","Insulin","Metformin","Sulfonylureas",
                             "TZD","DPP4","SGLT2","GLP1RA","AGI")


var_levels <- c(continuous_vars,categorical_vars)