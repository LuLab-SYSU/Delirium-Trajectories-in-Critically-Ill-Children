library(lcmm)
model_formula <- CAPD ~ gender + age_month + preterm.birth + admission.diagnosis + surgery.experience + PICU.experience + comorbidity + developmental.delay + STRONGkids + PELOD2 + PCIS + infection + acid.base.disorders + oxygenation.index + renal.dysfunction + liver.dysfunction + WBC + NE + LY + PLT + NLR + PLR + PWR + SII + AlbG + CRP + IL.6 + PCT + HSST0TNT + Cl + Glu + PROT + red.blood.cell.transfusion + physical.restraint.sites + fasting + Benzodiazepines + Opium + Vasoactive.drugs + Corticosteroid + Anticholinergics + Phenobarbital + Chloral.hydrate + length.of.PICU.stay + Mvtime + Timeindex

# 手动拟合
model1 <- hlme(model_formula, 
               random = ~Timeindex,
               subject = 'Patient_ID',
               ng = 1,
               data = completed1,
               nwg = FALSE,
               maxiter = 100, 
               verbose = TRUE)
model2 <- gridsearch(rep = 5, 
                     maxiter = 100, 
                     minit = model1, 
                     hlme(model_formula, 
                          mixture = ~Timeindex, 
                          random = ~Timeindex, 
                          subject = 'Patient_ID', 
                          ng = 2, 
                          data = completed1, 
                          nwg = TRUE, 
                          verbose = TRUE))
summarytable(model2, 
             which = c("loglik","BIC","AIC","SABIC","entropy","conv","%class"), 
             display = TRUE)



# 自动拟合
max_classes <- 8
model_formula <- CAPD ~ gender + age_month + preterm.birth + admission.diagnosis + surgery.experience + PICU.experience + comorbidity + developmental.delay + STRONGkids + PELOD2 + PCIS + infection + acid.base.disorders + oxygenation.index + renal.dysfunction + liver.dysfunction + WBC + NE + LY + PLT + NLR + PLR + PWR + SII + AlbG + CRP + IL.6 + PCT + HSST0TNT + Cl + Glu + PROT + red.blood.cell.transfusion + physical.restraint.sites + fasting + Benzodiazepines + Opium + Vasoactive.drugs + Corticosteroid + Anticholinergics + Phenobarbital + Chloral.hydrate + length.of.PICU.stay + Mvtime + Timeindex
for (k in 1:max_classes) {
  cat("正在拟合", k, "类别模型...\n")
  if(k == 1){
    best_fit <- hlme(model_formula, 
                   random = ~Timeindex,
                   subject = 'Patient_ID',
                   ng = 1,
                   data = completed1,
                   nwg = FALSE,
                   maxiter = 100, 
                   verbose = TRUE)
  } else {
    best_fit <- gridsearch(rep = 5, 
                         maxiter = 100, 
                         minit = best_fit, 
                         hlme(model_formula, 
                              mixture = ~Timeindex, 
                              random = ~Timeindex, 
                              subject = 'Patient_ID', 
                              ng = k, 
                              data = completed1, 
                              nwg = TRUE, 
                              verbose = TRUE))
  }
  cat("完成", k, "类别模型拟合！\n\n")
}