library(mice)
library(miceadds)
meth <- make.method(full_data)
meth[c("IL.6", "PCT", "HSST0TNT", "Cl", "Glu", "PROT")] <- "pmm"
meth[c("RASS", "CAPD")] <- "2l.pmm"
pred <- make.predictorMatrix(full_data)
pred[, "Patient_ID"] <- -2
pred["Patient_ID", ] <- 0
imp_full <- mice(full_data, m = 500, method = meth, predictorMatrix = pred, cluster = "Patient_ID", seed = 123)
completed1 <- complete(imp_full, 1)