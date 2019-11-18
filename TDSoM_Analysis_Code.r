#########################################################################################################################

## Load packages
require("Hmisc")
require("xlsx")
require("psych")

#########################################################################################################################

## Load data for reliability analysis
#setwd("C:\\Data\\")
setwd("C:\\Users\\cliff\\Dropbox\\Shared\\_WORK\\UChicago\\Political_Violence\\00_MANUSCRIPT\\Data\\")
tdsom_mcs <- read.csv('TDSoM_MoralCon_Data.csv')
if ("tdsom_mcs" %in% search()) {
  detach(tdsom_mcs)
}
attach(tdsom_mcs)
alpha(tdsom_mcs)

#########################################################################################################################

## Load data for correlation analyses
tdsom_corr <- read.csv('TDSoM_Wideform_Data.csv')
if ("tdsom_corr" %in% search()) {
  detach(tdsom_corr)
}
attach(tdsom_corr)

# Calculate means and SDs of variables
mean(tdsom_corr$MoralCon_CoreBeliefs_Lib.Con)
  sd(tdsom_corr$MoralCon_CoreBeliefs_Lib.Con)
mean(tdsom_corr$JSI_Vic)
  sd(tdsom_corr$JSI_Vic)

# Caculate correlations
correlations = rcorr(as.matrix(tdsom_corr), type="spearman")
corr_out.r = data.frame(correlations$r); corr_out.p = data.frame(correlations$P); corr_out.n = data.frame(correlations$n)
write.xlsx(corr_out.r,file=paste0("TDSoM_Correlations.xlsx"),sheetName="spearmans_rho")
write.xlsx(corr_out.p,file=paste0("TDSoM_Correlations.xlsx"),sheetName="p-values", append=TRUE)
write.xlsx(corr_out.n,file=paste0("TDSoM_Correlations.xlsx"),sheetName="Ns", append=TRUE)

#########################################################################################################################