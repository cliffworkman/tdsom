#########################################################################################################################

## Load packages
require("Hmisc")
require("xlsx")
require("psych")
require("tidyverse")
require("sjPlot")
require("sjstats")
require("lmerTest")

#########################################################################################################################

## Load data for reliability analysis
#setwd("C:\\Data\\")
setwd("C:\\Users\\cliff\\Dropbox\\Dropbox\\02_UChicago\\Dark Side of Morality MS\\Data")
tdsom_mcs <- read.csv('TDSoM_MCS_Data.csv')
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

## Load data for Figure 4
apvt_beh <- read.csv('TDSoM_Longform_Data.csv')
apvt_mcs <- read_csv('TDSoM_MCSfMRI_Data.csv')
apvt <- apvt_beh %>%
  left_join(apvt_mcs %>% 
              pivot_longer(bomb:welfare, names_to = "Issue", values_to = "MC"),
            by = c("Subject" = "Subject", "Issue"))
apvt <- apvt %>%
  mutate(ProtestView = case_when(
    str_detect(Thumbs, "up")   ~ "Support",
    str_detect(Thumbs, "down") ~ "Oppose",
    TRUE ~ NA_character_
  )) %>%
  mutate(Support.c = Support - 4) %>%
  mutate(ProtestCongruent = case_when(
    Support.c > 0 & ProtestView == "Support" ~  1,
    Support.c < 0 & ProtestView == "Oppose"  ~  1,
    Support.c < 0 & ProtestView == "Support" ~ -1,
    Support.c > 0 & ProtestView == "Oppose"  ~ -1,
    Support.c == 0 ~ 0
  )) %>%
  mutate(MC.c = MC - mean(MC, na.rm=TRUE))
apvt$Subject <- factor(apvt$Subject)
apvt$Issue   <- factor(apvt$Issue)

# Model MCS and appropriateness ratings for fMRI issues
apvt_mod <- lmer(Appropriate ~ Support.c + MC.c + ProtestCongruent + Support.c:MC.c + Support.c:ProtestCongruent + MC.c:ProtestCongruent + (1|Subject) + (1|Issue), data = apvt)
summary(apvt_mod)
effectsize::standardize_parameters(apvt_mod)

# Generate plot
plot_model(apvt_mod, type="pred", terms = c("ProtestCongruent"), colors = "v") +
  labs(title = element_blank(), y="Appropriateness Rating", x="Protest Congruency") +
  theme_classic() +
  geom_hline(yintercept = mean(apvt$Appropriate), linetype = "dashed", alpha=.3)
ggsave("APVT_Appropriate_SocialSupp.png", width=3.25, height=3, units = "in")

#########################################################################################################################