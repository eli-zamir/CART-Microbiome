library(dplyr)
library(psych)
library(ggplot2)



MDall <- Metadata_compiled_Update_04_24_22
MDall <- subset(MDall, Exclusion == "no")

MDall$Abx [MDall$Abx_RiskStrata == "none"] <- "none_LR"
MDall$Abx [MDall$Abx_RiskStrata == "low-risk"] <- "none_LR"
MDall$Abx [MDall$Abx_RiskStrata == "high-risk"] <- "HR"
#table(MDall$Abx)

####### CRP, LDH, T cells levels at Lymphodepletion and other time points ()

MDall$Abx <- factor(MDall$Abx, levels = c("none_LR", "HR"))
ggplot(MDall, aes(x = Abx, y = LDH_apharesis, color = Abx)) + 
  geom_boxplot (binaxis = 'y', stackdir = 'center', fill = "grey90") + geom_point() + scale_y_continuous(limits = c(150,1600), breaks = seq(0, 1600, by = 400)) +
  theme_classic() + scale_color_manual(values=c("#2E9FDF", "#E7B800"))
# use as additional parameters "LDH_depletion", "CRP_apharesis", "CRP_depletion", "CD3_mul_Aph", "CD4_mul_Aph",  "CD8_mul_Aph"

#statistical testing by wilcoxon test (unpaired group comparisons)
wilcox.test(LDH_apharesis~Abx, data = MDall, paired = FALSE, alternative = c("two.sided"))

### comparison for serum / T cell parameters across Abx strata "none", "low-risk", "high-risk"
MDall$Abx_RiskStrata <- factor(MDall$Abx_RiskStrata, levels = c("none", "low-risk", "high-risk"))
ggplot(MDall, aes(x = Abx_RiskStrata, y = LDH_depletion, color = Abx_RiskStrata)) + 
  geom_boxplot (binaxis = 'y', stackdir = 'center', fill = "grey90") + geom_point() + scale_y_continuous(limits = c(150,1600), breaks = seq(0, 1600, by = 400)) +
  theme_classic() + scale_color_manual(values=c("#2E9FDF", "#60b042", "#E7B800"))

pairwise.wilcox.test(MDall$LDH_depletion, MDall$Abx_RiskStrata, ### with FDR correction
                     p.adjust.method = "BH")

# group statistics
group_by(MDall, Abx_RiskStrata) %>% filter(!is.na(CRP_depletion)) %>% summarise(
  count = n(), 
  mean = mean(CRP_depletion, na.rm = TRUE),
  sd = sd(CRP_depletion, na.rm = TRUE)
)





