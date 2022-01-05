# Prepare visualizations for the BTK Max Route innovation paper

library(tidyverse)
library(readxl)

df <- read_csv("BTKMax_TidyData.csv")

# SnAr HTE Round 1 --------------------------------------------------------

SnAr <- df %>% filter(Step == "SnAr")

SnAr <- SnAr %>%
  filter(Temp_C == 100)

SnAr_Plot <- ggplot(SnAr, aes(x = `AP_PDT`, y = Base))+
  geom_point(aes(color = Solvent), size = 3, alpha = 0.7)+
  labs(x = "HPLC Area Percent Product", y = "Base")

SnAr_Plot

# Indole HTE Round 1 ------------------------------------------------------


Indole <- df %>% filter(Step == "Indole") %>% filter(Reaction_Round == "Round 1")

Indole$Substrate <- as.character(Indole$Substrate)


Indole_Plot <- ggplot(Indole, aes(x = `AP_PDT`, y = Lewis_Acid))+
  geom_point(aes(color = Substrate, shape = Solvent), size = 3, alpha = 0.7)+
  labs(x = "HPLC Area Percent Product 21", y = "Lewis Acid")

Indole_Plot


# Indole HTE Round 2 ------------------------------------------------------


Indole_2 <- df %>% filter(Step == "Indole") %>% filter(Reaction_Round == "Round 2")
Indole_2$Temp_C <- as.character(Indole_2$Temp_C)

Indole_2_Plot <- ggplot(Indole_2, aes(x = `AP_PDT`, y = Lewis_Acid))+
  geom_point(aes(color = Solvent), size = 3, alpha = 0.7)+
  labs(x = "HPLC Area Percent Product 21", y = "Lewis Acid")

Indole_2_Plot


# Amidation Rounds 1 and 2 ------------------------------------------------

Amidation <- df %>% filter(Step == "Amidation") %>% filter(Reaction_Round != "Round 3")

Amidation$Base[Amidation$Base == "1-Methylimidazole"] <- "NMI"
Amidation$Base[Amidation$Base == "2,6-Lutidine"] <- "Lut"
Amidation$Base[Amidation$Base == "N-methylmorpholine"] <- "NMM"
Amidation$Base[Amidation$Base == "Triethylamine"] <- "TEA"

Amidation$Temp_C <- as.character(Amidation$Temp_C)

Amidation_Plot <- ggplot(Amidation, aes(x = AP_PDT, y = Activator))+
  geom_point(aes(color = Base, shape = Solvent), size = 3, alpha = 0.7)+
  labs(x = "HPLC Area Percent 1", y = "Reagent")

Amidation_Plot


# Amidation Round 3 -------------------------------------------------------


Amidation_3 <- df %>% filter(Step == "Amidation") %>% filter(Reaction_Round == "Round 3")

Amidation_3$Solvent[Amidation_3$Solvent == "DIMETHYLFORMAMIDE"] <- "DMF"
Amidation_3$Solvent[Amidation_3$Solvent == "N-METHYL PYRROLIDINONE"] <- "NMP"
Amidation_3$Solvent[Amidation_3$Solvent == "TETRAHYDROFURAN"] <- "THF"
Amidation_3$Solvent[Amidation_3$Solvent == "ACETONITRILE"] <- "MeCN"
Amidation_3$Solvent[Amidation_3$Solvent == "ACETONE"] <- "Acetone"
Amidation_3$Solvent[Amidation_3$Solvent == "2-METHYLTETRAHYDROFURAN"] <- "MeTHF"


Amidation_3 <- ggplot(Amidation_3, aes(x = Activator, y = `AP_PDT`))+
  geom_jitter(aes(color = Base, shape = Solvent), size = 3, alpha = 0.7, width = 0.2, height = 0)+
  labs(x = "Reagent", y = "HPLC Area Percent 1")

Amidation_3


# C-N Coupling ------------------------------------------------------------

Catalysis_Data <- df %>% filter(Step == "Pd CN Coupling")
Catalysis_Data_Short <- Catalysis_Data %>%
  filter(Precatalyst %in% c("[(Allyl)PdCl]2", "[(Allyl)Pd]OTf")) %>%
  filter(Time_h > 10)

Catalysis_Ligand <- Catalysis_Data_Short %>%
  group_by(Ligand) %>%
  summarize(mean_Ligand = mean(AP_PDT),
            counts = n()) %>%
  arrange(desc(mean_Ligand)) %>%
  slice(1:12)

Catalysis_Ligand_List <- c(Catalysis_Ligand$Ligand)

Catalysis_Data_Short <- Catalysis_Data_Short %>%
  filter(Ligand %in% Catalysis_Ligand_List)

Catalysis_Data_Short$Precatalyst[Catalysis_Data_Short$Precatalyst == "[(Allyl)Pd]OTf"] <-  "[(Allyl)PdCl]2 + AgOTf"

Catalysis_Data_Short$`Ligand/Metal` <- ifelse(str_detect(Catalysis_Data_Short$Precatalyst, "AgOTf"), Catalysis_Data_Short$Ligand_Equiv/Catalysis_Data_Short$Precatalyst_Equiv, (Catalysis_Data$Ligand_Equiv/Catalysis_Data$Precatalyst_Equiv)/2)

Catalysis_Data_Short <- Catalysis_Data_Short %>% filter(`Ligand/Metal` < 2.9)

Catalysis_1 <- ggplot(Catalysis_Data_Short, aes(x = AP_PDT, y = Ligand))+
  geom_jitter(aes(color = Solvent, size = `Ligand/Metal`), alpha = 0.7, width = 0.2, height = 0)+
  facet_wrap(~Precatalyst)+
  ggtitle("Palladium Catalyzed C-N Coupling")+
  labs(x = "HPLC Area Percent Product", y = "Ligand")

Catalysis_1