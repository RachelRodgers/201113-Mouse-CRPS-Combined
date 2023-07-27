# 201113_Mouse_CRPS_Combined_RatioData.R

library("phyloseq")
library("tidyverse")

physeqBacteriaRare <- readRDS("../data/physeqObjects/physeqBacteria_rare.RDS")
# 877 x 207

phylumGlomDF <- physeqBacteriaRare %>% 
  tax_glom(taxrank = "Phylum", NArm = FALSE) %>% 
  transform_sample_counts(function(x) {x/sum(x)}) %>% 
  psmelt()

# Bacteroides and Firmicutes only
selectPhylaAbd <- phylumGlomDF %>% 
  filter(Phylum %in% c("Bacteroidetes", "Firmicutes")) %>% 
  select(sample, crps_type, cohort, Phylum, Abundance)

phylaAbdPivot <- selectPhylaAbd %>% 
  pivot_wider(names_from = Phylum,
              values_from = Abundance) %>% 
  mutate(ratio = Bacteroidetes/Firmicutes)

write.table(phylaAbdPivot, 
            file = "../analysis/results/bacteroidetes-firmicutes-ratio-combined-mouseCRPS-rarefied.txt",
            quote = FALSE, sep = "\t", row.names = FALSE)
