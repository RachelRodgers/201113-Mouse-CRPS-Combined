# 201113_MouseCRPSCombined_Format-Metadata.R

# Combining metadata for mouse CRPS cohort 1 and cohort 2.
# Needed variables:
#   sample, cage, week, condition ("control", "fracture"), num_reads,
#   crps_type("control", "acute", "chronic")   

library("tidyverse")

dataCohort1 <- readRDS("../data/metadata/sampleData_crps_cohort1.RDS")
dataCohort2 <- readRDS("../data/metadata/sampleData_crps_cohort2.RDS")

#----- Format cohort 1 -----#

weekNum <- c("0", "3", "5", "9", "16")
weekRenameLUT <- paste0("week_", weekNum)
names(weekRenameLUT) <- weekNum

formatCohort1 <- dataCohort1 %>% 
  select(SampleName, Cage, Week, Treatment, ReadsPerSample) %>% 
  dplyr::rename(sample = SampleName, cage = Cage, week = Week, 
                condition = Treatment,  num_reads = ReadsPerSample) %>% 
  mutate(week = weekRenameLUT[week],
         cohort = "cohort_1") %>% 
  filter(condition != "none")

#----- Format cohort 2 -----#

# read in cage data for cohort 2 (received Nov 14, 2020)
cageCohort2 <- read.delim("../data/cage_201019_LaraC_V4_Mouse_CRPS.txt",
                          stringsAsFactors = FALSE)

cageLUT <- cageCohort2 %>% 
  select(mouse_number, cage) %>% 
  deframe()

formatCohort2 <- dataCohort2 %>% 
  mutate(cage = cageLUT[mouse_number]) %>% 
  select(sample, cage, week, group, ReadsPerSample) %>% 
  dplyr::rename(condition = group, num_reads = ReadsPerSample) %>% 
  mutate(condition = ifelse(condition == "treatment",
                            yes = "fracture", no = "control"),
         cohort = "cohort_2")

#----- Combine -----#
# Combine, add crps type.

combinedMetadata <- rbind(formatCohort1, formatCohort2) %>% 
  mutate(crps_type = case_when(week %in% c("week_3", "week_5") & condition != "control" ~ "acute",
                               week %in% c("week_9", "week_16") & condition != "control" ~ "chronic",
                               week == "week_0" & condition != "control" ~ "baseline",
                               TRUE ~ "control"))

row.names(combinedMetadata) <- combinedMetadata$sample

write.table(combinedMetadata, 
            file = "../data/metadata/201113_Mouse_CRPS_combinedMetadata.txt",
            quote = FALSE, sep = "\t")
