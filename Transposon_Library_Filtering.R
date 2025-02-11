# Transposon Mutant Library Filtering
# Author: Huda Ahmad
# Date: 2023-11-21

# Load necessary libraries
library(readxl)
library(dplyr)
library(openxlsx)
library(highcharter)
library(tidyr)

# 1. Dataset Processing
# ----------------------

# Import and name the library file (Update file path)
library_data <- read_excel("path/to/transposon_mutant_library.xlsx")

# Check how many mutants are "intergenic" and remove them
sum(grepl("intergenic", library_data$`Locus`))
library_data <- library_data %>% filter(!grepl("intergenic", library_data$`Locus`))

# Create a column containing gene deletion percentage
library_data <- library_data %>%
  mutate(
    From = as.numeric(From),
    To = as.numeric(To)
  ) %>%
  mutate(
    `Deletion Percentage` = ((To - Position + 1) / (To - From + 1)) * 100
  ) %>%
  mutate(
    `Deletion Percentage` = round(`Deletion Percentage`, 2)
  )

# Manually verify deletion percentage for one mutant
library_data$To[1] - library_data$From[1] + 1 # Total gene length
library_data$To[1] - library_data$Position[1] + 1 # Deleted gene length
321 / 636 * 100 # Percentage calculation

# 2. Visualization
# ----------------------

# 2.1 Plot the number of unique genes by deletion percentage
gene_length <- sapply(seq(0,100,5), function(j){
  length(unique(library_data$`Locus`[which(library_data$`Deletion Percentage`>=j)]))
})

barplot(
  gene_length,
  names.arg = seq(0, 100, 5),
  las = 2,
  col = "black",
  xlab = "Deletion Percentage",
  ylab = "Number of Unique Genes",
  main = "Number of Unique Genes by Deletion Percentage"
)
box()

# 2.2 Interactive highcharter visualization
library_df <- data.frame(gene_length = gene_length, cutoff = seq(0, 100, 5))
hc <- library_df %>%
  hchart('column', hcaes(x = cutoff, y = gene_length)) %>%
  hc_xAxis(title = list(text = "Deletion Percentage"), tickInterval = 5) %>%
  hc_yAxis(title = list(text = "Number of Unique Genes")) %>%
  hc_colors("#8B0000")
hc

# 3. Filtering the library based on 20% deletion cutoff
# ----------------------

# Before filtering
nrow(library_data) # Total mutants
length(unique(library_data$`Locus`)) # Unique genes

# Apply 20% deletion cutoff
Filtered_Library <- library_data %>%
  group_by(`Locus`) %>%
  filter(`Deletion Percentage` >= 20)  

# Best unique mutants (earliest insertion = more deletion)
Best_Filtered_Library <- Filtered_Library %>%
  group_by(`Locus`) %>%
  filter(row_number() == which.min(Position)) 

# Extract remaining mutants and select second-best
rest <- Filtered_Library %>%
  group_by(`Locus`) %>%
  filter(row_number() != which.min(Position))  

second_Best <- rest %>%
  group_by(`Locus`) %>%
  filter(row_number() == which.min(Position))  

# 4. Save the filtered file
# ----------------------
write.xlsx(Best_Filtered_Library, "path/to/Best_Mutants.xlsx")
write.xlsx(second_Best, "path/to/Second_Best_Mutants.xlsx")
