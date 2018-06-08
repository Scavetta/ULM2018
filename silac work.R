# SILAC Analysis (Stable Isotope Labelling in Active Cells)
# Rick Scavetta
# 07 June 2018
# Case study for workshop

# clear workspace
rm(list = ls())

# Load packages
# Includes dplyr, ggplot2, purrr and other packages
library(tidyverse)

# Read in the data:
protein.df <- read.delim("Protein.txt")

# Examine the data:
summary(protein.df)
str(protein.df)
glimpse(protein.df)

# print to screen -- make a tibble
typeof(protein.df) # Before tibble
class(protein.df) # Before tibble
protein.df <- as_tibble(protein.df)
class(protein.df) # After 
protein.df # better print to screen

# Transformations
# log10 of intensities:
protein.df$Intensity.H <- log10(protein.df$Intensity.H)
protein.df$Intensity.M <- log10(protein.df$Intensity.M)
protein.df$Intensity.L <- log10(protein.df$Intensity.L)

# Add log10 intensities:
protein.df$Intensity.H.M <- protein.df$Intensity.H + protein.df$Intensity.M
protein.df$Intensity.M.L <- protein.df$Intensity.M + protein.df$Intensity.L

# log2 of ratios: HM & ML
protein.df$Ratio.H.M <- log2(protein.df$Ratio.H.M)
protein.df$Ratio.M.L <- log2(protein.df$Ratio.M.L)

# Exercises, p 58 & 59
subset(protein.df, Contaminant == "+")

nrow(subset(protein.df, Contaminant == "+"))

protein.df %>% 
  filter(Contaminant == "+") %>% 
  nrow()

# TRUE == T == 1
# FALSE == F == 0
# So.... you can do math on logical vectors!!
protein.df$Contaminant == "+"

sum(protein.df$Contaminant == "+")


# What proportion?
sum(protein.df$Contaminant == "+")/length(protein.df$Contaminant)
sum(protein.df$Contaminant == "+")/nrow(protein.df)

table(protein.df$Contaminant)/nrow(protein.df)
# e.g.
table(PlantGrowth$group)


# Remove contaminants:
protein.df <- subset(protein.df, Contaminant != "+")

# Remove the entire column:
protein.df$Contaminant <- NULL

# Exercise 2, p59
paste(c("GOGA7", "PSA6", "S10AB"), "MOUSE", sep = "_")
paste0(c("GOGA7", "PSA6", "S10AB"), "_MOUSE") # default sep = ""

subset(protein.df, Uniprot %in% paste0(c("GOGA7", "PSA6", "S10AB"), "_MOUSE"))

# or...
protein.df %>% 
  filter(Uniprot %in% paste0(c("GOGA7_MOUSE", "PSA6", "S10AB"),"_MOUSE"))

# Exercise 3, p59
subset(protein.df, Ratio.H.M.Sig < 0.05)
# or...
protein.df %>% 
  filter(Ratio.H.M.Sig < 0.05)

# Exercise 4, p59: HM ratio beyond [-2,2] - 
# NAs are removed!
subset(protein.df, Ratio.H.M > 2 | Ratio.H.M < -2)

protein.df %>% 
  filter(Ratio.H.M > 2 | Ratio.H.M < -2)


########

# Exercises 1-3, p64: similar to above, just using []
# Exercise 1, p64: (like exercise 2 above)
protein.df[protein.df$Uniprot %in% paste0(c("GOGA7", "PSA6", "S10AB"), "_MOUSE"),
           c("Uniprot", "Ratio.H.M", "Ratio.M.L")]

# Exercise 2, p64: use [] (like exercise 3 above)
protein.df %>% 
  filter(Ratio.H.M.Sig < 0.05)

protein.df[ protein.df$Ratio.H.M.Sig < 0.05 & !is.na(protein.df$Ratio.H.M.Sig), ]

# find non NAs with ! - produce a logical vector
!is.na(protein.df$Ratio.H.M.Sig)

# e.g. test if something is numeric: single T/F
is.numeric(protein.df$Peptides)

# Exercise 3, p64: In this case the NAs are included
protein.df[ protein.df$Ratio.H.M > 2 | protein.df$Ratio.H.M < -2 , ]

# Exercise 4+5: New, use [], or dplyr

# top 20 ML proteins
protein.df %>% 
  mutate(Uniprot = sub("_MOUSE.*$", "", Uniprot)) %>% 
  arrange(desc(Ratio.M.L)) %>% 
  .[1:20,] -> topML
  
# top 20 HM proteins
protein.df %>%
  mutate(Uniprot = sub("_MOUSE.*$", "", Uniprot)) %>% # clean names
  arrange(desc(Ratio.H.M)) %>% 
  .[1:20,] -> topHM

# Intersections between these vectors:
intersect(topHM$Uniprot, topML$Uniprot)

setdiff(topHM$Uniprot, topML$Uniprot)
setdiff(topML$Uniprot, topHM$Uniprot)

union(topHM$Uniprot, topML$Uniprot)


# Element 10: Introduction to dplyr

# start from the beginning:
rm(list = ls())
protein.df <- read.delim("Protein.txt", stringsAsFactors = FALSE)
glimpse(protein.df)

# Make a tibble
protein.df <- as_tibble(protein.df)
protein.df # Better print-out

# 3 main components to dplyr:
# 1 - %>%  pipe opertor (shift + ctrl + m)
1:10 %>% 
  mean()
mean(1:10) # old way

# 2 - 5 vers (the "grammar" of data analysis)
# 2a - filter
# 2b - arrange (lowest to highest "ascending")
# 2c - select (columns)
protein.df %>% 
  filter(Contaminant != "+") %>% 
  arrange(Ratio.H.M) %>% 
  select(Uniprot, Ratio.M.L, Ratio.H.M)

# 2c+ - Helper functions (see page 98)
protein.df %>% 
  filter(Contaminant != "+") %>% 
  arrange(Ratio.H.M) %>% 
  select(Uniprot, starts_with("R"), -ends_with("Sig"))  %>% 
  slice(1:20)

# Applying functions
# 2d - mutate, for Transformation functions
protein.df %>% 
  filter(Contaminant != "+") %>% 
  mutate(Ratio.M.L = log2(Ratio.M.L),
         Ratio.H.M = log2(Ratio.H.M))

# better, use the helper functions:
protein.df %>% 
  filter(Contaminant != "+") %>% 
  mutate_at(vars(starts_with("R"), -ends_with("Sig")), log2) %>% 
  mutate_at(vars(starts_with("Int")), log10) %>% 
  mutate(Intensity.H.M = Intensity.H + Intensity.M,
         Intensity.M.L = Intensity.M + Intensity.L) -> preview

# Go back to main tutorial:
# 2e - summarise, Aggregration functions
# 3 - group_by, an adverb for splitting

# Can you make this data tidy?
# Read in data, make tibble and remove contaminants:
read.delim("Protein.txt", stringsAsFactors = FALSE) %>% 
  as_tibble() %>% 
  filter(Contaminant != "+") -> protein.df

# Process intensities:
protein.df %>% 
  mutate_at(vars(starts_with("Int")), log10) %>%    # Calculate log10
  mutate(H.M = Intensity.H + Intensity.M,           # Add log10s and rename columns
         M.L = Intensity.M + Intensity.L) %>% 
  select(Uniprot, H.M, M.L) %>%                     # Take columns of interest
  gather(Ratio, Intensity, -Uniprot) -> onlyInt     # gather and save

# Process Ratios:
protein.df %>% 
  select(Uniprot, starts_with("Rat"), -ends_with("Sig")) %>%     # Calculate log2
  gather(Ratio, Expression, -Uniprot) %>%                        # Gather
  filter(Ratio != "Ratio.H.L") %>%                               # Remove uninteresting H.L Ratio
  mutate(Ratio = recode_factor(Ratio,                            # Relabel ratios to match the Int data frame
                               `Ratio.M.L` = "M.L",
                               `Ratio.H.M` = "H.M")) %>% 
  group_by(Ratio) %>%                                            # group according to ratios (2 groups)
  mutate(Expression = scale(Expression)[,1]) -> onlyRatios       # Apply z-score and save

# Process significant values and merge to intensities and ratios:
protein.df %>% 
  select(Uniprot, ends_with("Sig")) %>%                          # Take columns of interest
  gather(Ratio, Significance, -Uniprot) %>%                      # Gather
  filter(Ratio != "Ratio.H.L.Sig") %>%                           # Remove uninteresting H.L Ratio
  mutate(Ratio = recode_factor(Ratio,                            # Relabel ratios to match the Int data frame
                               `Ratio.M.L.Sig` = "M.L",
                               `Ratio.H.M.Sig` = "H.M"),
         SigCat = cut(Significance,                              # Make colour labels for sig values
                      c(-Inf, 1e-11, 1e-4, 0.05, Inf),
                      c("red", "orange", "blue", "grey30"))) %>%
  full_join(onlyRatios) %>%                                      # Merge with the log2 ratios
  full_join(onlyInt) %>%                                         # Merge with the Intensities
  filter(complete.cases(.), Uniprot != "") %>%                   # Take only observations that have complete data and non-empty Uniprot 
  arrange(desc(Significance)) -> allData                         # Order according to sig so that low sig are plotted first

# Make a plot
ggplot(allData, aes(Expression, Intensity, col = SigCat)) +
  geom_point(alpha = 0.5, shape = 16) +
  # scale_x_continuous(limits = c(-5,5)) +
  scale_colour_identity() +
  facet_grid(. ~ Ratio) +
  theme_classic()
