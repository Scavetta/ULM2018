# Intro to R
# Rick Scavetta
# 6.6.18
# IGRADU DA workshop

# Clear the workspace
rm(list = ls())

# Load packages
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)

# Call my functions:
# source("myFuncs.R")

# R syntax:
n <- log2(8)
n # short cut for print(n)

# A simple workflow:
# A built-in dataset:
PlantGrowth

# What are the groups?
levels(PlantGrowth$group)

# How many are there?
nlevels(PlantGrowth$group)

# Descriptive stats:
# ALL 30 values
mean(PlantGrowth$weight)

# Group-wise means:
# The apply family way:
tapply(PlantGrowth$weight, PlantGrowth$group, mean)

# The dplyr way: (first load package)
# shift+ctrl+m for the "pipe" operator, say "and then"

# <- use ctrl+- or alt+- for the assign operator
PGSummary <- PlantGrowth %>%
  group_by(group) %>%
  summarise(avg = mean(weight),
            stdev = sd(weight),
            med = median(weight))

# Make plots: (make sure to load ggplot2)
# 3 parts: data, aesthetics, geometries
# aesthetics: MAPPING data onto a visual scale (axes)
# e.g. x, y, col, shape, size

# save the first two layers as an object
g <- ggplot(PlantGrowth, aes(group, weight))

# Now add geometry: How the data will look
# Points: see shapes on page 67
g +
  geom_point(shape = 1,
             position = position_jitter(0.2))

# Boxplot:
g +
  geom_boxplot()

# Try a histogram: use the "x" and "fill" scales
# "identity" means they appear at the actual value,
# not to stack them. alpha controls transparency.
ggplot(PlantGrowth, aes(weight, fill = group)) +
  geom_histogram(position = "identity", alpha = 0.4)

# Do statistics:
# Group differences
# First build a linear model:
plant.lm <- lm(weight ~ group, data = PlantGrowth)
plant.lm

# t-test:
summary(plant.lm)

# summary is just a generic function
# The operates differently dependent
# on the input! e.g.
summary(PlantGrowth)

# ANOVA: pvalue = 0.01591
anova(plant.lm)

# Post-hoc tests:
# for doing tukeyHSD use aov() instead of lm()
# then use the results in tukeyHSD(), and...
# to get the ANOVA table, use summary() on the aov() output

# Make a report using a different data set:
chickwts

# Element 2: Functions
# Everything that happens,
# is because of a function

34 + 6
`+`(34, 6)

# Order of operations:
# BEDMAS - Brackets, Exp, Div, Mul, Add, Sub
2 - 3/4 # 1.25
(2 - 3)/4 # -0.25

# Make some objects:
n <- 34
p <- 6

# use like numbers
n + p

# Exercies 1, p 27:
m <- 1.12
b <- -0.4
m * 3 + b
m * 8 + b
m * 9 + b
m * 23 + b

# Generic form of functions:
# fun_name(fun_args)

# fun_args may be names of unnamed
# We can use names or only position
log2(8)
# the same as: use long form & names
log(x = 8, base = 2)
# long form & positional matching
log(8, 2)
# combination (typical)
log(8, base = 2)

# confusing: :(
log(2, x = 8)

# Some basic and common functions:
# Combine/concatenate: unnamed arguments
xx <- c(3, 8, 9, 23)
xx

# With characters
myNames <- c("healthy", "tissue", "quantity")

# Seqential numbers: seq()
seq(from = 1, to = 100, by = 7)
foo1 <- seq(1, 100, 7)

# use objects in functions:
foo2 <- seq(1, n, p)

# regular sequence with interval 1
# The : operator
1:10
seq(1, 10, 1)

# Two major types of math functions:
# 1 - Transformation
# addition, division, normalise
# (e.g. Z-score) (x_i - x_bar)/SD
# output length == input length
log2(foo1)
sqrt(foo1)

# 2 - Aggregration functions
# 1 (or a small # of) output
# e.g. mean, sd, etc.
mean(foo1)
sum(foo1) # addition
prod(foo1) # multiplication
length(foo1) # n

# Exercise 2, p30:
foo2 + 100 # Trans
foo2 + foo2 # Trans
sum(foo2) + foo2 # Trans (with aggr)
1:3 + foo2 # trans

#############################################################
############# Key Point # 1 #################################
############# Vector Recycling ##############################
#############################################################

1:4 + foo2

# e.g. Z-scores
# (x_i - x_bar)/SD
(foo1 - mean(foo1))/sd(foo1)

# or...
scale(foo1)

# Exercise 3, p30:
m * xx + b
# It doesn't matter how long xx is:
# m * seq(1, 100, 0.1) + b

# Exercise 4, p30:
# What if I had two slopes:
m2 <- c(0, 1.12)
# goal: get 8 values
m2 * xx + b

0 * xx + b
1.12 * xx + b

# Make a function:
equation <- function(x, m = 1.12, b = -0.4) {
  x * m + b
}

equation(xx) # default args
equation(xx, 123, 1242) # change both m and b
equation(xx, 123) # change m only
equation(xx, b = 1242) # change b only

# Call the function many times:
# Make sure to load purrr
# "map" each "element" of m2 to a function:
# use . for a place-holder
map(m2, ~ equation(xx, .))

# Element 3: Objects
# Anything that exists is an object
# common data storage:

# Vectors - 1 Dimensional, homogenous
# e.g.
foo1 # 15 "elements"
foo2 # 6 "elements"
myNames

# 4 most common user-defined
# Atomic vector types
# Logical - TRUE/FALSE, T/F, 1/0 (boolean, binary)
# Integer - 0,1,3,6,8 (whole)
# Double - 3.14, 6.25, 12.25 (real)
# Character - "x"

typeof(foo1)
typeof(myNames)

foo3 <- c("Liver", "Brain", "Testes",
          "Muscle", "Intestine", "Heart")

foo4 <- c(T, F, F, T, T, F)
typeof(foo4)

# Homogenous data types:
test <- c(1:10, "bob")
test

# remove bob:
test[-11]
# Still character :/

# Major problem # 1: Wrong type
# Solution: Coercion
mean(test)
test <- as.numeric(test)
test
mean(test, na.rm = T)

# Lists: 1 Dimensional, heterogenous
# e.g.
typeof(g) # our plot
typeof(plant.lm)

# Each element is named: access with $
plant.lm$coefficients # 3 element long numerical vector
plant.lm$fitted.values # 30 fitted values

# access the names using:
# Accessor Function
names(plant.lm)

# the names are "meta data", i.e. attributes:
attributes(plant.lm)

# Class by accessor function
class(plant.lm)
class(g)
# classes tell R what to do with this object

# Data frame: A special case of a list
# 2 dimensional, heterogenous
# a collection of vertical vectors
# of the same lengths!
foo.df <- data.frame(foo4, foo3, foo2)
foo.df
# Rows == observations
# Columns == variables

# change the attributes
names(foo.df) <- myNames
foo.df

# Basic functions:
dim(foo.df) # row, cols
# Don't use length! (only w/ 1D)
str(foo.df)
glimpse(foo.df) # in dplyr
summary(foo.df)

# Major problem # 2: Wrong data structure
# Solution: Rearrange or Coercion
# Use functions to find out:
typeof()
class()

# Element 4: Logical Expressions
# Asking and combining questions
# Relational operators: Yes/No questions
# == equivalency
# != non-equivalency
# !x the negation of x, where x is a logical vector
# <, >, <=, >=

!foo4
n > p
p < p

# The output is ALWAYS a logical vector!

# don't get confused! see ==, <=, and...
# <- assign (or =, but don't do that)
# = arguments in a function

# Logical operators: Combine Yes/No questions
# & AND - TRUE in EVERY question
# | OR  - TRUE in AT LEAST one question
# %in% WITHIN - equates to == with |

# Apply to logical data
# All healthy
subset(foo.df, foo.df$healthy == TRUE)
subset(foo.df, foo.df$healthy)
subset(foo.df, healthy)

# The dplyr way: The filter verb
foo.df %>%
  filter(healthy)

# All false:
foo.df %>%
  filter(healthy == F)

foo.df %>%
  filter(!healthy)

# Apply to numeric data (int or dbl)
# below 10
foo.df %>%
  filter(quantity < 10)

# Exactly 31
foo.df %>%
  filter(quantity == 31)

# Range: 10-20
foo.df %>%
  filter(quantity > 10 & quantity < 20)

# What happened:
foo.df$quantity > 10
foo.df$quantity < 20

# What if: Meaningless
foo.df %>%
  filter(quantity > 10 | quantity < 20)

# Extremes: beyond 10-20
foo.df %>%
  filter(quantity < 10 | quantity > 20)

# Impossible
foo.df %>%
  filter(quantity < 10 & quantity > 20)

# What happened:
foo.df$quantity < 10
foo.df$quantity > 20

# Apply to character data
# Here: NO PATTERN MATCHING
# Heart Samples:
foo.df %>%
  filter(tissue == "Heart")

# 2 or more: Heart & Liver
# quick and dirty:
foo.df %>%
  filter(tissue == "Heart" | tissue == "Liver")

# More efficient: DON'T DO THIS
foo.df %>%
  filter(tissue == c("Heart", "Liver"))

# This is Terrible... because...
foo.df %>%
  filter(tissue == c("Liver", "Heart"))

# Why??... VECTOR RECYCLING
foo.df$tissue == c("Heart", "Liver")
foo.df$tissue == c("Liver", "Heart")

# So... use %in% instead
foo.df %>%
  filter(tissue %in% c("Liver", "Heart"))

# This is now the same as
foo.df %>%
  filter(tissue %in% c("Heart", "Liver"))

# Element 5: Indexing
# Find info according to position using []

# Vectors:
foo1
foo1[6] # The 6th value
foo1[p] # The pth value, i.e. 6th
foo1[3:p] # the 3rd to the pth values
foo1[p:length(foo1)] # from 6th to last

# use combinations of:
# integers, objects, functions

# but the exciting part is... logical vectors:
# i.e. logical expressions
# e.g. all values less than 50
foo1[foo1 < 50]

# Data frames with []:
# 2 Dimentional: [ rows , columns ]
foo.df[3,] # 3rd row, ALL columns
foo.df[,3] # ALL rows, 3rd column
foo.df[3:6, "quantity"] # 3rd to 6th rows, only quantity
foo.df[3:6, 3] # 3rd to 6th rows, only quantity
foo.df[3:6, -1] # exclude healthy, or
foo.df[3:6, c("tissue", "quantity")]

# combine in all variety of ways!
# to prevent switching between vector and data frame
# use tibbles :)
foo.df <- as_tibble(foo.df)
foo.df[foo.df$quantity < 10, "tissue"] # low quantity, only tissue

# basically, this is subset() or filter()
subset(foo.df, quantity < 10, select = "tissue")

# or, easier...
foo.df %>%
  filter(quantity < 10) %>%
  select(tissue)

# NOT possible: missing comma :/
foo.df[foo.df$tissue == "Heart"] # only heart rows
# but... no comma is short-hand for columns
foo.df[3] # the 3rd column
foo.df[,3] # the same here

# So I don't need a comma in [] for a data frame
# But what if I DO have one for a vector?
foo1[,p]

# Element 8: Factor Variables (with levels)
# Categorical variables (with groups)
# aka discrete, qualitative

#e.g.
PlantGrowth$group

# factor is a special class of type integer
# with labels
typeof(PlantGrowth$group)
class(PlantGrowth$group)

str(PlantGrowth)

# also:
foo3 # character
foo.df$tissue # now a factor

str(foo.df)
# The integer values are: 4 1 6 5 3 2
# The associated labels are:
levels(foo.df$tissue)
# "Brain"     "Heart"     "Intestine" "Liver"     "Muscle"   "Testes"

# example of a common problem:
xx <- c(23:27, "bob")
xx # A character vector
# convert to a data.frame:
test <- data.frame(xx)
test$xx # automatically converted to factor :/

# convert from factor to integer: test$xx
as.integer(test$xx) # convert from factor, gets group ID
as.integer(as.character(test$xx)) # so, convert first to character

# Contrast that to converting from character to integer: xx
as.integer(xx) # convert from character, gets number

# Alternatively:
test <- data.frame(xx, stringsAsFactors = F)
test$xx # remains character

# Element 9 & 10: tidy data and split-apply-combine

# Work on a new dataset:
PlayData <- data.frame(type = rep(c("A", "B"), each = 2),
                       time = 1:2,
                       height = seq(10, 40, 10),
                       width = seq(50, 80, 10))

# make the data tidy using the tidyr package
# four arguments to gather()
# 1 - data (input),
# 2&3 - key, value (the names of the output columns)
# 4 - either ID or MEASURE vars
gather(PlayData, key, value, -c(type, time)) # with ID

PlayData.t <- gather(PlayData, key, value, c(height, width)) # with MEASURE

# now for split-apply-combine:
# Split according to some variable(s)
# Apply some functions
# Combine for output

# Scenario 1: compare by key (i.e. height/width)
PlayData$height/PlayData$width # using original data

# with tidy data: aggregration
# Scenario 1: (group according to type and time)
PlayData.t %>%
  group_by(type, time) %>%
  summarise(avg = mean(value))

# Scenario 2: (group according to time and key)
PlayData.t %>%
  group_by(time, key) %>%
  summarise(avg = mean(value))

# Scenario 3: (group according to type and key)
PlayData.t %>%
  group_by(type, key) %>%
  summarise(avg = mean(value))


#######################
# An example with regular expressions:

ratios <- c("Ratio.H.M.Sig", "Ratio.H.L.Sig", "Ratio.M.L.Sig")
sub("Ratio.", "", ratios)

# Which one is H.M?
library(stringr)
# logical vector
ratios == "Ratio.H.M.Sig"  # 100% match
grepl("H.M", ratios)       # pattern match
str_detect(ratios, "H.M")  # pattern match with stringr

# integer of TRUE positions
which(ratios == "Ratio.H.M.Sig")  # 100% match
grep("H.M", ratios)               # pattern match
which(str_detect(ratios, "H.M"))  # pattern match with stringr

# Matching patterns:
str_extract(ratios, "H.*$")
str_extract(ratios, "^.*H")
str_extract(ratios, "H.*i")

##########################
# An aside:

length(foo.df) # the number of elements
# i.e. ncol() --- confusing :/

# But this is ok:
dim(foo.df)
ncol(foo.df)
nrow(foo.df)

length(foo2) # 1D vector
dim(foo2)









