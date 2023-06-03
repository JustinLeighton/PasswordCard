
## ---------------------------
##
## Name: Password Card Generator v1
##
## Description: Generates password card based upon sha512 hash input
##
## Author: Justin T. Leighton
##
## Date Created: 2019-08-06
##
## Version Date:
##
## ---------------------------
##
## Notes: Input string in "Code" to generate unique card
##   
## ---------------------------

library(tidyverse)
library(digest)
library(gridExtra)
library(grid)

# Code
Code <- "password123"

# Hash Formula
Hash <- digest(Code,"sha512", serialize = FALSE)

# Convert hash value to vector of length 64 in base 10
hash_set <- Hash %>%
  gsub("(.{2})","\\1 ", .) %>%
  strsplit(split=" ") %>%
  unlist() %>%
  strtoi(base=16)

# Character sets
spcl <- c("!", "@", "#", "$", "%", "^", "&", "*", "?", "_", "-", "+", "=")
num <- c(0:9)
charset1 <- c(num, num, num, num, num, spcl, spcl, letters, LETTERS) # Special characters
charset2 <- c(num, num, num, num, num, letters, letters, LETTERS) # No special characters

# Generate card sides
card <- c()
for(i in 1:8){
  set <- c(outer(hash_set+(i%%4), 256, '%%'))+1
  if(i<=4){
    values <- c(charset1, charset1)[set]
  }else{
    values <- c(charset2, charset2)[set]
  }
  card <- c(card, values[1:(length(values)-1)]) # Exclude last value to end with 504 values instead of 512
}

# Building Table
df <- card %>%
  matrix(ncol=28, byrow=TRUE) %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  `colnames<-`(c(letters,'!','?'))

# Generate Cards
colors <- c("#ffb3ba","#ffdfba","#ffffba","#baffc9","#bae1ff","#e3c6f0","#ffffff","#bbbbbb","#654321")
t <- ttheme_default(core=list(bg_params = list(fill=colors)))
png("side1.png", height = 500, width = 1300)
grid.table(df[1:9,],rows=NULL,theme=t)
dev.off()
png("side2.png", height = 500, width = 1300)
grid.table(df[10:18,],rows=NULL,theme=t)
dev.off()

