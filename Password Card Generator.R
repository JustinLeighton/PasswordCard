
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

library(digest)
library(gridExtra)
library(grid)

# Code
Code <- "password123"

# Hash Formula
Hash <- digest("5110791000024554","sha512",serialize = FALSE)

# Convert base 16 to base 10
strtoi(unlist(strsplit(gsub("(.{2})","\\1 ",Hash),split=" ")), base=16)

# Table Generation
spcl <- c("!", "@", "#", "$", "%", "^", "&", "*", "?", "_", "-", "+", "=")
num <- c(0:9)
rand1 <- c(60,52,39,104,76,128,124,123,94,82,77,40,120,69,21,73
          ,3,98,19,108,38,5,28,46,45,48,74,64,107,105,9,4
          ,121,1,24,75,16,27,36,25,31,122,85,22,65,42,66,7
          ,18,62,59,117,37,109,14,56,92,97,84,111,13,95,81,43
          ,96,57,20,125,118,51,110,67,99,63,88,78,100,58,70,35
          ,54,71,101,113,10,102,86,30,34,41,91,93,90,68,114,106
          ,87,32,12,89,6,53,33,29,115,127,103,2,79,83,119,72
          ,15,126,55,8,11,47,49,112,61,116,26,44,80,17,50,23) 
rand2 <- c(76,19,61,81,68,86,59,48,3,23,122,56,93,126,49,97
          ,11,36,24,108,105,44,17,9,83,101,63,62,104,103,65,91
          ,75,72,114,57,53,118,106,70,16,98,2,31,34,92,29,6
          ,51,35,10,13,38,109,117,58,127,71,64,25,90,14,54,116
          ,124,77,46,5,123,112,60,95,40,55,102,120,45,115,74,12
          ,26,87,47,21,119,39,52,15,8,22,50,20,100,85,37,78
          ,69,42,33,125,73,113,18,111,94,66,32,107,84,121,7,88
          ,28,89,4,27,80,67,79,96,30,110,82,1,128,43,99,41)
rand3 <- c(64,24,35,95,29,117,37,17,81,83,124,100,65,51,40,19
          ,115,70,59,80,21,10,97,26,76,98,46,89,88,79,36,7
          ,93,104,14,90,101,43,86,32,111,28,41,96,99,78,50,74
          ,6,34,102,20,45,77,113,87,126,16,22,108,120,23,62,105
          ,49,82,110,47,73,92,56,122,4,39,84,119,127,114,106,58
          ,33,52,25,54,48,112,11,109,94,9,27,128,61,53,67,103
          ,5,85,72,38,2,66,57,63,123,71,44,15,8,68,30,3
          ,118,60,69,13,1,18,12,121,31,116,55,42,75,91,125,107)

# Output lists
comb1 <- c(num, num, num, num, num, spcl, spcl, letters, LETTERS)
comb2 <- comb1[rand1]
comb3 <- comb1[rand2]
comb4 <- comb1[rand3]
comb5 <- c(num, num, num, num, num, letters, letters, LETTERS)
comb6 <- comb5[rand1]
comb7 <- comb5[rand2]
comb8 <- comb5[rand3]

# Building Table
df <- c(c(comb1, comb1)[strtoi(unlist(strsplit(gsub("(.{2})","\\1 ",Hash),split=" ")), base=16)]
       ,c(comb2, comb2)[strtoi(unlist(strsplit(gsub("(.{2})","\\1 ",Hash),split=" ")), base=16)]
       ,c(comb3, comb3)[strtoi(unlist(strsplit(gsub("(.{2})","\\1 ",Hash),split=" ")), base=16)]
       ,c(comb4, comb4)[strtoi(unlist(strsplit(gsub("(.{2})","\\1 ",Hash),split=" ")), base=16)]
       ,c(comb5, comb5)[strtoi(unlist(strsplit(gsub("(.{2})","\\1 ",Hash),split=" ")), base=16)]
       ,c(comb6, comb6)[strtoi(unlist(strsplit(gsub("(.{2})","\\1 ",Hash),split=" ")), base=16)]
       ,c(comb7, comb7)[strtoi(unlist(strsplit(gsub("(.{2})","\\1 ",Hash),split=" ")), base=16)]
       ,c(comb8, comb8)[strtoi(unlist(strsplit(gsub("(.{2})","\\1 ",Hash),split=" ")), base=16)])
df <- data.frame(df[1:9],df[10:18],df[19:27],df[28:36],df[37:45],df[46:54],df[55:63],
               df[64:72],df[73:81],df[82:90],df[91:99],df[100:108],df[109:117],df[118:126],
               df[127:135],df[136:144],df[145:153],df[154:162],df[163:171],df[172:180],df[181:189],
               df[190:198],df[199:207],df[208:216],df[217:225],df[226:234],df[235:243],df[244:252],
               df[253:261],df[262:270],df[271:279],df[280:288],df[289:297],df[298:306],df[307:315],
               df[316:324],df[325:333],df[334:342],df[343:351],df[352:360],df[361:369],df[370:378],
               df[379:387],df[388:396],df[397:405],df[406:414],df[415:423],df[424:432],df[433:441],
               df[442:450],df[451:459],df[460:468],df[469:477],df[478:486],df[487:495],df[496:504])
colnames(df) <- c(letters,'!','?',letters,'!','?')

# Generate Cards
t <- ttheme_default(core=list(
  bg_params = list(fill=c("#ffb3ba","#ffdfba","#ffffba","#baffc9","#bae1ff","#e3c6f0","white","grey","#654321"))
  ))
grid.table(df[1:28],rows=NULL,theme=t)
grid.newpage()
grid.table(df[29:56],rows=NULL,theme=t)

