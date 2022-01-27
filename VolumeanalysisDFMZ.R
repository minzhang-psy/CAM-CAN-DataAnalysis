# no data for 135, 398,515,547,558
#install.packages("stringr")
rm(list=ls())
library(stringr)
library(dplyr)
library(tidyverse)
library(readxl)

######################################### Volume data #####################################################
# set working directory
setwd("~/EmoReg_CamCAN/data/CamCANSBrainVolume/")
Demographics <- read_xlsx("~/EmoReg_CamCAN/data/CamCANdemographics.xlsx")
options(digits=5)

# read in header 
file <- readLines("1_aseg.stats")
headeridx <- grep("# ColHeaders", file)
colnametext <- file[headeridx]

colnametext  <- str_split(colnametext, pattern = " (?=[[:alnum:]])") %>% unlist()
colnametext <- colnametext[3:11]
colnametext <- gsub(" ", "", colnametext)


# make a list of file names
filenames <- list.files("~/EmoReg_CamCAN/data/CamCANSBrainVolume/","*_aseg.stats")
df <- list()


# read in files for all participant, include colnames and ID 
for (f in seq(1:length(filenames))) {
  filen = str_split(filenames[f], pattern = "_") %>% unlist 
  ID <- filen[1]
  df[[f]] <- read.table(filenames[f]) 
  colnames(df[[f]]) <- colnametext
  df[[f]]$ID <- ID
}



# Volume <- df 
left_Hippocampus <- c()
right_Hippocampus <- c()
left_Amygdala <- c()
right_Amygdala <- c()
ssid <- c()

# Get size of Hippocampus (left & right) and Amygdala (left & right)
for (i in seq(1:length(filenames))){
  ssid [i] <-  df[[i]][12,"ID"]
  left_Hippocampus[i] <- df[[i]][12,"Volume_mm3"]
  right_Hippocampus[i] <- df[[i]][27, "Volume_mm3"]
  left_Amygdala[i] <- df[[i]][13,"Volume_mm3"]
  right_Amygdala[i] <- df[[i]][28, "Volume_mm3"]
}
  
# Create new df for Volume
Volume <- data.frame(ssid = as.numeric(ssid), left_Hippocampus, right_Hippocampus, left_Amygdala, right_Amygdala)  
# no data for 135, 398, 515,547, 558

######################################### Behavior data #####################################################
#set working directory
setwd("~/CamCAN/EmotionRegulationRSFC/EmoRegulation/data/raw_data")
raw.files <- data_frame(Beh_filenames =list.files("~/CamCAN/EmotionRegulationRSFC/EmoRegulation/data/raw_data"))
raw.file.paths <- raw.files %>%
  mutate(filepath = paste0("~/CamCAN/EmotionRegulationRSFC/EmoRegulation/data/raw_data", Beh_filenames))
raw.files < - raw.file.paths %>%
  # "do" the function for each row in turn
  rowwise() %>%
  do(., read.table(file = .$filepath, skip = 1))

raw.file <- do(Beh_filenames, read.table(file = ., skip = ))
d <- lapply(Beh_filenames, read.table, skip=1)
Beh_df <- list() 
read.table("EmotionRegulation_CC723197_scored.txt", skip = 1)



# Read in files for all participant include ID 
for (f in seq(1:length(Beh_filenames))){
  Beh_filen = str_split(Beh_filenames[f], pattern = "_") %>% unlist
  ID <- Beh_filen[2]
  Beh_df[[f]] <- read.table(Beh_filenames[f],skip = 1)
  col
  
 }

# read in files for all participant, 

# Filter ID by old analysis 
CAMCAN <- read.csv("~/EmoReg_CamCAN/camcan_ER_dataset041520.csv")

Volume <- merge(CAMCAN, Volume, by="ssid")

# Re-calculate Variable 
# Reactivity
Volume$Pos_reactivity = Volume$PosW_pos - Volume$NegW_pos
Volume$Neg_reactivity = Volume$NegW_neg - Volume$PosW_neg
Volume$Reactivity = (Volume$Pos_reactivity + Volume$Neg_reactivity) /2   # Mean
#Reactivity (compare to neutral watch)
Volume$Pos_reactivity_Neutral = Volume$PosW_pos - Volume$NeuW_pos
Volume$Neg_reactivity_Neutral = Volume$NegW_neg - Volume$NeuW_neg
Volume$Reactivity_Neutral = (Volume$Pos_reactivity + Volume$Neg_reactivity) /2   # Mean
# Success 
Volume$Pos_Reg_Success = Volume$NegR_pos - Volume$NegW_pos
Volume$Neg_Reg_Success = Volume$NegW_neg - Volume$NegR_neg
Volume$Reg_Success = (Volume$Pos_Reg_Success + Volume$Neg_Reg_Success)/2

# sub_Regulation
Volume$Sub_Regulation = Volume$NegR_WvsR - Volume$NegW_WvsR

# Mean for Hippocampus
Volume$Hippocampus <- (Volume$left_Hippocampus + Volume$right_Hippocampus)/2
Volume$Amygdala <- (Volume$left_Amygdala + Volume$right_Amygdala)/2

Volume <- Demographics %>% 
  select("ssid", "gender_text", "gender_code") %>%
  merge(Volume, by="ssid")
  
write.table(Volume, file = "~/EmoReg_CamCAN/camcan_ER_with_ROI_volume.csv", sep = ",", append = FALSE, quote = F, na = "", row.names = F)  
  


