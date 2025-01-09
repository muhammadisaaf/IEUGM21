library(haven)
library(dplyr)
bk_ar1 <- read_dta("C:/Users/muham/Documents/sok produktif/Stata/work stations/IFLS/IFLS5/hh14_all_dta/bk_ar1.dta")
View(bk_ar1)

# Muye Day 1: Generating Unique id in IFLS 5 ------------------------------

#Check the length of hhid14 & pid14 digits
check_hhid14 <- bk_ar1 |>
  mutate(check = nchar(hhid14)) #To check the length of the variable character
mean(check_hhid14$check)
max(check_hhid14$check)

check_pid14 <- bk_ar1 |>
  mutate(check = nchar(pid14)) #To check the length of the variable character
mean(check_pid14$check)
max(check_pid14$check)

#Create new variable
bk_ar1 <- bk_ar1 |>
  mutate(id_new = paste(hhid14, sprintf("%02d", pid14), sep = ""))

#Validation check, if all id is unique or otherwise
validation_check <- bk_ar1 |>
  summarize(
    distinct_count = n_distinct(id_new),  # Count of unique `id_new`
    total_count = n()                     # Total count of rows
  ) |>
  mutate(difference = total_count - distinct_count)  # Calculate the difference

ifelse(validation_check$difference == 0, "all unique", "not unique")

print(validation_check)


# Muye Day 1: Mutating Common Useful Variables ----------------------------

# b3a_d11 making years of schooling ---------------------------------------

b3a_dl1 <- read_dta("C:/Users/muham/Documents/sok produktif/Stata/work stations/IFLS/IFLS5/hh14_all_dta/b3a_dl1.dta")
View(b3a_dl1)
#Create new variable
b3a_dl1 <- b3a_dl1 |>
  mutate(id_new = paste(hhid14, sprintf("%02d", pid14), sep = ""))

##Recode d102 variable
b3a_dl1 <- b3a_dl1 |>
  mutate(read = case_when(
    dl02 == 3 ~ 0,
    dl02 == 8 ~ NA_real_, #8 become NA
    is.na(dl02) ~ NA_real_, #NA become NA
    TRUE ~ 1 #Everything else become 1
  ))

#Label the 'read' variable
attr(b3a_dl1$read, "label") <- "Able to read newspaper in Indonesian"
attr(b3a_dl1$read, "labels") <- c("No" = 0, "Yes" = 1)

#Check data validity
b3a_dl1 |>
  select(read) |>
  distinct() #Checking all categories in the variable

b3a_dl1 |>
  group_by(read) |> #Groping b3a_dl1 samples given the read variable
  summarize(n=n()) #Counting n per "read" category


##Recode d103 variable
b3a_dl1 <- b3a_dl1 |>
  mutate(write = case_when(
    dl03 == 3 ~ 0,
    dl03 == 8 ~ NA_real_, #8 become NA
    is.na(dl03) ~ NA_real_, #NA become NA
    TRUE ~ 1
  ))

#Label the 'write' variable
attr(b3a_dl1$write, "label") <- "Able to write letter in Indonesian"
attr(b3a_dl1$write, "labels") <- c("No" = 0, "Yes" = 1)

#Check data validity
b3a_dl1 |>
  select(write) |>
  distinct()

b3a_dl1 |>
  group_by(write) |>
  summarize(n=n())


##Recode d104 and dl07a variables -> If the sample is ever attending school
b3a_dl1$a777 <- 0 #Creating the middle variable where sample is currently attending shcool

for (x in 1:length(b3a_dl1$pid14)) {
  if (b3a_dl1$dl07a[x] == 1 & is.na(b3a_dl1$dl07a[x])==FALSE) {
    b3a_dl1$a777[x] <- 1
  } else if (is.na(b3a_dl1$dl07a[x])){
    b3a_dl1$a777[x] <- 0
  } else {
    b3a_dl1$a777[x] <- 0
  }
} #For all observation in b3a_dl1$a777: If dl07a=1 & is not missing, a777=1; If dl07a is missing, a777=0; If dl07a is otherwise, a777=0

b3a_dl1$everschl <- if_else(b3a_dl1$a777 == 1, b3a_dl1$everschl <- 1, b3a_dl1$everschl <- case_when(
  b3a_dl1$dl04 == 1 ~ 2,
  b3a_dl1$dl04 == 3 ~ 0,
  b3a_dl1$dl04 == 8 ~ NA_real_,
  is.na(b3a_dl1$dl04) ~ NA_real_,
)) #If a777=1 (currently attending school), everschl=1. If a777 is otherwise: everschl=2 if dl04=1 (ever attending school); everschl=0 if dl04=3 (never attending school); everschl=NA if dl04 is otherwise

#Label the 'everschl' variable
attr(b3a_dl1$everschl, "label") <- "Able to write letter in Indonesian"
attr(b3a_dl1$everschl, "labels") <- c("Never attended school" = 0,
                                      "Currently attending school" = 1,
                                      "Ever attended school" = 2)

#Check data validity
b3a_dl1 |>
  select(everschl) |>
  distinct()

b3a_dl1 |>
  group_by(everschl) |>
  summarize(n=n())


##Recode dl06 dl07 -> For level of eduction
b3a_dl1 <- b3a_dl1 %>%
  mutate(educlvl = case_when(
    # condition 1: no schooling
    (everschl == 0 | dl06 == 90 | (dl06 %in% c(2, 11, 72) & (dl07 < 7 | dl07 >= 98))) ~ 0,
    
    # condition 2: primary school
    ((dl06 %in% c(2, 11, 72) & dl07 == 7) | (dl06 %in% c(3, 4, 12, 73) & (dl07 < 7 | dl07 >=98)))~1,
    
    # condition 3: junior high school
    ((dl06 %in% c(3, 4, 12, 73) & dl07 == 7) | (dl06 %in% c(5, 6, 15, 74) & (dl07 < 7 | dl07 >= 98))) ~ 2,
    
    # condition 4: senior high school
    ((dl06 %in% c(5, 6, 15, 74) & dl07 == 7) | (dl06 %in% c(60, 61, 13) & (dl07 < 7 | dl07 >= 98))) ~ 3,
    
    # condition 5: higher education
    ((dl06 %in% c(60, 61, 62, 63, 13) & dl07 == 7) | (dl06 %in% c(62, 63) & (dl07 < 7 | dl07 >= 98))) ~ 4,
    
    # condition 6: Other categories
    dl06 == 95 ~ 10,
    dl06 == 14 ~ 14,
    dl06 == 17 ~ 17,
    
    # Missing values
    dl06 == 98 ~ NA_real_,  # .d in Stata (don't know)
    dl06 == 99 ~ NA_real_,  # .m in Stata (missing by design)
    TRUE ~ NA_real_  # Default for other cases (missing)
  ))

# Label the educlvl variable
attr(b3a_dl1$educlvl, "label") <- "Highest education level finished"

# Define value labels for educlvl
educlvl_labels <- c(
  "0" = "No schooling/not finished primary school",
  "1" = "Primary school/equivalent",
  "2" = "Junior high school/equivalent",
  "3" = "Senior high school/equivalent",
  "4" = "Higher education",
  "7" = "Madrasa",
  "10" = "Other",
  "14" = "Pesantren",
  "17" = "School for disabled"
)

# Apply value labels to educlvl
b3a_dl1 <- b3a_dl1 |>
  mutate(educlvl = factor(educlvl, levels = names(educlvl_labels), labels = educlvl_labels))

# Check the result
b3a_dl1 |>
  group_by(educlvl) |>
  summarize(n=n())

typeof(b3a_dl1$educlvl)


##Years of educ
#educ
b3a_dl1 <- b3a_dl1 %>%
  mutate(educ = case_when(
    # year 0–5
    (everschl == 0 | dl06 == 90 | (dl06 %in% c(2, 11, 72) & (dl07 %in% c(0, 98, 99)))) ~ 0,
    (dl06 %in% c(2, 11, 72) & dl07 %in% c(1:5)) ~ dl07,
    
    # year 6—8
    (((dl06 %in% c(2, 11, 72)) & (dl07 %in% c(6:7))) | ((dl06 %in% c(3, 4, 12, 73)) & (dl07 %in% c(0, 98, 99)))) ~ 6,
    ((dl06 %in% c(3, 4, 12, 73) & dl07 %in% c(1:2))) ~ 6 + dl07,
    # year 9—11
    (((dl06 %in% c(3, 4, 12, 73)) & (dl07 %in% c(3:7))) | ((dl06 %in% c(5, 6, 15, 74)) & (dl07 %in% c(0, 98, 99)))) ~ 9,
    ((dl06 %in% c(5, 6, 15, 74)) & (dl07 %in% c(1, 2))) ~ 9 + dl07,
    # year 12
    ((dl06 %in% c(5, 6, 15, 74)) & (dl07 %in% c(3:7))) ~ 12,
    ((dl06 %in% c(60, 61, 13)) & (dl07 %in% c(0, 98, 99))) ~ 12,
    
    # year 13
    ((dl06 %in% c(60)) & (dl07 %in% c(1))) ~ 13,
    
    # year 14
    ((dl06 %in% c(60)) & (dl07 %in% c(2:7))) ~ 14,
    
    # year 13—15
    ((dl06 %in% c(61, 13)) & (dl07 %in% c(1:3))) ~ 12 + dl07,
    
    # year 16
    (((dl06 %in% c(61, 13)) & (dl07 %in% c(4:7))) | ((dl06 %in% c(62)) & (dl07 %in% c(0, 98, 99)))) ~ 16,
    
    # year 17
    ((dl06 %in% c(62)) & (dl07 %in% c(1))) ~ 17,
    
    # year 18
    (((dl06 %in% c(62)) & (dl07 %in% c(2:7))) | ((dl06 %in% c(63)) & (dl07 %in% c(0, 98, 99)))) ~ 18,
    
    # year 18 + dl07
    ((dl06 %in% c(63)) & (dl07 %in% c(1:3))) ~ 18 + dl07,
    
    # year 22
    ((dl06 %in% c(63)) & (dl07 %in% c(4:7))) ~ 22,
    
    # Missing values
    dl06 == 98 ~ NA_real_,  # .d in Stata (don't know)
    (educlvl %in% c(7, 10, 14, 17)) | (dl06 == 99) ~ NA_real_,  # .m in Stata (missing by design)
    TRUE ~ NA_real_  # Default for other cases (missing)
  ))

# Label the educ variable
attr(b3a_dl1$educ, "label") <- "Years of education"


# Muye Day 1: Merging Book ------------------------------------------------

## remove irrelevant variable
#first dataset
bk_ar1 <- bk_ar1 |>
  select(id_new, ar02b, ar13, pidlink) #Selecting variables chosen from the bk_ar1 dataset

#second dataset
b3a_dl1 <- b3a_dl1 |>
  select(id_new, read, write, everschl, educlvl) #Selecting variables chosen from the b3a_dl1 dataset

merge <- merge(bk_ar1, b3a_dl1,by="id_new", all=TRUE) #Merging datasets based on "id_new" variable

length(merge$id_new) #Check the length of the new merged dataset

##Incase the base variable has different name
#Rename to match other dataset base variable
bk_ar1_1 <- bk_ar1 %>%
  rename(id = id_new)

#Just merge directly
merge2 <- merge(bk_ar1_1, b3a_dl1, by.x="id", by.y="id_new",all=TRUE) #X is the name of base variable on bk_ar1_1, and y is from b3a_dl1. all=TRUE means all not matched variables is still included


# Muye Day 1: Reshape from Long to Wide -----------------------------------
library(tidyr)
# reshape from long to wide. And also rowtotal to make total food consumption

b1_ks1 <- read_dta("C:/Users/muham/Documents/sok produktif/Stata/work stations/IFLS/IFLS5/hh14_all_dta/b1_ks1.dta")

consump <- b1_ks1 |>
  pivot_wider(
    id_cols = hhid14, #Variable that intended to be changed from long to wide
    names_from = ks1type, #New column names source
    values_from = ks02 #Source of value in the new columns
  )

View(consump)

#Create variable that summing all of the new widen variables
consump <- consump |>
  mutate(
    food_consump = rowSums(consump[2:length(consump)], na.rm = TRUE) #Food consumption is the total from the second row to the end
  )

#Check the descriptive statistics
mean(consump$food_consump)
max(consump$food_consump)
min(consump$food_consump)
var(consump$food_consump)
sd(consump$food_consump)


# Muye Day 2: Merging Waves -----------------------------------------------

##Prepare the first wave (IFLS 4) -----
library(haven)
library(dplyr)
bk_ar1_07 <- read_dta("C:/Users/muham/Documents/sok produktif/Stata/work stations/IFLS/IFLS4/hh07_all_dta/bk_ar1.dta")
View(bk_ar1_07)

#Firstly, create unique id for the bk_ar1_07
bk_ar1_07 <- bk_ar1_07 |>
  mutate(id_new = paste(hhid07, sprintf("%02d", pid07), sep = ""))

#Secondly, create unique id for the b3a_dl1_07
b3a_dl1_07 <- read_dta("C:/Users/muham/Documents/sok produktif/Stata/work stations/IFLS/IFLS4/hh07_all_dta/b3a_dl1.dta")
View(b3a_dl1_07)
b3a_dl1_07 <- b3a_dl1_07 |>
  mutate(id_new = paste(hhid07, sprintf("%02d", pid07), sep = ""))

#Recode d102 variable
b3a_dl1_07 <- b3a_dl1_07 |>
  mutate(read = case_when(
    dl02 == 3 ~ 0,
    dl02 == 8 ~ NA_real_, #8 become NA
    is.na(dl02) ~ NA_real_, #NA become NA
    TRUE ~ 1 #Everything else become 1
  ))

#Label the 'read' variable
attr(b3a_dl1_07$read, "label") <- "Able to read newspaper in Indonesian"
attr(b3a_dl1_07$read, "labels") <- c("No" = 0, "Yes" = 1)

#Recode d103 variable
b3a_dl1_07 <- b3a_dl1_07 |>
  mutate(write = case_when(
    dl03 == 3 ~ 0,
    dl03 == 8 ~ NA_real_, #8 become NA
    is.na(dl03) ~ NA_real_, #NA become NA
    TRUE ~ 1
  ))

#Label the 'write' variable
attr(b3a_dl1_07$write, "label") <- "Able to write letter in Indonesian"
attr(b3a_dl1_07$write, "labels") <- c("No" = 0, "Yes" = 1)

#Recode d104 and dl07a variables -> If the sample is ever attending school
b3a_dl1_07$a777 <- 0 #Creating the middle variable where sample is currently attending shcool

for (x in 1:length(b3a_dl1_07$pid07)) {
  if (b3a_dl1_07$dl07a[x] == 1 & is.na(b3a_dl1_07$dl07a[x])==FALSE) {
    b3a_dl1_07$a777[x] <- 1
  } else if (is.na(b3a_dl1_07$dl07a[x])){
    b3a_dl1_07$a777[x] <- 0
  } else {
    b3a_dl1_07$a777[x] <- 0
  }} #For all observation in b3a_dl1_07$a777: If dl07a=1 & is not missing, a777=1; If dl07a is missing, a777=0; If dl07a is otherwise, a777=0

b3a_dl1_07$everschl <- if_else(b3a_dl1_07$a777 == 1, b3a_dl1_07$everschl <- 1, b3a_dl1_07$everschl <- case_when(
  b3a_dl1_07$dl04 == 1 ~ 2,
  b3a_dl1_07$dl04 == 3 ~ 0,
  b3a_dl1_07$dl04 == 8 ~ NA_real_,
  is.na(b3a_dl1_07$dl04) ~ NA_real_,
)) #If a777=1 (currently attending school), everschl=1. If a777 is otherwise: everschl=2 if dl04=1 (ever attending school); everschl=0 if dl04=3 (never attending school); everschl=NA if dl04 is otherwise

#Label the 'everschl' variable
attr(b3a_dl1_07$everschl, "label") <- "Able to write letter in Indonesian"
attr(b3a_dl1_07$everschl, "labels") <- c("Never attended school" = 0,
                                         "Currently attending school" = 1,
                                         "Ever attended school" = 2)

#Recode dl06 dl07 -> For level of eduction
b3a_dl1_07 <- b3a_dl1_07 |>
  mutate(educlvl = case_when(
    # condition 1: no schooling
    (everschl == 0 | dl06 == 90 | (dl06 %in% c(2, 11, 72) & (dl07 < 7 | dl07 >= 98))) ~ 0,
    
    # condition 2: primary school
    ((dl06 %in% c(2, 11, 72) & dl07 == 7) | (dl06 %in% c(3, 4, 12, 73) & (dl07 < 7 | dl07 >=98)))~1,
    
    # condition 3: junior high school
    ((dl06 %in% c(3, 4, 12, 73) & dl07 == 7) | (dl06 %in% c(5, 6, 15, 74) & (dl07 < 7 | dl07 >= 98))) ~ 2,
    
    # condition 4: senior high school
    ((dl06 %in% c(5, 6, 15, 74) & dl07 == 7) | (dl06 %in% c(60, 61, 13) & (dl07 < 7 | dl07 >= 98))) ~ 3,
    
    # condition 5: higher education
    ((dl06 %in% c(60, 61, 62, 63, 13) & dl07 == 7) | (dl06 %in% c(62, 63) & (dl07 < 7 | dl07 >= 98))) ~ 4,
    
    # condition 6: Other categories
    dl06 == 95 ~ 10,
    dl06 == 14 ~ 14,
    dl06 == 17 ~ 17,
    
    # Missing values
    dl06 == 98 ~ NA_real_,  # .d in Stata (don't know)
    dl06 == 99 ~ NA_real_,  # .m in Stata (missing by design)
    TRUE ~ NA_real_  # Default for other cases (missing)
  ))

# Label the educlvl variable
attr(b3a_dl1_07$educlvl, "label") <- "Highest education level finished"

# Define value labels for educlvl
educlvl_labels <- c(
  "0" = "No schooling/not finished primary school",
  "1" = "Primary school/equivalent",
  "2" = "Junior high school/equivalent",
  "3" = "Senior high school/equivalent",
  "4" = "Higher education",
  "7" = "Madrasa",
  "10" = "Other",
  "14" = "Pesantren",
  "17" = "School for disabled"
)

# Apply value labels to educlvl
b3a_dl1_07 <- b3a_dl1_07 |>
  mutate(educlvl = factor(educlvl, levels = names(educlvl_labels), labels = educlvl_labels))

##Years of educ
#educ
b3a_dl1_07 <- b3a_dl1_07 |>
  mutate(educ = case_when(
    # year 0–5
    (everschl == 0 | dl06 == 90 | (dl06 %in% c(2, 11, 72) & (dl07 %in% c(0, 98, 99)))) ~ 0,
    (dl06 %in% c(2, 11, 72) & dl07 %in% c(1:5)) ~ dl07,
    # year 6—8
    (((dl06 %in% c(2, 11, 72)) & (dl07 %in% c(6:7))) | ((dl06 %in% c(3, 4, 12, 73)) & (dl07 %in% c(0, 98, 99)))) ~ 6,
    ((dl06 %in% c(3, 4, 12, 73) & dl07 %in% c(1:2))) ~ 6 + dl07,
    # year 9—11
    (((dl06 %in% c(3, 4, 12, 73)) & (dl07 %in% c(3:7))) | ((dl06 %in% c(5, 6, 15, 74)) & (dl07 %in% c(0, 98, 99)))) ~ 9,
    ((dl06 %in% c(5, 6, 15, 74)) & (dl07 %in% c(1, 2))) ~ 9 + dl07,
    # year 12
    ((dl06 %in% c(5, 6, 15, 74)) & (dl07 %in% c(3:7))) ~ 12,
    ((dl06 %in% c(60, 61, 13)) & (dl07 %in% c(0, 98, 99))) ~ 12,
    # year 13
    ((dl06 %in% c(60)) & (dl07 %in% c(1))) ~ 13,
    # year 14
    ((dl06 %in% c(60)) & (dl07 %in% c(2:7))) ~ 14,
    # year 13—15
    ((dl06 %in% c(61, 13)) & (dl07 %in% c(1:3))) ~ 12 + dl07,
    # year 16
    (((dl06 %in% c(61, 13)) & (dl07 %in% c(4:7))) | ((dl06 %in% c(62)) & (dl07 %in% c(0, 98, 99)))) ~ 16,
    # year 17
    ((dl06 %in% c(62)) & (dl07 %in% c(1))) ~ 17,
    # year 18
    (((dl06 %in% c(62)) & (dl07 %in% c(2:7))) | ((dl06 %in% c(63)) & (dl07 %in% c(0, 98, 99)))) ~ 18,
    # year 18 + dl07
    ((dl06 %in% c(63)) & (dl07 %in% c(1:3))) ~ 18 + dl07,
    # year 22
    ((dl06 %in% c(63)) & (dl07 %in% c(4:7))) ~ 22,
    # Missing values
    dl06 == 98 ~ NA_real_,  # .d in Stata (don't know)
    (educlvl %in% c(7, 10, 14, 17)) | (dl06 == 99) ~ NA_real_,  # .m in Stata (missing by design)
    TRUE ~ NA_real_  # Default for other cases (missing)
  ))

#Label the educ variable
attr(b3a_dl1_07$educ, "label") <- "Years of education"

##Chose only relevant variables
#First dataset
bk_ar1_07 <- bk_ar1_07 |>
  select(id_new, ar02b, ar13, pidlink) #Selecting variables chosen from the bk_ar1 dataset

#second dataset
b3a_dl1_07 <- b3a_dl1_07 |>
  select(id_new, read, write, everschl, educlvl) #Selecting variables chosen from the b3a_dl1 dataset

merge_07 <- merge(bk_ar1_07, b3a_dl1_07, by="id_new", all=TRUE) #Merging datasets based on "id_new" variable

##For the IFLS5, use previous steps in day 1 Muye

##Append the Wave 4 and 5
append <- rbind(merge, merge_07)


