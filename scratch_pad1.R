#2. Clean your Raw Data
#### a. Row and Column count
df.procrastination <- read.csv("csv/Procrastination.csv")
length(df.procrastination)
nrow(df.procrastination)


currentColumns <- names(df.procrastination)
# The columns that pertain to survey question are broken down into good "Gd" and bad "bad" categories.
newColumnNames <- c("Age", "Gender", "HaveKids", "Ed.Level", "WorkStatus", "Income", "Occupation", "YearsAtJob", 
                    "MonthsAtJob", "Community", "Country", "MaritalStat", "Sons", "Daughters", "BdWstTime", "BdDelayAct", 
                    "BdMustAct", "BdDelayMake", "BdPutOff", "GdPayBills", "GdPunctual", "GdPrepared", "BdRunLate", "BdCmpltTm",
                    "GdWantGrow", "BdFamView", "GdTmToSpare", "BdDeadline", "BdTmMngmt", "GdDocApt", "GdMorePunct", 
                    "GdMaint", "BdXpctLate", "BdCostMoney", "BdLateStart", "BdMissTckts", "GdPartyPln", "GdEasyWake", "BdSlowMail", 
                    "GdRtnCalls", "BdWrkr", "GdDecisive", "BdWrkDelay", "BdAirport", "GdSocial", "BdDeadline", "GdSMLBill", 
                    "GdRSVP", "GdTask", "BdBdayGFT", "BdEsntlItem", "GdCmplteDay", "BdDoTmrow", "GdEvngTasks", "GdIdealLIfe",
                    "GdExclLife", "GdSatisLife", "GdImprtThs", "GdChangeNil", "SelfView", "ExtView")
names(df.procrastination) <- newColumnNames
str(df.procrastination)

df.procrastination$Sons <- sapply(df.procrastination$Sons, function(x) {
  value = 0
  if(x == "Male") {
    value = 1
  } else if(x == "Female") {
    value = 0
  }
  value
})

# replace NA with zero.  No point in guessing what the value really or if it is actually zero.
df.procrastination$YearsAtJob[mapply(is.na, df.procrastination$YearsAtJob)] <- 0
df.procrastination$YearsAtJob[mapply(function(x) x == 999, df.procrastination$YearsAtJob)] <- 0

# round years at work to the nearest integer
df.procrastination$YearsAtJob <- round(df.procrastination$YearsAtJob, digits = 0)

# iii. Set 0 country of resisidence to NA
df.procrastination$Country[df.procrastination$Country == 0] <- NA

# iv. Clean Occupation Column
# convert zeros to NA's
df.procrastination$Occupation[df.procrastination$Occupation == 0] <- NA

# convert please specify to NA's
df.procrastination$Occupation[df.procrastination$Occupation == 'please specify'] <- NA

# first convert to chars for easier manipulation
df.procrastination$Occupation <- as.character(df.procrastination$Occupation)
# create writer category
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^[wW]rit")] <- "Writer"

# create VP category
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^[vV]ice")] <- "VP"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^VP")] <- "VP"

# create TV Broadcasting category
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^TV")] <- "Broadcasting"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^[tT]elevision")] <- "Broadcasting"

# add to IT Category
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^[tT]ech")] <- "IT"

# create Tax
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^[tT]ax")] <- "Tax Work"

# add to System Analyst
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^System")] <- "System Analyst"

# add to doctor
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "Surgeon")] <- "Doctor"

# add to supervisor
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^[sS]upervis")] <- "Supervisor"

# create Student
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^[sS]tude")] <- "Student"

# add to writer
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^Staff Writer")] <- "Writer"

# add to teacher
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^[sS]pecial [eE]ducation")] <- "Teacher"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "teacher / Administrator")] <- "Teacher"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^Teacher and Full")] <- "Teacher"

# Keeping software separate from IT as that would be overly broad
# add Software
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^Software")] <- "Software"

# add to writer
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "Writer$")] <- "Writer"

# Anyone who is self employeed should be group together regardless of trade given the tendencies of this group of people.
# add to self employed
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^[sS]elf")] <- "Self Employed"

# aggregate Secretary
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^[sS]ecretary")] <- "Secretary"

# aggregate sales
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^[sS]ales")] <- "Sales"

# add to student
df.procrastination$Occupation[df.procrastination$Occupation == "s"] <- "Student"

# aggregate RN
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^RN")] <- "RN"

# aggregate retired
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^retired")] <- "retired"

# aggregate Retail
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^Retail")] <- "Retail"

#aggregate Manager
df.procrastination$Occupation[df.procrastination$Occupation == "Restaurant operations manager"] <- "Manager"

# add to student
df.procrastination$Occupation[df.procrastination$Occupation == "restaurant mgr / student / and looking f"] <- "Student"

# add to Doctor
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "[pP]hysician")] <- "Doctor"

# aggregate Researcher
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^[rR]esearch")] <- "Research"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^Reasearch")] <- "Research"

# aggregate Real Estate
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^realtor")] <- "Real Estate"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^[rR]eal [eE]state")] <- "Real Estate"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "realtor$")] <- "Real Estate"

# aggregate managers given their similarity in responsibilities
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "Manager$")] <- "Manager"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^Manager -")] <- "Manager"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^Divisional Manager")] <- "Manager"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^Program Manager")] <- "Manager"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^Manager,Interacitve")] <- "Manager"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "manager")] <- "Manager"

# add to software
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^Programmer")] <- "Software"

#----------------------------------------
# add to engineer
df.procrastination$Occupation[df.procrastination$Occupation == 'Process Engineer'] <- "Engineer"

# aggregate president
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "[pP]resident")] <- "President"

# create finance
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "Private Equity Principal")] <- "Finance"

# add to manager
# looks like a copy paste issue so adding it to Manager
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "Please specify title Manager for Regulat")] <- "Manager"

# aggregrate director do to important shared qualities
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "pjublic relations director")] <- "director"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "director")] <- "Director"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "Director")] <- "Director"

# aggregrate Supervisor do to important shared qualities
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "[sS]upervisor")] <- "Supervisor"

# aggregate PCA
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^PCA")] <- "PCA"

# add to self employed
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "Attorney - self employed for 2 years – f")] <- "Self Employed"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^[oO]wner")] <- "Self Employed"

# aggregate Nurse
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "RN")] <- "Nursing"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "Nurse")] <- "Nursing"

# add to engineer
# this is a very broad title, but again it comes down to shared traits that might influence procrastination
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "[eE]ngineer")] <- "Engineer"

# change to just Nanny to correspond to the other subjects 
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "Nanny")] <- "Nanny"

# add to Musician
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^musician")] <- "Musician"

# aggregate Medical
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^[mM]edical")] <- "Medical"

# aggregate Marketing
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^marketing")] <- "Marketing"

# aggregate Market Research
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "Market Research Analyst")] <- "Market Analyst"