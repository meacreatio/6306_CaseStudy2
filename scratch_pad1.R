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
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "VP")] <- "VP"

# create TV Broadcasting category
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^TV")] <- "Broadcasting"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^[tT]elevision")] <- "Broadcasting"

# add to IT Category
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "[tT]ech")] <- "IT"

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

# add to nursing
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "LPN")] <- "Nursing"

# add to college professor
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "lecturer")] <- "college professor"

# add to Journalist
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "journalist")] <- "Journalist"

# add to IT
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^IT")] <- "IT"

# add to finance
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "[iI]nvestment")] <- "Finance"

# aggregate insurance
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^[iI]nsurance")] <- "Insurance"

# add to IT
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "Information Technology")] <- "IT"

# add to self employed
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "Bar & Restaurant Owner")] <- "Self Employed"

# add to Student 
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "Graduate")] <- "Student"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "full time student and part time bartende")] <- "Student"

# add to Teacher
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "teacher$")] <- "Teacher"

# aggregate Financial
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "[fF]inancial")] <- "Finance"

# aggregate Fitness
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "Fitness")] <- "Fitness"

# aggregate film
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "[fF]ilm")] <- "Film"

# add to teacher
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "Teacher")] <- "Teacher"

# add to Entrepreneur
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "[eE]ntrepreneur")] <- "Entrepreneur"

# add to Student
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "Education")] <- "Student"

# add to Editor
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^Editor")] <- "Editor"

# aggregate Economist
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^Econom")] <- "Economist"

# add to Student
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^Doctoral Candidate!!!")] <- "Student"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "Doctor Research")] <- "Student"

# aggregate customer service
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "Customer Service")] <- "Customer Service"

# aggregate legal
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "^[lL]egal")] <- "Legal"

# add to student
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "Computer Science")] <- "Student"

# add to software 
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "Computer Programmer")] <- "Software"

# aggregate Civil Servant
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "[cC]ivil")] <- "Civil Servant"

# add to Self Employed
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "businesswoman")] <- "Self Employed"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "Business Owner")] <- "Self Employed"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "self employed$")] <- "Self Employed"

# aggregate Attorney 
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "[aA]ttorney")] <- "Attorney"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "In-house Legal Counsel")] <- "Attorney"

# add to college  
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "Asst. Prof.")] <- "college professor"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "[aA]ssistant [pP]rofessor")] <- "college professor"
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "university faculty")] <- "college professor"

# add to student
df.procrastination$Occupation[grep(df.procrastination$Occupation, pattern = "Utterly shiftless arts student")] <- "Student"

# return to factor
df.procrastination$Occupation <- as.factor(df.procrastination$Occupation)
# set to int
df.procrastination$Sons <- as.integer(df.procrastination$Sons)

# e
df.procrastination$DPMean <- mean(df.procrastination$BdWstTime + df.procrastination$BdDelayAct 
                                  + df.procrastination$BdMustAct + df.procrastination$BdDelayMake 
                                  + df.procrastination$BdPutOff)

df.procrastination$AIPMean <- mean(df.procrastination$GdPayBills + df.procrastination$GdPunctual 
                                   + df.procrastination$GdPrepared + df.procrastination$BdRunLate 
                                   + df.procrastination$BdCmpltTm + df.procrastination$GdWantGrow 
                                   + df.procrastination$BdFamView + df.procrastination$GdTmToSpare 
                                   + df.procrastination$BdDeadline + df.procrastination$BdTmMngmt 
                                   + df.procrastination$GdDocApt + df.procrastination$GdMorePunct 
                                   + df.procrastination$GdMaint + df.procrastination$BdXpctLate 
                                   + df.procrastination$BdCostMoney)

df.procrastination$GPMean <- mean(df.procrastination$BdLateStart 
                                  + df.procrastination$BdMissTckts 
                                  + df.procrastination$GdPartyPln 
                                  + df.procrastination$GdEasyWake 
                                  + df.procrastination$BdSlowMail 
                                  + df.procrastination$GdRtnCalls 
                                  + df.procrastination$BdWrkr 
                                  + df.procrastination$GdDecisive 
                                  + df.procrastination$BdWrkDelay 
                                  + df.procrastination$BdAirport 
                                  + df.procrastination$GdSocial 
                                  + df.procrastination$BdDeadline 
                                  + df.procrastination$GdSMLBill 
                                  + df.procrastination$GdRSVP 
                                  + df.procrastination$GdTask 
                                  + df.procrastination$BdBdayGFT
                                  + df.procrastination$BdEsntlItem 
                                  + df.procrastination$GdCmplteDay
                                  + df.procrastination$BdDoTmrow
                                  + df.procrastination$GdEvngTasks)

df.procrastination$SWLSMean <- mean(df.procrastination$GdIdealLIfe + df.procrastination$GdExclLife 
                                  + df.procrastination$GdSatisLife + df.procrastination$GdImprtThs 
                                  + df.procrastination$GdChangeNil)


#3
html.hdi <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_Human_Development_Index#Complete_list_of_countries")

# find the index of the tables
tables.hdi <- html_nodes(html.hdi, "table")

fetchTable <- function(index) {
  tables <- html.hdi %>%
    html_nodes("table") %>%
    .[index] %>%
    html_table(fill = T)
  data.frame(tables[1])
}

# very high human development tables 
df.hdi.vh1 <- fetchTable(4)
df.hdi.vh2 <- fetchTable(5)

# high human development tables
df.hdi.h1 <- fetchTable(7)
df.hdi.h2 <- fetchTable(8)

# medium human development tables
df.hdi.m1 <- fetchTable(10)
df.hdi.m2 <- fetchTable(11)

# low human development tables
df.hdi.l1 <- fetchTable(13)
df.hdi.l2 <- fetchTable(14)

# delete columns names in first row
df.hdi.vh1 <- df.hdi.vh1[-c(1), ]
df.hdi.vh2 <- df.hdi.vh2[-c(1), ]
df.hdi.h1 <- df.hdi.h1[-c(1), ]
df.hdi.h2 <- df.hdi.h2[-c(1), ]
df.hdi.m1 <- df.hdi.m1[-c(1), ]
df.hdi.m2 <- df.hdi.m2[-c(1), ]
df.hdi.l1 <- df.hdi.l1[-c(1), ]
df.hdi.l2 <- df.hdi.l2[-c(1), ]

# add column names
names.hdi <- c("Rank", "RankChange", "Country", "HDI", "HDIChange")
names(df.hdi.vh1) <- names.hdi
names(df.hdi.vh2) <- names.hdi
names(df.hdi.h1) <- names.hdi
names(df.hdi.h2) <- names.hdi
names(df.hdi.m1) <- names.hdi
names(df.hdi.m2) <- names.hdi
names(df.hdi.l1) <- names.hdi
names(df.hdi.l2) <- names.hdi

# remove unnecessary columns
deleteUnusedHDIColumns <- function(df) {
  df$Rank <- NULL
  df$RankChange <- NULL
  df$HDIChange <- NULL
  df
}

df.hdi.vh1 <- deleteUnusedHDIColumns(df.hdi.vh1)
df.hdi.vh2 <- deleteUnusedHDIColumns(df.hdi.vh2)
df.hdi.h1 <- deleteUnusedHDIColumns(df.hdi.h1)
df.hdi.h2 <- deleteUnusedHDIColumns(df.hdi.h2)
df.hdi.m1 <- deleteUnusedHDIColumns(df.hdi.m1)
df.hdi.m2 <- deleteUnusedHDIColumns(df.hdi.m2)
df.hdi.l1 <- deleteUnusedHDIColumns(df.hdi.l1)
df.hdi.l2 <- deleteUnusedHDIColumns(df.hdi.l2)