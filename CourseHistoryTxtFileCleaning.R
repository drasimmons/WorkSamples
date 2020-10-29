#########################################################################################################
#########################################################################################################
# course history files read in and cleaning 
# as part of historical physics grade analytics project

# term codes 1128-1182

# read and format course history txt files 
# all files have identical formatting

library(stringr)


# function to read in txt file from local hard drive and complete light formatting

read_chtxt <- function(txtfile_loc, term_code){
    term <- read.fwf(file = txtfile_loc, widths=c(18,12,2,15,7,14,9,14,25))
    term$V4 <- NULL
    colnames(term) <- c('courseNum','classNum','component','meetDays','classTime','classRoom',
                        'classEnrollment','I_EMPL')
    term <- subset(term,!is.na(term$courseNum))
    term <- subset(term, term$classEnrollment != '0/0')
    term$courseNum <- substring(term$courseNum,9)
    term$TERM <- toString(term_code)
    
    return(term)
}


# In the following section, I read in each txt file I need to create a complete
# set of all terms to be examined. After binding the first two terms' files, 
# each additional term is appended onto the complete data set I call "courseHistory"

t1128 <- read_chtxt('E:\\PER\\UndergradDataBase\\CourseHistoryTxtFiles\\1128.txt',1128)
t1132 <- read_chtxt('E:\\PER\\UndergradDataBase\\CourseHistoryTxtFiles\\1132.txt',1132)

courseHistory <- rbind(t1128,t1132)


t1134 <- read_chtxt('E:\\PER\\UndergradDataBase\\CourseHistoryTxtFiles\\1134.txt',1134)

courseHistory <- rbind(courseHistory,t1134)


t1138 <- read_chtxt('E:\\PER\\UndergradDataBase\\CourseHistoryTxtFiles\\1138.txt',1138)

courseHistory <- rbind(courseHistory,t1138)


t1142 <- read_chtxt('E:\\PER\\UndergradDataBase\\CourseHistoryTxtFiles\\1142.txt',1142)

courseHistory <- rbind(courseHistory,t1142)


t1144 <- read_chtxt('E:\\PER\\UndergradDataBase\\CourseHistoryTxtFiles\\1144.txt',1144)

courseHistory <- rbind(courseHistory,t1144)


t1148 <- read_chtxt('E:\\PER\\UndergradDataBase\\CourseHistoryTxtFiles\\1148.txt',1148)

courseHistory <- rbind(courseHistory,t1148)


t1152 <- read_chtxt('E:\\PER\\UndergradDataBase\\CourseHistoryTxtFiles\\1152.txt',1152)

courseHistory <- rbind(courseHistory,t1152)


t1154 <- read_chtxt('E:\\PER\\UndergradDataBase\\CourseHistoryTxtFiles\\1154.txt',1154)

courseHistory <- rbind(courseHistory,t1154)


t1158 <- read_chtxt('E:\\PER\\UndergradDataBase\\CourseHistoryTxtFiles\\1158.txt',1158)

courseHistory <- rbind(courseHistory,t1158)


t1162 <- read_chtxt('E:\\PER\\UndergradDataBase\\CourseHistoryTxtFiles\\1162.txt',1162)

courseHistory <- rbind(courseHistory,t1162)


t1164 <- read_chtxt('E:\\PER\\UndergradDataBase\\CourseHistoryTxtFiles\\1164.txt',1164)

courseHistory <- rbind(courseHistory,t1164)


t1168 <- read_chtxt('E:\\PER\\UndergradDataBase\\CourseHistoryTxtFiles\\1168.txt',1168)

courseHistory <- rbind(courseHistory,t1168)


t1172 <- read_chtxt('E:\\PER\\UndergradDataBase\\CourseHistoryTxtFiles\\1172.txt',1172)

courseHistory <- rbind(courseHistory,t1172)


t1174 <- read_chtxt('E:\\PER\\UndergradDataBase\\CourseHistoryTxtFiles\\1174.txt',1174)

courseHistory <- rbind(courseHistory,t1174)


t1178 <- read_chtxt('E:\\PER\\UndergradDataBase\\CourseHistoryTxtFiles\\1178.txt',1178)

courseHistory <- rbind(courseHistory,t1178)


t1182 <- read_chtxt('E:\\PER\\UndergradDataBase\\CourseHistoryTxtFiles\\1182.txt',1182)

courseHistory <- rbind(courseHistory,term1182)


######################
# some final cleaning 

# eliminating excess white space within each column

courseHistory$I_EMPL <- trimws(courseHistory$I_EMPL)
courseHistory$courseNum <- trimws(courseHistory$courseNum)
courseHistory$classNum <- trimws(courseHistory$classNum)
courseHistory$component <- trimws(courseHistory$component)
courseHistory$meetDays <- trimws(courseHistory$meetDays)
courseHistory$classTime <- trimws(courseHistory$classTime)
courseHistory$classRoom <- trimws(courseHistory$classRoom)
courseHistory$classEnrollment <- trimws(courseHistory$classEnrollment)
courseHistory$TERM <- trimws(courseHistory$TERM)

# removing course listings that were enrolled in by students
courseHistory <- courseHistory[courseHistory$classEnrollment!='0/0',]

# removing the lab section for each course (and blank listings)
courseHistory <- courseHistory[courseHistory$component != 'B',]
courseHistory <- courseHistory[courseHistory$courseNum!='',]

# keeping only the courses of interest to our study
courseHistory <- courseHistory[courseHistory$courseNum == '1250' | courseHistory$courseNum == '1251' | 
                                 courseHistory$courseNum == '1200' | courseHistory$courseNum == '1201' | 
                                 courseHistory$courseNum == '1260' | courseHistory$courseNum == '1261' |
                                 courseHistory$courseNum == '2300' | courseHistory$courseNum == '1250H' |
                                 courseHistory$courseNum == '1251H' | courseHistory$courseNum == '2301',]

# removing some excess special characters in the employee_id columns
courseHistory$I_EMPL <- str_replace(courseHistory$I_EMPL, " \\(.*\\)", "")
courseHistory$I_EMPL <- str_replace(courseHistory$I_EMPL, "\\{.*\\}", "")
courseHistory$I_EMPL <- str_replace(courseHistory$I_EMPL, "\\(SI", "")
courseHistory$I_EMPL <- trimws(courseHistory$I_EMPL)
courseHistory$I_EMPL <- as.numeric(courseHistory$I_EMPL)  #this makes courses with two instructors listed NA

# removing any rows without a listed meeting day and instructor
courseHistory <- subset(courseHistory, courseHistory$meetDays != "" & courseHistory$I_EMPL != "")

# removing the additional descriptor of other OSU campuses (partner data set contains this info,
# so throwing it away entirely here)
courseHistory$classNum <- str_replace(courseHistory$classNum, "LMA", "")
courseHistory$classNum <- str_replace(courseHistory$classNum, "MNS", "")
courseHistory$classNum <- str_replace(courseHistory$classNum, "MRN", "")
courseHistory$classNum <- str_replace(courseHistory$classNum, "NWK", "")
courseHistory$classNum <- str_replace(courseHistory$classNum, "WST", "")
courseHistory$classNum <- trimws(courseHistory$classNum)
courseHistory$classNum <- as.integer(courseHistory$classNum)


#########################################################################################################