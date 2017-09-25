
#----------------------#
#### Initial section ####
#----------------------#
source("cred.R")
source("functions.R")

# Sign in to google
gs_auth() # this might launch a browser so you can sign into your account
ScannedTests.url = gs_url("https://docs.google.com/spreadsheets/d/1js6XcxzF4y3uFtc_Uxr8e3UfvvcKjUrQzL8lKV2st1I/edit") #enter the URL of the scanned test document here

# Read and format the last time the program was run
lastTime = gs_read(ss = ScannedTests.url, ws = 2)
lastTime = as.character(lastTime$LastTime[1])
lastTime = strptime(lastTime, format = "%Y-%m-%d %H:%M:%S")
lastTime = as.POSIXct(lastTime)

#----------------------#
#### LinkIt section ####
#----------------------#
# set the account
myAccount = Account$new("production")
myAccount$setPriKey(ProdKey$Private)
myAccount$setPubKey(ProdKey$Public)

# Get all the results since lastTime
ResultFrame = RecentResults(acct = myAccount, handle = LinkItHandle, lastTime = lastTime)

#-------------------------#
#### Check the results ####
#-------------------------#

# See if there are any results that need to be resolved before they can be reported
# Counts of results in each grading status
summary(factor(ResultFrame$GradingStatus)) 

# tests that need review
unique(ResultFrame$TestName[ResultFrame$GradingStatus %in% c("Incomplete", "Multi-Mark")]) 

# Sections that need review
unique(apply(X = ResultFrame[ResultFrame$GradingStatus %in% c("Incomplete", "Multi-Mark"),c("CourseName", "Section")], MARGIN = 1, paste, collapse = " "))

# Not really sure why the next line is necessary
ResultFrame = ResultFrame[ResultFrame$ModifiedDate >= lastTime,]

# Summarize the results since lastTime
ResultSummary = SummarizeResults(ResultFrame)

# store the date and time of the current run
gs_edit_cells(ss = ScannedTests.url, ws = 2, input = Sys.time(), anchor = "A2") 

#----------------------#
#### Report section ####
#----------------------#

# Read in the TABLE (Test Address Book, LinkIt Edition)
TAB = read.xlsx(xlsxFile = "J:/tests/2017-2018/TAB.xlsx")

# Check for tests not included in the tab
missingTests = ResultSummary$TestName[!(ResultSummary$TestName %in% TAB$TestName)]
print(missingTests)
# If there are any missing tests, add them to the TAB
# write.csv(x = ResultSummary[!(ResultSummary$TestName %in% TAB$TestName), c("TestID", "TestName")], file = "addtoTAB.csv")
# After adding them to the tab, reload it
TAB = read.xlsx(xlsxFile = "J:/tests/2017-2018/TAB.xlsx")

# Print out the tests that will need score files exported from LinkIt
ResultSummary$TestName
# Download the student item response files for those exams.  
# Protip: select all of them and click View Summary.  
#         Then, for each one, click on the magnifying glass to get to the exports.
#         When you are done with each one, click the X to close it.
# Put the exported files in a folder called "exports" inside the folder for that test

# Generate the reports
for(i in 1:nrow(ResultSummary)){
  generateReport(DataLocation = TAB$Folder.location[TAB$TestName == ResultSummary$TestName[i]])
}



#--------------------------#
#### Monitoring section ####
#--------------------------#


# Get the current Scanned Tests document and then clear it out
ScannedTests = gs_read(ss = ScannedTests.url, ws = 1)
if(nrow(ScannedTests) > 0){
  Blank = matrix(data = "", nrow = nrow(ScannedTests), ncol = ncol(ScannedTests)) 
  gs_edit_cells(ss = ScannedTests.url, ws = 1, input = Blank, anchor = "A2") # Start at A2 to leave the header row in place
}

# Modify ScannedTests to include the newly scanned tests
NewScannedTests = data.frame(Test = ResultSummary$TestName, Folder = TAB$Folder.location[match(ResultSummary$TestName,TAB$TestName)], Analyze = T, Update = F, Monitor = T)
AllScannedTests = rbind(ScannedTests, NewScannedTests)
UniqueScannedTests = AllScannedTests[!duplicated(AllScannedTests$Test),]
for(i in 1:nrow(UniqueScannedTests)){
  for(j in c("Analyze","Update","Monitor")){
    UniqueScannedTests[1,j] = any(unlist(AllScannedTests[AllScannedTests$Test == UniqueScannedTests$Test[i],j]))
  }
}

# This next line has not been tested
# Remove from UniqueScannedTests any records that require no action
# UniqueScannedTests = UniqueScannedTests[apply(X = UniqueScannedTests[,c("Analyze","Update","Monitor")], MARGIN = 1, FUN = any),]



# Update the Scanned Tests document with the modified ScannedTests 
gs_edit_cells(ss = ScannedTests.url, ws = 1, input = UniqueScannedTests, anchor = "A1") # Start at A1 b/c the header row is also added




#--------------------------#
#### Follow up section ####
#--------------------------#

# Go to the Scanned Tests document
# If the Analyze column is marked TRUE, send an email narrative
# If the Analyze column is not marked TRUE but the Update column is, send an email with the update
# Either way, attach the score report xlsx file
# When you send an analysis or update, delete the values in the Analyze or Update columns
# When you do score monitoring, delete the values in the Monitor column




