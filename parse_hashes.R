library(tidyverse)
library(learnrhash)

### Read in the csv downloaded from the google form where the hashes were submitted
hash_form = read_csv(file.choose())

### Extract the answers from the hash information.
names(hash_form)[5] = "hash"
hashes = learnrhash::extract_hash(hash_form)

### Initialize a "report" that will show each student id and the tutorials that they completed.
report = tibble(student_id = unique(hashes$`Enter your student id`),
                "1-1"=NA, "1-2"=NA, "1-3"=NA, "1-4"=NA,
                "2-1"=NA, "2-2"=NA, "2-3"=NA, "2-4"=NA,
                "3-1"=NA, "3-2"=NA, "3-3"=NA, "3-4"=NA, "3-5"=NA,
                "4-1"=NA, "4-2"=NA, "4-3"=NA, "4-4"=NA,
                "5-1"=NA, "5-2"=NA, "5-3"=NA, "5-4"=NA, "5-5"=NA, "5-6"=NA, "5-7"=NA, "5-8"=NA,
                "6-1"=NA, "6-2"=NA, "6-3"=NA, "6-4"=NA, "6-5"=NA)

#### Create useful helper functions

### Check whether "enough" questions were answered in the tutorial.
check.enough = function(hash, enough=5){dim(hash)[1] > enough + 1}

### Check whether the student submitting the hash has the same id as what was entered in the tutorial.
check.idmatch = function(hash){as.numeric(hashes_curr$`Enter your student id`)[1]== as.numeric(hashes_curr$answer[hashes_curr$label=="student_id"][[1]])}

### Check whether "enough" minutes were spent on the tutorial.
check.timing = function(hash, enough=0){difftime(head(hash$timestamp, 1), tail(hash$timestamp, 1), units="mins") > enough}


### Identify which tutorial has been submitted
find.tutorial.id = function(hash){
  if("hsb2-science-math-prog" %in% hash$label){return("1-1")}
  if("count-male" %in% hash$label){return("1-2")}
  if("simple-random-sample" %in% hash$label){return("1-3")}
  if("scatterplot-added-layer" %in% hash$label){return("1-4")}

  if("levels3" %in% hash$label){return("2-1")}
  if("bandwidth-try" %in% hash$label){return("2-2")}
  if("tut0203ex1" %in% hash$label){return("2-3")}
  if("count-images" %in% hash$label){return("2-4")}

  if("cut-ex" %in% hash$label){return("3-1")}
  if("glimpse-noise" %in% hash$label){return("3-2")}
  if("preview" %in% hash$label){return("3-3")}
  if("tut0304ex4" %in% hash$label){return("3-4")}
  if("tut0305ex6" %in% hash$label){return("3-5")}

  if("hhanes-exercise" %in% hash$label){return("4-1")}
  if("gender-discrimination-perm-1000" %in% hash$label){return("4-2")}
  if("randomizing_cost_2" %in% hash$label){return("4-3")}
  if("variability_phat" %in% hash$label){return("4-4")}

  if("tut0501ex6" %in% hash$label){return("5-1")}
  if("tut0502ex7" %in% hash$label){return("5-2")}
  if("tut0503ex6" %in% hash$label){return("5-3")}
  if("tut0504ex2" %in% hash$label){return("5-4")}
  if("doctor_4" %in% hash$label){return("5-5")}
  if("Average_number_of_hours_worked" %in% hash$label){return("5-6")}
  if("smoking_3" %in% hash$label){return("5-7")}
  if("vocabulary_5" %in% hash$label){return("5-8")}

  if("ex110" %in% hash$label){return("6-1")}
  if("exer21" %in% hash$label){return("6-2")}
  if("ex310" %in% hash$label){return("6-3")}
  if("exer45" %in% hash$label){return("6-4")}
  if("ex55" %in% hash$label){return("6-5")}
}


#####




### Identify unique submissions to the google form (i.e., unique hashes).
submission_times = unique(hashes$Timestamp)

### Consider each unique hash in turn.
for (i in submission_times){
  hashes_curr = hashes[hashes$Timestamp == i,]
  ## Proceed only with "valid" hashes
  if(check.enough(hashes_curr) & check.idmatch(hashes_curr) & check.timing(hashes_curr)){
    # Identify which tutorial has been submitted
    tutorial_id = find.tutorial.id(hashes_curr)
    # Indicate that this student has completed this tutorial in the report
    report[report$student_id == hashes_curr$`Enter your student id`[1], colnames(report)==tutorial_id] = TRUE
  }
}

report$total= rowSums(report[,-1], na.rm = TRUE)


