library(readxl)
library(sets)
library(Hmisc)
library(stringr)
library(reshape2)

## The general idea for this script is: 
## read in the coded and non-coded answers. 
## Then, for every question with ordered answers, 
## we need to set up an ordered factor in R, and a 
## normal factor for all other questions. 


## This is the data file downloaded as "normal"
answers.text <- as.data.frame(read_excel('SurveyReport-8442723-06-13-2021-T115103.809.xlsx', "Raw Data"))
## This is the data file downloaded with "codes" from survey website
answers.coded <- as.data.frame(read_excel('SurveyReport-8442723-06-13-2021-T173313.482.xlsx', "Raw Data"))
allQuestions <- as.data.frame(read_excel("All_Questions_From_Survey.xlsx", col_names=F))

which(!allQuestions[[1]]%in%colnames(answers.coded))






combineAnswers <- function(vocab, ans.to.comb){
  
  combine.index <- sapply(ans.to.comb, function(x){
    grep(pat=paste0("^", Hmisc::escapeRegex(x), "$"), x = vocab)
  })
  
  stopifnot(length(combine.index) == length(ans.to.comb))
  
  vocab[[combine.index[1]]] <- paste(ans.to.comb, collapse=", ")
  vocab <- vocab[-combine.index[-1]]
  return(vocab)
}

apply.surv.stucture <- function(survey.text, survey.coded, questionTypes){
  
  clmns <- intersect(questionTypes[[1]], colnames(survey.text))
  for(cl in clmns){
    myx <- which(questionTypes[[1]]==cl)
    typ <- questionTypes[[2]][myx]
    ## getting the same orders as the coding in the survey, to transfer this coding into R factors
    factorLevelsOrdered <- unique(na.omit(survey.text[[cl]][order(survey.coded[[cl]])]))

    switch(typ, 
           "Categorical" = {
             survey.text[[cl]] <- factor(survey.text[[cl]], levels = factorLevelsOrdered)
           }, "Ordinal" = {
            # browser()
             survey.text[[cl]] <- factor(survey.text[[cl]], levels = factorLevelsOrdered, ordered = TRUE)
           }, "Binary" = {
            survey.text[[cl]] <- as.logical(survey.text[[cl]]=="Yes")
           })
   
  }
  ## NA codes the no category for these
  multipleChoiceQs <- questionTypes[questionTypes[[2]] == "Multiple Choice",1]

  for(mCQ in multipleChoiceQs){
    # browser()
    clms <- grep(pattern=mCQ, x=colnames(survey.text))
    for(cl in clms){
      survey.text[[cl]][!is.na(survey.text[[cl]])] <- "Yes"
      survey.text[[cl]][is.na(survey.text[[cl]])] <- "No"
      survey.text[[cl]] <- as.logical(survey.text[[cl]]=="Yes")
    }
  }
  return(survey.text)
}



makeComparison <- function(q1, q2, data){
  # q1 <- make.names(q1)
  # q2 <- make.names(q2)
  # browser()
  
  q1.ans <- data[,q1]
  q2.ans <- data[,q2]
  if((is.ordered(q1.ans) || is.numeric(q1.ans)) && (is.ordered(q2.ans) || is.numeric(q2.ans))){
    
    test.res <- cor.test(as.numeric(q1.ans), as.numeric(q2.ans), method="kendall")
    
  } else if(is.ordered(q1.ans) && !is.ordered(q2.ans)){
    
    q1.ans <- as.numeric(q1.ans)
    
    test.res <- kruskal.test(q1.ans, g=q2.ans)
    
    
  } else if(is.ordered(q2.ans) && !is.ordered(q1.ans)){
    q2.ans <- as.numeric(q2.ans)
    
    test.res <- kruskal.test(q2.ans, g=q1.ans)
    
  } else {
    
    test.res <- fisher.test(q1.ans, q2.ans)
    
  }
  return(test.res)
}




mbp.surv <- apply.surv.stucture(answers.text, answers.coded, allQuestions)
completed.rows <- mbp.surv[["Response Status"]]=="Completed"

mbp.surv <- mbp.surv[completed.rows,]

# lets calculate the PHQ-9 and GAD-7 Scores

phq9_qs <- readLines("phq9_questions.txt")
gad7_qs <- readLines("gad7_questions.txt")

## NB: There is a person who missed some of the questions? Excluding from analysis since I cannot calculate full score 
phq9_scores <- rowSums(data.frame(lapply(mbp.surv[,phq9_qs], function(x) return(as.numeric(x)-1)), check.names=FALSE), na.rm = FALSE)
gad7_scores <- rowSums(data.frame(lapply(mbp.surv[,gad7_qs], function(x) return(as.numeric(x)-1)), check.names=FALSE), na.rm = FALSE)

mbp.surv[["PHQ-9 score"]] <- phq9_scores
mbp.surv[["GAD7"]] <- gad7_scores


#### Now lets do the analysis (finally)

## Checking which columns have low answer diversity (>90% of answers the same)

message("These columns have low diversity of answers:")
message(paste(colnames(mbp.surv)[which(apply(mbp.surv, 2, function(x) max(table(x)) > length(x) * 0.9))], collapse = "; \n"))

### Still need to clean up the comparisons to make, then read in the table here:
### Read in comparisons to make
questions.to.compare <- data.frame(read_excel("MBP-Mental-Health-and-Wellness-Survey-Analysis-Plan_curated.xlsx", col_names = FALSE))
q1.names <- questions.to.compare[,1]
unique(q1.names[!q1.names %in% colnames(mbp.surv)])

q2.names <- questions.to.compare[,2]
unique(q2.names[!q2.names %in% colnames(mbp.surv)])

## Here I filter to all the regular questions without subgroup analysis. Needs to be updated for this year once the sheet is cleaned up.
## First lets analyze all the non-clinical scores and the non-filter to phd > 4 questions.
myx <- questions.to.compare[,2] != "As of right now, how significant are the following impediments to the frequency with which you meet with faculty?" & is.na(questions.to.compare[,3]) & questions.to.compare[,1] != "If you had an issue with mental health in the last 2 months, to whom did you turn for help? How helpful were they in addressing your mental health issue?"
myx2 <- questions.to.compare[,1] %in% names(complicated.matrix) | questions.to.compare[,2] %in% names(complicated.matrix)
myx <- myx & ! myx2
simple.comparisons <- questions.to.compare[myx,]




test <- apply(simple.comparisons, 1, function(x) {
  return(makeComparison(x[1], x[2], mbp.surv))
})

res.table <- data.frame("Question 1" = simple.comparisons[,1], "Question 2" = simple.comparisons[,2], check.names=FALSE)

res.table[["Statistical Test"]] <- sapply(test, `[[`, "method")
res.table[["p Value"]] <- sapply(test, `[[`, "p.value")
res.table[["Correlation"]] <- unlist(sapply(test, function(x){
  if(!is.null(x[["estimate"]])){
    return(x[["estimate"]])
  } else return(NA_real_)
}))


### Now lets analyze those limited to some co-horts. This also needs to be updated for this year with specific subcohorts

## First, correlating with change supervisors

q1s <- questions.to.compare[which(questions.to.compare[,3] == "Change supervisor(s)"),1]
q2s <- questions.to.compare[which(questions.to.compare[,3] == "Change supervisor(s)"),2]
for(i in seq_along(q2s)){
  q1_ans <- factor(complicated.matrix[[q1s[i]]][,"Change supervisor(s)"])
  q2_ans <- mbp.surv[,q2s[i]]
  dt <- data.frame(q1_ans, q2_ans)
  colnames(dt) <- c(q1s[i], q2s[i])
  rs <- makeComparison(q1s[i], q2s[i], dt)
  res.table <- rbind(res.table, list("Question 1" = q1s[i], "Question 2" = q2s[i], "Statistical Test" = rs$method, "p Value" = rs$p.value, Correlation = ifelse(is.null(rs[["estimate"]]), NA,rs[["estimate"]])))
}


## Now questions that are only interested in those correlating with upper years


myx <- which(questions.to.compare[,3] == "Narrow responses to those who are currently in years 2 and greater and in a MSc. program" & is.na(questions.to.compare[,2]))

mbp.surv[["Upper Year Masters"]] <- mbp.surv$`Which year of your degree are you currently in?` >= "Year 2" & mbp.surv$`What is your current degree status?` == "M.Sc. student"

q1s <- questions.to.compare[myx,1]
for(i in seq_along(q1s)){
  
  rs <- makeComparison(q1s[[i]], "Upper Year Masters", mbp.surv)
  res.table <- rbind(res.table, list("Question 1" = q1s[i], "Question 2" = "Upper Year Masters", "Statistical Test" = rs$method, "p Value" = rs$p.value, Correlation = ifelse(is.null(rs[["estimate"]]), NA,rs[["estimate"]])))
}


myx <- which(questions.to.compare[,3] == "Narrow responses to those who are currently in years 4 and greater and in a PhD program" & is.na(questions.to.compare[,2]))

mbp.surv[["Upper Year Ph.D"]] <- mbp.surv$`Which year of your degree are you currently in?` >= "Year 4" & mbp.surv$`What is your current degree status?` == "Ph.D. student"

q1s <- questions.to.compare[myx,1]
for(i in seq_along(q1s)){
  
  rs <- makeComparison(q1s[[i]], "Upper Year Ph.D", mbp.surv)
  res.table <- rbind(res.table, list("Question 1" = q1s[i], "Question 2" = "Upper Year Ph.D", "Statistical Test" = rs$method, "p Value" = rs$p.value, Correlation = ifelse(is.null(rs[["estimate"]]), NA,rs[["estimate"]])))
}

### Last two questions interested in correlations specific to upper years

myx <- which((questions.to.compare[,3] == "Narrow responses to those who are currently in years 4 and greater and in a PhD program" | questions.to.compare[,3] == "Narrow responses to those who are currently in years 2 and greater and in a MSc. program") & !is.na(questions.to.compare[,2]))

q1s <- questions.to.compare[myx,1]
q2s <- questions.to.compare[myx,2]

for(i in seq_along(q2s)){
  upper.years <- mbp.surv[["Upper Year Ph.D"]] | mbp.surv[["Upper Year Masters"]]
  q1_ans <- mbp.surv[upper.years,q1s[i]]
  q2_ans <- mbp.surv[upper.years,q2s[i]]
  dt <- data.frame(q1_ans, q2_ans)
  colnames(dt) <- c(q1s[i], q2s[i])
  rs <- makeComparison(q1s[i], q2s[i], dt)
  res.table <- rbind(res.table, list("Question 1" = q1s[i], "Question 2" = paste(q2s[i], "- Upper Years Only", sep=" "), "Statistical Test" = rs$method, "p Value" = rs$p.value, Correlation = ifelse(is.null(rs[["estimate"]]), NA,rs[["estimate"]])))
}

res.table[["FDR"]] <- p.adjust(res.table[["p Value"]], method = "fdr")
res.table <- res.table[order(res.table[,"p Value"]),]

write.csv(res.table, file="Correlation_Result_Table.csv")



stop()

## The rest is rough plotting code
res.sig <- res.table[res.table$FDR < 0.10,]

## Lets make some plots:
pres_ready <- theme(axis.title = element_text(size=24), axis.text = element_text(size=16), axis.text.x = element_text(angle = 45, hjust = 1), legend.text = element_text(size=16), title = element_text(size=20),legend.key.height = unit(1.0, 'cm'))  

## Top 5 are about changing supervisor
library(ggplot2)
change.sup.ans <- complicated.matrix[["What would you do differently right now if you were starting your program? Please select as many as apply"]][,"Change supervisor(s)"]

q1 <- "What would you do differently right now if you were starting your program? Please select as many as apply"

q2s <- res.sig[res.sig$`Question 1` == q1,"Question 2"]



for(q2 in q2s){
  toPlot <- data.frame("Change Supervisor" = change.sup.ans, mbp.surv[[q2]])
  colnames(toPlot) <- c("Change Supervisor", str_wrap(q2, width="50"))
  toPlot <- toPlot[complete.cases(toPlot),]
  pdf(paste0("figures2/ChangeSup_vs_", q2, ".pdf"), height = 10, width = 10)
  p <- ggplot(toPlot, aes_(y=as.name("Change Supervisor"), x = as.name(colnames(toPlot)[2]))) + geom_bin2d()+ scale_x_discrete() + scale_y_discrete() + pres_ready
  print(p)
  dev.off()
  
}

##Other qs

res.sig[res.sig$`Question 1` != "What would you do differently right now if you were starting your program? Please select as many as apply",]

## Gad7

q1 <- "GAD7"
q2s <- res.sig[res.sig$`Question 1` == q1,"Question 2"]


for(q2 in q2s){
  toPlot <- data.frame("GAD7" = mbp.surv$GAD7, mbp.surv[[q2]])
  colnames(toPlot) <- c(q1, str_wrap(q2, width="50"))
  toPlot <- toPlot[complete.cases(toPlot),]
  pdf(paste0("figures2/",q1,"_vs_", q2, ".pdf"), height = 10, width = 10)
  p <- ggplot(toPlot, aes_(y=as.name(q1), x = as.name(colnames(toPlot)[2]))) + geom_boxplot()+ pres_ready
  print(p)
  dev.off()
}


## PHQ-9 score



q1 <- "PHQ-9 score"
q2s <- res.sig[res.sig$`Question 1` == q1,"Question 2"]


for(q2 in q2s){
  toPlot <- data.frame("PHQ-9 score" = mbp.surv$`PHQ-9 score`, mbp.surv[[q2]])
  colnames(toPlot) <- c(q1, str_wrap(q2, width="50"))
  toPlot <- toPlot[complete.cases(toPlot),]
  pdf(paste0("figures2/",q1,"_vs_", q2, ".pdf"), height = 10, width = 10)
  p <- ggplot(toPlot, aes_(y=as.name(q1), x = as.name(colnames(toPlot)[2]))) + geom_boxplot()+ pres_ready
  print(p)
  dev.off()
}

## All others

all_others <- res.sig[!res.sig$`Question 1` %in% c("GAD7", "PHQ-9 score", 
                                    "What would you do differently right now if you were starting your program? Please select as many as apply"),]


for(ii in seq_len(nrow(all_others))){
  q1 <- all_others[ii,"Question 1"]
  q2 <- all_others[ii,"Question 2"]
  toPlot <- data.frame(mbp.surv[[q1]], mbp.surv[[q2]])
  colnames(toPlot) <- c(str_wrap(q1, width="50"), str_wrap(q2, width="50"))
  pdf(paste0("figures2/",make.names(q1),"_vs_", make.names(q2), ".pdf"), height = 10, width = 10)
  if(q2 == "Where is your lab located?"){
    toPlotTable <- table(toPlot)
    toPlotTable <- apply(toPlotTable, 2, function(x) return(x/sum(x)))
    toPlot <- melt(toPlotTable)
    p <- ggplot(toPlot, aes_(y=as.name(colnames(toPlot)[1]), x = as.name(colnames(toPlot)[2]))) + geom_tile(mapping = aes(fill = value))  + ggplot2::coord_fixed() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    toPlot <- toPlot[complete.cases(toPlot),]
    p <- ggplot(toPlot, aes_(y=as.name(colnames(toPlot)[1]), x = as.name(colnames(toPlot)[2]))) + geom_bin2d()  + ggplot2::coord_fixed()
  }
  print(p)
  dev.off()
}

## Force plotting the finances and lab location related plots

any(res.table$`Question 1` == "Where is your lab located?")
myx <- (res.table$`Question 2` == "Where is your lab located?")

q1s <- res.table$`Question 1`[myx]
q2 <- "Where is your lab located?"


for(q1 in q1s){
  toPlot <- data.frame(mbp.surv[[q1]], mbp.surv[[q2]])
  colnames(toPlot) <- c(str_wrap(q1, width="50"), str_wrap(q2, width="50"))
  pdf(paste0("figures2/",make.names(q1),"_vs_", make.names(q2), ".pdf"), height = 10, width = 10)
  if(q2 == "Where is your lab located?"){
    toPlotTable <- table(toPlot)
    toPlotTable <- apply(toPlotTable, 2, function(x) return(x/sum(x)))
    toPlot <- melt(toPlotTable)
    p <- ggplot(toPlot, aes_(y=as.name(colnames(toPlot)[1]), x = as.name(colnames(toPlot)[2]))) + geom_tile(mapping = aes(fill = value))  + ggplot2::coord_fixed() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    toPlot <- toPlot[complete.cases(toPlot),]
    p <- ggplot(toPlot, aes_(y=as.name(colnames(toPlot)[1]), x = as.name(colnames(toPlot)[2]))) + geom_bin2d()  + ggplot2::coord_fixed()
  }
  print(p)
  dev.off()
}

## Now for finances

q1 <- "To what extent would you say stress over finances affects your mental health, on a scale of 1-5?"
myx <- (res.table$`Question 1` == q1)
q2s <- res.table$`Question 2`[myx]



for(q2 in q2s){
  toPlot <- data.frame(mbp.surv[[q1]], mbp.surv[[q2]])
  colnames(toPlot) <- c(str_wrap(q1, width="50"), str_wrap(q2, width="50"))
  pdf(paste0("figures2/",make.names(q1),"_vs_", make.names(q2), ".pdf"), height = 10, width = 10)
  if(q2 == "Where is your lab located?"){
    toPlotTable <- table(toPlot)
    toPlotTable <- apply(toPlotTable, 2, function(x) return(x/sum(x)))
    toPlot <- melt(toPlotTable)
    p <- ggplot(toPlot, aes_(y=as.name(colnames(toPlot)[1]), x = as.name(colnames(toPlot)[2]))) + geom_tile(mapping = aes(fill = value))  + ggplot2::coord_fixed() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    toPlot <- toPlot[complete.cases(toPlot),]
    p <- ggplot(toPlot, aes_(y=as.name(colnames(toPlot)[1]), x = as.name(colnames(toPlot)[2]))) + geom_bin2d()  + ggplot2::coord_fixed()
  }
  print(p)
  dev.off()
}

## Comparisons for Tim

Q1s <- c(rep("Where is your lab located?", times=3), rep("PHQ-9 score", times=3), rep("GAD7", times=3), "PHQ-9 score", "GAD7")

Q2s <- c(rep(c("Since entering MBP, how often do you feel you: [are left out?]", 
    "Since entering MBP, how often do you feel you: [are isolated from others?]", 
    "Since entering MBP, how often do you feel you: [lack companionship?]"), times=3), 
  rep("On average, how many hours a week do you typically work on your graduate school project?", times=2))
res.table.tim <- res.table[0,-6]

for(i in seq_along(Q1s)){
  
  rs <- makeComparison(Q1s[[i]], Q2s[[i]], mbp.surv)
  res.table.tim <- rbind(res.table.tim, list("Question.1" = Q1s[i], "Question.2" = Q2s[[i]], "Statistical.Test" = rs$method, "p.Value" = rs$p.value, Correlation = ifelse(is.null(rs[["estimate"]]), NA,rs[["estimate"]])))
}

write.csv(res.table.tim, file="Correlation_Results_for_Tim.csv")

