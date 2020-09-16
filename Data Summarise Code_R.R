library(plyr)
library(dplyr)

# FILES NEEDED TO BE IMPORTED "OSCC_with_Sample_type.xlsx" and "Clinical data.xlsx"


filtered <- filter(OSCC_with_Sample_type)
#filtered <- filter(OSCC_with_Sample_type, PrimarySite == "Palate", SampleType == "solid tissue normal")
diseasetype <- select(filtered, DiseaseType)
diseasetype
filtered <- select(filtered, Files:Bio, Slides)
nrow(filtered)
colSums(filtered)
files <-  with(filtered, table(Files))
seq <-  with(filtered, table(Seq))
exp <-  with(filtered, table(Exp))
snv <-  with(filtered, table(SNV))
cnv <-  with(filtered, table(CNV))
meth <-  with(filtered, table(Meth))
clinical <-  with(filtered, table(Clinical))
bio <-  with(filtered, table(Bio))
slides <-  with(filtered, table(Slides))
diseasetype <-  with(diseasetype, table(DiseaseType))
(diseasetype/nrow(filtered))*100
(files[1]/nrow(filtered)-1)*100
(seq[1]/nrow(filtered)-1)*100
(exp[1]/nrow(filtered)-1)*100
(snv[1]/nrow(filtered)-1)*100
(cnv[1]/nrow(filtered)-1)*100
(meth[1]/nrow(filtered)-1)*100
(clinical[1]/nrow(filtered)-1)*100
(bio[1]/nrow(filtered)-1)*100
(slides[1]/nrow(filtered)-1)*100

Stage <- select(Clinical_data, ajcc_clinical_stage)
Stage <-  with(Stage, table(ajcc_clinical_stage))
Stage / nrow(Clinical_data)*100

test <- select(OSCC_with_Sample_type, CaseID:Slides)
Stage <- select(Clinical_data, ajcc_clinical_stage, submitter_id...2)
colnames(Stage)[colnames(Stage) == "ajcc_clinical_stage"] <-  "Stages"
colnames(Stage)[colnames(Stage) == "submitter_id...2"] <-  "CaseID"
joined <- join(test, Stage)
stage_specific_data <- filter(joined, Stages == "Stage IVC")
stage_specific_data
stage_specific_data <- select(stage_specific_data, Files:Bio, Slides)
nrow(stage_specific_data)
colSums(stage_specific_data)