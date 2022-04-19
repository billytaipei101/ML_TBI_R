# Application of Random Forest to classify EEG data using R

This project is an example on how to use a machine learning algorithm in R for classification purposes; the Random Forest computational framework was used to classify EEG data of a sample of patients with acute Traumatic Brain Injury (mTBI) and Control subjects.

The data used was obtained from an [OpenNeuro repository](https://openneuro.org/datasets/ds003523/versions/1.1.0) (RRID: ds003523) but the subjects that conformed the final groups for the present analysis, acute Traumatic Brain Injury patients (TBI, *n* = 27) and Healthy Controls (HC, *n* = 27), were matched using demographic variables and their score in a Visuospatial Working Memory (VSWM) task, thus ensuring that both groups did not differ significantly by age (*p* = 0.67), sex (*p* = 0.58), nor by hit ratio (*p* = 0.97). The EEG epoched signals covering three memory phases (i.e. Baseline, Encoding, Retention) were analyzed to extract 5 frequency components from each of the 63 scalp sites during the aforementioned phases. The EEG data was labelled by group and separated as either correct or incorrect for classification purposes.



## Descriptive Analysis and Statistics

To begin with the analysis, create a new folder on your local disk and name it `EEG_VisualWorkingMemory`, inside this folder put the reposoritory available in ds003523 data set ([OpenNeuro repository](https://openneuro.org/datasets/ds003523/versions/1.1.0)) 

To run the `descriptiveAnalysis.R` you must change line 13 to the directory that currently has the ds003523 data set in it.

## Machine Learning

Afterwards we can run an example implementing the Random Forest Algorithm for classification purposes with a smaller sample using plain files that have been previously preprocessed in MATLAB.  You need some of the files provided inside the `EEG_VisualWorkingMemory` folder in order to run the second R script `VisualWorkingMemory.R`. Change line 6 with the corresponding directory.

The section that starts from line 81 ***preprocessing of files per condition*** has to be run twice, the first time for the *incorrect* files and a second time for the *correct* files, until line 197, so you need to change the end of line 84, 85, 189 and 190, exchanging the words correct-incorrect each time, or 0 and 1 correspondingly as indicated in the commented text.

Due to the limitation of large files to be stored in Github the folders `incorrect` and `correct` are stored separatadely in this link [Drive](https://drive.google.com/drive/folders/1ZczZzXB8vkXNWA5urG2n0djD7sDoRI9_?usp=sharing).

The section that stars from line 217 ***Models*** needs to be run for each data set considered, as an example the first model classication of trial accuracy when considering only the Healhty controls data (*Model 1*)

1. Accuracy classification of Healthy Controls = controlOnly
2. Accuracy classification of Patients = mtbiOnly
3. Classification of trials into groups given correct responses = correctOnly
4. Classification of trials into groups given incorrect responses = incorrectOnly









