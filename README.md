# Application of Random Forest to classify EEG data using R

This project is an example on how to use a machine learning algorithm in R for classification purposes; the Random Forest computational framework was used to classify EEG data of a sample of patients with acute Traumatic Brain Injury (mTBI) and Control subjects.

The data used was obtained from an [OpenNeuro repository](https://openneuro.org/datasets/ds003523/versions/1.1.0) (RRID: ds003523) but the subjects that conformed the final groups for the present analysis, acute Traumatic Brain Injury patients (TBI, *n* = 27) and Healthy Controls (HC, *n* = 27), were matched using demographic variables and their score in a Visuospatial Working Memory (VSWM) task, thus ensuring that both groups did not differ significantly by age (*p* = 0.67), sex (*p* = 0.58), nor by hit ratio (*p* = 0.97). The EEG epoched signals covering three memory phases (i.e. Baseline, Encoding, Retention) were analyzed to extract 5 frequency components from each of the 63 scalp sites. The EEG data was labelled by group and separated as either correct or incorrect for classification purposes.

