---
title: "README.md"
author: "Om Mudgal"
date: "2026-11-09"
output: md_document
---

# Proxy Means Testing using ML: Who do government schemes miss?

HCES data used to find the bottom 20% by MPCE across three models: a logit model, elastic net, and boosted trees. The logit model had the lowest exclusion error, but it selected roughly twice as many people as the other two. Across all three models, Non-SC/ST/OBCs, buddhists, and people from Andhra Pradesh and Tamil Nadu were most likely to be excluded unfairly.
![A chart showing exclusion rates for the XGBoost model by religion. Buddhism is the highest,
followed by Christianity, Hinduism, Others, and Islam] (exclusion_by_religion.png)

##Data

The raw data was HCES 2022-23 and 2023-24, sourced from the Indian government's microdata 
portal. It is not included, as the terms bar you from doing so, but it is available for free at https://microdata.gov.in/NADA/index.php/catalog/224/ (2022-23) and https://microdata.gov.in/NADA/index.php/catalog/237/ (2023-24). 

##Method

The target variable was MPCE (Monthly Per Capita Consumption and Expenditure). This
variable had to be built by hand, as it is not published alongside the rest of the data.
Unfortunately, even the weighted MPCE I built was slightly off from the published national
averages for urban areas (~Rs. 800). 
I used an out-of-time design, trained on 2022-23, tested on 2023-24 to avoid temporal
spillovers.
The states were used as folds for the 5-fold cross-validation to calibrate the 
hyperparameters for the ML models (Enet and XGBoost). The values for the parameters are
available in cv_summary.csv
The three models I used were a logistic regression model, an elastic net model (a mix
of ridge and lasso), and gradient-boosted decision trees. The logit model served as a 
baseline to compare the ML models against.

##Results
The results are given in test_metrics.csv and overall_errors.csv. All the models beat the
random-selection baseline, in both the traditional (inclusion/exclusion) errors and
the AUC metrics. The logit model had a very low exclusion error (0.27), but that came
with a higher inclusion error as well (0.62). This was because, even while all three
models picked the same number of households, the population of poor people selected 
by the logit model was around twice that of the ML models. Evidently, it skewed towards a
higher household size. 
The ML models outperformed the logit model in ROC AUC by 7-10%, but the logit model did
better on PR AUC. 
Across all models, people who were most likely to be excluded unfairly were Buddhists,
Non-SC/ST/OBCs, and people from Andhra Pradesh and Tamil Nadu. 

##Limitations
Obviously, this is not incredibly scientific. The MPCE variable did not match the published
averages. Prediction is not causation, I am not saying the government leaves out 
Non-SC/ST/OBCs or Buddhists or people from Andhra Pradesh in scheme implementation.
This project is merely to find the limits of data in public policy. Groups with 
less than 200 households were flagged in the fairness audit and not reported because
of noise.

##Reproduction and layout
The script is built to be run in order. Most of what each section of code does is flagged
within the script itself. One thing to note: 
```dev_mode = TRUE
```
means that the script will only use 30% of the data for the script. This is just to save 
time, because it takes a long time to run if you use the whole dataset. There's nothing
much to the repo layout, everything is in the same folder. 
