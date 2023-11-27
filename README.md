---
author: bbaasan
date: '2023-11-13'
about: 'working on the All of Us dataset'
---

## All OF US data  
- url: [address](https://allofus.nih.gov/)

- location: 'D:/F2023/AllOfUs'  

## Data (csv format)  
- df_A: patients personal profile: gender, race, ethnicity, etc.,  
- df_B: patients with Liver Cancer, contains: person_id, standard_concept_name, standard_concept_code, conditin_start_datetime  
- df_C: patients with other diseases: breast cancer, nervous cancer, etc., 

## Data Processed (csv format): 
- patient_with_nth_condtions: some patients have more than 1 condition (max = 8)  
    - groupby patient_id, standard_condtion_name
    - total unique patient id = 36,040

- nth_condition_wider: data toward the binary table where features are conditions  
    - consistent where some patients have up to 8 conditions  

- patient_sometype_livercondition:  
    - groupby patient_id, standard_concept_name  
    - total unique patient id = 2,171


# PROMPT  

## **Identify the Temporal Sequence of Variables**  
All independent variables should occur before dependent variables

a) Assume that age, gender, and race occur at birth.  Assume that death occurs as the last event.  
b) Establish the order with which diseases (conditions) occur.  
- Count for each pair of condition, the number of times one condition occurs before another in the same person.  Use the pairwise count of one condition occurring before another to establish a sequence of occurrence of conditions.  
- Use shifted dates in All of US to exclude diseases that occur after cancer  

## **Feature Construction**  
Create body system variables across diseases by using the hierarchy in the SNOMED ontology of diseases.  

a) For each body system, identify the conditions within All of Us that fall within the body system.  Some conditions may fall within more than one body system  

b) For each body system, regress occurrence of cancer on the conditions that occur within it.  

c) Create a new feature called "worst condition within disorder xxx", where for each patient you select the disease with largest regression coefficient.  

The following resources may be of use in this task:

Vladimir Cardenas's list of body systems and members of body system

## Predictive Modeling of Cancer** 
Create several LASSO regression models.  

a) Use indicators to address missing body systems.  Regress occurrence of cancer on body systems (include pair and triplet of body systems), and indicators (include pair and triplet of indicators).  

b) Assume that missing diseases have not occurred, i.e., replace missing values for disease conditions with 0. Regress occurrence of cancer on diseases (include pair of diseases and triplet of diseases).  

c) Regress occurrence of cancer on all independent variables including body systems, diseases, and missing value indicators.  

## Report Your findings** 
This report should include the following section and provided at approximate times indicated by email to the instructor:  

1. Background literature review should not exceed 1 page. Your one page literature review should assume a reader familiar with the literature and not exceed three paragraph.  The first paragraph should address the signficance of the area you are addressing, including prevlance of the cancer and importance of early detection in improving outcomes of care.  The second paragraph should describe how US Preventive Task Force recommends who should be screened and point out that such an approach misses importance risk factors.  This paragraph should list the risk factors missed and reference articles that point these risk factors.  The paragraph should not exceed two or three sentences but can have numerous references.  The last paragraph should discuss how your paper provides a comprehensive review of non-genetic risk factors by examining all of the patients' medical history.  Background section should be a brief synthesis of existing research findings related to the problem being addressed in the study. This section is due in 3rd week of the course.  

2. Method section should be a complete description of the methods; and there is no page limit but brevity is appreciated. It should include a paragraph or a sentence on source of data. It should describe the inclusion and exclusion criteria for the creation of the cohort and compare these criteria to what has been done in the literature. It should have a sentence or a paragraph, with citations, on definition of the dependent/response variable.  It should have a sentence or a paragraph on number of, and definition of, independent variables; including interaction among pairs, and triplets of variables. These statements should clarify how missing values were treated and explain what steps were taken to ensure that independent variables occur prior to response/dependent variable. There should be a paragraph on feature construction.  In particular, it should describe how ontological adjustmetns were made to construct body systems or other features. This work should reference relevant papers on feature construction.  There should be a sentence or two on how data were transformed to meet assumptions of regression. There should be a paragraph on analytical methods used, e.g. LASSO, and how hyperparameters were set. e The methods paragraph should describe in one sentence how the performance of the US Preventive Task Force recommendations was simulated given that some key variables were missing. The method section is due 2 weeks after lecture on LASSO regression.   

3. Results section should describe the findings and there is no page limit.  Table 1 should be description of the population studied.  Figures and additional tables should summarize the statistical findings. These should include parameters of your model and the fit between the model and data. Include the fit between US Preventive Task Force model and the data. Describe the fit between the data with and without interaction terms. Describe the fit with the data with and without feature construction. There should not be any discussion of findings in the result section.  This section should be complete 3 weeks before end of the course.  

4. Discussion section should include 4 distinct sections and there is no page limits.  The first section should be a summary of the key findings.  The second section should be a review of support for the findings in the literature. The third section should summarize study limitations.  The last section should conclude with policy implications. This section is due at last week of classes.
Example Completed Assignments: All of the following listed projects use patient's medical history to screen for the indicated disease. The recommendation for screening that emerges from these projects differs radically from the recommendation of US Preventive Services Task Force. The Task Force has systematically ignored how data in EHRs can be used to improve screening.  The projects listed here are pilot studies designed to understand the nature of the data.  Some of these projects have low percent of variation explained, which suggests alternative analysis is necessary. For more completed published studies, please use PubMed. All of these pilot projects use data from All of Us database.  All of these projects construct network models, through regression analysis.

## Summary of GLM (using important variables with LASSO)  
![GLM Summary](docs/glm_important_variables.png = x.5)
