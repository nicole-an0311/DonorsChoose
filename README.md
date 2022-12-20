# DonorsChoose
# Final Project - Donors Choose Improving Funding Outcomes and User Experience
## Introduction
Participating a K-12 school fundraising campaign can be very overwhelming. With many moving parts and ever-changing components, it is important for individual philanthropists to find the right platform that delivers and accomplishes high quality projects. Donors Choose aims to integrate with the best schools and teachers to provide the donors with a distinctive user experience that both support their local communities and well-funded projects. Identifying the “exciting projects” early and recommending those projects to the right donors increase the chances of reaching a funding goal. As the website gains traffic, understanding customer behaviors not only develops a good customer rapport but also drives funding outcomes and retention rate. Forecasting for fundraising projects can be a difficult task for nonprofits because hundreds of thousands of projects are proposed to enhance education of the children each year. To help the site deliver more materials to the students thirst of knowledge, 4 machine learning models are constructed based upon the dataset given, 2 random forest models with PCA and Variable Clustering, a logistic regression model, and a unsupervised clustering model to study the donors. The objective is to make better predictions on “exciting projects” in early stage and to recommend them to the donors to improve overall user experience with more customized marketing strategies.

## Business Problem
Amid so much uncertainty about what the future holds, what projects are likely to be “exciting projects”? What factors are the most significant in predicting high quality projects in increasing overall donations? What are the key characteristics of different types of donors and how to retain them? What are the next steps for Donors Choose in order to reach their goals?

## Explanatory Analysis on Data Quality Issues
### Overview of Donation Dataset
•	Target variable
 
Project_Label	n
False (non-exciting)	291308
True (exciting)	36710

### Overview of Donor Dataset
There are 1048567 pieces of donor data in the dataset. The payment made is very left skewed, meaning that there are a few generous donors who like to give out large sum of donations, or there is a data quality issue. 

Variables	Mean	SD	Min	Max
donation_to_project	65.75060	215.43197	-11.80	85000
donation_optional_support	10.69741	32.97161	-0.02	15000
donation_total	76.44840	243.75396	-11.80	100000


### Data Quality 
•	The two levels are relatively unbalanced in terms of sizes, so accuracy of the model will tend to be very high (88.8%) for making correct predictions. However, accuracy is not a good evaluation alone because it does not entail the model’s ability to correctly identify the exciting projects from non-exciting projects. When selecting the best predictive model, not only should the model accuracy rate be higher than the default accuracy (88.8%), but the model should also have relatively high precision and recall as indications of generating correct predictions for the target class, which is the exciting class.
•	Data with >30% missing data is one of the biggest concern for the donations dataset. Poor-quality data can be the source of inaccurate analytics or economic damage. For instance, the site may endure lost marketing opportunities because of incomplete school district records and fines for improper regulatory compliance reporting with missing NCES ID data. The impute function from the imputeMissings package will fill missing categorical data with mode while filling numerical data with median.
•	There are outliers with extremely high total donations in the donors dataset. They are usually donors who donated large funds ($2000) compared to a mean total donations of  ~$71. They are removed from modelling process because these data influence the cluster analysis by creating very small clusters compared to other clusters (non-uniform sized clusters). However, these observations are a distinctive cluster and will be treated as a distinctive group.
•	The variable cluster picks up on a few variables (under the same branch) that closely correlate with the other. They may influence some models’ performance and thus are removed during feature selection. To operate more efficiently, these pairs can be joined to save space because they share similar interpretations. 
 
## Model Performance
### Prediction Models
Models	part	accuracy	roc_auc	precision	recall
Logistic Regression	training	0.9038291	0.9069763	0.6962311	0.2523055
Logistic Regression	testing	0.9040709	0.9064037	0.6980323	0.2512942
Random Forest with PCA	training	0.99580597	0.99996198	0.9670342	0.9964979
Random Forest with PCA	testing	0.98310062	0.99647849	0.8996238	0.9555899
Random Forest with Variable Cluster; trees = 500; mtry=10	training	0.99533125	0.99985316	0.9638744	0.9957757
Random Forest with Variable Cluster; trees = 500; mtry=10	testing	0.98719590	0.99760088	0.9231512	0.9647933
### •	Model selection
The best performing model with the highest testing performance is the random forest model + variable clustering.
### •	Top 10 Important Variables for Projects
 
   
The key factors that can be used for predicting exciting projects are teacher referred count, non-teacher-referred donor, and great messages proportion. All three factors seem to be positively correlated with the target variable.
### •	Operational Business Rules w. Expected Performance (Precision & Recall) 
The best model has a 92.3% precision and 96.5% recall. For instance, in 1000 exciting projects, the model will correctly identify 923 of them. Another example, out of the 1000 projects predicted as exciting projects, 965 of them are truly exciting projects. This means that the model is very good at making accurate predictions. 
  
### Clustering Model 
  
The sizes of the clusters are shown below. There are a total of 6 clusters including 1 outlier cluster which was left out of the modelling process. The cluster sizes vary a lot from approximately 90,000 to 290,000, and the “corporate sponsored convenience seekers” is the major donor group. From the cluster tree above we can see that extremely large or small total donation amount are key factors that separate donors into different groups while payment methods and optional tip amount are very important. 3 clusters are highlighted and their characters are summarized.
### Cluster #1: Corporate Sponsored Convenience Seekers 
•	Largest group with the median total donation (individual donor makes an average of ~$18.76)
•	Makes the most donations with corporate sponsored campaign gift cards (~83%)
•	Almost never pays in cash, rarely uses promo codes and goes directly to donation without visiting campaign pages 
•	Donates to the projects only
•	Geographically concentrated in low income tax states (WA, FL, and TN are in the top 5 states)
### Cluster #4: Visionary Supportive Teachers

•	Usually makes a relatively small donation (individual donor makes an average of ~$3.44)
•	All donations are under $10
•	A lot of teachers
•	Loves to visit the giving/campaign page (~44% of donations are given through campaign pages)
•	Usually gives out optional support (~85% of donations included optional support)
•	Rarely targeted to an honoree 
•	Diverse in geographical locations
### Cluster #3: Target-Oriented Generous Philanthropists

•	Makes the most generous donations (individual donor makes an average of ~$100.39)
•	Most donations are over $100 (~81%)
•	Likes to donate to a target honoree 
•	Loves to give out large tips (~91% of donations included optional support with an average amount of ~$14.33)
•	Prefers credit card or no-cash payment methods
•	Less engaged with gift cards and promotions
## Summary
### Key Findings
•	Although most donors make payments via no-cash or credit card methods, those made with double-your-impact-match yields the highest median donation. Therefore, the campaigns may incentivize donors to make more generous donations. 
   
•	In similar notes, corporate sponsored campaigns also proved to be successful because a very large group of donors use corporate gift cards, and they give out anaverage of $18.76 donations. However, this group seems to have less user stickiness because they rarely use other promotions nor visiting the campaign pages.
•	Amount of teacher-referred donors is very important for the best model to predict for exciting rate. According to the regression model, 1 increase in amount of teacher-referred donors would increase 77% chance of the project to be excited. Donors who see excited projects will tend to tip more.
•	Many donations are made by teacher’s accounts and they bring a lot of traffic to the Donors Choose’s giving/campaign pages. Although they make less-than-average donations (~$3.44), 85% of the donors support the school projects by giving optional tips are they do not target on specific honorees. 
### Recommendations
•	Donors Choose could incentivize donors by advertising exciting projects with promotional campaign pages through teachers’ google email ads. By making more buzz about the website in the teacher group, more donors will come and support school projects because teachers will refer to likely donors. This way, teachers will leave optional tips to project they think are exciting, increasing the funding process of smaller projects to bring up the traffic of the website. 
•	The site should leverage holiday traffic and the back-to-school seasons to partner with more corporations, promoting users’ favorite payment promotions (double-your-impact) to increase user experience and thus increase fundings. The site should also incentivize refer codes because they gives benefits to users, brings traffic, and are convenient to use for those who lack incentivization. 
•	For the large donations, Donors Choose should further investigate the reason behind these observations because they can be potential generous donors that the website should try to retain, or they can be data quality issues proposed above. If they influence the prediction performance, there will be loss in marketing opportunities and thus loss in potential revenue. In addition, investigating on the networks of these large donors help the website pick the target honoree to promote along with exciting projects because large donors tend to be target-oriented. 

