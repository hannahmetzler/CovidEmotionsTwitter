# CovidEmotionsTwitter

Code and data for the paper Collective emotions during the COVID-19 outbreak.

Preprint here: https://psyarxiv.com/qejxv/

* Use *git clone https://github.com/hannahmetzler/CovidEmotionsTwitter.git* from your local Terminal to copy all code and data to your computer. 
* Add empty folders called *figures* and *output*
* Open the CovidEmotionsTwitter.Rproj with RStudio
* Start reproducing our analyses with the file *01_MainAnalysis_Timelines_Regression_Duration.Rmd*. It contains: 
    - Descriptive Statistics like sample sizes, emotion levels per time period and country
    - the time series of emotions per country, and of total tweets and unique authors per country
    - Country comparison figure: average emotion levels in the 5 weeks after the outbreak in each country
    - the main analysis: linear mixed effects regressions analysing the changes in emotions per week after the outbreak, first with regressions comparing each period to the baseline, then with regressions comparing each period to the previous period
    - the figure of sustained emotion periods above/below the baseline (duration of emotional changes)
* Continue in the order in which scripts are numbered, all other .Rmd files contain exactly the analysis indicated by their name. 

**Structure of folders:**

- All files you need to execute are the .Rmds in the main directory
- scripts/ contains scripts with functions, for data processing etc. that are called in some of the .Rmd files. 
- Tables with statistical results created during the analysis will be written to *output/*, and created figures to *figures/*.
- data_twitter_volumes/ contains the daily volumes of tweets per emotion per country, as well as the daily number of unique authors and total tweets. The *CountrySocEmoLong.csv* files contains combined volumes for all emotions for one country. 
- data_main/ contains data files created by the *process_data.R* and *proces_data_light.R* scripts, as well as additional data sets with the population numbers, covid-19 case and death numbers, and pandemic intervention measures. 
- data_roberta/ contains data sets with the processed/aggregated emotion levels based on the machine learning labels predicted with a fine-tuned RoBERTa model. The scores represent the percent of tweet per day for which the model predicted an emotion label with more than 0.9 probability. 
- data_wordcloudsPeriods/ includes datasets per country, time period (baseline, control, outbreak) and emotion with the 60 most frequent terms in tweets in this category.
