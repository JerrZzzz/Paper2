# Title

This paper is a replication of Professor Feldman's paper on action and inaction called ["Evaluations of action and inaction decision-makers in risky decisions resulting in negative outcomes: Inaction agents are preferred to and perceived as more competent and normative than action agents"] 

All Files are collected in the following way: 

input
- data - includes all the raw data download from paper. 
  - inactionwetrust_study1b.csv - Raw data download from original paper on study 1. 
  - inactionwetrust_study2.csv - raw data download from original paper on study2. 
  - osf-past-normality-regret-replication-exp2-data-v2.csv - raw data download from from Feldman's regret paper part3. 
- literature - contains all scholars we used in our paper.
  - VanHooftKammeyer-MuellerWanbergKanferBasbugJAP2020withsupplmat.pdf - contained the idea which Van Hooft et al. support where success man is easy to get a job.
  - Fillon-etal-2023-action-inaction-preference-competence-norms-print.pdf - is the copy of the paper we replicated. 
  - Kutscher-Feldman-2018-CogEmo-past-behavior-regret-exceptionality-replication-extension-supplementary.pdf - is the paper written by Professor Feldman on this idea of regret while people make decisions.
  - Brockner &amp; Higgins (2001).pdf - Regulatory focus theory, another explanation on inaction. 
  - 4642.pdf - definition of action and inaction. 
  - ImpactofBehavioralBiasesonInvestorDecisionsMaking-MaleVSFemale.pdf - article about difference of decision making process between man and woman. 

output
- paper - includes the our written paper in qmd and pdf format along with ref.bib which includes all bibliography. 
  - paper2.pdf - is our paper in pdf format.
  - paper2.qmd - is our paper in qmd format.
  - ref.bib - contained all reference and literature cited in bibtex format. 
- data - includes all csv file we used to produce graphs.
  - robscene_study.csv - contained the data collected from Feldman's regret paper.
  - study1.csv - contained the cleaned data of study 1 collected from our replication original paper.
  - study2.csv - contained the cleaned data of study 2 collected from our replication original paper.

replicate
- graphs - contained all graphs we used in our paper.
  - combined_historgrams_study2.png - is the output graph of study2.
  - combined_histograms.png - is the combined plot of study 1 which contained 4 graphs.
  - combined_rob_compensation.png - is one of the plot in robscene about compensation.
  - combined_rob_regret.png - is another plot in robscene about regret.
- script - includes all the code that plot the figures.
  - 00_Plot_robscene.r - contained the code which plot the robscene and output 2 graphs to the above graphs folder.
  - 00_plot_study1.r - contained the code which plot the four graphs in study1. 
  - 00_plot_study2.r - contained the code which plot the 6 graphs in study2.
  - 00_cleandata.r - contain all code which clean data from raw in input data.
  
# LLM 

We used chatgpt to help us generate code which would substantially help us reduce our work load on writting code by ourselves. Note that we only used chatgpt to write the code. We did NOT use chatgpt in any way of helping us writing the content in our paper meaning we wrote the paper all by our hands. The chat history with chatgpt is available in this directory "/cloud/project/llms/llm". 

# Reproducibility

For reproducing this paper, you can go to the data website of our original replication paper(https://osf.io/a8e4d/) and Feldman's regret paper(https://osf.io/fnmk4/), download the data path for original paper(".../Data and code/inactionwetruststudy_1.csv") and regret paper (".../Data and code/osf-past-normality-regret-replication-exp2-data-v2.csv"). Put them into input/data file, run 00_cleandata.r, run 3 different plot code, you are able to recreate the same graph again. You can also access the SSRP in this link https://www.socialsciencereproduction.org/reproductions/7d354427-c539-4284-83a0-b794d98e7539/index 
