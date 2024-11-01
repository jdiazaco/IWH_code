Main - fake.R: Sets parameters for all scripts
a. Data preparation - fake.R: 

This file prepares all needed datasets for analysis

It takes information from BS, BR and PRODCOM information to create a dataset with the variables 
needed for our product reallocation analysis.

b. Economy-level revenue growth calculations - fake.R: 

This file calculates measures of aggregate revenue growth and reallocation from prodcom product revenue information, 
decomposed by product status within the firm (first introduction, continuing products, discontinued and reintroduced products).
It creates plots for these results.

Sections 2 and 3 are validations of the aggregate revenue growth measures using aggregate and revenue weighted measures of growth.
Section 4 aims to compare revenue growth in prodcom against growth from revenue as reported in the BR and against France's GDP growth.


c. Product growth - fake: This file uses firm (FARE/FICUS) and product data (EAP) 

Part 1 and 1* clean the data

Part 2 graphs the distributions of annual product revenue growth and annual firm employment growth and colors the graph by industry.
It is a first attempt at answering: Which were the key industries in terms of product growth and labor growth.

Part 3 is mainly focused on analysing the elasticity of employment growth and capital growth to revenue growth.
These regressions include age, size and superstar status controls
It uses two samples: one with only continuing firms, other with both continuing and exiting firms.
It runs both weighted and unweighted regressions.

Part 4 creates graphs of high growth vs. high decline for NACE industries

d. Reallocation effect on factor growth - fake.R: 

This file uses firm (FARE/FICUS) and product data (EAP) to analyze the effect of product entry and exit on firm growth
