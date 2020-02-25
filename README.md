# school_readiness
Code for an analysis of the effect of Sure Start spending on school readiness in English local authorities.

# Details
This repository contains code for an analysis looking at whether Sure Start spending in English local authorities is associated with school readiness (a measure of child development)

The repo contains three scripts. One assembles a panel data set comprising 150 English local authorities over 5 years (2012 - 2017) with data on school readiness (among all children and those eligible for free school meals, an indicator of poverty), Sure Start spending, total children's services spending, and child poverty rates.

The second script runs the analysis using fixed-effects poission regression models.

The third script checks that the analysis holds when Sure Start spending is taken from another data source (derived from local authority s251 outturn expenditure).
