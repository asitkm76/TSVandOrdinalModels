# Analyzing thermal sensation votes from field studies - appreciaiton of the human element
This repository holds the R code related to the short communication on appreciating the human element in analyzing thermal comfort research data.
The work intends to illustrate how statsitical modelling would need to consider the qualitative and discrete nature of occupant responses and the occupant-thermal environment interactions.

The code assumes you have ASHRAE Global Thermal Comfort database II data set locally available.
It improts the data and choses a specific building in the dataset (ID 735). The code then constructs linear and ordinal models for the thermal sensation - thermal environment data and creates illustrative plots.

In the next part, to model the two-way interaction between thermal sensation and thermal environment, a two stage least squares linear model is used, with instrument variables. The code creates the model and outputs the model summary. 

