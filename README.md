# K_modes_rivals_tier_list
This project concerns using K-modes clustering to explore and produce tier lists for the game Rivals of Aether, based on top player data provided by players Darai and CakeAssault. Originally, they used K-means clustering as a method to cluster the ordered lists of characters. However, the ordered list of characters is categorical data, which K-means is less suitable for.

In the project report file, I investigate and adapt the K-modes method to be able to properly cluster the data. One of the main issues with the data set is the small sample size, which is somewhat necessitated by the fact that only top players should contribute to a tier list. Nevertheless, the project report includes an exploration of ideas which could allow for larger sample sizes to be reached.

The repo contains one project report, two R scripts and six csv files for the data. To produce the reduced data sets for the tier_list_script.R file, please first run the data_cleaning_script.R file, which will produce one of the two reduced data sets depending on an input variable. tier_list_script.R will output 14 bar graphs, which are the player ranking distributions for each character.
