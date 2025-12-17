<img src="image.png" alt="Banner" style="width:100%; height:auto;">

# Stranger Things: An Analysis of Character, Location, and Sentiment 

*Stranger Things* has been a pop culture phenomenon since it originally aired in 2016. The first half of the latest and final season dropped on November 26, 2025. The show features an intricate network of characters and locations, whose interactions can be studied to identify patterns among the appearances of characters.

The research question for this analysis is:

"Can we predict if someone will be in a scene based on sentiment, character presence, and location?"

Specifically, in this analysis, the "someone" will be Mike Wheeler. Mike is one of the main characters in Stranger Things, and he appears in numerous scenes throughout seasons 1 and 2. This means he has a large dataset to pull from to understand the patterns of his behavior.

## Datasets

Two datasets were used in this analysis:

-   **TidyTuesday**: contains all of the dialogue and episodes of *Stranger Things*

-   [**Jeffrey Lancaster**](%22%3Chttps://raw.githubusercontent.com/jeffreylancaster/stranger-things/master/data/episodes.json%3E%22): contains all of the information about specific scenes from *Stranger Things*, including characters that are present

An additional dataset, **stranger_words**, was created to assist in the data analysis as a sentiment assigner to *Stranger Things* specific words.

Because the **Jeffrey Lancaster** dataset only contains information from the first two seasons, this analysis is solely of the patterns of the first two seasons of Stranger Things.
