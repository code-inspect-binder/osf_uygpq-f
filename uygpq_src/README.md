# README

Latest version updated on 2021-10-21.

Data and code to accompany the article entitled, The Role of Emotion Regulation and Choice Repetition Bias in the Ultimatum Game, 2021, by Justin Cheuk Yin Chung, Raj Seraya Bhatoa, Ruth Kirkpatrick, and Kate Anne Woodcock.

Correspondence about the article or this material should be addressed to Justin Chung (justincychung@googlemail.com) or Kate Woodcock (papers@katewoodcock.com), School of Psychology, University of Birmingham, Birmingham, UK, B15 2SA.

#### License

This work is licensed under the Creative Commons Attribution 4.0 International License. To view a copy of this license, visit http://creativecommons.org/licenses/by/4.0/ or send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

## Data

The final datasets after exclusion are found in datasets.RData, which can be loaded through R. 

This file contains six dataframes:

1. mainData - all fair and unfair trials of participants across all three paradigms (n = 429, n trials = 14,397).
2. stakesContrastData - fair and unfair trials of 7- and 8-year-olds from the Two-stakes paradigm (n = 44, n trials = 1,335) and 8- and 9-year-olds from the Four-stakes paradigm (n = 65, n trials = 2,269).
3. commentsContrastData - fair and unfair trials of 6- to 8-year-olds from the Two-stakes (n = 52, n trials = 1,569) and Comments paradigms (n = 71, n trials = 2,439).
4. p1Data - unfair trials of participants from the Two-stakes paradigm (n = 52, n trials = 1,051).
5. p2Data - unfair trials of participants from the Four-stakes paradigm (n = 240, n trials = 5,454).
6. p3Data - unfair trials of participants from the Comments paradigm (n = 137, n trials = 3,100).

#### Notes on variables
- 'comments' denoted whether the participant encountered a paradigm with or without proposer comments.
- 'stakes' denoted the number of stake sizes that the participant encountered.
- 'recodedDecision' and 'prevRecodedDecision' were reverse-coded from the 'decision' variable of the current or previous trial, respectively. This procedure ensured that the levels of these variables were ordered appropriately within the mixed effects models.

## Code

The R code for analysing and visualising the data are split into three files (please see comments within for more details):

1. Cross-paradigm.R - generates mixed-effects models from stakesContrastData and commentsContrastData.
2. Within-paradigm.R - generates mixed-effects models from p1Data, p2Data, and p3Data.
3. Model figures.R - generates figures found within the article.
