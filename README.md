# Experience of Groove Questionnaire (EGQ)
R package to include the Experience of Groove Questionnaire as various psychTestR functions

## Installation instructions (local use)

1. If you don't have R installed, install it from here: https://cloud.r-project.org/

2. Open R.

3. Install the ‘devtools’ package with the following command:

`install.packages('devtools')`

4. Install the EGQ:

`devtools::install_github('KilianSander/groovescale')`

## Implemented variants of the EGQ
The Experience of Groove Questionnaire is implemented as a single page (`EGQ`) and with one page per item (`GRV`).
Both can be used within a battery of tests and questionnaires (`EGQ()` or `GRV()`), i.e. a psychTestR timeline,
or in their standalone versions (`EGQ_standalone()` or `GRV_standalone()`).
