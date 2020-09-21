# EQpredictionWithRFandSR
EarthQuake Prediction with Random Forests and Schumann Resonance

To run the R code you will need:

1. A working installation of >R3.0
2. packages 'tseries','nnet','xtable','pracma','matrixStats','e1071','ROCR','verification','randomForest' in R installed


The runs have been checked in a windows 64 machine with R3.2.4revised 64bit and R4.0.2 64bit (latest as of September 20, 2020).


## (>4 Richter) EQ in next 48h in a window of +/- 3 degrees from Measurement Station

Put

```
a) Script_Monte_AnalysisSep16toFeb18Deeper_tune.R  (R main) 
b) functions_Monte_AnalysisSep16toFeb18Deeper_tune.R (R functions) 
c) all the provided input files of type *.txt 

```
in same directory 

Run Script_Monte_AnalysisSep16toFeb18Deeper_tune.R in R or RStudio
The script takes a few minutes to run.


In the current directory many files are produced.

The final result is the skill score statistics vector: Accuracy (ACC), True Skill Statistic (TSS) and Heidke Skill Score (HSS), for all methods which is also produced as a latex file, and displayed at the console tab window of R or RStudio with latex code.

Also, figures are produced for all five methods validated.

The methods are
```
0: Neural network
1: Linear regression
2: Probit regression
3: Logit regression
4: Random forest
5: Support vector Machine
```
Focus is only on method 4: Random Forest which is the one discussed in the paper.
Runs in the paper are done with R3.2.4revised 64bit.
Runs have been also made with latest R4.0.2 64bit.
For reference, in the paper the former results are presented. They differ very slightly due to R's lack of backward compatibility.
It might just be an issue of different random number generators used between R3.2.4revised and R4.0.2.

## Reference
Florios, K., Contopoulos, I., Tatsis, G., Christofilakis, V., Chronopoulos, S., Repapis, C and Tritakis, V., Possible Earthquake Forecasting in a narrow Space-Time-Magnitude Window, submitted, 2020
