# PDtoolkit 0.1.0
Changes:<br/>

1. The new argument added to ```stepMIV``` function - ```coding.start.model``` which allows user to have different coding types for starting and final model. 
Additionally, the same function is improved adding the check for its output value - ```if	(nrow(steps) > 0) {steps <- cbind.data.frame(target = target, steps)}``` and correction 
for ```miv``` table for missing/infinite values is introduced. <br/>
2. Improvement of ```cat.bin``` function is performed. If merging of special case bins is selected (argument ```sc.merge```), then summary table output reports the bin with which 
   it is merged. </br>
4. Package is extended with a new functions: ```psi``` and ```create.partitions```.

# PDtoolkit 0.2.0
Changes:<br/>

1. ```rf.clustering``` - increased number of maximum clusters from 30 to 100 for manual selection. For ```x2y``` metric, ```minsplit``` and ```minbucket``` added in order to speed 
up the algorithm. <br/>
2. ```segment.vld``` - correction for possible 0 and 1 observed default rate in the ```prop.test```. <br/>
3. ```replace.woe``` - extended list of elements for WoE check (```c(NA, NaN, Inf, -Inf)```).
4. The new argument added to ```stepMIV``` function - ```offset.vals```. The same function, now returns the model development database also for ```coding = "dummy"```.
5. Package is extended with a new functions: ```evrs``` and ```interaction.transformer```.

# PDtoolkit 0.3.0
Changes:<br/>

1. Package is extended with a new functions: ```stepFWD``` and ```stepRPC```.

# PDtoolkit 0.4.0
Changes:<br/>

1. Package is extended with a new functions: ```staged.blocks```, ```embeded.blocks``` and ```ensemble.blocks```.
2. Fixed bug in ```create.partitions``` function - risk factors with more than 10 modalities. 

# PDtoolkit 1.0.0
Changes:<br/>

1. ```psi**``` value added to the output of ```psi``` function (for comparison with ```cv.chisq``` critical value)
2. Improvement of ```cat.bin``` output consistency for ```sc.merge``` option 
3. Additional check of ```segment```argument in ```homogeneity``` function (has to be of length one)
4. Function ```segment.vld``` parameterized with the new argument ```min.leaf```
5. Additional condition considered for selection of next entry in ```stepFWD``` (now AIC value can be possibly considered in the selection process)
6. Bug fix in ```interaction.transformer``` function - identification of upper bound for partitioning
7. Argument ```sc``` in the functions of univariate analysis extended for ```-Inf``` value 
8. New functions:
      + helpers functions: ```num.slice```, ```cat.slice``` and ```encode.woe```
      + ```nzv``` - near-zero variance
      + ```smote``` - Synthetic Minority Oversampling Technique
      + ```constrained.logit``` - constrained logistic regression
      + ```rf.interaction.transformer``` - extract interactions from random forest
      + ```hhi``` - Herfindahl-Hirschman Index
      + ```normal.test``` - Multi-period predictive power test
      + ```confusion.matrix``` and ```cutoff.palette``` - confusion matrix analysis
      + ```ush.test``` and ```ush.bin``` - U-shape testing and binning procedures
      + ```kfold.idx``` - indices for K-fold validation
      + ```fairness.vld``` - model fairness validation
      + ```decision.tree``` - custom decision tree algorithm and its ```predict``` method

# PDtoolkit 1.0.1
Changes:<br/>

1. ```print``` from within the functions (```stepMIV```, ```stepFWD```, ```stepRPC```, ```staged.blocks```, ```embedded.blocks```, ```ensemble.blocks```) replaced with ```messsage```
2. Examples modified (```stepMIV```) to keep the execution time under 10s during check_win_release() 
