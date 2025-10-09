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

1. ```psi**``` value added to the output of ```psi``` function (for comparison with ```cv.zscore`` and ```cv.chisq``` critical value)
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
2. Examples modified (```stepMIV```, ```boots.vld```, ```segment.vld```, ```scaled.score```, ```kfold.vld```, ```fairness.vld```, ```evrs```, ```staged.blocks```) to keep the execution time under 10s during check_win_release() 

# PDtoolkit 1.1.0
Changes:<br/>

1. ```imp.outliers``` function did not replace identified outliers properly. Small adjustment made (```db[, rf.l] <- rf.imp``` have been added). 
2. ```nzv``` - label of the second most frequent values was wrongly assigned. (```cc.lbl.2 = x.cc.lb1``` replaced by  ```cc.lbl.2 = x.cc.lb2```)
3. ```rf.clustering``` - updated link for x2y metric
4. improvement of trend check (```cc.dummy```) in stepwise regressions
5. New functions:
      + ```stepFWDr``` - stepwise regression for mixed risk factor types 
      + ```stepRPCr``` - stepwise regression based on risk profile concept and mixed risk factor types
6. New method available for ```staged.blocks```, ```embedded.blocks```, ```ensemble.blocks``` - "stepFWDr" & "stepRPCr"
7. Examples modified (```staged.blocks```, ```embedded.blocks```, ```ensemble.blocks```, ```rf.clustering```, ```hhi```, ```evrs```) to decrease the execution time 
during check_win_release() 
8. ```stepFWD``` and ```stepRPC``` - additional check for ```dummy``` coding and ```check.start.model``` introduced
9. ```rs.calibration``` output exteneded. Now, besides calibrated values it returns also parameters

# PDtoolkit 1.1.1
Changes:<br/>

1. ```cat.bin``` function adjusted in part after dealing with special cases.
2. ```psi``` - typo in helper function ```num.bt``` corrected (instead of ```incluse.lowest = TRUE```, now ```include.lower = TRUE```), This change should not affect previous usage of the ```psi``` function because argument ```breaks``` is a single number which already ensures inclusion of extreme values (for details see ```?cut```)
3. The following helper functions renamed to avoid S3 method mismatches: ```tbl.correction``` (```tbl_correction```), ```summary.tbl``` (```summary_tbl```),
   ```log.likelihood```(```log_likelihood```), ```best.split.num```(```best_split_num```), ```best.split```(```best_split```), ```best.split.cat```(```best_split_cat```),
   ```sum.adjacent``` (```sum_adjacent```), ```c.best.split.num```(```c_best_split_num```), ```c.best.split```(```c_best_split```), ```c.best.split.cat```(```c_best_split_cat```)

# PDtoolkit 1.2.0
Changes:<br/>
1. ```pp.testing``` - description of the Hosmer-Lemeshow test results changed
2. ```power``` - for the Hosmer-Lemeshow test removed condition which checks if the observed portfolio default rate is less than predicted one.

# PDtoolkit 1.2.1
Changes:<br/>
1. ```pp.testing``` - typo in the part of the arguments checks (```is.numeric(n)``` converted to ```is.numeric(no)```). This change does not impact the function output.
                      Erorr message adjusted (instead of ```stop("All arguemnts have to of numeric type.")``` new message is ```stop("Arguments pdc, no, and nb have to of numeric 
                      type.")```)

# PDtoolkit 1.2.2
Changes:<br/>
1. ```pp.testing``` - typo corrected in the column `res` for `two.sided` version - `H0: AUC != AUC test` corrected to `H0: AUC == AUC tes`
