# PDtoolkit 0.1.0
Changes:<br/>

1. The new argument added to ```stepMIV``` function - ```coding.start.model``` which allows user to have different coding types for starting and final model. 
Additionally, the same function is improved adding the check for its output value - ```if	(nrow(steps) > 0) {steps <- cbind.data.frame(target = target, steps)}``` and correction 
for ```miv``` table for missing/infinite values is introduced. <br/>
2. Improvement of ```cat.bin``` function is performed. If merging of special case bins is selected (argment ```sc.merge```), then summary table output reports the bin with which 
   it is merged. </br>
4. Package is extended with a new functions: ```psi``` and ```create.partitions```.
