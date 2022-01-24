# PDtoolkit 0.1.0
Changes:<br/>

1. The new argument added to ```stepMIV``` function - ```coding.start.model``` which allows user to have different coding types for starting and final model. Additionally, the same function is improved adding the check for its output values - ```if	(nrow(steps) > 0) {steps <- cbind.data.frame(target = target, steps)}```. <br/>
2. Package extended with a new functions: ```psi``` and ```create.partitions```.
