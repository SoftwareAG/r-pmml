# pmml 1.5.7.9000
  * Transformation functions from `pmmlTransformations` are now in this package.

  * Tidyverse style has been applied to exported functions.

  * All documentation is now created with roxygen.

# pmml 1.5.7
  * Add support for one-class svm (anomaly detection) models from e1071

  * Add support for iForest (anomaly detection) models from isofor

  * Add support for boolean values in functionToPMML

# pmml 1.5.6
  * Fix missing nodes issue in xgboost caused by deep trees

  * Update savePMML to output LF line endings, independent of OS

  * Update Description and pmml.R documentation

# pmml 1.5.5
  * Add dataType argument to every Output node

  * Fix the family check in pmml.cv.glmnet to only allow "gaussian" and "poisson"

  * Add note about supported family types in pmml.cv.glmnet documentation

  * Add note about sparse matrices in pmml.xgb.Booster documentation

  * Add vignette with list of supported packages and models

  * Fix bug in xgboost when using transformations

# pmml 1.5.4
  * Add invalidValueTreatment="returnInvalid" in MiningSchema by default for all models

  * Add arguments for invalidValueTreatment attributes to pmml.xgboost() and pmml.randomForest()

# pmml 1.5.3
  * Fix bug due to 1- vs 0-based indexing in gbm converter

  * Change default pmml version to 4.3 in gbm converter

  * Fix comment added at top of XML doc by savePMML()

  * Fix bug in xgboost regression end element

  * Fix bug in xgboost when feature names are substrings of each other

  * Fix bug in xgboost binomial model

  * Add check for xgboost objectives

  * Add support for multi:softmax objective in xgboost

  * Update pmml.rfsrc.R doc to state dependency on version 2.5.0 or earlier


# pmml 1.5.2
  * Add converter for xgboost models

  * Add converter for gbm models

  * Add converter for neighbr model

  * Output error on detecting an unsupported kernel in kernlab::ksvm() models

  * Add examples and note to pmml.glm.Rd

  * Output error on detecting unsupported feature classes

  * Output error on detecting interaction terms in linear models

  * Fix error created when fitting random forest model with 1 tree

  * Update references to PMML spec 4.3

# pmml 1.5.1
  * Fix example in documentation for pmml.rfsrc.R after randomForestSRC was updated

# pmml 1.5.0
  * Add functionToPMML

  * Add addDDInfo

  * Add addOutputInfo

  * Edit to work with pmmlTransformations::FunctionXform: pmml, pmml.datadictionary and pmml.miningschema

  * Add warning message to addLT; will deprecate in next version

# pmml 1.4
  * Add support for the svm model in e1071 package
  * Add support for ada package
  
  * Add classificationMethod attribute for ksvm model (kernlab) for information purpose
  
  * Add usageType for AssociationModel MiningField elements
  
  * Fix bug for combining algorithm PMML and transformation XML fragment
  
  * Remove obsolete codes
  

# pmml 1.2.32

  * Updates to pmml.arules from Michael Hahsler.
  
  * Begin using a ChangeLog (this) file and simply dump previous changes
    below.
  
  * Begin support for PMML 4.0.
  
  * Small changes to pmml.lm to support UTF-8/Japanese.
  
  * Fix spelling error in DESCRIPTION - CRAN check found it.
  
  * Various updates from Zementis to the randomForest schema.  All the
    mining schemas in each segment of the random forest now also output
    the attribute invalidValueTreatment as asis.
  
  * Change standard to have an R file per exported function, and all
    internal functions begin with .
  
 -- Graham Williams <Graham.Williams@togaware.com>  Sun, 09 Dec 2012 22:28:00 +1100

# rattle 1.2.27
  
  * Bug fixes to ksvm model from Zementis and Graham
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.26
  
  * Bug fix glm models with weights
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.25
  
  * Fix MapValues compliance
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.24
  
  * Bug fix glm regression - note as classification
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.23
  
  * Ensure pmml.ksvm at least runs
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.22
  
  * Header extension must be fist elmt. lm NA coeff now 0
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.21
  
  * Fix bug in pmml export of TNM transform.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.20
  
  * Support coxph as regression.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.19)
  
  * Several fixes for PMML conformance.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.18
  
  * Fix export of pmml for hclust with transforms.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.17
  
  * Zementis: add Output node.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.16
  
  * Support TJN (joincat).
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.15
  
  * Update documentation
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.14
  
  * Support mult transforms for rpart
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.13
  
  * Change strcutre used to record transforms.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.12
  
  * Fix pmml.lm handling of singularities -> inactive
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.11
  
  * Fix categroics with one singularity in lm were marked inactive.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.10
  
  * Fix typo in pmml.lm
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.9
  
  * Further fix a pmml.lm bug.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.8
  
  * Fix a pmml.lm bug.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.7
  
  * Export logistic classes
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.6
  
  * Support RMA transforms.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.5
  
  * Include .TRANSFORM constants within pmml package.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.4
  
  * Include collection of utility transform functions.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.3
  
  * Bug fixes
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.2
  
  * Add test for transform support.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.1
  
  * Streamline conditional handling of transforms.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.0
  
  * Fix documentation and packaing and release to CRAN
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.1.20
  
  * Bug - fix rpart var names with transforms
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.1.19
  
  * Tidyup and update ClusterField
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.1.18
  
  * Include pmml.hclust in NAMESPACE
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.1.17
  
  * Export hclust as kmeans.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.1.16
  
  * export pmml.multinom
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.1.15
  
  * Handle multinomial model.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.1.14
  
  * Handle singularities in lm/glm better.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.1.13
  
  * Support export of poisson(log)
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.1.12
  
  * Tree Array have quoted values. 0 for base in regression
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.1.11
  
  * Bug fix for pmml.lm - continuing to fix below problem
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.1.10
  
  * Bug fix for pmml.lm with categorical logistic target
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.1.9
  
  * Update rpart/nnet/ksvm from Zementis + many improvements
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.1.8
  
  * Increase number of digits extracted for rpart tests.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.1.7
  
  * Add arules.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.1.5
  
  *  Add pmml.nnet.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.1.4
  
  * Add pmml.ksvm. Fix extensions. 
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.1.3
  
  * Fixes for new version of randomSurvivalForest.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.1.2
  
  * Expose pmml.lm in NAMESPACE - woops.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.1.1
  
  * Add pmml.lm
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
