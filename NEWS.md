# pmml 2.2.0.9000
## New Features
* `pmml.ARIMA()` can now export models with StateSpaceModel representation. This is controlled by a new parameter, `ts_type`. The default option (`ts_type = "arima"`) still exports an ARIMA representation as before. When `ts_type = "statespace"`, the exported PMML uses a StateSpaceModel element.
* The `exact_least_squares` parameter in `pmml.ARIMA()` is now deprecated. In a future release, all exports where `ts_type = "arima"` will be exported with the Exact Least Squares method.

## Other Changes
* Package is now compliant with the `stringsAsFactors` change in R 4.0.0.


# pmml 2.2.0
## Breaking Changes
  * Deprecated parameters in the following functions have been removed, and replaced with equivalent in snake case.
    - `pmml.iForest`: `anomalyThreshold` -> `anomaly_threshold`
    - `pmml.kmeans`: `algorithm.name` -> `algorithm_name`
    - `rename_wrap_var`: `wrap_data` -> `wrap_object`
    - `xform_norm_discrete`: `inputVar` -> `input_var`

## New Features
* `pmml.ARIMA()` now exports prediction intervals for non-seasonal models.
* `pmml.ARIMA()` can now export seasonal models with Exact Least Squares representation, and does so by default.


# pmml 2.1.0

## Major Changes
* Package now exports PMML with schema version 4.4.

* `pmml.ARIMA()` function added - exports ARIMA time series models from the `forecast` package.


## Breaking Changes
* `pmml.svm()` now has a `detect_anomaly` argument, allowing the user to specify whether the PMML detects anomalies or inliers. The exported PMML now has two OutputField elements: `anomalyScore` and one of `anomaly` or `inlier`.

* The following arguments are deprecated. They can still be used, but will produce a warning message and will be removed in a future release.
  - `pmml.iForest`: `anomalyThreshold` -> `anomaly_threshold`
  - `pmml.kmeans`: `algorithm.name` -> `algorithm_name`
  - `rename_wrap_var`: `wrap_data` -> `wrap_object`
  - `xform_norm_discrete`: `inputVar` -> `input_var`


## Other Changes
* Application version in PMML Header corresponds to pmml package version.

* `pmml.iForest` now uses attribute `sampleDataSize` instead of element `ParameterList` to store the `model$phi` value.

* `pmml.gbm` now adds `modelName` attribute to the final segment for multinomial gbm models.

* `testthat` file names correspond to the functions being tested.

* Edited `make_output_nodes` doc for clarity.

* Updated formatting in vignettes.

* Fixed spelling and added word list via `spelling` package.



# pmml 2.0.0

With this release, `pmmlTransformations ` has been merged into `pmml` and package development has been moved to GitHub. This was also a good opportunity to apply a style and rename many functions and parameters to make the code more uniform and easier to understand. 

## Breaking Changes

We used the tidyverse style guide when renaming functions, parameters, and arguments. In addition, some parameters and arguments have been renamed for clarity, and several functions were removed.

For functions that use a dots (`...`) parameter, the old parameters will still be accepted, even though these old parameters will not be used. 

* `pmml()` parameters and default arguments have been changed as follows:
	- `model.name = "Rattle_Model"` -> `model_name = "R_Model"`
	- `app.name = "Rattle/PMML"` -> `app_name = "SoftwareAG PMML Generator"`
	- `unknownValue` -> `missing_value_replacement`

* Individual exporters had the following changes:
	- `pmml.iForest()`
		- `parentInvalidValueTreatment` -> `parent_invalid_value_treatment`
		- `childInvalidValueTreatment` -> `child_invalid_value_treatment`
	- `pmml.lm()`
		- The unused `dataset` argument has been removed.
	- `pmml.naiveBayes()`
		- `predictedField` - `predicted_field`
	- `pmml.randomForest()`
		- `unknownValue` -> `missing_value_replacement`
		- `parentInvalidValueTreatment` -> `parent_invalid_value_treatment`
		- `childInvalidValueTreatment` -> `child_invalid_value_treatment`
	- `pmml.xgb.Booster()`
		- `inputFeatureNames` -> `input_feature_names`
		- `outputLabelName` -> `output_label_name`
		- `outputCategories` -> `output_categories`
		- `xgbDumpFile` -> `xgb_dump_file`
		- `parentInvalidValueTreatment` -> `parent_invalid_value_treatment`
		- `childInvalidValueTreatment` -> `child_invalid_value_treatment`

* The following additional functions had name and parameter changes:
	- `AddAttributes()` -> `add_attributes()`
		- `xmlmodel` -> `xml_model`
	- `addDDAttributes()` -> `add_data_field_attributes()`
	- `addDFChildren()` -> `add_data_field_children()`
	- `addMSAttributes()` -> `add_mining_field_attributes()`
		`xmlmodel` -> `xml_model`
	- `addOutputField()` -> `add_output_field()`
		- `xmlmodel` -> `xml_model`
	- `makeIntervals()` -> `make_intervals()`
	- `makeOutputNodes()` -> `make_output_nodes()`
	- `makeValues()` -> `make_values()`

* Functions from `pmmlTransformations` have been merged into `pmml` and had the following name and parameter changes:
	- All functions had the following parameters renamed where present:
		- `xformInfo` -> `xform_info`
		- `boxdata` -> `wrap_object` (except in `RenameVar`)
		- `mapMissingTo` -> `map_missing_to`
	- `DiscretizeXform()` -> `xform_discretize()`
		- `defaultValue` -> `default_value`
		- `mapMissingTo` -> `map_missing_to`
	- `FunctionXform()` -> `xform_function()`
		- `origFieldName` -> `orig_field_name`
		- `newFieldName` -> `new_field_name`
		- `newFieldDataType` -> `new_field_data_type`
		- `formulaText` -> `expression`
	- `MapXform()` -> `xform_map()`
		- `defaultValue` -> `default_value`
	- `MinMaxXform()` -> `xform_min_max()`
	- `NormDiscreteXform()` -> `xform_norm_discrete()`
	- `RenameVar()` -> `rename_wrap_var()`
		- `boxdata` -> `wrap_data`
	- `WrapData()` -> `xform_wrap()`
		- `indata` -> `data`
		- `useMatrix` -> `use_matrix`
	- `ZScoreXform()` -> `xform_z_score()`

## Deleted/moved functions
* The following functions have been removed from the package:
	- `pmmltoc()` - empty function.
	- `addLT()` - unused function.
	- `pmmlCanExport()` - unused function.
	- `pmml.survreg()` - untested exporter that may be added in the future.

* `Initialize()` has been made internal.

## Other Changes

  * All documentation is created with roxygen.

  * Documentation is now uniform across different exporters.

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
  
  * Header extension must be first element. lm NA coeff now 0
  
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
  
  * Change structure used to record transforms.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.12
  
  * Fix pmml.lm handling of singularities -> inactive
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.2.11
  
  * Fix categorics with one singularity in lm were marked inactive.
  
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
  
  * Fix documentation and packaging and release to CRAN
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.1.20
  
  * Bug - fix rpart var names with transforms
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.1.19
  
  * Tidy up and update ClusterField
  
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
  
  * Expose pmml.lm in NAMESPACE - whoops.
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
  
# rattle 1.1.1
  
  * Add pmml.lm
  
 -- Graham Williams <Graham.Williams@togaware.com>  Thu, 06 Jan 2011 06:43:50 +1100
