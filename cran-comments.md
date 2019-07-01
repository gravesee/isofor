## Test environments
* OS X
* linux
* win-builder

## R CMD Check Results
There were no ERRORs or WARNINGs or NOTEs.

## Addressed concerns by CRAN
* Only shipping CRAN template for MIT license
* Added reference about method to DESCRIPTION
* enclose the article title in double quotes: "Isolation based Anomaly Detection"
* Please add a small examples for your prediction function.
* Expanded the description
* removed dontrun directive in predict method examples
* added donttest directive per CRAN suggestion -- passes windows now
* fixed typo in DESCRIPTION - "implemenation -> implementation"
* added return value description for predict method
* removed OMP so examples pass on i386

