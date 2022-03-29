* This DO file converts the occupation variable in the working dataset from ISCO to ESeC.
* https://github.com/benjann/iscogen/blob/master/README.md


* net install iscogen, replace from(https://raw.githubusercontent.com/benjann/iscogen/master/)
clear all
set more off, permanently
use "data.dta" /*load data*/

* net install iscogen /* make sure package is installed */
iscogen oesch5 = oesch5(ISCO_code), replace
iscogen oesch8 = oesch8(ISCO_code), replace
iscogen oesch = oesch(ISCO_code), replace
iscogen esec = esec(ISCO_code), replace


saveold dat, version(12) replace
