*******************************************************************************
*   Title: master.do
* Purpose: Master do file for FTHB paper replication file.
*   Owner: Eric Zwick
* Authors: David Berger, Nicholas Turner, Eric Zwick
*     RAs: Tom (Tianfang) Cui, Caleb Wroblewski
*    Date: 2019-02-01
*******************************************************************************

* To initialize, define these environment variables:
global userprofile
global localcodedir
global analysis
global rawdatadir
do "../20180195_code_private/config_env.do"

* Local directory.
global dodir "$localcodedir"
* Make sure template files are in the outputdir to create combined tables that
* look like the tables in the draft. 
global outputdir "$localcodedir/out"

display "$S_TIME  $S_DATE"

*******************************************************************************
* Programs for Internal Environment
*******************************************************************************
do $dodir/programs.do

*******************************************************************************
* Databuild Programs
*******************************************************************************
do $dodir/make-fthb.do
*main

*******************************************************************************
* Production Code for FTHB Figures and Tables
*******************************************************************************
do $dodir/draft-fthb.do
*main

*******************************************************************************
* Code for FTHB Appendix Tables and Figures
*******************************************************************************
do $dodir/appendix-fthb.do
*main

