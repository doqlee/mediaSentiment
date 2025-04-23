*** Created by: Do Lee
*** Order of commands

clear all
set more off
set matsize 10000 
set maxvar 30000
macro drop _all
sysdir set PLUS "..\ado"

* News sentiment indicators

do 1_sentiment_country.do
do 2_sentiment_local.do
do 3_sentiment_global.do
do 4_merge.do

* Replicate main results in the paper

do 5_figure_1.do
do 6_figure_2.do
do 7_figure_3.do

*** End of File ***
