*setting the file name of the data location;
filename csv "C:/SAS/SAS_Data/projectDataset.csv" TERMSTR=CRLF;

*Import the dataset from the local to the WORK space;
proc import datafile=csv
out = work.loan1
dbms=csv
replace;
run;

*fico_avg - sets current fico mean by taking the lowest and highest values of the fico_range;
*last_fico_avg - sets last fico mean by taking the lowest and highest values of the last_fico_range;
*loan_status - sets loan status to 1 if the loan is Fully Paid, Current or Grace Period else it is set to 0;
*categorizedIncome = sets the value to 1 if the annual_inc >= 4000 and annual_inc <= 30000 else 
 the value is set to 2 if the annual_inc >= 30001 and annual_inc <= 50000 else
 the value is set to 3 if the annual_inc >= 50001 and annual_inc <= 100000 else 
 the value is set to 4 if the annual_inc >= 100001 and annual_inc <= 1200000 else the value is set to 5;
data loandata;
set loan1;
fico_avg= (fico_range_low + fico_range_high )/ 2;
last_fico_avg= (last_fico_range_low + last_fico_range_high )/ 2;

if loan_status="Fully Paid" or "Current" or "Grace Period" then loan_paid=1;
	else loan_paid=0;


if annual_inc >= 4000 and annual_inc <= 30000 then categorizedIncome=1;
else if annual_inc >= 30001 and annual_inc <= 50000 then categorizedIncome=2;
else if annual_inc >= 50001 and annual_inc <= 100000 then categorizedIncome=3;
else if annual_inc >= 100001 and annual_inc <= 1200000 then categorizedIncome=4;
else categorizedIncome=5;
run;

*Summarizes the dataset;
proc contents data=loandata;
run;

*Displays the frequeny of values in the categorizedIncome column;
proc freq data=loandata;
table categorizedIncome;
run;

*Displays the frequeny of values in the loan_paid column;
proc freq data=loandata;
table loan_paid;
run;

*Effectively analyses the values of the numeric variables;
proc means data=loandata;
run;


**********************Calculating the VIF**********************************;
title "Variance Inflation";
proc reg data=loandata ;
     model loan_paid = categorizedIncome fico_avg loan_amnt total_pymnt delinq_2yrs last_fico_avg/ vif;
	 run;



*************Logistic Regression******************;
title "Logistic Regression for Credit Risk Analysis";	
proc logistic data= loandata descending;
  class categorizedIncome/ param=ref ;
  model loan_paid = categorizedIncome fico_avg loan_amnt total_pymnt delinq_2yrs last_fico_avg;
run;

title "Logistic Regression for Credit Risk Analysis without delinq_2yrs";	
proc logistic data= loandata descending;
  class categorizedIncome/ param=ref ;
  model loan_paid = categorizedIncome fico_avg loan_amnt total_pymnt last_fico_avg;
run;

*************Multiple Regression******************;
title "Multiple Regression for Credit Risk Analysis";
proc reg data = loandata  PLOTS(MAXPOINTS=NONE) ;
     model loan_paid = categorizedIncome fico_avg loan_amnt total_pymnt delinq_2yrs last_fico_avg;
	 Output OUT=reg_loan_OUT PREDICTED = c_predict
	 RESIDUAL=c_RES
	 rstudent =c_rstudent h=lev cookd=Cookd dffits = dffit
;
	 quit;

***************Forward Selection****************;
title "Multiple Regression(Forward Selection) for Credit Risk Analysis";
proc reg data=loandata outest=est_loan PLOTS(MAXPOINTS=NONE);
    model loan_paid = categorizedIncome fico_avg loan_amnt total_pymnt delinq_2yrs last_fico_avg
                        / selection=forward;
     OUTPUT OUT=reg_loan_OUT  PREDICTED=   RESIDUAL=Res   L95M=C_l95m  U95M=C_u95m  L95=C_l95 U95=C_u95
      rstudent=C_rstudent h=lev cookd=Cookd  dffits=dffit;  
quit;

******************Backward Elimination****************;
title "Multiple Regression(Backward Elimination) for Credit Risk Analysis";
proc reg data=loandata  outest=est_loan ;
    model loan_paid = categorizedIncome fico_avg loan_amnt total_pymnt delinq_2yrs last_fico_avg
                        / selection= backward;
     OUTPUT OUT=reg_loan_OUT  PREDICTED=   RESIDUAL=Res   L95M=C_l95m  U95M=C_u95m  L95=C_l95 U95=C_u95
      rstudent=C_rstudent h=lev cookd=Cookd  dffits=dffit;  
quit;

*******************Stepwise Selection*****************;
title "Multiple Regression(Stepwise Selection) for Credit Risk Analysis";
proc reg data=loandata  outest=est_loan ;
    model loan_paid = categorizedIncome fico_avg loan_amnt total_pymnt delinq_2yrs last_fico_avg
                        / selection= stepwise;
     OUTPUT OUT=reg_loan_OUT  PREDICTED=   RESIDUAL=Res   L95M=C_l95m  U95M=C_u95m  L95=C_l95 U95=C_u95
      rstudent=C_rstudent h=lev cookd=Cookd  dffits=dffit;  
quit;
**********************Best subset selection**********************************;
title "Multiple Regression(Best subset selection) for Credit Risk Analysis";
proc reg data=loandata  outest=est_loan ;
    model loan_paid = categorizedIncome fico_avg loan_amnt total_pymnt delinq_2yrs last_fico_avg
                        / selection= maxr;
     OUTPUT OUT=reg_loan_OUT  PREDICTED=   RESIDUAL=Res   L95M=C_l95m  U95M=C_u95m  L95=C_l95 U95=C_u95
      rstudent=C_rstudent h=lev cookd=Cookd  dffits=dffit;  
quit;


