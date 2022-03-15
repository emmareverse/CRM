*import cleaned data*;
proc import out=x1
datafile=' C:\Chromedownload\cleaned_final.csv'
dbms=csv replace;
getnames= yes;
run;

*based on business understanding, we divided the catagorical variable review_score into 2 groups: 4 and above 4 is considered highscore, while others as lowscore. Dummy varaible created.*;
data x2; set x1;
if review_score >=4 then highscore =1;
else highscore = 0;
run;

*check the correlation between our 7 independant variables and one chosen dependant variables*;
proc corr data=x2;
var highscore product_description_lenght product_photos_qty late seller_response_time customer_response_time freight_value_by_order total_product_value_by_order;
run;
*results show significance for all variabls, which forwards us to the next step of model buidling*;

*logistic regression model 1*;
proc logistic data=x2 descending;
model1: model highscore = product_description_lenght product_photos_qty 
late seller_response_time customer_response_time freight_value_by_order total_product_value_by_order ;
run;
*many variables turns out to be insignificant*;

*check the skewness of the 8 variables*;
proc sgplot data=x2;
histogram seller_response_time;
density seller_response_time;
run;

proc sgplot data=x2;
histogram highscore;
density highscore;
run;

proc sgplot data=x2;
histogram customer_response_time;
density customer_response_time;
run;

proc sgplot data=x2;
histogram product_description_lenght;
density product_description_lenght;
run;

proc sgplot data=x2;
histogram product_photos_qty;
density product_photos_qty;
run;

proc sgplot data=x2;
histogram late;
density late;
run;

proc sgplot data=x2;
histogram freight_value_by_order;
density freight_value_by_order;
run;

proc sgplot data=x2;
histogram total_product_value_by_order;
density total_product_value_by_order;
run;

*loglog model and lag model transformation*;
data x4; 
set x2;
lagseller_response_time=lag(seller_response_time);
lncustomer_response_time=log(customer_response_time);
lnproduct_description_lenght=log(product_description_lenght);
lnproduct_photos_qty=log(product_photos_qty);
lnfreight_value_by_order=log(freight_value_by_order);
lntotal_product_value_by_order=log(total_product_value_by_order);
run;

*spliting the dataset*;
proc surveyselect data=x4 out=x5 method=srs
samprate=0.80 outall seed=12345 noprint;
run;

data train1; set x5;if selected=1;run;
data test1;  set x5;if selected=0;run;

*using train data for model 1*;
proc logistic data=train1 descending;
model1:  model highscore = product_description_lenght product_photos_qty 
late seller_response_time customer_response_time freight_value_by_order total_product_value_by_order ;
run;

*logistic regression model 2*;
proc logistic data=train1 descending;
model2: model highscore = lnproduct_description_lenght lnproduct_photos_qty 
late lagseller_response_time lncustomer_response_time lnfreight_value_by_order lntotal_product_value_by_order ;
run;

*model comparison based on ROC curve*; 
proc logistic data=test1 descending;
model1: model highscore = product_description_lenght product_photos_qty 
late seller_response_time customer_response_time freight_value_by_order total_product_value_by_order ;
roc;
run;

proc logistic data=test1 descending;
model2: model highscore = lnproduct_description_lenght lnproduct_photos_qty 
late lagseller_response_time lncustomer_response_time lnfreight_value_by_order lntotal_product_value_by_order ;
roc; 
run;





       
         





