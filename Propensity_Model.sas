/* Assigning the library name */

libname ishu '/folders/myshortcuts/SAS Folder' ;

/* Creating the dataset to work on because we dont wanna messup oringnal dataset */

data ishu.annuity ;
set '/folders/myshortcuts/SAS Folder/develop.sas7bdat' ;
run ;

/* DATA CHECK */
/* Checking the data, Checking Number of variables and Observations, identified datatype  */

proc contents data = ishu.annuity varnum  ; 
run ;
/*  Significane of varnum is it will show Variables in Creation Order */


/* SANITY CHECK */

/* detect the outliers and missing values */

proc means min p1 p5 p95 p99 max n nmiss ;
var AcctAge DDA DDABal CashBk Checks DirDep NSF NSFAmt Phone Teller Sav SavBal ATM ATMAmt POS POSAmt CD CDBal
IRA IRABal LOC LOCBal ILS ILSBal MM MMBal MTG MTGBal CC CCBal CCPurc SDB Income HMOwn LORes HMVal Age
CRScore Moved InArea Dep DepAmt Inv InvBal ;
run ;

/* step used to detect the missing values in categorical data type */
Proc freq data = ishu.annuity ;
tables branch res ;
run ;

/* DATA PREP */

/* Treatment of Outliers  */

proc means data = ishu.annuity min p1 p5 p25 p50 p75 p95 p99 max;
var AcctAge DDABal Checks NSFAmt Phone Teller SavBal ATMAmt POS POSAmt CDBal IRABal LOCBal
ILSBal MMBal MTGBal CCBal Income HMVal Dep DepAmt InvBal
;
run;

data ishu.OTT ; 
set ishu.annuity ;
if AcctAge > 31.4000 then AcctAge = 31.4000 ;
if AcctAge < 0.3000 and AcctAge ne . then AcctAge = 0.3000 ;

if ddabal > 26557.49 then ddabal = 26557.49 ;
if ddabal < 0 and ddabal ne . then ddabal = 0 ;

if Checks > 22.00 then Checks = 22.00 ;
if Checks < 0 and Checks ne . then Checks = 0 ;

if NSFAmt > 59.1600 then NSFAmt = 59.1600 ;
if NSFAmt < 0 and NSFAmt ne . then NSFAmt = 0 ;

if Phone > 6 then Phone = 6 ;
if Phone < 0 and Phone ne . then Phone = 0 ;

if Teller > 10 then Teller = 10 ;
if Teller < 0 and Teller ne . then Teller = 0 ;

if SavBal > 50718 then SavBal = 50718 ;
if SavBal < 0 and SavBal ne . then SavBal = 0 ;

if ATMAmt > 14018.74 then ATMAmt = 14018.74 ;
if ATMAmt < 0 and ATMAmt ne . then ATMAmt = 0 ;

if POS > 14 then POS = 14 ;
if POS < 0 and POS ne . then POS = 0 ;

if POSAmt > 664.230 then POSAmt = 664.230 ;
if POSAmt < 0 and POSAmt ne . then POSAmt = 0 ;

if CDBal > 58800 then CDBal = 58800 ;
if CDBal < 0 and CDBal ne . then CDBal = 0 ;

if IRABal > 14563.98 then IRABal = 14563.98 ;
if IRABal < 0 and IRABal ne . then IRABal = 0 ;

if LOCBal > 34993.29 then LOCBal = 34993.29 ;
if LOCBal < 0 and LOCBal ne . then LOCBal = 0 ;

if ILSBal > 10405.62 then ILSBal = 10405.62 ;
if ILSBal < 0 and ILSBal ne . then ILSBal = 0 ;

if MMBal > 19167.38 then MMBal = 19167.38 ;
if MMBal < 0 and MMBal ne . then MMBal = 0 ;

if MTGBal > 220889.88 then MTGBal = 220889.88 ;
if MTGBal < 0 and MTGBal ne . then MTGBal = 0 ;

if CCBal > 205911.56 then CCBal = 205911.56 ;
if CCBal < 0 and CCBal ne . then CCBal = 0 ;

if Income > 134 then Income = 134 ;
if Income < 3 and Income ne . then Income = 3 ;

if HMVal > 176 then HMVal = 176 ;
if HMVal < 79 and HMVal ne . then HMVal = 79 ;

if Dep > 7 then Dep = 7 ;
if Dep < 0 and Dep ne . then Dep = 0 ;

if DepAmt > 19548.49 then DepAmt = 19548.49 ;
if DepAmt < 0 and DepAmt ne . then DepAmt = 0 ;

if InvBal > 13949.74 then InvBal = 13979.74 ;
if InvBal < 0 and InvBal ne . then InvBal = 0 ;

run ;

/* Missing Values Treatment */

proc stdize data = ishu.ott out = ishu.TMISS missing = 0  reponly ;
run;
/* if we do not use reponly then it will normalize the data. data points will be changed. */


/* VARIABLE REDUCTION  */
/* Finding Information Value IV for All Variables including categorical*/
/* bucketing */

proc rank data = ishu.tmiss groups=10 out = decile ;
var DDABal ;
ranks DDABal_rank ;
run ;

proc freq data = decile ;
tables DDABal_rank / out = ABC1 ;
run ;

proc freq data = decile ;
tables DDABal_rank / out = ABC2 ;
where ins = 1 ;
run ;

proc freq data = decile ;
tables DDABal_rank / out = ABC3 ;
where ins = 0 ;
run ;

/* Information Value (IV) */

data IVALUE ;
merge 
ABC1 (rename = (count = count1 percent = percent1)) 
ABC2 (rename = (count = count2 percent = percent2))
ABC3 (rename = (count = count3 percent = percent3));
by DDABal_rank ;
iv = (percent2 - percent3 ) * log (percent2 / percent3) ;
sum+iv ;
run ;

/*IV For categorical variables */

proc freq data = ishu.tmiss ;
tables branch / out = ABC1 ;
run ;

proc freq data = ishu.tmiss ;
tables branch / out = ABC2 ;
where ins = 1 ;
run ;

proc freq data = ishu.tmiss ;
tables branch / out = ABC3 ;
where ins = 0 ;
run ;

data IVALUE ;
merge 
ABC1 (rename = (count = count1 percent = percent1)) 
ABC2 (rename = (count = count2 percent = percent2))
ABC3 (rename = (count = count3 percent = percent3));
by branch ;
iv = (percent2 - percent3 ) * log (percent2 / percent3) ;
sum+iv ;
run ;

/* dropping the redundant variables based on IV in desc */

data ishu.POIValue ;
set ishu.tmiss ;
keep 
ins SavBal DDABal CDBal CD DepAmt DDA Dep ATMAmt MMBal MM Sav CC
CCBal ATM Phone IRA Branch IRABal POSAmt Checks Inv MMCred POS CCPurc DirDep ;
run ;


/* Testing with corelation for a statistical approach  */
/* as the dependent variable is qualitative we use spearman */

proc corr spearman rank nosimple;
with ins ;
run ;

/* Sorting the data */

Proc sort data = ishu.poivalue ;
by ins ;
run;


/* Dividing the data into 70/30 split */

proc surveyselect data= ishu.poivalue out = splited outall rate = 0.7 seed = 1234 ;
strata ins;
run;

/* Split of 70/30 */

data ishu.train ishu.valid ;
set splited ;
if selected = 1 then output ishu.train ;
else output ishu.valid ;
run ;


/* detecting & treating multicollinearity in training data set */

proc reg data = ishu.train ;
model ins = SavBal DDABal CDBal CD DepAmt DDA Dep ATMAmt MMBal MM Sav CC
CCBal ATM Phone IRA IRABal POSAmt Checks Inv MMCred POS CCPurc
DirDep / vif ;
run ;

/* removing varibles one by one with high VIF  */

proc reg data = ishu.train ;
model ins = SavBal DDABal CDBal CD DepAmt DDA Dep ATMAmt MMBal  Sav CC
CCBal ATM Phone IRA IRABal POSAmt Checks Inv MMCred POS CCPurc
DirDep / vif ;
run ;

/* Changing the categorical variable to Numeric form is DUMMY Variables Creation  */

/* data test ; */
/* set sashelp.class ; */
/* male_flag = (sex='M'); */
/* run; */


/* Building Model */

proc logistic data = ishu.train desc;
model ins = SavBal DDABal CDBal CD DepAmt DDA Dep ATMAmt MMBal Sav CC
CCBal ATM Phone IRA IRABal POSAmt Checks Inv MMCred POS CCPurc
DirDep /selection = stepwise sle=0.05 sls=0.05 ;
run;

/* priority of the variable selection is measured with the wald chi square  */

/* Selection of best Contributing variables */

proc logistic data = ishu.train desc;
model ins = SavBal DDABal CD MMBal Sav CC ATMAmt DDA
 /selection = stepwise sle=0.05 sls=0.05 ;
run;

/* Model development  */

Proc Logistic data = ishu.train outest = TFinal1 desc ;
model ins = SavBal DDABal CD MMBal Sav CC ATMAmt 
DDA / outroc = TFinal2 ;
Score data = ishu.train out = TFinal3 ;
run ;

/* outest: Betas estimates  */
/* outroc: TP TN FP FN or ROC Curve of Sensitivity and 1-Specificity  */
/* Score: Scoring of the data (predicted probability of outcome (y) is scoring) */


/* Checking the model on validation data set */

Proc Logistic data = ishu.valid outest = VFinal1 desc;
model ins = SavBal DDABal CD MMBal Sav CC ATMAmt 
DDA / outroc = VFinal2 ;
score data = ishu.valid out = VFinal3;
run ;

/* [Gini Coefficient = 2*AUC – 1] */
/* Gini refers to the quality and evalute the prediction power of a model */

/* Kolmogorov-Smirnov test */
/* finding KS - for Training data set  */

/* first step is to split the predicted probability into decile then compute cumulative */
/*  %events and %nonevents in each decile & check the decile where difference is MAX  */

Proc rank data = TFinal3 out = Ranked descending groups=10 ;
var P_1 ;
ranks P_1_Rank;
run ;

Proc freq data = ranked ;
tables P_1_rank / out = events ;
where ins = 1 ;
run ;

Proc freq data = ranked ;
tables P_1_rank / out = nonevents ;
where ins = 0;
run ;

data ishu.KSTFINAL ;
merge 
events (rename = (percent=percent1))
nonevents (rename=(percent=percent2));
by P_1_rank ;
CUM_EVENTS_PERCENT + Percent1 ;
CUM_NEVENTS_PERCENT + Percent2 ;
KS = CUM_EVENTS_PERCENT - CUM_NEVENTS_PERCENT ;
run;

/* Kolmogorov-Smirnov test */
/* finding KS - for Validation data set  */

proc rank data = VFinal3 out = ranked descending groups=10 ;
var P_1 ;
ranks P_1_rank ;
run ;

Proc freq data = ranked ;
tables P_1_rank / out = events ;
where ins = 1 ;
run ;

proc freq data = ranked ;
tables P_1_rank /out = nonevents ;
where ins = 0 ;
run ;

Data ishu.KSVFINAL ;
merge
events (rename= (percent = percent1))
nonevents (rename = (percent = percent2));
by P_1_rank ;
CUM_EVENTS_PERCENT + Percent1 ;
CUM_NEVENTS_PERCENT + Percent2 ;
KS = CUM_EVENTS_PERCENT - CUM_NEVENTS_PERCENT ;
run;

/* The value of KS shows the gap between the %EVENTS AND %NONEVENTS */

