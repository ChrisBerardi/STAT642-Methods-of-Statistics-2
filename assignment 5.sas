*Assignment 5 STAT642 Spring, 2016;

*1 a
Input Data;
option ls=120 ps=55 nocenter nodate;
title 'Hours to Failure';
data Temps; array Y Y1-Y6;                                                    
input T  Y1-Y6; do over Y; HF=Y; output; end;                            
      drop Y1-Y6;
      label T = 'Temperature' HF = 'Hours to Failure';
cards;                                                             
40 1953 2135 2471 4727 6134 6314
45 1190 1286 1550 2125 2557 2845
55 651  817  848  1038 1361 1543
70 511  651  651  652  688  729
run;

*Analysis of Variance;
proc glm data=Temps;
class T;
model HF = T;

*Brown-Forsythe-Levene Test;
means T/hovtest=bf;
means T/ tukey;

*Residual analysis;
output out=ASSUMP r=RESID p=MEANS STUDENT=SR;
DATA TEMPSRESID; SET ASSUMP; TSR=SQRT(ABS(SR));
proc univariate def=5 plot normal; var RESID;
proc gplot data=assump; plot resid*means;
PROC gplot DATA=TTEMPSRESID; PLOT TSR*MEANS;
RUN;


*Plots;
proc boxplot;
plot HF*T/boxstyle=schematic;
run;

*1b;
*analysis of transformed data;
option ls=120 ps=55 nocenter nodate;
title 'Hours to Failure - LogTransformation';
data trans;
set Temps;
label TY1 = 'Transformed Hours to Failure';
TY1 = log(HF);
proc boxplot;
plot ty1*T/boxstyle=schematic;
run;
proc glm data=trans;
class T;
model TY1 = T;
*BFL Test;
means T/hovtest=bf;
means T/ tukey;
output out=ASSUMP2 r=RESID2 p=MEANS2 STUDENT=SR2;
data TEMPRESID2; set ASSUMP2; TRS2 = sqrt(abs(SR2));
proc univariate def=5 plot normal; var RESID2;
proc gplot data=ASSUMP2; plot RESID2*MEANS2;
proc gplot data=TEMPRESID2; plot TRS2*MEANS2;
run;

*1c. Box Cox Transformation
option ls=120 ps=55 nocenter nodate;
title 'Hours to Failure - Box-Cox Tranform';
data trans;
set Temps;
label TY1 = 'Transformed Hours to Failure';
TY1 = HF**-.64;
proc boxplot;
plot ty1*T/boxstyle=schematic;
run;
proc glm data=trans;
class T;
model TY1 = T;
*BFL Test;
means T/hovtest=bf;
means T/ tukey;
output out=ASSUMP2 r=RESID2 p=MEANS2 STUDENT=SR2;
data TEMPRESID2; set ASSUMP2; TRS2 = sqrt(abs(SR2));
proc univariate def=5 plot normal; var RESID2;
proc gplot data=ASSUMP2; plot RESID2*MEANS2;
proc gplot data=TEMPRESID2; plot TRS2*MEANS2;
run;

*1g. trend testing;
proc glm data=trans order=data;
class T;
model TY1=T/ss3;
lsmeans T/stderr;    
contrast 'LINEAR' T -5 -3 1 7;
contrast 'QUADRATIC' T 9 -3 -13 7; 
contrast 'CUBIC' T -5 9 -5 1;
*simultaneous test of all 3 contrasts;
contrast '3 TREND CONTRASTS' T -5 -3 1 7
                             T 9 -3 -13 7
                             T -5 9 -5 1;
run;

*2;

*2a use Kruskal-Wallis;
title 'Hours to Failure Tukey untransformed';
proc npar1way anova wilcoxon;
var HF;
class T;
run;
title 'Hours to Failure Tukey transformed';
proc npar1way anova wilcoxon;
var TY1;
class T;
run;

*2b;
title 'Hours to Failure Tukey untransformed';
proc glm data=Temps;
class T;
model HF = T;
*BFL Test;
means T/hovtest=bf;
means T/ tukey;
output out=ASSUMP2 r=RESID2 p=MEANS2 STUDENT=SR2;
data TEMPRESID2; set ASSUMP2; TRS2 = sqrt(abs(SR2));
proc univariate def=5 plot normal; var RESID2;
proc gplot data=ASSUMP2; plot RESID2*MEANS2;
proc gplot data=TEMPRESID2; plot TRS2*MEANS2;
run;

*3;
data count;                                                  
array Y Y1-Y15;
input S $ Y1-Y15;
do over Y;
NE=Y;
output; end;
      drop Y1-Y15;
cards;                                                             
USDA   418 906 28  277 634 48  369 137 29 522 319 242 261 566 734
FIELD  211 276 415 787 18  118 1   151 0  253 61  0   275 0   153
RESIST 0   9   143 1   26  127 161 294 0  348 0  14   21  0   218
run;
title "Poisson Regression on Moth Egg Data";
proc genmod data=count;
class S;
model NE = S/Dist=P link=log;
run;
Title "Overdispersed Poisson Regression on Moth Egg Data";
proc genmod data=count;
class S;
model NE = S/dist=P link=log scale = pearson;
run;

*4;
DATA SOIL; ARRAY X X1-X2;
INPUT FIELD $ SECTION X1-X2 @@;
DO OVER X;Y=X;OUTPUT; END;DROP X1-X2;
LABEL FIELD ='FIELD' Y= 'POROSITY';
cards;
F1 1  3.846 3.712  F1 2  5.629 2.021
F2 1  5.087 .      F2 2 4.621 .
F3 1  4.411 .      F3 2  3.357 .
F4 1  3.991 .      F4 2  5.766 .
F5 1  5.677 .      F5 2  3.333 .
F6 1  4.355 6.292  F6  2 4.940 4.810
F7 1  2.983 .      F7  2 4.396 .
F8 1  5.603 .      F8  2 3.683 .
F9 1  5.942 .      F9  2 5.014 .
F10 1 5.143 .      F10 2 4.061 .
F11 1 3.835, 2.964 F11 2 4.584, 4.398
F12 1 4.193 .      F12 2  4.125 .
F13 1 3.074 .      F13 2 3.483 .
F14 1 3.867 .      F14 2 4.212 .
F15 1 6.247 .      F15 2 4.730 .
RUN;

title 'ANALYSIS USING THE GLM PROCEDURE';
PROC GLM;
CLASS FIELD SECTION;
MODEL Y = FIELD SECTION(FIELD)/E1;
RANDOM FIELD SECTION(FIELD)/TEST;
RUN;
title 'ANALYSIS USING PROC MIXED-REML';
PROC MIXED METHOD=REML;
CLASS FIELD SECTION;
MODEL Y = /E3;
RANDOM FIELD SECTION(FIELD)/ALPHA=.05;
RUN; 

*5;
TITLE 'Fixed treatments, find reps';
data;
input r @@;
t=8;
u1=t-1; u2=t*(r-1);
gamma=.9;
alpha=.05;
tau=2;
lambda=sqrt(1+(r)*(tau));
Fcr=finv(1-alpha,u1,u2);
C=(1/lambda**2)*Fcr;
power=1-probf(C,u1,u2);
cards;
2 3 4 5
run;
proc print; var r u1 u2 C Fcr lambda power;
run;

*6;
TITLE 'Fixed reps, find treatments';
data;
input t @@;
r=5;
u1=t-1;
u2=t*(r-1);
sigma_e=2;
sigma_a=2.1;
gamma=.9;
alpha=.01;
tau=sigma_a/sigma_e;
lambda=sqrt(1+(r)*(tau));
Fcr=finv(1-alpha,u1,u2);
C=(1/lambda**2)*Fcr;
power=1-probf(C,u1,u2);
cards;
7 8 9 10 11 12 13
run;
proc print; var t u1 u2 Fcr lambda power;
run;
