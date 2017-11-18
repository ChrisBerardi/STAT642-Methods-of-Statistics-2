*Homework 6, Stat642 Spring 2016;

*Problem 2;
ods html;ods graphics on;
option ls=80 ps=50 nocenter nodate;
title 'PROBLEM 2 Assignment 6 2014';

data old;
array Y Y1-Y4; INPUT C $ N $  Y1-Y4;  
TRT=COMPRESS (C) || COMPRESS (N); 
     label C = 'CROP' N = 'NITROGEN' R = 'ACE-REDUCTION' X = 'LOGARITHM OF ACE-REDUCTION'
            TRT = 'CROP-NITRATE';
do over Y;
R=Y;
W=Y;
X=log(R);
output; end;
      drop  Y1-Y4;
cards;
A 0    2.6 1.1 0.9 1.2    
A 50   1.5 1.8 0.7 2.2
A 100  0.6 1.3 1.9 2.6
S 0    6.5 2.6 3.9 4.3
S 50   0.6 0.6 0.3 0.8
S 100  0.5 0.1 0.1 0.3
G 0    0.5 0.9 0.7 0.7
G 50   0.3 0.5 0.4 0.4
G 100  0.2 0.1 0.1 0.2
M 0    0.8 0.9 2.2 1.2
M 50   0.7 0.7 0.5 0.6
M 100  0.3 0.4 0.2 0.2

Run;
data new;set old;
I1=0;I2=0;I3=0;I4=0;I5=0;I6=0;I7=0;I8=0;I9=0;I10=0;I11=0;
If TRT='A0' then I1=1;
If TRT='S0' then I2=1;
If TRT='G0' then I3=1;
If TRT='M0' then I4=1;
If TRT='A50' then I5=1;
If TRT='S50' then I6=1;
If TRT='G50' then I7=1;
If TRT='M50' then I8=1;
If TRT='A100' then I9=1;
If TRT='S100' then I10=1;
If TRT='G100' then I11=1;
run;

title 'Untransformed fails normality';
proc glm data=old order=data;
class C N;
model R = C N  C*N/ss3;
lsmeans C N C*N/stderr pdiff;
output out=ASSUMP r=RESID p=MEANS;
PROC PLOT; PLOT RESID*MEANS=C/vref=0;
PROC PLOT; PLOT MEANS*N=C;
proc univariate def=5 plot normal; var RESID;

PROC GLM data=old order=data;
CLASS TRT;
MODEL R = TRT/SS3;
CONTRAST  'LIN-A VS S' TRT    -1  0  1  1  0 -1  0  0  0  0 0  0;
CONTRAST  'LIN-A VS G'  TRT   -1  0  1  0  0  0  1  0 -1  0 0  0;
CONTRAST  'LIN-A VS M'  TRT   -1  0  1  0  0  0  0  0  0  1 0 -1;
CONTRAST  'LIN-S VS G'  TRT    0  0  0 -1  0  1  1  0 -1  0 0  0;
CONTRAST  'LIN-S VS M' TRT     0  0  0 -1  0  1  0  0  0  1 0 -1;
CONTRAST  'LIN-G VS M'  TRT    0  0  0  0  0  0 -1  0  1  1 0 -1;
MEANS TRT/hovtest=bf;
RUN;

title 'transformed for normality';
proc glm data=new order=data;
class C N;
model X = C N  C*N/ss3;
lsmeans C N C*N/stderr pdiff;
output out=ASSUMP r=RESID p=MEANS;
PROC PLOT; PLOT RESID*MEANS=C/vref=0;
PROC PLOT; PLOT MEANS*N=C;
proc univariate def=5 plot normal; var RESID;

PROC GLM data=new order=data;
CLASS TRT;
MODEL X = TRT/SS3;
CONTRAST  'LIN-A VS S' TRT    -1  0  1  1  0 -1  0  0  0  0 0  0;
CONTRAST  'LIN-A VS G'  TRT   -1  0  1  0  0  0  1  0 -1  0 0  0;
CONTRAST  'LIN-A VS M'  TRT   -1  0  1  0  0  0  0  0  0  1 0 -1;
CONTRAST  'LIN-S VS G'  TRT    0  0  0 -1  0  1  1  0 -1  0 0  0;
CONTRAST  'LIN-S VS M' TRT     0  0  0 -1  0  1  0  0  0  1 0 -1;
CONTRAST  'LIN-G VS M'  TRT    0  0  0  0  0  0 -1  0  1  1 0 -1;
MEANS TRT/hovtest=bf;
RUN;
proc transreg details data=new ss2
                 plots=(transformation(dependent) obp);
      model BoxCox(R / convenient lambda=-2 to 2 by 0.001) =
            identity(I1 I2 I3 I4 I5 I6 I7 I8 I9 I10 I11);
   run;
   ods graphics off;ods html close;



   *Problem 4;
   ODS HTML; ODS GRAPHICS ON;
option ls=75 ps=55 nocenter nodate;
title 'COATED FABRIC PROBLEM';

data PROB66;
array Y Y1-Y4; INPUT S  $ F $ P $ Y1-Y4 @@;  
TRT1 = COMPRESS (S) || (F);
TRT = COMPRESS (TRT1) || (P);
do over Y;
W=Y;
output; end;
      drop  Y1-Y4;
      label S  = 'SURFACE TRT' F = 'FILLERS' P = 'PROPORTION FILLER'
            W = 'WEIGHT LOSS';
cards;
S1 F1 25 194 208 197 205
S1 F1 50 233 241 235 239
S1 F1 75 260 279 266 270
S1 F2 25 229 197 200 212
S1 F2 50 224 243 228 235
S1 F2 75 249 232 236 245
S2 F1 25 155 173 160 168
S2 F1 50 198 177 185 192
S2 F1 75 235 219 230 225
S2 F2 25 137 160 142 155
S2 F2 50 184 163 175 177
S2 F2 75 212 189 195 204
run;
TITLE 'EFFECTS MODEL ANALYSIS';
proc glm order=data;
class F S P;
model W = S F P F*S S*P F*P F*S*P/SS3;
lsmeans  S|F|P/stderr pdiff;
output out=ASSUMP r=RESID p=MEANS;
run; 
proc glm order=data;
class F S;
model W =F S F*S/SS3;
lsmeans  S|F/stderr pdiff;
output out=ASSUMP r=RESID p=MEANS;
run; 

proc glm order=data;
class F P;
model W =F P F*P/SS3;
lsmeans  P|F/stderr pdiff;
output out=ASSUMP r=RESID p=MEANS;
run; 

proc glm order=data;
class S P;
model W =S P S*P/SS3;
lsmeans  S|P/stderr pdiff;
output out=ASSUMP r=RESID p=MEANS;
run; 

TITLE 'CELL MEANS MODEL ANALYSIS';
proc glm order=data;
class TRT;
model W = TRT/SS3;
means TRT/hovtest=bf; 
lsmeans TRT/stderr;
CONTRAST  'LIN-S1F1'   TRT    -1   0  1;
CONTRAST  'LIN-S1F2'   TRT     0   0  0 -1  0  1;
CONTRAST  'LIN-S2F1'   TRT     0   0  0  0  0  0 -1  0  1;
CONTRAST  'LIN-S2F2'   TRT     0   0  0  0  0  0  0  0  0  -1 0 1;
CONTRAST  'QUA-S1F1'   TRT     1  -2  1;
CONTRAST  'QUA-S1F2'   TRT     0   0  0  1 -2  1;
CONTRAST  'QUA-S2F1'   TRT     0   0  0  0  0  0  1  -2  1;
CONTRAST  'QUA-S2F2'   TRT     0   0  0  0  0  0  0   0  0  1 -2 1;

CONTRAST  'LIN-F1'   TRT      -1   0  1  0  0  0  -1 0 1;
CONTRAST  'LIN-F2'   TRT       0   0  0 -1  0  1   0  0  0  -1  0  1;
CONTRAST  'QUA-F1'   TRT       1  -2  1  0  0  0   1 -2  1;
CONTRAST  'QUA-F2'   TRT       0   0  0  1 -2  1   0  0  0   1 -2  1;
run;
output out=ASSUMP r=RESID p=MEANS;
means trt/hovtest=bf;
proc gplot; plot resid*means;
proc univariate def=5 plot normal; var RESID;
run;
ODS GRAPHICS OFF; ODS HTML CLOSE;
