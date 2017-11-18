*Stat 642 Assignment 3
Spring, 2016;

*Problem I*******************************************************************************************************************;

data old;
array Y Y1-Y5;
input T $ Y1-Y5;
do over Y;
TCOUNT=Y;
output; end;
      drop Y1-Y5;
cards;
FASTING 98.81 103.55 115.23 129.06 117.61
60gBRAN 197.18 207.31 177.50 226.05 222.74  
80gBRAN 102.93 117.51 119.92 112.01 101.10
LAYMASH 83.14 89.59 87.76 96.43 82.94
PREMOLT 94.09 90.45 99.38 73.56 74.39
run;

proc glimmix data=old order=data;
class T;
model  TCOUNT=T/solution;
lsmeans T/ plot = meanplot cl ;
proc glm data=old order=data;
class T;
model  TCOUNT=T/solution;
lsmeans T/stderr cl ;
means T/HOVTEST=bf;
output out=ASSUMP R=RESID P=PRED;
proc univariate def=5 plot normal; var RESID;
run;



*Problem II************************************************************************************;

data old;
array Y Y1-Y5;
input T $ Y1-Y5;
do over Y;
TCOUNT=Y;
output; end;
      drop Y1-Y5;
cards;
FASTING 98.81 103.55 115.23 129.06 117.61 
60gBRAN 197.18 207.31 177.5 .      .  
80gBRAN 102.93 117.51 119.92 112.01 101.10
LAYMASH 83.14 89.59 87.76 82.94     .
PREMOLT 94.09 90.45 99.38 73.56     .
run;

proc glimmix data=old order=data;
class T;
model  TCOUNT=T/solution;
lsmeans T/ plot = meanplot cl ;
proc glm data=old order=data;
class T;
model  TCOUNT=T/solution;
lsmeans T/stderr cl ;
means T/HOVTEST=bf;
output out=ASSUMP R=RESID P=PRED;
proc univariate def=5 plot normal; var RESID;
run;


*Problem III****************************************************************************************************************
Use approach three;
Data;
Input r @@;
t=3;
alpha=.01;
s2=12;
u1=20;
u2=18;
u3=16;
mean_u=(u1+u2+u3)/t;
L=r*((u1-mean_u)**2 + (u2-mean_u)**2 + (u3-mean_u)**2) /s2;
n1=t-1;
n2=t*(r-1);
Fcr=finv(1-alpha,n1,n2);
P=1-PROBF(Fcr,n1,n2,L);
Cards;
20 21 22 23 24 25 26 27 28 29
run;
proc print;
var  r n1 n2 Fcr L P;
Run;


*Problem IV ***************************************************************************************************************
use approach 5;
data;
input  r @@;
t=5; alpha=.05;
u1=t-1; u2=t*(r-1);
S=12.247;
D=30;
L=r*D**2/(2*(S**2));
phi=sqrt(L/t);
c=finv(1-alpha,u1,u2);
p=1-probf(c,u1,u2,L);
cards;
2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
run;
proc print; var t r c u1 u2 L phi p;
run;
