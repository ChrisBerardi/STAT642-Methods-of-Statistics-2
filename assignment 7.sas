*Homework 7;

*II ;

*1;
data cell;
input TRT $ Y @@;
LABEL Y='CHEMICAL YIELD';
CARDS;
150L 90.4 150L 90.4
150M 90.7 150M 90.6
200L 90.1 200L 90.3
200H 89.9 200H 90.1
250L 90.5 250L 90.7
250M 90.8 250M 90.9
250H 92.4 250H 92.1
;
proc glm data=cell;
Class TRT;
model y = TRT;
CONTRAST 'a.' TRT 1 1 0 0 -1 -1 0,
 			  TRT 0 0 -1 -1 1 0 1;
CONTRAST 'b.' TRT 1 -1 0 0 1 -1 0,
			  TRT 0 0 1 -1 1 0 -1;
run;

*3;

data temp;
input TEMP $ PRESSURE $ Y @@;
LABEL Y='CHEMICAL YIELD';
CARDS;
150 L 90.4 150 L 90.4
150 M 90.7 150 M 90.6
200 L 90.1 200 L 90.3
200 H 89.9 200 H 90.1
250 L 90.5 250 L 90.7
250 M 90.8 250 M 90.9
250 H 92.4 250 H 92.1
;
title 'Anaylsis as a CR 3x3 factorial';
proc glm;
class TEMP PRESSURE;
model Y = TEMP PRESSURE TEMP*PRESSURE/ss4;
LSMEANS TEMP PRESSURE TEMP*PRESSURE/pdiff stderr adjust=tukey;
run;


*V 3;
options ps=55 ls=70 nocenter nodate;

*Factorial Experiment - Rep Size
SAS Program to compute sample size when the specification is that
at least one pair of treatment means are at least D units apart.
It is necessary to provide
t=Number of Treatments
D=size of effect to be detected
S=an estimate of the experimental standard deviation(sigma);
title 'rep size calculations for factorial experiment main effects';
data maineffect;
input  r2 @@;
a2=5;
b2=2; 
t2=a2*b2;
alpha2=.05;
u12=b2-1; 
u22=t2*(r2-1);
S2=6.5;
D2=6;
L2=r2*D2**2/(2*(S2**2));
phi2=sqrt(L2/b2);
c2=finv(1-alpha2,u12,u22);
p2=1-probf(c2,u12,u22,L2);
cards;
2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
run;
proc print; var b2  r2 c2 u12 u22 L2 phi2 p2;
run;
