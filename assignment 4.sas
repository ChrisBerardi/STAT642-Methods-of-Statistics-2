* Assignment 4 STAT642 Spring, 2016;

*Problem 1 b,e
5 treatments, constant difference of 20, 10 reps for each treatment level;
option ls=80 ps=50 nocenter nodate;
title 'Heat Loss for 5 Thickness of Coating';
data poly; array Y Y1-Y10;                                                    
input T  Y1-Y10; do over Y; HL=Y; output; end;                            
      drop Y1-Y10;
      label T = 'Thickness of Coating' HL = 'Heat Loss';
cards;                                                             
0  10.2 10.8 10.1 10.9 11.1 11.8 11.3 11.9 9.3 9.9
20 9.2  9.8  9.1  9.9  10.1 10.8 10.3 10.9 9.3 9.8
40 9.0  9.9  9.2  9.8  10.0 10.8 10.2 10.8 9.9 9.0
60 8.1  8.1  8.0  8.9  8.2  8.9  8.1  8.8  9.2 9.9 
80 7.2  7.8  7.1  7.9  8.1  8.8  8.3  8.9  9.3 9.8
run;   
proc plot;
plot HL*T='*';
run;
proc glm data=poly order=data;
class T;
model HL=T/ss3;
lsmeans T/stderr;    
contrast 'CONTROL VS OTHERS' T 4 -1 -1 -1 -1; 
contrast 'LINEAR' T -2 -1 0 1 2;
contrast 'QUADRATIC' T 2 -1 -2 -1 2; 
contrast 'CUBIC' T -1 2 0 -2 1;
*simultaneous test of all 3 contrasts;
contrast '3 TREND CONTRASTS' T -2 -1 0 1 2,
                             T 2 -1 -2 -1 2, 
                             T -1 2 0 -2 1;
run;

title 'Scheffé and Bonferroni Tests';
*1c,d;
proc glimmix;
class T; 
model HL = T;   
lsmestimate T 'CONTROL VS OTHERS' 4 -1 -1 -1 -1, 'LINEAR' -2 -1 0 1 2,
'QUADRATIC' 2 -1 -2 -1 2, 'CUBIC' -1 2 0 -2 1 /cl alpha=.05 adjust=BON;         
lsmestimate T 'CONTROL VS OTHERS' 4 -1 -1 -1 -1, 'LINEAR' -2 -1 0 1 2,
'QUADRATIC' 2 -1 -2 -1 2, 'CUBIC' -1 2 0 -2 1 /cl alpha=.05 adjust=SCHEFFE;
run;  


*e;
title 'Martix Approach';
data poly;
array Y Y1-Y10;
input T Y1-Y10;
T2=T**2; T3=T**3;
do over Y; HL=Y; output; end;
drop Y1-Y10;
      label T = 'Thickness of Coating' HL = 'Heat Loss';
cards;                                                             
0  10.2 10.8 10.1 10.9 11.1 11.8 11.3 11.9 9.3 9.9
20 9.2  9.8  9.1  9.9  10.1 10.8 10.3 10.9 9.3 9.8
40 9.0  9.9  9.2  9.8  10.0 10.8 10.2 10.8 9.9 9.0
60 8.1  8.1  8.0  8.9  8.2  8.9  8.1  8.8  9.2 9.9 
80 7.2  7.8  7.1  7.9  8.1  8.8  8.3  8.9  9.3 9.8
run;
proc reg data=poly ;
model HL= T T2 T3 / ss1 ss2;
run;
proc reg data=poly ;
model HL= T T2 / ss1 ss2;
run;
proc reg data=poly ;
model HL= T / ss1 ss2;
run;


*2;
*a. use Hsu looking for smallest;
*b. use Dunnet;
*c. Use Tukey;
title 'Dunnet and Tukey Results';
data old; array Y Y1-Y10;
input T $ Y1-Y10; do over Y; HL=Y; output; end;
drop Y1-Y10;
      label T = 'Thickness of Coating' HL = 'Heat Loss';
cards;                                                             
0  10.2 10.8 10.1 10.9 11.1 11.8 11.3 11.9 9.3 9.9
20 9.2  9.8  9.1  9.9  10.1 10.8 10.3 10.9 9.3 9.8
40 9.0  9.9  9.2  9.8  10.0 10.8 10.2 10.8 9.9 9.0
60 8.1  8.1  8.0  8.9  8.2  8.9  8.1  8.8  9.2 9.9 
80 7.2  7.8  7.1  7.9  8.1  8.8  8.3  8.9  9.3 9.8
run;
proc glm data=old order=data;
class T;
model HL=T;
run;
lsmeans T/cl pdiff alpha=.05 adjust=tukey;
lsmeans T/cl pdiff=controll('0') adjust=DUNNETT alpha=.05;
run;
means T/hovtest=bf;
output out=ASSUMP r=RESID p=MEANS;
run;

