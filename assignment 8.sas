*Assignment 8 STAT642 Spring, 2016;

*Problem 1;
OPTIONS LS=90 PS=55 nocenter nodate;
DATA ;
INPUT S $ I $ M $ T $ Y @@;
LABEL S='SIGNAL' I='INTERSECTION' M='MEASUREDEVICE' T='TRAFFIC';
CARDS;

P 1 PS R  61.7    P 1 PS NR 57.4    P 1 PT R  53.1    P 1 PT NR 36.5
P 2 PS R  35.8    P 2 PS NR 18.5    P 2 PT R  35.5    P 2 PT NR 15.9
S 3 PS R  20.0    S 3 PS NR 24.6    S 3 PT R  17.0    S 3 PT NR 21.0
S 4 PS R   2.7    S 4 PS NR  3.1    S 4 PT R   1.5    S 4 PT NR  1.1
F 5 PS R  35.7    F 5 PS NR 26.8    F 5 PT R  35.4    F 5 PT NR 20.7
F 6 PS R  24.3    F 6 PS NR 25.9    F 6 PT R  27.5    F 6 PT NR 23.3
run;

PROC GLM;
CLASS S M T I ;
MODEL Y = S T I(S) S*T T*I(S) M S*M T*M M*I(S) S*T*M;
RANDOM I(S) T*I(S) M*I(S)/TEST;
LSMEANS S|M|T/ADJUST=TUKEY;
RUN;

PROC MIXED CL ALPHA=.05 COVTEST ;
CLASS S M T I ;
MODEL Y = S T  S*T  M S*M T*M  S*T*M/RESIDUALS;
RANDOM I(S) T*I(S) M*I(S);
LSMEANS S|M|T/ADJUST=TUKEY;
RUN;

*Problem 10, 5;
DATA ten;
INPUT A $ B $ C $ D $ E $ Y @@;
CARDS;

L L L H H .78
H L H H H 1.1
H H L L L 1.7
H L H L L 1.28
L H L L H .97
L L H L H 1.47
L H L H L 1.85
H H H H L 2.1
L H H H H .76
H H L H H .62
L L H H L 1.09
L L L L L 1.13
H L L L H 1.25
H H H L H .98
H L L H L 1.36
L H H L L 1.18
run;

proc glm;
class A B C D E;
model Y = A B C D E A*B A*C A*D A*E B*C B*D B*E C*D C*E D*E ;
estimate 'A' A 1 -1;
estimate 'B' B 1 -1;
estimate 'C' C 1 -1;
estimate 'D' D 1 -1;
estimate 'E' E 1 -1;
estimate 'A*B' A*B 1 -1 -1 1;
estimate 'A*C' A*C 1 -1 -1 1;
estimate 'A*D' A*D 1 -1 -1 1;
estimate 'A*E' A*E 1 -1 -1 1;
estimate 'B*C' B*C 1 -1 -1 1;
estimate 'B*D' B*D 1 -1 -1 1;
estimate 'B*E' B*E 1 -1 -1 1;
estimate 'C*D' C*D 1 -1 -1 1;
estimate 'C*E' C*E 1 -1 -1 1;
estimate 'D*E' d*E 1 -1 -1 1;
run;
