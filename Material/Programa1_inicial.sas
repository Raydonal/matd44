

data cadastro;
set import;
run;

proc means data=cadastro;
var idh00 espvi00;
run; 

proc surveyselect data=cadastro method=SRS sampsize=30 out=amostra;
run; 

proc surveymeans data=amostra N=5565;
var idh00 espvi00;
run;
