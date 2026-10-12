$PROB
PK model translated from NONMEM

$SET 
end = 1368
delta = 12

$PARAM @annotated
THETA1 : 0.064  : MF CL 
THETA2 : 13.8   : MF V2 
THETA3 : 0.22   : MF KA (PKDL)                
THETA4 : 0.04   : MF KA (VL) 
THETA5 : 0.0016 : MF Q 
THETA6 : 2.01   : MF V3
THETA7 : -0.13  : MF F-CODE
THETA9 : 1.84        : h0
THETA10 : 167.28      : I50
THETA11 : 0.454         : Scale parameter for IIV V


$PARAM @covariates
FFM   = 25
WT    = 25
VL    = 1 
PKDL  = 1
AGE = 70;
HT = 70;
SEX = 70;
FLAG = 1;
TSWITCH = 0 // Time after which to start calculating EOC AUC (e.g., day 58)
ADH   = 0.95   
PILLS = 1


$CMT @annotated
DEPO   : 1 MF Dosing compartment (mg)
CENT   : 2 MF Central compartment (mg)
PERI   : 3 MF Peripheral compartment (mg) 
AUC     : 4 MF AUC (mg*hr/L);
TEC90   : 5 MF Time to reach 90% of EC90 (hr)  
Cmax_track : 6 MF Cmax (mg/L)
Tmax_track : 7 MF Tmax (hr)
AUCEOC : 8 AUCEOC




$OMEGA @annotated
ETA1          : 0.0384  : CL
ETA2          : 0.503    : COVF1 F1

$SIGMA @annotated
EPS1          : 0.098   : proportional_PK // Wendy

$MAIN
double CL = THETA1 * pow(FFM/25,0.75) * exp(ETA1)  ;
double V2 = THETA2 * pow(FFM/25,1)  * exp(ETA1*THETA11);
double KAPKDL = THETA3 * PKDL          ;
double KAVL   = THETA4 * VL            ;
double KA     = KAPKDL + KAVL          ;
double Q  = THETA5                     ;
double V3 = THETA6 * pow(FFM/25,1)     ;
double EC90 = 10.6; 
double h0     = THETA9;
double I50    = THETA10;

std::bernoulli_distribution take_dose(ADH);
double ADHERENCE = (EVID == 1) ? (double)take_dose(rng_eng) : 1.0;


if(ID != last_id) {
  cumamt = 0;
  last_id = ID;
}

if(EVID == 1) {
  cumamt += AMT * ADHERENCE;
}

if(ID != last_id_ec90) {
  time_to_ec90 = -1;
  last_id_ec90 = ID;
}

double CDOSE = (cumamt > 0) ? cumamt/WT : 1e-6;

double FLAG1 = (TIME < 168) ? 1 : 0;
double FLAG2 = (TIME >= 168) ? 1 : 0;

// MF bioavalibility 
double TVF    = 1          ;
// w1
double MFPKDL = 1   ; 
double MFVL   = 0.3 * exp(ETA2); 
double MFW1   =  (FLAG1*VL*TVF*MFVL) + (FLAG2*TVF);
// cdose
double MFDOSE = pow(CDOSE,THETA7) ;

F_DEPO  =  TVF * MFW1 * MFDOSE * ADHERENCE;


double K20 = CL/V2  ;
double K23 = Q/V2  ;
double K32 = Q/V3  ;

CENT_0 = 0;
Cmax_track_0= 0; 
Tmax_track_0 = 0; 


$GLOBAL
#include <random>
#define A1 (DEPO) 
#define A2 (CENT) 
#define A3 (PERI) 
#define CONC_CENT (CENT/V2)
static double cumamt = 0;
static double last_id = -1;
static double time_to_ec90 = -1;  
static double last_id_ec90 = -1;  
static std::mt19937 rng_eng(std::random_device{}());


$ODE
dxdt_DEPO = -KA*A1 ; 
dxdt_CENT =  KA*A1  - K20*A2 - K23*A2 + K32*A3  ;
dxdt_PERI =                    K23*A2  - K32*A3 ; 
dxdt_AUC          = CONC_CENT;
dxdt_Cmax_track = 0;
dxdt_Tmax_track = 0;
if(CONC_CENT > Cmax_track){
  dxdt_Cmax_track = CONC_CENT - Cmax_track;
  dxdt_Tmax_track = SOLVERTIME - Tmax_track; // Update Tmax
} 

if(SOLVERTIME >= TSWITCH) {
  dxdt_AUCEOC= CONC_CENT;
} else {
  dxdt_AUCEOC = 0;
}


double RT1 = 0;
if(CONC_CENT > EC90 && SOLVERTIME > 0) RT1 = 1; //https://github.com/metrumresearchgroup/mrgsolve/issues/375

if(time_to_ec90 < 0 && CONC_CENT > EC90 && SOLVERTIME > 0) {
  time_to_ec90 = SOLVERTIME;
}

dxdt_TEC90 = RT1;

$TABLE 
double TOEC90 = TEC90;
double Cmax = Cmax_track;
double Tmax = Tmax_track;
double T_EC90 = time_to_ec90;  

double MIL = TEC90;  

double h = h0 * (1- (MIL/(I50+MIL)));

double AUC_accum = AUCEOC; // The accumulated AUC since TSWITCH
double AUC_inf = 0;  // Initialize AUC to infinity

// Only calculate AUC to infinity if we're past TSWITCH
if(TIME >= TSWITCH) {
  AUC_inf = AUC_accum + (CONC_CENT/K20);
} else {
  AUC_inf = 0; // Before TSWITCH, AUC is zero
}


$CAPTURE
A1 A2 A3 CONC_CENT T_EC90 TOEC90 AUC_inf MFW1 MFDOSE F_DEPO PILLS ADH ADHERENCE Cmax Tmax WT HT AGE FFM SEX FLAG AMT h CDOSE