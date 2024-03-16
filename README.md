# Read keywords form an input file, identify parameter definition, perform addition (plus, +), subtraction (minus,-), multiplication (*), and division (/) calculations in FORTARN language.
+ Author: Fang Shi
+ Website: http://phipsi.top
+ Email: shifang@hyit.edu.cn / shifang@ustc.edu.cn

---

The source codes are parts of the FORTRAN finite element method program PhiPsi: http://phipsi.top/downloads.html.

---

# Keywords rules.

+ Keyword lines are in no particular order.
  
+ Lines starting with "%" are comments.

+ The keyword line starts with "*", followed by a corresponding data line (multiple data are separated by commas ","). For example:
  
    ~~~bash
    *num_Crack
    2
    *InSitu_S1_nv_3D
    1.0,0.0,0.0
    ~~~

+ Keywords are not case sensitive.

+ Support parameter definition. For example:
  
    ~~~bash
    num_of_crack = 2
    *num_Crack
    num_of_crack
    ~~~
  
+ Support basic four arithmetic operations ($+$, $-$, $*$, $()$, and $/$). For example:
  
    ~~~bash
    *num_Crack
    1+1

    *INI_CRACK_PRESSURE_1
    (15.0e6-10.0e6)*(3.0-1.0)

    Time_Hour=1.0
    *INJECTION_T_STAGES_WELLBORES_1_1
    Time_hour*60*60
    ~~~

# Main program and subroutine.

+ PhiPsi_Read_Input.F90 is the main program.
+ Tool_String_arithmetic.f90 performs the addition (plus, +), subtraction (minus,-), multiplication (*), and division (/) calculations.

# Example.
The following input files will be translated. The comment lines will be removed, he parameters will be replaced with values, and the four operations (+, -, *, / calculations) will be performed.
~~~matlab
% Parameters.
ONE = 1
TWO = 2
THR = 3

% Working directory.
*Work_Directory
X:\PhiPsi_work\FEM

% Filename of input files.
*Filename
FEM

% Analysis type (Quasi-static).
*Key_Analysis_Type
1

% Plane stress.
*Key_Type_2D
1

% Linear system solver (LAPACK).
*Key_SLOE
5

% Number of propagation steps.
*Num_Substeps
1

% Material(1-E,2-v,3-density,4-thick,5-St,6-KIc,7-Sc,8-20(blank))
*Material_Para_1
70.0e9-(10.0e9+10.0e9),0.3,2700.0,TWO,1.0e6,1.0e6,100.0e6,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0

*Material_Para_2
8-(1+(2+1)/(1+1))+1,10.0e-1-2.0e-1+20.0e-1+2.0e-1*ONE/THR

*INI_CRACK_PRESSURE_1
(15.0e6-10.0e6)*(THR-1.0)

Time_Hour=1.0
*INJECTION_T_STAGES_WELLBORES_1_1
Time_hour*60*60
~~~

The tranlated file reads:

~~~matlab
 *WORK_DIRECTORY
 X:\PHIPSI_WORK\FEM
 *FILENAME
 FEM
 *KEY_ANALYSIS_TYPE
 1
 *KEY_TYPE_2D
 1
 *KEY_SLOE
 5
 *NUM_SUBSTEPS
 1
 *MATERIAL_PARA_1
 0.500000E11,0.3,2700.0,2,1.0E6,1.0E6,100.0E6,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0
 *MATERIAL_PARA_2
 6.500000,3.728318
 *INI_CRACK_PRESSURE_1
 10000000
 *INJECTION_T_STAGES_WELLBORES_1_1
 3600

~~~
  