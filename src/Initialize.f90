!     .................................................
!             ____  _       _   ____  _____   _        
!            |  _ \| |     |_| |  _ \|  ___| |_|       
!            | |_) | |___   _  | |_) | |___   _        
!            |  _ /|  _  | | | |  _ /|___  | | |       
!            | |   | | | | | | | |    ___| | | |       
!            |_|   |_| |_| |_| |_|   |_____| |_|       
!     .................................................
!     PhiPsi:     a general-purpose computational      
!                 mechanics program written in Fortran.
!     Website:    http://phipsi.top                    
!     Author:     Shi Fang, Huaiyin Institute of       
!                 Technology, HuaiAn, JiangSu, China   
!     Email:      shifang@hyit.edu.cn                  
!     ------------------------------------------------ 
!     Please cite the following papers:                
!     (1)Shi F, Liu J. A fully coupled hydromechanical 
!        XFEM model for the simulation of 3D non-planar
!        fluid-driven fracture propagation. Computers  
!        and Geotechnics, 2021, 132: 103971.           
!     (2)Shi F, Wang X L, Liu C, Liu H, Wu H A. A      
!        coupled extended finite element approach      
!        for modeling hydraulic fracturing in          
!        consideration of proppant. Journal of         
!        Natural Gas Science and Engineering, 2016,    
!        33: 885-897.                                  
!     (3)Shi F, Wang X L, Liu C, Liu H, Wu H A. An     
!        XFEM-based numerical model to calculate       
!        conductivity of propped fracture considering  
!        proppant transport, embedment and crushing.   
!        Journal of Petroleum Science and Engineering, 
!        2018, 167: 615-626.                           
!     (4)Shi F, Wang X L, Liu C, Liu H, Wu H A. An     
!        XFEM-based method with reduction technique    
!        for modeling hydraulic fracture propagation   
!        in formations containing frictional natural   
!        fractures. Engineering Fracture Mechanics,    
!        2017, 173: 64-90.                             
 
SUBROUTINE Initialize
!��ʼ��������ر���.

!------------------         
!��ȡ��������ģ��
!------------------
use Global_Float_Type
use Global_Common
use Global_Filename
use Global_Crack
use Global_Crack_Common
use Global_Crack_3D
use Global_HF
use Global_Model
use Global_Field_Problem
use Global_Inclusion
use Global_Dynamic
use Global_MD
use Global_Cohesive
use Global_POST
use Global_NonLinear
use Global_Field_Problem
!use Global_NonLinear_HYPLAS
use Global_Elem_Area_Vol
use Global_Stress
use Global_Surface_Load
use Global_Material
use Global_3D_HF_Experiment

implicit none

!-------------------
!ִ�в�����ʼ������
!-------------------
print *, "    Initializing parameters...."
!����ȫ�ֱ��������ֵ. 2023-08-13.
Max_Max_N_FluEl_3D = maxval(Max_N_FluEl_3D)  
Max_Max_N_CalP_3D  = maxval(Max_N_CalP_3D) 
Max_Max_N_Node_3D  = maxval(Max_N_Node_3D) 

!���������ʼ��
Crack_Coor(1:Max_Num_Cr,1:Max_Num_Cr_P,1:2)=ZR
!�������������ʼ��
Arc_Crack_Coor(1:Max_Num_Cr,1:Max_Num_Cr_P-1,1:11)  = ZR
!��ʼ��Բ�ο׶�
Hole_Coor(1:Max_Num_Hl,1:3)=ZR
!��ʼ����Բ�׶�
Ellip_Hole_Coor(1:Max_Num_Hl,1:5)=ZR
!��ʼ��Բ�μ���
Circ_Inclu_Coor(1:Max_Num_Circ_Incl,1:3) =ZR
Circ_Inclu_Mat_Num(1:Max_Num_Circ_Incl)  =0
!��ʼ������μ���
Poly_Incl_Coor_x(1:Max_Num_Poly_Incl,1:Max_Num_Edges_Poly)=-1.0D8
Poly_Incl_Coor_y(1:Max_Num_Poly_Incl,1:Max_Num_Edges_Poly)=-1.0D8
Poly_Inclu_Mat_Num(1:Max_Num_Poly_Incl) = 0
Poly_Inclu_Edges_Num(1:Max_Num_Poly_Incl) =0
Poly_Incl_Coor_x_Cl(1:Max_Num_Poly_Incl,1:Max_Num_Edges_Poly)=-1.0D8
Poly_Incl_Coor_y_Cl(1:Max_Num_Poly_Incl,1:Max_Num_Edges_Poly)=-1.0D8
!������ɲ��������μ������
Key_Rand_Poly_Incl_Irregular =0                !�Ƿ�������ɲ��������μ���
num_Rand_Poly_Incl_for_Each_Type(1:10)  =0     !�������ӵ���Ŀ,���ɶ���10���ߴ�
Rand_Poly_Incl_R_Min_and_Max(1:10,1:2)  =ZR    !�������ӵİ뾶��Χ
Rand_Poly_Incl_Irregular_Extension_Factor  = ONE    !�쳤��(1.0-3.0)
Rand_Poly_Incl_Irregular_Inclination  = ZR     !���
Rand_Poly_Incl_Irregular_R_Delta_Factor = ZR   !��������ɹ����еİ뾶�仯ϵ��:(0.0 to 1.0),delta_R = Factor*R
Rand_Poly_Incl_Irregular_Angle_Factor = ZR     !��������ɹ����еĽǶȱ仯ϵ��:(0.0 to 1.0)
!������ɿ׶����
Key_Random_Hole     = 0                      !�Ƿ�������ɿ׶�
!��ʼ��������Ŀ
num_Crack = 0
!��ʼ������������Ŀ
num_Arc_Crack = 0
!��ʼ���׶���Ŀ
num_Hole  = 0
num_Circ_Hole  = 0
num_Ellip_Hole  = 0
!�׶������ѷ����
Key_Hole_Crack_Generate  =0           !�Ƿ�����׶�λ�������ѷ�
Num_Crack_Hole_Generated    = 2       !ÿ���׶��������ɵ��ѷ���Ŀ
num_Hole_Crack_Generated(1:1000) = 0      !����Hole��Ӧ�����ɵ��ѷ���Ŀ
Hole_Crack_Generated_num(1:1000,1:10) =0  !����Hole��Ӧ�����ɵ��ѷ��
!��ʼ��������Ŀ
num_Circ_Incl   = 0         !Բ�μ���,ͨ��Բ�κͰ뾶����
!num_Tri_Incl    = 0         !�����μ���,ͨ���������궨��
!num_Quad_Incl   = 0         !�ı��μ���,ͨ��4�����궨��
!num_Penta_Incl  = 0         !����μ���,ͨ��5��������궨��
num_Poly_Incl =0             !����μ��ӵ���Ŀ
num_Ellip_Incl  = 0         !��Բ�μ��ӵ���Ŀ
num_Inclusion = 0           !�ܵļ�����Ŀ
!��ʼ���ļ���
Filename  = '*blank*'
!��ʼ���ļ�·��
Work_Directory = '*blank*'
!��ʼ��ÿ�����Ƶĺ�ˮ״̬
Cracks_HF_State(1:Max_Num_Cr)  = 0
!��ʼ��ÿ�����ƺ�֧�ż���״̬
Cracks_HF_Propp(1:Max_Num_Cr)  = 0
!��ʼ��עˮ������(ȫHFģ��)
Inj_Point_Loc(1:2) =ZR
Yes_Arc_Crack      = .False. !�Ƿ���������ѷ���ѷ��
!�������
Key_Post_CS_G_Coor = 1        !�Ƿ���㲢����Gauss������(Ĭ��ֵΪ1)
Key_Post_CS_G_Disp = 1        !�Ƿ���㲢����Gauss��λ��(Ĭ��ֵΪ1)
Key_Post_CS_N_Strs = 0       !�Ƿ���㲢����Node��Ӧ��(Ĭ��ֵΪ0)
Key_Post_CS_G_Strs = 1        !�Ƿ���㲢����Gauss��Ӧ��(Ĭ��ֵΪ1)
Key_Post_S_Dof_F   = 0        !�Ƿ񱣴�����ɶȵ��غ�ֵ,�Ա����ں���(Ĭ��ֵΪ0)
Key_Post_Cracked_Ele=0        !�Ƿ���㲢���浥ԪӦ��״̬�ļ�(�Ƿ��1-��3>Tol,�ֶ�ѹ��Ĭ�ϴ�)
Key_Post_S_TanDisp = 0        !�Ƿ���㲢�����ѷ���������λ��(Ĭ����0)
Key_Node_Value     = 1        !�ڵ�Ӧ�������㷨(=1,ֱ��ƽ����(Ĭ��); =2,��С����ƥ�䷨)
Key_Save_vtk       = 1        !�Ƿ񱣴�vtk�ļ�(Ĭ��Ϊ1,����)
Key_Simple_Post    = 0        !������������������ݣ���ڵ�λ�ơ��ѷ쿪�ȵ�. 2022-06-25.
Key_Save_Nothing   = 0        !�������κ�����. 2022-09-06. NEWFTU2022090601.
Key_Post_Elements_Gauss_Num= 0!�Ƿ񱣴�ÿ����Ԫ�ĸ�˹���ֵ���Ŀ. �����ں���.2022-07-16.
Key_Get_Permeability       = 0!���㲢�����ѷ���͸��. 2022-11-26.
Key_Post_CS_N_Stra  = 0       !�Ƿ���㲢����Node��Ӧ��(Ĭ��ֵΪ0)
Key_Save_avol_file         = 0       !�����ѷ�������ļ�. 2022-12-18. Ĭ��Ϊ0.
Key_Post_CS_N_Stra_Cylindr= 0!�Ƿ���㲢����Node��Ӧ��(Ĭ��ֵΪ0)������ϵ
Key_Post_CS_N_Strs_Cylindr= 0!�Ƿ���㲢����Node��Ӧ��(Ĭ��ֵΪ0)������ϵ
Key_Post_CS_N_Disp_Cylindr =0!�Ƿ���㲢����Node��λ��(Ĭ��ֵΪ0)������ϵ
Post_cylinder_Coor_Center(1:3)    = -Con_Big_20       !Բ����������ϵԭ��
Post_cylinder_Coor_Vector_x(1:3)  = -Con_Big_20     !Բ����������ϵx������
Post_cylinder_Coor_Vector_y(1:3)  = -Con_Big_20     !Բ����������ϵy������
Post_cylinder_Coor_Vector_z(1:3)  = -Con_Big_20     !Բ����������ϵz������

!��ʼ��עˮ��֧�ż���ز���
Inject_Q_Time(1:200) = ZR
Inject_Q_Val(1:200)  = ZR
Inject_c_Time(1:200) = ZR
Inject_c_Val(1:200)  = ZR
Inject_P_Time(1:200) = ZR
Inject_P_Val(1:200)  = ZR
!ˮ��ѹ�ѷ������Ʋ���Ĭ������
MNR_Tol              = 0.05     !Tolerance of MNR iteration(�Ƽ�ֵ:5%,p+w����������ͬʱ����).
Key_HF_Conv_Crite    = 2        !��������׼��, 1: ͨ���ѷ쿪���ж�
                           !              2: ͨ���ѷ쿪�Ⱥ�ˮѹ
Key_HF_LineSearch    = 1        !Ĭ�ϴ���������

!���֧�ż�Ũ��ֵ
Max_c=ZR
!C     !��ʼ��������������
!C     Cracks_CalP_Pres(1:Max_Num_Cr,1:Max_Num_Cr_CalP)     = ZR
!C	   Cracks_CalP_Tractions(1:Max_Num_Cr,1:Max_Num_Cr_CalP,1:2)= ZR     !ת����ֱ������ϵ�µ�ճ��ǣ����
!C     Cracks_CalP_Aper(1:Max_Num_Cr,1:Max_Num_Cr_CalP)     = ZR
!C     Cracks_CalP_Coors(1:Max_Num_Cr,1:Max_Num_Cr_CalP,1:2)= ZR
!C     Cracks_CalP_Orient(1:Max_Num_Cr,1:Max_Num_Cr_CalP)   = ZR
!C     Cracks_CalP_Seg(1:Max_Num_Cr,1:Max_Num_Cr_CalP)      = 0
!C     Cracks_CalP_Elem(1:Max_Num_Cr,1:Max_Num_Cr_CalP)     = 0
!C     Cracks_CalP_Pgra(1:Max_Num_Cr,1:Max_Num_Cr_CalP)     = ZR
!C     Cracks_CalP_Velo(1:Max_Num_Cr,1:Max_Num_Cr_CalP)     = ZR
!C     Cracks_CalP_Quan(1:Max_Num_Cr,1:Max_Num_Cr_CalP)     = ZR
!C     Cracks_CalP_Conc(1:Max_Num_Cr,1:Max_Num_Cr_CalP)     = ZR
!C     Cracks_CalP_Remo_Strs(1:Max_Num_Cr,1:Max_Num_Cr_CalP)= ZR
!C     Cracks_CalP_wdeform(1:Max_Num_Cr,1:Max_Num_Cr_CalP)  = ZR
!C     MS_CalP_Propped_Aper(1:Max_Num_Cr,1:Max_Num_Cr_CalP) = ZR  !�ֶ�ѹ����صļ��������

!֧�ż��Ƿ���������
Propp_Trans_Start = .False.
!�����Ѳ�������Ӧ���ܵĵ�������
Counter_Num_iFrac(1:Max_Num_Frac) =0

!ȫ���ѷ��ʼ��Ϊ:��������չ: 2022-08-21. BUGFIX2022082101.
allocate(Cracks_Allow_Propa(max(Max_Num_Cr,Max_Num_Cr_3D)))
Cracks_Allow_Propa(1:size(Cracks_Allow_Propa,1)) = 1
!print *,Cracks_Allow_Propa

!Cracks_Tips_Allow_Propa(1:Max_Num_Cr,1:2)=0
!��̬ճ����������ճ�ȷŴ�ϵ��,����HF֧�ż����з���
Visco_Zoom_Factor=20.0D0
!----ʱ��ճ�Ȳ���----
Viscosity_td_m    = 1.86D-2 !ʱ��ճ�Ȳ���m,Ref:�����Ҳ�����ע������(default:1.86D-2)
Viscosity_td_n    = 2.5     !ʱ��ճ�Ȳ���n,Ref:�����Ҳ�����ע������(default:2.5)
!��ʼ���ֶ�ѹ���������
MS_Crack_Num  =0                                      !�ֶ��ѷ����(�ѷ���,���֧��20��ѹ��)
MS_Crack_Order(1:Max_MS_Num_Crack)         = 0
!MS_Crack_Coor(1:Max_MS_Num_Crack,1:200,1:2)= ZR   !�����ѷ�����
MS_InP_Loc(1:Max_MS_Num_Crack,1:2)         = ZR    !����עˮ������
!C     !��ʼ����Ӧ��������(����HF��6�ź�8�ŵ�����)���Ʋ���
!C     Key_InSitu_Method = 0
!��ʼ������Key_HF_MS_Contc_Check:�ֶ�ѹ���Ƿ���к�֧�ż��ѷ���ĽӴ��������ƹؼ���
i_MS=0
Key_HF_MS_Contc_Check = -999
!����ѹ�����ʱ��Ӧ���ܵĵ�����
MS_Finish_Counter_Iter(1:Max_MS_Num_Crack)= 0
!��Ӧ�������㷨���.
Key_InSitu_Strategy = 0     !HF������Ӧ���Ĵ�����(������HF������5�ŵ�����)
                          ! 0:�����ǳ�ʼӦ������
                          ! 1:ͨ�����Ե��ӵķ��������Ӧ��������λ�Ƴ�,��HF����������ɾ��
                          !   ��Ӧ��,�����ѷ���չ����ʱ���ǵ�Ӧ��,�������Ѽ�ˮѹ����Ϊ0
                          ! 2:ͨ��Zienkiewicz_7ed_P216(Ĭ��)
                          ! 3:��ʩ�ӵĵ�Ӧ������(�������ˮ��ѹ�ѷ���)
                          ! 4: �̶�Լ��,ָ����ʼӦ��������ʼӦ���ļ�.����:Xsite���ۼ���֤�����еķ���.
Key_Read_Initial_Node_Stress_File = 0    !�ӳ�ʼӦ���ļ�(*.istn,�ڵ�Ӧ���ļ�)�����ʼӦ��������.
Key_InStress_for_Mat   = 0          !���ض�����ʩ��ԤӦ��
Mat_Number_of_InStress(1:100) =0    !ʩ��ԤӦ���Ĳ��Ϻ�
Mat_InStress_x         = ZR         !x����ԤӦ��
Mat_InStress_y         = ZR         !y����ԤӦ��
Mat_InStress_z         = ZR         !z����ԤӦ��

Label_number = 0
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!ˮ��ѹ��HF��NF������ر����ĳ�ʼ��
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!HF��NF�ཻ���ɵ��¼����ѷ��Ӧ����Ȼ�ѷ��
Cracks_NF_JiS_Cr_Num(1:Max_Num_Cr)  = 0
!HF��NF�ཻ����״̬
Cracks_NF_JiS_Stat(1:Max_Num_Cr,1:2) = 0
!HF��NF�����������,ȷ���������ѷ��T�ͽ���״̬(���Ѽ��޹�)
Cracks_NF_T_Stat(1:Max_Num_Cr,1:2)   = 0
Cracks_NF_Cement_Tip_Num(1:Max_Num_Cr,1:2) =0
!����Ħ������Ȼ�ѷ�,������ѷ��Ӧ��Ħ����Ȼ�ѷ��
Cracks_fric_NF_num(1:Max_Num_Cr)  =0
!����Ħ������Ȼ�ѷ�,���ڱ��ÿһ���ѷ��Ƿ�ˮѹ�ѷ���ʴ��
Cracks_QinS_Stat(1:Max_Num_Cr)  =0
!��Ӧ��
InSitu_x =ZR
InSitu_y =ZR
!��ԪGauss�������
!num_GP_Elem(1:Num_Elem) = 0
!Ele_GP_Start_Num(1:Num_Elem) = 0
!���������
Key_Fracture_Zone = 0          !������ε���������(�������ڸ÷�Χ����չ),��������,����ȫģ������չ
Frac_Zone_MinX    = ZR         !��������x���귶Χ
Frac_Zone_MaxX    = ZR         !��������x���귶Χ
Frac_Zone_MinY    = ZR         !��������y���귶Χ
Frac_Zone_MaxY    = ZR         !��������y���귶Χ
Frac_Zone_MinZ    = ZR         !��������y���귶Χ
Frac_Zone_MaxZ    = ZR         !��������y���귶Χ
!���������Ȼ�ѷ����
Key_Random_NaCr   = 0          !�Ƿ����������Ȼ�ѷ�
num_Rand_Na_Crack = 0          !������ɵ���Ȼ�ѷ���Ŀ
NaCr_Orientation  = ZR         !��Ȼ�ѷ�ƽ����λ(��)
NaCr_Ori_Delta    = ZR         !��Ȼ�ѷ췽λ�Ĳ�������(��)
NaCr_Length       = ZR         !��Ȼ�ѷ��ƽ������
NaCr_Len_Delta    = ZR         !��Ȼ�ѷ��ƽ�����ȵĲ�������
!ϡ�����K���ϡ���
Sparse_Ratio      = 0.03       !Ĭ��ֵΪ0.03
!ϡ����м����(Location_COO,MASK_COO)���ݴ洢��ʽ,1:ȫӲ��(Ĭ��);2:ȫ�ڴ�2:����Ӳ�̶������ļ�
Sparse_Store_Method  = 1
!��ʼ��ÿ���ѷ���������ĿΪ0
Each_Cr_Poi_Num(1:Max_Num_Cr) = 0

!�ѷ���Ӵ�
Key_Contact =0
Contact_Aper_Tol  = ZR   !�Ӵ�����ݲ�
fric_mu_Cont      = ZR
!������Ӵ�(��֧�ż�����)������������ >=50
Max_Contact_Iter  = 50
!�Ӵ����ֵ���Ŀ(1 or 2,Ĭ��Ϊ1)
Conta_Integ_Point =1
!�Ӵ���������׼��(1:ͨ���в�ȷ��;2:ͨ��λ��ȷ��;3:ͨ���ѷ쿪��)
Key_Conta_ConCrit = 2
!�������Ӵ��㷨���
kn_Cont_Penalty   = 1.0D13       !�Ӵ�����������������Ӵ��ն�
kt_Cont_Penalty   = 1.0D13       !�Ӵ�����������������Ӵ��ն�
Conve_Tol_Penalty = 1.0D-3       !�������������ݲ�(0.1%)
Key_HF_Cont_Scheme= 0   !ˮ��ѹ�ѷ����Ӵ���ⷽ��
Key_CS_Natural_Crack =0 !����ѹ����Ȼ�ѷ췣��������. 2022-10-22.
Penalty_CS_Natural_Crack  = 1.0D11          !ѹ�����ѷ�ķ�����. Ĭ��1.0D11. 2022-12-23.
Key_Penalty_CS_Method  = 1    !=1(default)�򷣺�������l��m��nλ�ƣ�=2�������������n����λ�ƣ������߷���λ��. NEWFTU2023082701.

!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!��������س�ʼ��(������ʼ��϶ѹ����)
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!Fd_kxx            = -999.0D0      !ϵ��kxx
!Fd_kxy            = -999.0D0      !ϵ��kxy
!Fd_kyy            = -999.0D0      !ϵ��kyy
!��ʼ��϶ѹ��
!Key_PoreP         = 1             !�Ƿ��г�ʼ��϶ˮѹ��
Key_PoreP         = 0             !�Ƿ��г�ʼ��϶ˮѹ��. 2023-05-07.
Initial_PoreP     = -999.0D0      !��ʼ��϶ѹ�Ĵ�С
Initial_Biot_Coeff= 0.75D0        !��ʼ�Ȱ�ϵ��. 2023-03-19.

!���ϲ�����ʼ��
ALLOCATE(Material_Type(Max_Materials))
Material_Type(1:Max_Materials) = 1   !Ĭ�Ͼ����õ�һ�ֲ���
ALLOCATE(Material_Para(Max_Materials,30))
Material_Para(1:Max_Materials,1:30) = ZR
ALLOCATE(Material_Para_Added(Max_Materials,30))
Material_Para_Added(1:Max_Materials,1:30) = ZR
ALLOCATE(Mat_Cartesian_Coor_Vector_x(Max_Materials,3))  !���ϲ���������������ϵx������
ALLOCATE(Mat_Cartesian_Coor_Vector_y(Max_Materials,3))  !���ϲ���������������ϵy������
ALLOCATE(Mat_cylinder_Coor_Center(Max_Materials,3))     !���ϲ���Բ����������ϵԭ��
ALLOCATE(Mat_cylinder_Coor_Vector_z(Max_Materials,3))   !���ϲ���Բ����������ϵz������
Mat_Cartesian_Coor_Vector_x(1:Max_Materials,1:3) = ZR !���ϲ���������������ϵx������
Mat_Cartesian_Coor_Vector_y(1:Max_Materials,1:3) = ZR !���ϲ���������������ϵy������
Mat_cylinder_Coor_Center(1:Max_Materials,1:3) =ZR    !���ϲ���Բ����������ϵԭ��
Mat_cylinder_Coor_Vector_z(1:Max_Materials,1:3)  =ZR !���ϲ���Բ����������ϵz������
GasP_Well_Nodes(1:100) = -999   !�����������ڽڵ��



!Gauss����Ŀ��ʼ��
Num_Gauss_Points  = 64      !��ǿ��Ԫ�ĸ�˹���ֵ���,Ĭ��64,����ȡ����,��9x9=81
                          !֧�ֵ�Gauss����Ŀ:16,36,64,100,144(12*12),196(14*14),400(20*20),676(26*26),900(30*30)
Num_Gauss_P_Inc   = 400     !����Inclusion��ǿ��Ԫ��Щ����:�����Ͻ���ĵ�Ԫ����ȡ����400����˹��
Num_Gau_Points_3D = 512     !3D��ǿ��Ԫ�ĸ�˹���ֵ���,Ĭ��512=8*8*8
                          !֧�ֵ�Gauss����Ŀ:16,36,64,100,144(12*12),196(14*14),400(20*20),676(26*26),900(30*30)
Num_Gau_Points_3D_MC= 1000  !��3�������ѷ��3D��ǿ��Ԫ�ĸ�˹���ֵ���: 1000=10^3(default);3375=15^3;5832=18^3;8000=20^3
Num_Gau_P_SubInteg_6= 64     !3D�ֿ����6���嵥Ԫ���ֵ���Ŀ. Ĭ��Ϊ4*4*4. 2022-07-28.
Num_Gau_P_SubInteg_4= 4     !3D�ֿ����4���嵥Ԫ���ֵ���Ŀ. Ĭ��Ϊ4. 2022-07-28.
Num_Gauss_P_FEM   = 4       !��ͳ��Ԫ�ĸ�˹���ֵ���,Ĭ��4
Num_Gauss_P_FEM_3D= 8       !��ͳ3D��Ԫ�ĸ�˹���ֵ���,Ĭ��8=2*2*2
!��̬�������
Num_Ivex =0
Num_Ivey =0
Num_Iacx =0
Num_Iacy =0
!���������ϵ��������ٶ�
!g_X_Y_Z(1:3) = ZR     !���������ϵ��������ٶ�
g_X_Y_Z           = [0.0D0,9.8D0,0.0D0]   !���������ϵ��������ٶ�
!�������
Key_EQ   = 0                     !�Ƿ��ǵ������,����,����Ҫ���������ٶ�ֵ
EQ_Ac_Time_Gap  =-999.0D0        !������ٶ�����ʱ����
num_EQ_Ac_nodes = 0              !������ٶ�ʩ�ӵ��Ľڵ���Ŀ
EQ_Ac_nodes(1:5000) = 0          !������ٶ�ʩ�ӵ��Ľڵ��б�(���5000���ڵ���)
!���Ҽ��ٶȼ������
Key_Sin_Accel       = 0          !�Ƿ񼤻����Ҽ��ٶȼ���
Sin_Accel_Dire      = 1          !���Ҽ��ٶȼ����ķ���:=1,x;=2,y
Sin_Accel_A         = -999.0D0   !���Ҽ��ٶȼ��������
Sin_Accel_T         = -999.0D0   !���Ҽ��ٶȼ���������
Sin_Accel_num_Nodes = 0          !���Ҽ��ٶȼ����Ľڵ���Ŀ
Sin_Accel_Nodes(1:5000)= 0       !���Ҽ��ٶȼ����Ľڵ��б�
!�ڵ�������
num_CP_x_nodes = 0
num_CP_y_nodes = 0
CP_x_nodes(1:5000)  = 0          !Ҫ��ϵ�x�������ɶȽڵ��б�
CP_y_nodes(1:5000)  = 0          !Ҫ��ϵ�y�������ɶȽڵ��б�
num_nodes_CP_set_x(1:10)=0       !x����������ɶȵļ�����Ŀ
num_nodes_CP_set_y(1:10)=0       !y����������ɶȵļ�����Ŀ
CP_nodes_x(1:10,1:5000)=0        !x����������ɶȵĸ����Ͻڵ��б�
CP_nodes_y(1:10,1:5000)=0        !y����������ɶȵĸ����Ͻڵ��б�
!3D�ѷ����
!allocate(Crack3D_Coor(Max_Num_Cr_3D,10,3))
!Crack3D_Coor(1:Max_Num_Cr_3D,1:4,1:3) = ZR                     !�ѷ�ţ��ѷ���4�������ţ�(x,y,z)
allocate(Crack3D_Coor(Max_Num_Cr_3D,200,3)) !BUGFIX2024022601.
Crack3D_Coor(1:Max_Num_Cr_3D,1:200,1:3) = ZR                     !�ѷ�ţ��ѷ���4�������ţ�(x,y,z)

allocate(Crack3D_Cir_Coor(Max_Num_Cr_3D,7))
Crack3D_Cir_Coor(1:Max_Num_Cr_3D,1:7) = ZR                     !Բ�γ�ʼ�ѷ�,�ѷ��,Բ������(x,y,z)+�ⷨ������+�뾶
allocate(Crack3D_Ellip_Coor(Max_Num_Cr_3D,8))
Crack3D_Ellip_Coor(1:Max_Num_Cr_3D,1:8) = ZR                   !��Բ��ʼ�ѷ�,�ѷ��,Բ������(x,y,z)+�ⷨ������+�뾶a+�뾶b
!Crack3D_Meshed_Node(1:Max_Num_Cr_3D,1:Max_N_Node_3D,1:3)=ZR             !��ɢ��֮���3D�ѷ�ڵ�����,ÿ���ѷ������1000�������
!Crack3D_Meshed_Ele(1:Max_Num_Cr_3D,1:Max_N_Node_3D,1:3) =0              !��ɢ��֮���3D�ѷ쵥Ԫ���,ÿ���ѷ������1000�������
!Crack3D_Meshed_Ele_Attri(1:Max_Num_Cr_3D,1:Max_N_Node_3D,1:5) = ZR      !��ɢ��֮���3D�ѷ쵥Ԫ���Բ���(�ܳ��������)
!Cr3D_Meshed_Node_in_Ele_Num(1:Max_Num_Cr_3D,1:Max_N_Node_3D)=0          !��ɢ��֮���3D�ѷ�ڵ����ڵ�Ԫ��
!Cr3D_Meshed_Node_in_Ele_Local(1:Max_Num_Cr_3D,1:Max_N_Node_3D,1:3) =ZR         !��ɢ��֮���3D�ѷ�ڵ����ڵ�Ԫ�ŵľֲ�����
allocate(Crack3D_Meshed_Node_num(Max_Num_Cr_3D))
Crack3D_Meshed_Node_num(1:Max_Num_Cr_3D)                       =0       !��ɢ��֮���3D�ѷ�ڵ���Ŀ
!Crack3D_Meshed_Ele(1:Max_Num_Cr_3D,1:Max_N_Node_3D,1:3)        =0       !��ɢ��֮���3D�ѷ쵥Ԫ���,ÿ���ѷ������1000�������
allocate(Crack3D_Meshed_Ele_num(Max_Num_Cr_3D))
Crack3D_Meshed_Ele_num(1:Max_Num_Cr_3D)                        =0       !��ɢ��֮���3D�ѷ쵥Ԫ��Ŀ
!Crack3D_Meshed_Outline(1:Max_Num_Cr_3D,1:Max_N_Node_3D,1:2)    =0       !��ɢ��֮���3D�ѷ���߽�
!Crack3D_Meshed_Outline(1:Max_Num_Cr_3D,1:Max_N_Node_3D,4)      =1       !����4���ڱ�Ǹñ߽��ߵ��������Ƿ�������չ,��չ�ǳ�С�Ĳ���(2021-08-20)
!Crack3D_Meshed_Outline_num(1:Max_Num_Cr_3D)                    =0       !��ɢ��֮���3D�ѷ���߽�������
!C     Crack3D_Meshed_Outline_Grow_From(1:Max_Num_Cr_3D,
!C    &                                 1:Max_N_Node_3D)              =0       !���ڱ��3D�ѷ���߽綥����ĸ�����չ����. NEWFTU2022071301.
!C     Crack3D_Meshed_Outline_Vertex(1:Max_Num_Cr_3D,
!C    &                                          1:Max_N_Node_3D,1:3)=ZR        !��ɢ��֮���3D�ѷ���߽綥������
!C     Crack3D_Meshed_Outline_Vertex_Ele_num(1:Max_Num_Cr_3D,
!C    &                                          1:Max_N_Node_3D) =0       !��ɢ��֮���3D�ѷ���߽綥�����ڹ��嵥Ԫ��
!
Crack_Pressure(1:Max_Num_Cr) = ZR                             !����ѹ��(���100���ѷ�)
!------
Crack_Pressure_Type = 1                                           !����ѹ������:=1,�̶�ѹ��;=2,ѹ���Զ�����,ʹ���ѷ�������չ
!Cracks_FluidEle_CalP_3D(1:Max_Num_Cr_3D,1:Max_N_CalP_3D,1:7) =0  !ÿ���ѷ����嵥Ԫ�������
!Cracks_FluidEle_num_CalP_3D(1:Max_Num_Cr_3D,1:Max_N_CalP_3D)=0    !ÿ���ѷ����嵥Ԫ�ļ������Ŀ
!Cracks_FluidEle_num_3D(1:Max_Num_Cr_3D) =0                        !ÿ���ѷ����嵥Ԫ����Ŀ
!Cracks_CalP_Coors_3D(1:Max_Num_Cr_3D,1:Max_N_CalP_3D,1:3)  = ZR
!Cracks_CalP_Orient_3D(1:Max_Num_Cr_3D,1:Max_N_CalP_3D,1:3) = ZR
!Cracks_CalP_MeshedEl_3D(1:Max_Num_Cr_3D,1:Max_N_CalP_3D)   = 0
!Cracks_CalP_Elem_3D(1:Max_Num_Cr_3D,1:Max_N_CalP_3D,1:2)   = 0
allocate(Cracks_CalP_Num_3D(Max_Num_Cr_3D))
Cracks_CalP_Num_3D(1:Max_Num_Cr_3D)                        = 0
!ÿ�����Ѳ�ִ�еĽӴ�������Ŀ(Ĭ�Ͻ��ڵ�һ��������ϵ�����ִ�нӴ�����,��=1)
Key_HF_num_Contact= 1
!��̬�������
IDy_Num_Iteras    = 0               !��ʽ��̬�����ܲ���
IDy_Num_force_Itr = 0
Factor_Prop_Dy    = 1.63D0
Key_Mass_Lumped   = 0               !�Ƿ���ü�����������(�Խ���������,Ĭ�Ϲر�)
!���Ӷ���ѧ�������,�������ͺ�:21
MD_num_molecule   = -999            !���ӵ���Ŀ
MD_mss_molecule   = -999.0D0        !���ӵ�����
MD_num_time_step  = -999            !ʱ�䲽��
MD_Delt_Time      = -999.0D0        !ʱ�䲽��
MD_Dimension_x    = -999.0D0        !�˶�����x����ߴ�
MD_Dimension_y    = -999.0D0        !�˶�����y����ߴ�
MD_Dimension_z    = -999.0D0        !�˶�����z����ߴ�
MD_step_print_num = 10              !��Ļ��ʾ������㲽��
MD_step_save_num  = 10              !���ݱ��������㲽��
!���Է������
Key_Plasticity    = 0
!ճ���ѷ����
Coh_Constitutive_type   = -999       !Constitutive type of the cohesive crack.
Coh_Width_Critical1     = -999.0D0   !�������쿪��1,�˿��ȶ�Ӧ����ǣ����,Coh_Constitutive_type=1ʱ����
Coh_Width_Critical2     = -999.0D0   !�������쿪��2,�˿�����ǣ����Ϊ0
Coh_f_Ultimate          = -999.0D0   !���ǣ����,�Ѽ��ǣ����
Coh_Tangential_Key      = -999       !�Ƿ�������ǣ����,������,Ӧ������Ӧ�Ĳ���
Coh_Width_Critical1_T   = -999.0D0   !�������쿪��1,�˿��ȶ�Ӧ��������ǣ����,Coh_Constitutive_type=1ʱ����
Coh_Width_Critical2_T   = -999.0D0   !�������쿪��2,�˿���������ǣ����Ϊ0
Coh_f_Ultimate_T        = -999.0D0   !���ǣ����,�Ѽ������ǣ����
Key_Save_f_d_Curve      = -999
f_d_Curve_node_num      = -999
Key_Save_f_COD_Curve    = 0          !���������ѷ���غ�-�ѷ��ſ�λ��(COD)����,���浽fccu�ļ���.
f_COD_Curve_Crack_Num   = 1          !�غ�-�ѷ��ſ�λ��(COD)�����ѷ��.
Cracks_Coh_Ele_Type(1:Max_Num_Cr,1:Max_Num_Cr_CalP-1) =0   !ÿ���ѷ�ճ�۵�Ԫ������,=1��ʾΪճ�۵�Ԫ��=0Ϊһ�㵥Ԫ
Cracks_Tip_Num_of_Coh_Ele(1:Max_Num_Cr,1:2)  =0  !ÿ���ѷ������Ѽ��Ӧ��ճ�۵�Ԫ��Ŀ(��Ϊ��Ŀ,�����Ǳ��)
Max_Cohesive_Iter       = 50         !ճ���ѷ������������ >=50
Coh_Integ_Point         = 2          !ճ���ѷ���ֵ���Ŀ(1��2)
Key_Coh_ConCrit         = 1          !ճ���ѷ��������׼��(1:ͨ���в�ȷ��;2:ͨ��λ��ȷ��)
Key_Save_f_d_Curve      = 0
!����
Key_Close_Window        = 0          !����������Ƿ��Զ��رմ���
Key_Play_Sounds         = 0          !�Ƿ񲥷���ʾ��
Key_Memo_Moni           = 0          !�Ƿ����ڴ�������
Key_Window_Log          = 0          !���洰�ڼ�¼
Key_Clear_All           = 1          !���㿪ʼʱɾ������

!Key_OpenMP              = 0          !Close openmp
Key_Num_Process         = 1          !Ĭ�ϵ��߳�
Key_Data_Format         = 1          !Save data with format
Key_XA                  = 0          !�°����. 2022-09-10.
Key_Integral_Sol        = 2          !Ĭ�ϻ����㷨
Num_Sub_Quads           = 16         !���ı����ʷ��ı�����Ŀ(Key_Integral_Sol= 3ʱ����),Ĭ��16,����ȡ����
Num_Sub_3D_Cubes        = 125        !3D��ǿ��Ԫ��������ֿ���(Key_Integral_Sol= 3ʱ����),Ĭ��125=5^3
                                   !(8=2^3,27=3^3,64=4^3,125=5^3,216=6^3,343=7^3,512=8^3,729=9^3,1000=10^3)
Num_Gau_Points_3D_Cube  = 8          !3D���������ʷַֿ��˹���ֵ���Ŀ��Ĭ��Ϊ8��
Seed                    = 123456789  !�������������������
!�����ڲ��ؼ����Ʋ���(���ɸĶ�)
Key_Heaviside_Value    = -1          !Value keyword of Heaviside enrichmenet function:-1 (1 and -1) or 0 (1 and 0)
Key_Hole_Value         =  0          !Value keyword of Hole enrichmenet function:-1 (1 and -1) or 0 (1 and 0)
Key_Visit_PhiPsi_top   =  0
Key_Cond_Number        =  0          !���㲢��ʾ������,������Lapack�����,Ĭ�Ϲر�
Key_Determinant        =  0          !���㲢��ʾ�նȾ���K������ʽ(��֧��ϡ��������������֧��EBE-PCG�����),Ĭ�Ϲر�
Key_Eigenvalue         =  0          !���㲢��ʾ�նȾ���K������ֵ(��֧��ϡ��������������֧��EBE-PCG�����),Ĭ�Ϲر�
Key_BLAS               =  0          !ʹ��BLAS��(����Intel Fortran���Զ�����),���Խ����Ӱ��. Ĭ��Ϊ0. ʹ��BLAS������ӳ���:
                                   !EBE_XFEM_PCG_3D_with_K.f90
!���ϲ������
Material_Interface(1:2) = ZR              !���ӵĽ�����ϲ���:1-St,2-KIc
!�����Է������(2018-01-19)
NL_TIMS(1,1:5) = [ZR,ONE,ZP1,ZR,ONE]      !ÿ�д���һ���غɲ�����,����(��ʼʱ��,����ʱ��,ʱ������,��ʼ�غ�����,��ֹ�غ�����)
NL_TIMS(2:1000,1:5) = ZR
NL_ITRA        = 30                       !N-R��������Ŀ
NL_ATOL        = 1.0D8                    !��������в��norm2ģ
NL_NTOL        = 6                        !���������غɶ�����Ŀ
NL_TOL         = 1.0D-6                   !N-R���������ݲ�(1.0D-6)
!NL_NLOAD       = 1                       !�غɲ���
!HYPLAS�����Է������(2021-07-19)
!NL_Time_Steps(1,1:5) = [1.0D0,1.0D0,1.0D-6,20.0D0,0.0D0]  !ÿ�д���:(1)ʱ��������(2)�غ�����������(3)�����ݲ(4)������������(5)Blank
!NL_Time_Steps(2:1000,1:5) = ZR

!���������
Key_Fd_Body_Source = 0
!���˲��ϱ��
Key_Damage =0
Material_Dam2Frac_Value   = TWO           !�����γ��ѷ�ļ�������ֵ(���˳�����ֵ�������ѷ�)
Crack_Gen_from_Damage =0                  !�������ɵ��ѷ���Ŀ

!����λ�Ʊ߽��������
Num_Boux_nonzero = 0
Num_Bouy_nonzero = 0
Num_Bouz_nonzero = 0
penalty_k_bou_nonzero = 1.0D15            !����λ�Ʊ߽��������������ķ�����

!������Ԫ
Key_EKILL  = 0
Ele_Killed_Each_Load_Step(1:Max_Step,1:1000)  =0  !ÿ���غɲ�ɱ���ĵ�Ԫ���,���֧��1000����Ԫ
EKILL_Weaken_Factor = 1.0D-6              !������Ԫ����ϵ��

!ϡ�����洢K
Key_K_Sparse =0

!��Ӧ��
Key_Thermal_Stress =0
Key_Scheme_Thermal_Stress = 1    !�����趨�¶�Ӧ�����㷽��. = 1�����ݾ����¶ȼ�����Ӧ��; = 2�������²������Ӧ��. 2023-03-13.
Key_Initial_Temperature   = 1    !��ʼ�¶ȸ�������. =1, ���ݲ��Ϻ�. =2, ���ļ����루��ʱ�����ã�. 2023-03-13.
Thermal_Str_Temper(1:100) = ZR     !�����¶�

!����һ�����󣬴洢12����ߵĽڵ���Ele_3D_Edges_Node(12,2),2022-04-14
!�ֲ����
Ele_3D_Edges_Node(1,1:2)  = [1,2]
Ele_3D_Edges_Node(2,1:2)  = [2,3]
Ele_3D_Edges_Node(3,1:2)  = [3,4]
Ele_3D_Edges_Node(4,1:2)  = [4,1]
Ele_3D_Edges_Node(5,1:2)  = [1,5]
Ele_3D_Edges_Node(6,1:2)  = [2,6]
Ele_3D_Edges_Node(7,1:2)  = [3,7]
Ele_3D_Edges_Node(8,1:2)  = [4,8]
Ele_3D_Edges_Node(9,1:2)  = [5,6]
Ele_3D_Edges_Node(10,1:2) = [6,7]
Ele_3D_Edges_Node(11,1:2) = [7,8]
Ele_3D_Edges_Node(12,1:2) = [8,5]

!����
file_Sparse_K_Location_COO_bin = .False.  !���ڱ��Sparse_K_Location_COO.bin�������ļ��Ƿ����
file_Sparse_K_Mask_COO_bin     = .False.  !���ڱ��Sparse_K_Mask_COO.bin�������ļ��Ƿ����
Key_Schollmann_Twist = 0                  !Schollmann��s criterion׼���Ƿ����ѷ����Ť��
Key_Ave_Stress    = 1                     !��Ȩƽ��Ӧ���ļ��㷽��: =1, SHI Fang's Formation; =2, Ordinary Formation; =3, single point
S1_Point_Factor   = 0.2D0                 !��ȨӦ��������ƶ�ϵ��(ƫ���ѷ����������ƶ�; defaut to 0.2)
num_Gauss_S1      = 216                   !��Ȩƽ������Ӧ�����㵥Ԫ��˹����Ŀ(default to 6^3=216)
Key_Adj_Prp_Step_3D = 0                   !3D�ѷ���չ���Ʋ���(default:0); =1�����Ӧ����Ӧ��ǿ�����ӿ����ѷ���չ������ʵ�ʴ�С
Prp_3D_Factor_m     = 0.3333D0            !delta_l  = delta_L_Max*(S1/St)^m��ʽ�е�ϵ��m(default:1/3). �ò�����Ӱ���\changelog.src\2022-05-09-01.webp
!Adj_Prp_Step_3D_Max = 3.0D0               !3D�ѷ���չ���Ʋ���(default:3.0),Key_Adj_Prp_Step_3D = 1ʱ������,3D�ѷ���չ��󲽳�ϵ��.
Adj_Prp_Step_3D_Max = 1.5D0               !3D�ѷ���չ���Ʋ���(default:1.5),Key_Adj_Prp_Step_3D = 1ʱ������,3D�ѷ���չ��󲽳�ϵ��.IMPROV2024022701.
!Prp_Bisec_Factor_3D = 1.5D0              !3D�ѷ���չ���Ʋ���:�߽��ʷ�ϵ��,��3D�ѷ���ɢ��Ԫ�߽糤��>Prp_Bisec_Factor_3D*Ave_Elem_L_Enrich,�����
Prp_Bisec_Factor_3D = 2.0D0               !2022-09-28.
Key_Smooth_Front    = 0                   !3D�ѷ���չ���Ʋ���:(default:0),>=1����ѷ�ǰԵ���й⻬����
Key_Smooth_Front_Twice =0                 !=1ʱ��ִ������Smooth������Ȼ�����δ���������ȡƽ��ֵ
!Key_Smooth_Pressure_Curve = 0             !ѹ�����߹⻬����,2�ι⻬����. 2022-10-14.
Key_3D_FluEle_Triang= 1                   !��3D���嵥Ԫ���в��,ȷ��ÿ�����嵥Ԫ���������ε�Ԫ(default: 1)
Key_Crack_Inner_Pressure = 0              !Ĭ���޷�������ѹ��
Key_Multi_TipEnrNode=0                    !�������Ѽ���ǿ�ڵ�(Key_TipEnrich_Radius_Factorȷ����Բ�ڵ�ȫ���ڵ㶼�����Ѽ���ǿ)
Key_Junction_Enrich         =  0     !�Ƿ���Junction��ǿ(Ĭ��Ϊ0)��������3D. 2022-08-25.
Key_TipEnrich_Radius_Factor =  2.0D0      !Key_Multi_TipEnrNode=1ʱ������,Key_TipEnrich_Radius_Factorȷ����Բ�ڵ�ȫ���ڵ㶼�����Ѽ���ǿ
Num_Check_T_Matrix   = 180                !Tool_ThetaX_ThetaY_ThetaZ_3D_rotation.f����������ֲ�����ϵת�ǵĻ��ַ���(Ĭ�ϵ���180;ԽС����Խ��)
Key_Pre_Conditioner  =0                   !����Pre_Conditioner,���ڼ�С�նȾ�������������Ӷ����ͷ����鲡̬���ԡ���С������;Ŀǰ������3D
Lis_Number_Iteration = 5000               !Lis������ĵ�������(Ĭ��5000)
MDOF_2D              = 200                !�����ڲ�����,Max_number_of_DOFs, 2D���ⵥԪ�նȾ���������ɶ���Ŀ(default to 200;��Ķ�).
MDOF_3D              = 156                !�����ڲ�����,Max_number_of_DOFs, 3D���ⵥԪ�նȾ���������ɶ���Ŀ(default to 156;��Ķ�).
Key_Initiation       = 0                  !Ĭ�ϲ����ɳ�ʼ�ѷ�
Key_Ini_Rule         = 1                  !���ɳ�ʼ�ѷ���ѭ��׼��:=1,�����Ӧ��׼��(Ĭ��)
Key_FS_Seque_Coupling= 0                  !Sequential coupling of solid and field problems (only for 2D problem and Key_Analysis_Type = 1)
Key_TS_Seque_Coupling= 0                  !�ȹ���ϼ���(2D��̬+˲̬�¶ȳ�����) (only for 2D problem AND key_analysis_type = 1), date: 2021-11-03
Key_SIFs_DIM_Points  = 2                  !λ�Ʋ�ֵ������Ӧ��ǿ�����ӵ���ȡ�����Ŀ(2��3,Ĭ��Ϊ2)
Key_SIFs_DIM_Method  = 1                  !=1,����ƽ��Ӧ����ʽ;=2,����ƽ��Ӧ�乫ʽ. 2023-03-19.
Factor_Propagation   = 1.5                !�ؼ��֣�������չ����ϵ��,default to 1.5
Propagation_Length   = -99.0D0            !�ѷ���չ����,�������ò�����>0,��Factor_Propagation��������,default to -99.0
Key_Local_Mesh_Refine= 0                  !����ֲ��Ż�, =0,�رգ�=1������ȫ����ǿ�ڵ㣻=2���������Ѽ���ǿ�ڵ�
Key_Large_Deform     = 0                  !���������(1), ����Key_Analysis_Type = 8ʱ��Ч
Key_Front_Segmentation = 0                !Allow fracture front segmentation (Avaliable only for 3D)
Number_front_Segments  =2                 !Number of Segments(default:2).
Key_Adj_Ini_Crack_3D = 0                  !Adjust initail crack. For 3D only. default: 0.
Key_Check_and_Adjust_Cracks_3D =  0       !���ݸ��ڵ�ķ��ž�������ѷ�������. 2022-08-01.
Adjust_Cracks_Delta_Angle   =  45.0D0     !��λ�����ֱ���. Key_Check_and_Adjust_Cracks_3D =  2��3ʱ�õ�. 90.0D0, 60.0D0, 45.0D0, 30.0D0, 22.5D0
Adjust_Cracks_Resolution    =  6          !��λ�����ֱ���. Ĭ��Ϊ6.
num_Suspended_Point  = 0                  !����Fracture Front Segmentation,��ʷ�����Ƶĵ����Ŀ(2021-08-20)
key_tip_fluid_element_3d = 1              !����3D�ѼⵥԪ�����������嵥Ԫ(�Ӵ���Ԫ)
allocate(Suspended_Points(1000,3))
Suspended_Points(1:1000,1:3)=-TEN_15      !����Fracture Front Segmentation,��ʷ�����Ƶĵ������(2021-08-20)
Flag_Local_Refined   = .false.            !���ڱ���Ƿ��Ѿ��ֲ�����
Ave_Elem_L_Enrich_Unlocalrefined = ZR     !�ֲ�����ǰ����ǿ��Ԫ��������(2021-08-22)
Key_Ini_Cr_3D_Type   =1                   !����3D��ʼ�ѷ������(����3D����ʱ��Ҫ):=1,Բ��;=2,����(��ʱ������)
Picard_Alpha         = 0.25D0             !ˮ��ѹ��Picard����ϵ��(3Dˮ��ѹ��)(2021-12-05)
Picard_Tol           = 0.01               !ˮ��ѹ��Picard���������ݲ�(0.01)(2021-12-05)
Max_Picard_Iter      = 50                 !ˮ��ѹ��Picard��������(50)
Key_Block_Model      = 0                  !�Ƿ�Ϊ����ģ��
Num_Elem_Block_Bou   = 0                  !����ģ�ͱ߽��ϵĵ�Ԫ��Ŀ
Key_InPlane_Growth   = 0                  !����3D XFEM��������ѷ����ԭƽ������չ(2022-04-18).
Key_Stop_Outside_Crack =0                 !һ���ѷ���չ��ģ���ⲿ�������Ϊ������չ(2022-10-02). NEWFTU2022100201.
!---Wellbore��Ͳ���(2022-04-19)----
Key_HF_Multistage_3D = 0                  !�Ƿ���3D�ֶ�ѹ�ѷ���
num_Wellbore         = 0                  !��Ͳ��Ŀ�����5����Ͳ��Ĭ��0��
num_Points_WB(1:20)  = 2                  !ÿ����Ͳ���������Ŀ,���20����Ͳ
Wellbore_Coors(1:20,1:20,1:3) = ZR        !��Ͳ�ĵ������,ÿ����Ͳ���20����
num_Stages_Wellbores(1:20)    = 1         !ÿ����Ͳ�ķֶ���
num_Crs_Stages_Wellbores(1:20,1:20) = 1!ÿ����Ͳÿ���ֶεķֶδ���
Key_Gen_Ini_Crack_Wellbores   = 0         !�Ƿ��Զ����ɳ�ʼ�ѷ죨ע:�Զ����ɵĳ�ʼ�ѷ촹ֱ�ھ�Ͳ��
Size_Ini_Crack_Wellbores      = 5.0D0     !�Զ����ɵĳ�ʼ�ѷ�ߴ�(������)
Num_Poly_Edges_NaCr_WB        = 6         !����γ�ʼ�ѷ�ı���

Wellbores_Start_Point(1:20,1:3) = -TEN_15 !��Ͳ�Ϸֶ�ѹ�ѵ����������յ�����(ע:�����ѷ��ڸ��ξ��ȷֲ������Գ�ʼ�ѷ첻һ���ڶ˵�λ��)
Wellbores_End_Point(1:20,1:3)   = -TEN_15 !��Ͳ�Ϸֶ�ѹ�ѵ����������յ�����(ע:�����ѷ��ڸ��ξ��ȷֲ������Գ�ʼ�ѷ첻һ���ڶ˵�λ��)
Injection_Q_Stages_Wellbores(1:20,1:20)= -TEN_15       !ÿ����Ͳÿ���ֶε�ѹ��Һ����,��λ��m^3/s
Injection_T_Stages_Wellbores(1:20,1:20)= -TEN_15       !ÿ����Ͳÿ���ֶε�ѹ��Һע��ʱ��,��λ��s
!----3D�ѷ�ǰԵS1��K�⻬����-(2022-04-25)----
Key_Denoise_Vertex_Value =0               !3D�ѷ�ǰԵS1��K�����ݽ���ȥ�����.
Key_Smooth_Vertex_Value  =0               !��3D�ѷ�ǰԵS1��K�����ݽ��й⻬����, =1, ����ƽ����,��ǰ��ʱ�̵�һ��2n+1��ֵ��ƽ��.
Smooth_Vertex_n          =4               !Key_Smooth_Vertex_Method= 1ʱ�õ�. ����ƽ������n. Ĭ��ֵΪ4.
Key_Smooth_Vertex_Value2 =0               !��3D�ѷ�ǰԵS1��K�����ݽ��й⻬�������δ���, =1, ����ƽ����,��ǰ��ʱ�̵�һ��2n+1��ֵ��ƽ��.
Smooth_Vertex_n2         =3               !Key_Smooth_Vertex_Method= 1ʱ�õ������δ���. ����ƽ������n.
!-------3D�ѷ�ǰԵTheta�⻬����CFCP=3ʱ����)-------Added on 2022-07-14. ���������
Key_Denoise_Theta_Value= 0 !3D�ѷ�ǰԵS1��K�����ݽ���ȥ�����. Ĭ��ֵΪ0.
Key_Smooth_Theta_Value = 0 !��3D�ѷ�ǰԵS1��K�����ݽ��й⻬����. Ĭ��ֵΪ0.!=1, ����ƽ����,��ǰ��ʱ�̵�һ��2n+1��ֵ��ƽ��.
Smooth_Theta_n         = 2 !Key_Smooth_Vertex_Method= 1ʱ�õ�. ����ƽ������n. Ĭ��ֵΪ4.
Key_Smooth_Theta_Value2= 0 !��3D�ѷ�ǰԵS1��K�����ݽ��й⻬����(���δ���). Ĭ��ֵΪ0.!=1, ����ƽ����,��ǰ��ʱ�̵�һ��2n+1��ֵ��ƽ��.
Smooth_Theta_n2        = 2 !���δ���:Key_Smooth_Vertex_Method= 1ʱ�õ�. ����ƽ������n. Ĭ��ֵΪ3.
!-------3D�ѷ�ǰԵGrowthFactor�⻬����CFCP=3ʱ����)-------Added on 2022-07-14.
Key_Denoise_GF_Value= 0 !3D�ѷ�ǰԵS1��K�����ݽ���ȥ�����. Ĭ��ֵΪ0.
Key_Smooth_GF_Value = 0 !��3D�ѷ�ǰԵS1��K�����ݽ��й⻬����. Ĭ��ֵΪ0.!=1, ����ƽ����,��ǰ��ʱ�̵�һ��2n+1��ֵ��ƽ��.
Smooth_GF_n         = 2 !Key_Smooth_Vertex_Method= 1ʱ�õ�. ����ƽ������n. Ĭ��ֵΪ4.
Key_Smooth_GF_Value2= 0 !��3D�ѷ�ǰԵS1��K�����ݽ��й⻬����(���δ���). Ĭ��ֵΪ0.!=1, ����ƽ����,��ǰ��ʱ�̵�һ��2n+1��ֵ��ƽ��.
Smooth_GF_n2        = 2 !���δ���:Key_Smooth_Vertex_Method= 1ʱ�õ�. ����ƽ������n. Ĭ��ֵΪ3.
!----ѹ���ѷ����----
Key_CS_Crack(1:Max_Num_Cr) = 0            !��ʼ��Ϊ��ѹ���ѷ�
allocate(Cracks_Initial_Meshed(Max_Num_Cr_3D))
Cracks_Initial_Meshed(1:Max_Num_Cr_3D) =0         !���ڱ��ÿ���ѷ��Ƿ񱻳�ʼ����ɢ����.
allocate(Cracks_Initial_Adjusted(Max_Num_Cr_3D))
Cracks_Initial_Adjusted(1:Max_Num_Cr_3D)=0        !���ڱ��ÿ���ѷ��Ƿ񱻵�������.
allocate(Cracks_Checked_and_Adjusted(Max_Num_Cr_3D))
Cracks_Checked_and_Adjusted(1:Max_Num_Cr_3D)=0    !���ڱ��ÿ���ѷ��Ƿ񱻼���ҵ�������. 2022-08-01.

Key_EBE_Precondition      = 1                     !EBE�����Ԥ����ؼ���.
Key_EBE_Condition_Number  = 0                     !=1ʱ�����ÿ����ǿ��Ԫ��������������ȫ�ֱ���EBE_Condition_Number(Num_Elem). NEWFTU2022070901.
Key_EBE_Sym_Storage_K     = 0                     !�Գƴ洢��Ԫ�նȾ���storK_FEM. 2022-11-10. NEWFTU2022111001.
!���ڱ���ѷ����ͺ��ѷ�״̬. 2022-06-02.
allocate(Crack_Type_Status_3D(Max_Num_Cr_3D,10))
Crack_Type_Status_3D(1:Max_Num_Cr_3D,1)=1   !��1��(�ѷ�����)��               =1��HF�ѷ죻=2����Ȼ�ѷ�
Crack_Type_Status_3D(1:Max_Num_Cr_3D,2)=1   !��2��(�ѷ�״̬)��               =1��HF�ѷ�ѹ��δ��ɣ�=2��HF�ѷ�ѹ�����
Crack_Type_Status_3D(1:Max_Num_Cr_3D,3)=1   !��3��(�ѷ��ܷ������չ)��       =1���ܣ�=0������
Crack_Type_Status_3D(1:Max_Num_Cr_3D,4)=0   !��4��(�ѷ��Ƿ��ѻ������ڵ�):  =1����; =0; ��
Crack_Type_Status_3D(1:Max_Num_Cr_3D,5)=0   !��5��(��һ���ѷ��Ƿ�������չ):=1����; =0; ��
Flag_HF_3D = 0                              !3Dˮ��ѹ�ѷ������(������ˮѹ)
Key_Cpp_Call_Fortran_Lib = 0                !2023-03-23.

!���±������ڿ��ٶ����ʼӦ��������������. 2022-06-03.
InSitu_S1_3D = ZR
InSitu_S2_3D = ZR
InSitu_S3_3D = ZR
InSitu_S1_nv_3D = [1.0D0,0.0D0,0.0D0]
InSitu_S2_nv_3D = [0.0D0,1.0D0,0.0D0]
InSitu_S3_nv_3D = [0.0D0,0.0D0,1.0D0]
!�Ǿ��ȳ�ʼӦ����(x,y,z����). 2022-07-06. NEWFTU2022070601.
Key_Nonuniform_InSitu_X_with_Z          = 0              !X������������ȳ�ʼӦ����.
InSitu_Sx_3D_Seg_Strs_X_with_Z(1:100)   = ZR             !�ֳ�n��.
InSitu_Sx_3D_Seg_Loca_X_with_Z(1:100)   = ZR             !����λ��n+1.
Key_Nonuniform_InSitu_X_with_Y          = 0              !X������������ȳ�ʼӦ����.
InSitu_Sx_3D_Seg_Strs_X_with_Y(1:100)   = ZR             !�ֳ�n��.
InSitu_Sx_3D_Seg_Loca_X_with_Y(1:100)   = ZR             !����λ��n+1.
Key_Nonuniform_InSitu_Y_with_Z          = 0              !X������������ȳ�ʼӦ����.
InSitu_Sy_3D_Seg_Strs_Y_with_Z(1:100)   = ZR             !�ֳ�n��.
InSitu_Sy_3D_Seg_Loca_Y_with_Z(1:100)   = ZR             !����λ��n+1.
Key_Nonuniform_InSitu_Y_with_X          = 0              !X������������ȳ�ʼӦ����.
InSitu_Sy_3D_Seg_Strs_Y_with_X(1:100)   = ZR             !�ֳ�n��.
InSitu_Sy_3D_Seg_Loca_Y_with_X(1:100)   = ZR             !����λ��n+1.
Key_Nonuniform_InSitu_Z_with_X          = 0              !X������������ȳ�ʼӦ����.
InSitu_Sz_3D_Seg_Strs_Z_with_X(1:100)   = ZR             !�ֳ�n��.
InSitu_Sz_3D_Seg_Loca_Z_with_X(1:100)   = ZR             !����λ��n+1.
Key_Nonuniform_InSitu_Z_with_Y          = 0              !X������������ȳ�ʼӦ����.
InSitu_Sz_3D_Seg_Strs_Z_with_Y(1:100)   = ZR             !�ֳ�n��.
InSitu_Sz_3D_Seg_Loca_Z_with_Y(1:100)   = ZR             !����λ��n+1.
allocate(Cracks_FluidEle_CalP_Glo_Insitu(Max_Num_Cr_3D*Max_Max_N_CalP_3D))
Cracks_FluidEle_CalP_Glo_Insitu(:)  = ZR    !ȫ�ֱ������ڵ�ĵ�Ӧ��(��ֱ���ѷ��淽��), 2022-06-04.
!3D��Ȼ�ѷ����
Key_NaCr_Type_3D       = 1                  !��ʼ��Ȼ�ѷ�����:=1,����;=2,Բ��
Num_Poly_Edges_NaCr    = 6                  !����γ�ʼ��Ȼ�ѷ�ı�����Ĭ��Ϊ6
Key_NaCr_Cross         = 0                  !�Ƿ������ʼ�ѷ콻��(Ĭ��Ϊ0,������)
Key_NaCr_Growth        = 0                  !�Ƿ������ʼ��Ȼ�ѷ���չ(��HF��ͨǰ)(Ĭ��Ϊ0,������)
NaCr_3D_n_Vector(1:3)  = [ONE,ZR,ZR]        !��ʼ�ѷ�ķ��߷���
NaCr_3D_n_Vector_Delta = ZR                 !���߷���Ĳ�������(��λΪ��,Ĭ��Ϊ0)
NaCr_3D_Size           = -999.0D0           !���ھ����ѷ�ָ���Ǳ߳�,����Բ���ѷ�ָ���ǰ뾶
NaCr_3D_Sz_Delta       = ZR                 !�ߴ�Ĳ�������(Ĭ��Ϊ0)
NaCr_3D_Rect_Longside_Vector = ZR           !�������εĳ��߷�������.
NaCr_3D_Rect_L         = -999.0D0           !�������εĳ��߳���.
NaCr_3D_Rect_W         = -999.0D0           !�������εĶ̱߳���.
NaCr_3D_Rect_L_Delta   = ZR                 !�������εĳ��߳��Ȳ�������.
NaCr_3D_Rect_W_Delta   = ZR                 !�������εĶ̱߳��Ȳ�������.
NaCr_3D_Rect_Longside_Vector_Delta = ZR     !�������εĳ��߷���������������(��λΪ��,Ĭ��Ϊ0).
num_XA_Input_Cracks    = 0                  !�°´������Ȼ�ѷ���Ŀ. 2023-03-23.
XA_Min_Frac_Radius     = ZR                 !��Ȼ�ѷ�뾶��ֵ. 2023-03-23.
!3D�ѷ콻��״̬
!allocate(Cracks_3D_Inter_Status(Max_Num_Cr_3D,Max_Num_Cr_3D) )
!Cracks_3D_Inter_Status(1:Max_Num_Cr_3D,1:Max_Num_Cr_3D) = 0
!-------3D��Ȼ�ѷ켤���㷨������3D��-----------2023-01-07
Key_NaCr_Active_Scheme_3D  =  1  !���ڿ���3D��Ȼ�ѷ켤���㷨.
Size_Factor_of_Active_NaCr = 6.0D0 !����ͨ����Ȼ�ѷ��ֱ��(Size_Factor_of_Active_NaCr*�������ڵ�Ԫ�������ߴ�),Key_NaCr_Active_Scheme_3D = 3ʱ�õ�. 2023-01-11.
KIc_NaCr                   = ZR     !��Ȼ�ѷ�Ķ����Ͷ�. 2023-01-12.
St_NaCr                    = ZR     !��Ȼ�ѷ�Ŀ���ǿ��. 2024-02-22.
Key_CFCP_3_Type   = 1                      !CFCP=3׼���ѷ���չ��Ϊ, 1: ���ڶ����Ͷ�KIc������չ(Ĭ��)
                                           !                        2: �����㼴����չ(Ref: Tang_2019_Analysis of stress interference among_Eq.16)
Key_3D_Cr_Update_Scheme = 2                !3D�ѷ�������㷨(NEWFTU2022071201), 1: �����Ƿ���չ������չһ�����������ڲ���չ���Ѽ⣬����һ��΢С��.
                                           !                                    2: ����չ�Ѽⲻ��չ����չ��С�������Ѽ��������(default).
Schollm_Max_Theta = 55.0D0                 !�����3D Schollmann��s criterion�����Thetaƫת��(��λΪ��,����С��75,Ĭ��55). NEWFTU2022071001.

!�ڴ��Ż��β��������. 2022-09-04.
!Solid_El_Arrays_Objects_Created  = .False.        !�߼����������ڱ��Solid_El��زβ������Ƿ��Ѿ����ɶ���
!Crack3D_Meshed_Arrays_Objects_Created = .False.         !�߼����������ڱ��Crack3D_Meshed��زβ������Ƿ��Ѿ����ɶ���

!���غ����.
Num_Surface_Loads = 0         !���غ���Ŀ. 2023-01-21.
Surface_Pressure(1:100) = ZR  !���غɴ�С. 2023-01-21.

!�������������ڴ�. 2022-09-04.
allocate(Cracks_FluidEle_num_3D(Max_Num_Cr_3D))
Cracks_FluidEle_num_3D(1:Max_Num_Cr_3D) = 0
allocate(Cracks_Real_CalP_Num_3D(Max_Num_Cr_3D))
Cracks_Real_CalP_Num_3D(1:Max_Num_Cr_3D)= 0
allocate(Cracks_Volume(Max_Num_Cr_3D))                    !ÿ���ѷ�����
Cracks_Volume     = ZR
allocate(Cracks_Volume_Old(Max_Num_Cr_3D))                !ÿ���ѷ�����(��һ����)
Cracks_Volume_Old = ZR
allocate(Cracks_FluidEle_CalP_Glo_Info(Max_Num_Cr_3D*Max_Max_N_CalP_3D,3))
Cracks_FluidEle_CalP_Glo_Info(1:Max_Num_Cr_3D*Max_Max_N_CalP_3D,1:3)  = 0
allocate(Crack3D_Centroid(Max_Num_Cr_3D,3))
Crack3D_Centroid(1:Max_Num_Cr_3D,1:3) = ZR
allocate(Crack3D_Meshed_Outline_num(Max_Num_Cr_3D))
Crack3D_Meshed_Outline_num(1:Max_Num_Cr_3D) = 0
allocate(KI_3D(Max_Num_Cr_3D))    !�β��������
allocate(KII_3D(Max_Num_Cr_3D))    !�β��������
allocate(KIII_3D(Max_Num_Cr_3D))    !�β��������
allocate(KI_eq_3D(Max_Num_Cr_3D)) !�β��������

!-----------------------------------------------------------------------
!2022-10-09. IMPROV2022100901. ������ԭ����PhiPsi_Read_Input.f�ڵı���.
!-----------------------------------------------------------------------
Key_Junction_Check= 1
Key_Propa_Type    = 1       !��չ����:=1,�̶�������չ;=2,ʵ�ʲ�����չ(����ƣ�ͷ����Ͷ�̬����,�Զ��߼�����)
Max_MNR_Iter      = 30        !�ܵĵ�������,30                     (++++++++++++++++)
Max_Num_Lnsrch    = 20       !ÿ��NR���������ִ�е��������ͻ��ݴ���
Prop_Angle_Alowed = 180.0D0   !������ѷ���չƫת��(0��180֮�����)
Key_Unit_System = 1         !��λ��Ϊ���ʵ�λ��
k_tt = ZR                !���߸ն�,1.0D15
k_nn = 1.0D16               !����ն�,1.0D15
a_Ave_Shi         = 5.0     !������ļ�Ȩƽ�������㷨�еĲ���a(1-100,Խ��Խ����ƽ��)
Factor_Ave_R      = 0.7     !��Ȩƽ������Ӧ����������뾶ϵ��(0.1-1.5)
Key_HF_Multistage = 0       !�ݲ�֧�ֶַ�ѹ��
Delta_Factor_SIFs = ZP1   !λ�Ʋ�ֵ������Ӧ��ǿ������ʱ���Ѽ�����ƫ�õ���,dela_L = factor_SIFs_DIM*ƽ����Ԫ����
Key_Tip_Pres_Zero = 1       !����ˮ��ѹ�ѷ�������,�Ƿ������Ѽ�ˮѹΪ0(Ĭ���Ѽ�ˮѹΪ��:Key_Tip_Pres_Zero=1)
Key_HF_Del_Ne_Pres= 0
Coff_a_AMPT       = ZP1   !�ؼ��֣���Ȩƽ���������Ӧ��׼����Ʋ���
Water_Pressure    = 1.0D6   !��������4�ľ���ˮѹ��С
Key_Leakoff       = 0       !�Ƿ���й¶
Coeff_Leak        = 1.0D-5  !й¶ϵ��1.0D-5
Key_IniPre_PassOn = 0       !�������Ѳ���ʼ����ʱ�Ƿ�̳���һ���Ѳ�����ˮѹ�����ȵ�
                          !                    0:���̳�,total_time����,�ѷ쿪�ȴ�0��ʼ����(Ĭ��)
                          !                    1:�̳�,˼·����ҵıʼ�,V3_P78(��֧��4��PNR��������)
Key_Cal_deltaTime = 1       !����HF��Ԫ����ʱ��������׼��(���ε�Ԫ����Ҫ)
                          !          1:���η�(default),delta_V=delta_L*delta_w1;
                          !            �����delta_VƫС,����delta_TimeƫС;
                          !            Picard����������þ��η�,�����������ˮѹ�������½���0��
                          !          2:���η�,delta_L*0.5*(w1+w2);
                          !            �����delta_VƫС,����delta_Timeƫ��
SOR_factor        = 0.75D0  !NR�����͸��߷�������γ��ɳڵ���ϵ����Ĭ��ֵΪ0.75
Key_Visco_Type    = 1       !ѹ��Һճ�ȼ��㷽ʽ, 1: ճ�ȱ��ֲ���(��̬ճ��)
                          !                    2: ճ����֧�ż���Ũ�ȱ仯(��̬ճ��)
Viscosity_Par_m   = TWO   !��̬ճ��ָ��m(1< m <3).
Max_c             = ZP6                  !��������Ũ��
Factor_Check_Dis  = 3.0     !�ڲ�����:�ѷ콻���ⳤ��ϵ��(�ܹؼ�,̫���������������bug,��V5-P50)
Factor_L_Jun_NewCr= 5.23    !HF��NF�������γɵ����ѷ�ĳ���ϵ��
Key_Kink_Point    = 1       !�Ƿ���յ㻮�ּ����(Ĭ�Ͽ���,��Key_Kink_Point=1)
First_XFEM_Step   = 0       !�����ɵĳ�ʼ�ѷ켤��ĵ�һ��XFEM��. 2023-01-23.
!------------------
Desired_KIc  = 2.0D6        !ˮ��ѹ��Ŀ����K_C,����PhiPsi3D_Static_HF_SlipWater. 2023-02-12.


!------------------
Key_Ele_Max_Related_Cracks = 10 !����ָ����Ԫ�����Թ������ѷ���Ŀ. NEWFTU2023022501.

St_KIc_Conversion = 6.88D0      !����ǿ�ȺͶ����ͶȻ���ϵ��. KIc = St/St_KIc_Conversion. 2023-03-25.
!------------------
XA_Step_Count =  0              !���ڱ���ܵ����Ѳ�����ȫ�ֱ���. 2023-04-03.

!--------ˮ��ѹ��ʵ��������----------
HFE_Initial_Try_Pressure = ZR        !�����ĳ�ʼ�ױ�ѹ��. 2023-04-18.
HFE_Initial_Pressure_Step_Size = 0.1D6  !���ѷ���ѹ����������.

!------------
Key_Allow_3D_Outside_Crack = 0

Key_3D_HF_Time_Step_Method = 1 !2023-05-18. 3D��ˮѹ�ѷ���ʱ�䲽��������:=1,NR����(default);=2,���ַ�.


Key_3D_HF_SlipWater_fk_Type =1 !2023-08-08. 3D��ˮѹ���Ѽ�fk������������. NEWFTU2023080801.

!IMPROV2023081001.
SIFs_DIM_3D_Offset_Delta_Factor = 0.5D0
!SIFs_DIM_3D_r_1_Factor          = 0.1D0
!SIFs_DIM_3D_r_2_Factor          = 0.2D0
!2023-08-22.
SIFs_DIM_3D_r_1_Factor          = 1.0D0
SIFs_DIM_3D_r_2_Factor          = 2.0D0
SIFs_DIM_3D_r_k_Factor          = 1.5D0

Key_Crack_Aperture_Method =2 !�ѷ쿪�ȼ��㷽��, =1:���ݹ�ʽ����; =2:����ƫ�õ����(default). 2023-08-12.

!2023-08-22.
Key_Print_SIFs_to_Screen = 0

!2023-08-27.
Crack_Max_Min_Aperture(1:Max_Num_Cr,1:3) = -999.0D0   !����ÿ���ѷ�������Сƽ������.

!2024-02-15.
Key_Save_Crack_Radius=0      !���ڱ����ѷ쿪��. 2023-02-15.

!3DԲ�ε�ЧΪ����εķֱ���. 2024-02-26.
Circle_3D_Eqv_Polygon_Resolution = 21 !3DԲ�ε�ЧΪ����εķֱ���. 2024-02-26.

!3D SlipWater NR����ʱ�䲽��ѹ��������������. IMPROV2024022801.
SlipWater_Max_Time_Steps_3D   = 20  
SlipWater_Max_Pres_Steps_3D   = 10 
SlipWater_Time_Step_Conv_Check= 1   !3D SlipWater NR����ʱ�䲽�������(Ĭ��Ϊ0�������м��). 
SlipWater_Pres_Step_Conv_Check= 1   !3D SlipWater NR����ʱ�䲽�������(Ĭ��Ϊ0�������м��). 

!SlipWater_Max_Time_Steps_3D   = 3  
!SlipWater_Max_Pres_Steps_3D   = 3 
!SlipWater_Time_Step_Conv_Check= 0   !3D SlipWater NR����ʱ�䲽�������(Ĭ��Ϊ0�������м��). 
!SlipWater_Pres_Step_Conv_Check= 0   !3D SlipWater NR����ʱ�䲽�������(Ĭ��Ϊ0�������м��). 
!Max_Contact_Iter              = 1

!2024-03-09.
Key_Random          = 1
Inject_Crack_Num    = 1
Key_TipEnrich       = 1
Key_CFCP_3_Type     = 1
Key_FD_Tipenrich    = 1

!2024-03-13.
MAT_ALLOW_CRACK_Initiation(1:Max_Materials) = 1  !�Ƿ�����ĳ�ֲ������ɳ�ʼ�ѷ�. Ĭ��ȫ������.
Key_Max_Num_Initiation_Cracks               = 1  !������������ĳ�ʼ�ѷ���Ŀ.
Num_Initiation_Cracks                   = 0  !�����ĳ�ʼ�ѷ���Ŀ.

print *," "

RETURN
END SUBROUTINE Initialize
