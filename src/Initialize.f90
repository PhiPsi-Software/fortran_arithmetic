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
!初始化程序及相关变量.

!------------------         
!读取公共变量模块
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
!执行参数初始化过程
!-------------------
print *, "    Initializing parameters...."
!几个全局变量的最大值. 2023-08-13.
Max_Max_N_FluEl_3D = maxval(Max_N_FluEl_3D)  
Max_Max_N_CalP_3D  = maxval(Max_N_CalP_3D) 
Max_Max_N_Node_3D  = maxval(Max_N_Node_3D) 

!裂纹坐标初始化
Crack_Coor(1:Max_Num_Cr,1:Max_Num_Cr_P,1:2)=ZR
!弧形裂纹坐标初始化
Arc_Crack_Coor(1:Max_Num_Cr,1:Max_Num_Cr_P-1,1:11)  = ZR
!初始化圆形孔洞
Hole_Coor(1:Max_Num_Hl,1:3)=ZR
!初始化椭圆孔洞
Ellip_Hole_Coor(1:Max_Num_Hl,1:5)=ZR
!初始化圆形夹杂
Circ_Inclu_Coor(1:Max_Num_Circ_Incl,1:3) =ZR
Circ_Inclu_Mat_Num(1:Max_Num_Circ_Incl)  =0
!初始化多边形夹杂
Poly_Incl_Coor_x(1:Max_Num_Poly_Incl,1:Max_Num_Edges_Poly)=-1.0D8
Poly_Incl_Coor_y(1:Max_Num_Poly_Incl,1:Max_Num_Edges_Poly)=-1.0D8
Poly_Inclu_Mat_Num(1:Max_Num_Poly_Incl) = 0
Poly_Inclu_Edges_Num(1:Max_Num_Poly_Incl) =0
Poly_Incl_Coor_x_Cl(1:Max_Num_Poly_Incl,1:Max_Num_Edges_Poly)=-1.0D8
Poly_Incl_Coor_y_Cl(1:Max_Num_Poly_Incl,1:Max_Num_Edges_Poly)=-1.0D8
!随机生成不规则多边形夹杂相关
Key_Rand_Poly_Incl_Irregular =0                !是否随机生成不规则多边形夹杂
num_Rand_Poly_Incl_for_Each_Type(1:10)  =0     !各级夹杂的数目,最多可定义10级尺寸
Rand_Poly_Incl_R_Min_and_Max(1:10,1:2)  =ZR    !各级夹杂的半径范围
Rand_Poly_Incl_Irregular_Extension_Factor  = ONE    !伸长率(1.0-3.0)
Rand_Poly_Incl_Irregular_Inclination  = ZR     !倾角
Rand_Poly_Incl_Irregular_R_Delta_Factor = ZR   !多边形生成过程中的半径变化系数:(0.0 to 1.0),delta_R = Factor*R
Rand_Poly_Incl_Irregular_Angle_Factor = ZR     !多边形生成过程中的角度变化系数:(0.0 to 1.0)
!随机生成孔洞相关
Key_Random_Hole     = 0                      !是否随机生成孔洞
!初始化裂纹数目
num_Crack = 0
!初始化弧形裂纹数目
num_Arc_Crack = 0
!初始化孔洞数目
num_Hole  = 0
num_Circ_Hole  = 0
num_Ellip_Hole  = 0
!孔洞生成裂缝相关
Key_Hole_Crack_Generate  =0           !是否允许孔洞位置萌生裂缝
Num_Crack_Hole_Generated    = 2       !每个孔洞允许生成的裂缝数目
num_Hole_Crack_Generated(1:1000) = 0      !各个Hole对应的生成的裂缝数目
Hole_Crack_Generated_num(1:1000,1:10) =0  !各个Hole对应的生成的裂缝号
!初始化夹杂数目
num_Circ_Incl   = 0         !圆形夹杂,通过圆形和半径定义
!num_Tri_Incl    = 0         !三角形夹杂,通过三点坐标定义
!num_Quad_Incl   = 0         !四边形夹杂,通过4点坐标定义
!num_Penta_Incl  = 0         !五边形夹杂,通过5个点的坐标定义
num_Poly_Incl =0             !多边形夹杂的数目
num_Ellip_Incl  = 0         !椭圆形夹杂的数目
num_Inclusion = 0           !总的夹杂数目
!初始化文件名
Filename  = '*blank*'
!初始化文件路径
Work_Directory = '*blank*'
!初始化每条裂纹的含水状态
Cracks_HF_State(1:Max_Num_Cr)  = 0
!初始化每条裂纹含支撑剂的状态
Cracks_HF_Propp(1:Max_Num_Cr)  = 0
!初始化注水点坐标(全HF模型)
Inj_Point_Loc(1:2) =ZR
Yes_Arc_Crack      = .False. !是否包含弧形裂缝或裂缝段
!后处理相关
Key_Post_CS_G_Coor = 1        !是否计算并保存Gauss点坐标(默认值为1)
Key_Post_CS_G_Disp = 1        !是否计算并保存Gauss点位移(默认值为1)
Key_Post_CS_N_Strs = 0       !是否计算并保存Node点应力(默认值为0)
Key_Post_CS_G_Strs = 1        !是否计算并保存Gauss点应力(默认值为1)
Key_Post_S_Dof_F   = 0        !是否保存各自由度的载荷值,以便用于后处理(默认值为0)
Key_Post_Cracked_Ele=0        !是否计算并保存单元应力状态文件(是否σ1-σ3>Tol,分段压裂默认打开)
Key_Post_S_TanDisp = 0        !是否计算并保存裂缝的切向相对位移(默认是0)
Key_Node_Value     = 1        !节点应力计算算法(=1,直接平均法(默认); =2,最小二乘匹配法)
Key_Save_vtk       = 1        !是否保存vtk文件(默认为1,保存)
Key_Simple_Post    = 0        !简洁后处理，仅保存基本数据，如节点位移、裂缝开度等. 2022-06-25.
Key_Save_Nothing   = 0        !不保存任何数据. 2022-09-06. NEWFTU2022090601.
Key_Post_Elements_Gauss_Num= 0!是否保存每个单元的高斯积分点数目. 仅用于后处理.2022-07-16.
Key_Get_Permeability       = 0!计算并保存裂缝渗透率. 2022-11-26.
Key_Post_CS_N_Stra  = 0       !是否计算并保存Node点应变(默认值为0)
Key_Save_avol_file         = 0       !保存裂缝体积和文件. 2022-12-18. 默认为0.
Key_Post_CS_N_Stra_Cylindr= 0!是否计算并保存Node点应变(默认值为0)柱坐标系
Key_Post_CS_N_Strs_Cylindr= 0!是否计算并保存Node点应力(默认值为0)柱坐标系
Key_Post_CS_N_Disp_Cylindr =0!是否计算并保存Node点位移(默认值为0)柱坐标系
Post_cylinder_Coor_Center(1:3)    = -Con_Big_20       !圆柱材料坐标系原点
Post_cylinder_Coor_Vector_x(1:3)  = -Con_Big_20     !圆柱材料坐标系x轴向量
Post_cylinder_Coor_Vector_y(1:3)  = -Con_Big_20     !圆柱材料坐标系y轴向量
Post_cylinder_Coor_Vector_z(1:3)  = -Con_Big_20     !圆柱材料坐标系z轴向量

!初始化注水和支撑剂相关参数
Inject_Q_Time(1:200) = ZR
Inject_Q_Val(1:200)  = ZR
Inject_c_Time(1:200) = ZR
Inject_c_Val(1:200)  = ZR
Inject_P_Time(1:200) = ZR
Inject_P_Val(1:200)  = ZR
!水力压裂分析控制参数默认设置
MNR_Tol              = 0.05     !Tolerance of MNR iteration(推荐值:5%,p+w收敛规则需同时满足).
Key_HF_Conv_Crite    = 2        !迭代收敛准则, 1: 通过裂缝开度判断
                           !              2: 通过裂缝开度和水压
Key_HF_LineSearch    = 1        !默认打开线性搜索

!最大支撑剂浓度值
Max_c=ZR
!C     !初始化计算点相关数据
!C     Cracks_CalP_Pres(1:Max_Num_Cr,1:Max_Num_Cr_CalP)     = ZR
!C	   Cracks_CalP_Tractions(1:Max_Num_Cr,1:Max_Num_Cr_CalP,1:2)= ZR     !转换到直角坐标系下的粘聚牵引力
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
!C     MS_CalP_Propped_Aper(1:Max_Num_Cr,1:Max_Num_Cr_CalP) = ZR  !分段压裂相关的计算点数据

!支撑剂是否发生了运移
Propp_Trans_Start = .False.
!各破裂步结束对应的总的迭代次数
Counter_Num_iFrac(1:Max_Num_Frac) =0

!全部裂缝初始化为:都允许扩展: 2022-08-21. BUGFIX2022082101.
allocate(Cracks_Allow_Propa(max(Max_Num_Cr,Max_Num_Cr_3D)))
Cracks_Allow_Propa(1:size(Cracks_Allow_Propa,1)) = 1
!print *,Cracks_Allow_Propa

!Cracks_Tips_Allow_Propa(1:Max_Num_Cr,1:2)=0
!动态粘度最大允许的粘度放大系数,用于HF支撑剂运行分析
Visco_Zoom_Factor=20.0D0
!----时变粘度参数----
Viscosity_td_m    = 1.86D-2 !时变粘度参数m,Ref:深埋岩层劈裂注浆机理(default:1.86D-2)
Viscosity_td_n    = 2.5     !时变粘度参数n,Ref:深埋岩层劈裂注浆机理(default:2.5)
!初始化分段压裂相关数据
MS_Crack_Num  =0                                      !分段裂缝段数(裂缝数,最多支持20级压裂)
MS_Crack_Order(1:Max_MS_Num_Crack)         = 0
!MS_Crack_Coor(1:Max_MS_Num_Crack,1:200,1:2)= ZR   !各段裂缝坐标
MS_InP_Loc(1:Max_MS_Num_Crack,1:2)         = ZR    !各段注水点坐标
!C     !初始化地应力处理方法(用于HF的6号和8号迭代器)控制参数
!C     Key_InSitu_Method = 0
!初始化变量Key_HF_MS_Contc_Check:分段压裂是否进行含支撑剂裂缝面的接触迭代控制关键字
i_MS=0
Key_HF_MS_Contc_Check = -999
!各段压裂完成时对应的总的迭代号
MS_Finish_Counter_Iter(1:Max_MS_Num_Crack)= 0
!地应力处理算法相关.
Key_InSitu_Strategy = 0     !HF分析地应力的处理方法(仅用于HF分析的5号迭代器)
                          ! 0:不考虑初始应力问题
                          ! 1:通过线性叠加的方法处理地应力产生的位移场,即HF迭代过程中删除
                          !   地应力,计算裂缝扩展方向时考虑地应力,本方案裂尖水压设置为0
                          ! 2:通过Zienkiewicz_7ed_P216(默认)
                          ! 3:将施加的地应力屏蔽(仅仅针对水力压裂分析)
                          ! 4: 固定约束,指定初始应力或读入初始应力文件.理论:Xsite理论及验证算例中的方法.
Key_Read_Initial_Node_Stress_File = 0    !从初始应力文件(*.istn,节点应力文件)读入初始应力场数据.
Key_InStress_for_Mat   = 0          !给特定材料施加预应力
Mat_Number_of_InStress(1:100) =0    !施加预应力的材料号
Mat_InStress_x         = ZR         !x方向预应力
Mat_InStress_y         = ZR         !y方向预应力
Mat_InStress_z         = ZR         !z方向预应力

Label_number = 0
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!水力压裂HF和NF交叉相关变量的初始化
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!HF和NF相交生成的新计算裂缝对应的天然裂缝号
Cracks_NF_JiS_Cr_Num(1:Max_Num_Cr)  = 0
!HF和NF相交寄生状态
Cracks_NF_JiS_Stat(1:Max_Num_Cr,1:2) = 0
!HF和NF交汇问题相关,确定新生成裂缝的T型交叉状态(与裂尖无关)
Cracks_NF_T_Stat(1:Max_Num_Cr,1:2)   = 0
Cracks_NF_Cement_Tip_Num(1:Max_Num_Cr,1:2) =0
!用于摩擦型天然裂缝,保存各裂缝对应的摩擦天然裂缝号
Cracks_fric_NF_num(1:Max_Num_Cr)  =0
!用于摩擦型天然裂缝,用于标记每一个裂缝是否被水压裂缝侵蚀过
Cracks_QinS_Stat(1:Max_Num_Cr)  =0
!地应力
InSitu_x =ZR
InSitu_y =ZR
!单元Gauss点编号相关
!num_GP_Elem(1:Num_Elem) = 0
!Ele_GP_Start_Num(1:Num_Elem) = 0
!破裂区相关
Key_Fracture_Zone = 0          !定义矩形的破裂区域(仅允许在该范围内扩展),若不定义,则在全模型内扩展
Frac_Zone_MinX    = ZR         !破裂区的x坐标范围
Frac_Zone_MaxX    = ZR         !破裂区的x坐标范围
Frac_Zone_MinY    = ZR         !破裂区的y坐标范围
Frac_Zone_MaxY    = ZR         !破裂区的y坐标范围
Frac_Zone_MinZ    = ZR         !破裂区的y坐标范围
Frac_Zone_MaxZ    = ZR         !破裂区的y坐标范围
!随机生成天然裂缝相关
Key_Random_NaCr   = 0          !是否随机生成天然裂缝
num_Rand_Na_Crack = 0          !随机生成的天然裂缝数目
NaCr_Orientation  = ZR         !天然裂缝平均方位(度)
NaCr_Ori_Delta    = ZR         !天然裂缝方位的波动幅度(度)
NaCr_Length       = ZR         !天然裂缝的平均长度
NaCr_Len_Delta    = ZR         !天然裂缝的平均长度的波动幅度
!稀疏矩阵K最大稀疏比
Sparse_Ratio      = 0.03       !默认值为0.03
!稀疏矩中间变量(Location_COO,MASK_COO)数据存储方式,1:全硬盘(默认);2:全内存2:部分硬盘二进制文件
Sparse_Store_Method  = 1
!初始化每个裂缝的坐标点数目为0
Each_Cr_Poi_Num(1:Max_Num_Cr) = 0

!裂缝面接触
Key_Contact =0
Contact_Aper_Tol  = ZR   !接触检测容差
fric_mu_Cont      = ZR
!裂纹面接触(含支撑剂问题)的最大迭代次数 >=50
Max_Contact_Iter  = 50
!接触积分点数目(1 or 2,默认为1)
Conta_Integ_Point =1
!接触分析收敛准则(1:通过残差确定;2:通过位移确定;3:通过裂缝开度)
Key_Conta_ConCrit = 2
!罚函数接触算法相关
kn_Cont_Penalty   = 1.0D13       !接触分析罚函数法法向接触刚度
kt_Cont_Penalty   = 1.0D13       !接触分析罚函数法切向接触刚度
Conve_Tol_Penalty = 1.0D-3       !罚函数法收敛容差(0.1%)
Key_HF_Cont_Scheme= 0   !水力压裂分析接触检测方案
Key_CS_Natural_Crack =0 !考虑压剪天然裂缝罚函数处理. 2022-10-22.
Penalty_CS_Natural_Crack  = 1.0D11          !压剪型裂缝的罚参数. 默认1.0D11. 2022-12-23.
Key_Penalty_CS_Method  = 1    !=1(default)则罚函数控制l、m、n位移，=2则仅罚函数控制n方向位移，即法线方向位移. NEWFTU2023082701.

!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!场问题相关初始化(包括初始孔隙压力等)
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!Fd_kxx            = -999.0D0      !系数kxx
!Fd_kxy            = -999.0D0      !系数kxy
!Fd_kyy            = -999.0D0      !系数kyy
!初始孔隙压力
!Key_PoreP         = 1             !是否有初始孔隙水压力
Key_PoreP         = 0             !是否有初始孔隙水压力. 2023-05-07.
Initial_PoreP     = -999.0D0      !初始孔隙压的大小
Initial_Biot_Coeff= 0.75D0        !初始比奥系数. 2023-03-19.

!材料参数初始化
ALLOCATE(Material_Type(Max_Materials))
Material_Type(1:Max_Materials) = 1   !默认均采用第一种材料
ALLOCATE(Material_Para(Max_Materials,30))
Material_Para(1:Max_Materials,1:30) = ZR
ALLOCATE(Material_Para_Added(Max_Materials,30))
Material_Para_Added(1:Max_Materials,1:30) = ZR
ALLOCATE(Mat_Cartesian_Coor_Vector_x(Max_Materials,3))  !复合材料正交材料坐标系x轴向量
ALLOCATE(Mat_Cartesian_Coor_Vector_y(Max_Materials,3))  !复合材料正交材料坐标系y轴向量
ALLOCATE(Mat_cylinder_Coor_Center(Max_Materials,3))     !复合材料圆柱材料坐标系原点
ALLOCATE(Mat_cylinder_Coor_Vector_z(Max_Materials,3))   !复合材料圆柱材料坐标系z轴向量
Mat_Cartesian_Coor_Vector_x(1:Max_Materials,1:3) = ZR !复合材料正交材料坐标系x轴向量
Mat_Cartesian_Coor_Vector_y(1:Max_Materials,1:3) = ZR !复合材料正交材料坐标系y轴向量
Mat_cylinder_Coor_Center(1:Max_Materials,1:3) =ZR    !复合材料圆柱材料坐标系原点
Mat_cylinder_Coor_Vector_z(1:Max_Materials,1:3)  =ZR !复合材料圆柱材料坐标系z轴向量
GasP_Well_Nodes(1:100) = -999   !产量评估井口节点号



!Gauss点数目初始化
Num_Gauss_Points  = 64      !增强单元的高斯积分点数,默认64,不能取奇数,如9x9=81
                          !支持的Gauss点数目:16,36,64,100,144(12*12),196(14*14),400(20*20),676(26*26),900(30*30)
Num_Gauss_P_Inc   = 400     !对于Inclusion增强单元有些特殊:含材料界面的单元建议取最少400个高斯点
Num_Gau_Points_3D = 512     !3D增强单元的高斯积分点数,默认512=8*8*8
                          !支持的Gauss点数目:16,36,64,100,144(12*12),196(14*14),400(20*20),676(26*26),900(30*30)
Num_Gau_Points_3D_MC= 1000  !含3个以上裂缝的3D增强单元的高斯积分点数: 1000=10^3(default);3375=15^3;5832=18^3;8000=20^3
Num_Gau_P_SubInteg_6= 64     !3D分块积分6面体单元积分点数目. 默认为4*4*4. 2022-07-28.
Num_Gau_P_SubInteg_4= 4     !3D分块积分4面体单元积分点数目. 默认为4. 2022-07-28.
Num_Gauss_P_FEM   = 4       !传统单元的高斯积分点数,默认4
Num_Gauss_P_FEM_3D= 8       !传统3D单元的高斯积分点数,默认8=2*2*2
!动态分析相关
Num_Ivex =0
Num_Ivey =0
Num_Iacx =0
Num_Iacy =0
!各个方向上的重力加速度
!g_X_Y_Z(1:3) = ZR     !各个方向上的重力加速度
g_X_Y_Z           = [0.0D0,9.8D0,0.0D0]   !各个方向上的重力加速度
!地震相关
Key_EQ   = 0                     !是否是地震分析,若是,则需要读入地震加速度值
EQ_Ac_Time_Gap  =-999.0D0        !地震加速度数据时间间隔
num_EQ_Ac_nodes = 0              !地震加速度施加到的节点数目
EQ_Ac_nodes(1:5000) = 0          !地震加速度施加到的节点列表(最多5000个节点上)
!正弦加速度激励相关
Key_Sin_Accel       = 0          !是否激活正弦加速度激励
Sin_Accel_Dire      = 1          !正弦加速度激励的方向:=1,x;=2,y
Sin_Accel_A         = -999.0D0   !正弦加速度激励的振幅
Sin_Accel_T         = -999.0D0   !正弦加速度激励的周期
Sin_Accel_num_Nodes = 0          !正弦加速度激励的节点数目
Sin_Accel_Nodes(1:5000)= 0       !正弦加速度激励的节点列表
!节点耦合相关
num_CP_x_nodes = 0
num_CP_y_nodes = 0
CP_x_nodes(1:5000)  = 0          !要耦合的x方向自由度节点列表
CP_y_nodes(1:5000)  = 0          !要耦合的y方向自由度节点列表
num_nodes_CP_set_x(1:10)=0       !x方向耦合自由度的集合数目
num_nodes_CP_set_y(1:10)=0       !y方向耦合自由度的集合数目
CP_nodes_x(1:10,1:5000)=0        !x方向耦合自由度的各集合节点列表
CP_nodes_y(1:10,1:5000)=0        !y方向耦合自由度的各集合节点列表
!3D裂缝相关
!allocate(Crack3D_Coor(Max_Num_Cr_3D,10,3))
!Crack3D_Coor(1:Max_Num_Cr_3D,1:4,1:3) = ZR                     !裂缝号，裂缝面4个坐标点号，(x,y,z)
allocate(Crack3D_Coor(Max_Num_Cr_3D,200,3)) !BUGFIX2024022601.
Crack3D_Coor(1:Max_Num_Cr_3D,1:200,1:3) = ZR                     !裂缝号，裂缝面4个坐标点号，(x,y,z)

allocate(Crack3D_Cir_Coor(Max_Num_Cr_3D,7))
Crack3D_Cir_Coor(1:Max_Num_Cr_3D,1:7) = ZR                     !圆形初始裂缝,裂缝号,圆形坐标(x,y,z)+外法线向量+半径
allocate(Crack3D_Ellip_Coor(Max_Num_Cr_3D,8))
Crack3D_Ellip_Coor(1:Max_Num_Cr_3D,1:8) = ZR                   !椭圆初始裂缝,裂缝号,圆形坐标(x,y,z)+外法线向量+半径a+半径b
!Crack3D_Meshed_Node(1:Max_Num_Cr_3D,1:Max_N_Node_3D,1:3)=ZR             !离散化之后的3D裂缝节点坐标,每个裂缝最多由1000个点组成
!Crack3D_Meshed_Ele(1:Max_Num_Cr_3D,1:Max_N_Node_3D,1:3) =0              !离散化之后的3D裂缝单元编号,每个裂缝最多由1000个点组成
!Crack3D_Meshed_Ele_Attri(1:Max_Num_Cr_3D,1:Max_N_Node_3D,1:5) = ZR      !离散化之后的3D裂缝单元特性参数(周长、面积等)
!Cr3D_Meshed_Node_in_Ele_Num(1:Max_Num_Cr_3D,1:Max_N_Node_3D)=0          !离散化之后的3D裂缝节点所在单元号
!Cr3D_Meshed_Node_in_Ele_Local(1:Max_Num_Cr_3D,1:Max_N_Node_3D,1:3) =ZR         !离散化之后的3D裂缝节点所在单元号的局部坐标
allocate(Crack3D_Meshed_Node_num(Max_Num_Cr_3D))
Crack3D_Meshed_Node_num(1:Max_Num_Cr_3D)                       =0       !离散化之后的3D裂缝节点数目
!Crack3D_Meshed_Ele(1:Max_Num_Cr_3D,1:Max_N_Node_3D,1:3)        =0       !离散化之后的3D裂缝单元编号,每个裂缝最多由1000个点组成
allocate(Crack3D_Meshed_Ele_num(Max_Num_Cr_3D))
Crack3D_Meshed_Ele_num(1:Max_Num_Cr_3D)                        =0       !离散化之后的3D裂缝单元数目
!Crack3D_Meshed_Outline(1:Max_Num_Cr_3D,1:Max_N_Node_3D,1:2)    =0       !离散化之后的3D裂缝外边界
!Crack3D_Meshed_Outline(1:Max_Num_Cr_3D,1:Max_N_Node_3D,4)      =1       !数据4用于标记该边界线的两个点是否允许扩展,扩展非常小的步长(2021-08-20)
!Crack3D_Meshed_Outline_num(1:Max_Num_Cr_3D)                    =0       !离散化之后的3D裂缝外边界线条数
!C     Crack3D_Meshed_Outline_Grow_From(1:Max_Num_Cr_3D,
!C    &                                 1:Max_N_Node_3D)              =0       !用于标记3D裂缝面边界顶点从哪个点扩展而来. NEWFTU2022071301.
!C     Crack3D_Meshed_Outline_Vertex(1:Max_Num_Cr_3D,
!C    &                                          1:Max_N_Node_3D,1:3)=ZR        !离散化之后的3D裂缝外边界顶点坐标
!C     Crack3D_Meshed_Outline_Vertex_Ele_num(1:Max_Num_Cr_3D,
!C    &                                          1:Max_N_Node_3D) =0       !离散化之后的3D裂缝外边界顶点所在固体单元号
!
Crack_Pressure(1:Max_Num_Cr) = ZR                             !缝内压力(最多100条裂缝)
!------
Crack_Pressure_Type = 1                                           !流体压力类型:=1,固定压力;=2,压力自动调整,使得裂缝正好扩展
!Cracks_FluidEle_CalP_3D(1:Max_Num_Cr_3D,1:Max_N_CalP_3D,1:7) =0  !每条裂缝流体单元计算点编号
!Cracks_FluidEle_num_CalP_3D(1:Max_Num_Cr_3D,1:Max_N_CalP_3D)=0    !每条裂缝流体单元的计算点数目
!Cracks_FluidEle_num_3D(1:Max_Num_Cr_3D) =0                        !每条裂缝流体单元的数目
!Cracks_CalP_Coors_3D(1:Max_Num_Cr_3D,1:Max_N_CalP_3D,1:3)  = ZR
!Cracks_CalP_Orient_3D(1:Max_Num_Cr_3D,1:Max_N_CalP_3D,1:3) = ZR
!Cracks_CalP_MeshedEl_3D(1:Max_Num_Cr_3D,1:Max_N_CalP_3D)   = 0
!Cracks_CalP_Elem_3D(1:Max_Num_Cr_3D,1:Max_N_CalP_3D,1:2)   = 0
allocate(Cracks_CalP_Num_3D(Max_Num_Cr_3D))
Cracks_CalP_Num_3D(1:Max_Num_Cr_3D)                        = 0
!每个破裂步执行的接触迭代数目(默认仅在第一个流固耦合迭代步执行接触迭代,即=1)
Key_HF_num_Contact= 1
!动态分析相关
IDy_Num_Iteras    = 0               !隐式动态分析总步数
IDy_Num_force_Itr = 0
Factor_Prop_Dy    = 1.63D0
Key_Mass_Lumped   = 0               !是否采用集中质量矩阵(对角质量矩阵,默认关闭)
!分子动力学分析相关,分析类型号:21
MD_num_molecule   = -999            !分子的数目
MD_mss_molecule   = -999.0D0        !分子的质量
MD_num_time_step  = -999            !时间步数
MD_Delt_Time      = -999.0D0        !时间步长
MD_Dimension_x    = -999.0D0        !运动区域x方向尺寸
MD_Dimension_y    = -999.0D0        !运动区域y方向尺寸
MD_Dimension_z    = -999.0D0        !运动区域z方向尺寸
MD_step_print_num = 10              !屏幕显示间隔计算步数
MD_step_save_num  = 10              !数据保存间隔计算步数
!塑性分析相关
Key_Plasticity    = 0
!粘聚裂缝相关
Coh_Constitutive_type   = -999       !Constitutive type of the cohesive crack.
Coh_Width_Critical1     = -999.0D0   !极限拉伸开度1,此开度对应极限牵引力,Coh_Constitutive_type=1时可用
Coh_Width_Critical2     = -999.0D0   !极限拉伸开度2,此开度下牵引力为0
Coh_f_Ultimate          = -999.0D0   !最大牵引力,裂尖的牵引力
Coh_Tangential_Key      = -999       !是否考虑切向牵引力,若考虑,应给出对应的参数
Coh_Width_Critical1_T   = -999.0D0   !极限拉伸开度1,此开度对应极限切向牵引力,Coh_Constitutive_type=1时可用
Coh_Width_Critical2_T   = -999.0D0   !极限拉伸开度2,此开度下切向牵引力为0
Coh_f_Ultimate_T        = -999.0D0   !最大牵引力,裂尖的切向牵引力
Key_Save_f_d_Curve      = -999
f_d_Curve_node_num      = -999
Key_Save_f_COD_Curve    = 0          !导出给定裂缝的载荷-裂缝张开位移(COD)曲线,保存到fccu文件中.
f_COD_Curve_Crack_Num   = 1          !载荷-裂缝张开位移(COD)曲线裂缝号.
Cracks_Coh_Ele_Type(1:Max_Num_Cr,1:Max_Num_Cr_CalP-1) =0   !每条裂缝粘聚单元的类型,=1表示为粘聚单元，=0为一般单元
Cracks_Tip_Num_of_Coh_Ele(1:Max_Num_Cr,1:2)  =0  !每条裂缝两个裂尖对应的粘聚单元数目(仅为数目,而不是编号)
Max_Cohesive_Iter       = 50         !粘聚裂缝的最大迭代次数 >=50
Coh_Integ_Point         = 2          !粘聚裂缝积分点数目(1或2)
Key_Coh_ConCrit         = 1          !粘聚裂缝迭代收敛准则(1:通过残差确定;2:通过位移确定)
Key_Save_f_d_Curve      = 0
!其他
Key_Close_Window        = 0          !计算结束后是否自动关闭窗口
Key_Play_Sounds         = 0          !是否播放提示音
Key_Memo_Moni           = 0          !是否监控内存消耗量
Key_Window_Log          = 0          !保存窗口记录
Key_Clear_All           = 1          !计算开始时删除数据

!Key_OpenMP              = 0          !Close openmp
Key_Num_Process         = 1          !默认单线程
Key_Data_Format         = 1          !Save data with format
Key_XA                  = 0          !新奥相关. 2022-09-10.
Key_Integral_Sol        = 2          !默认积分算法
Num_Sub_Quads           = 16         !子四边形剖分四边形数目(Key_Integral_Sol= 3时设置),默认16,不能取奇数
Num_Sub_3D_Cubes        = 125        !3D增强单元的立方体分块数(Key_Integral_Sol= 3时设置),默认125=5^3
                                   !(8=2^3,27=3^3,64=4^3,125=5^3,216=6^3,343=7^3,512=8^3,729=9^3,1000=10^3)
Num_Gau_Points_3D_Cube  = 8          !3D子立方体剖分分块高斯积分点数目（默认为8）
Seed                    = 123456789  !用于生成随机数的种子
!程序内部关键控制参数(不可改动)
Key_Heaviside_Value    = -1          !Value keyword of Heaviside enrichmenet function:-1 (1 and -1) or 0 (1 and 0)
Key_Hole_Value         =  0          !Value keyword of Hole enrichmenet function:-1 (1 and -1) or 0 (1 and 0)
Key_Visit_PhiPsi_top   =  0
Key_Cond_Number        =  0          !计算并显示条件数,仅用于Lapack求解器,默认关闭
Key_Determinant        =  0          !计算并显示刚度矩阵K的行列式(不支持稀疏矩阵求解器，不支持EBE-PCG求解器),默认关闭
Key_Eigenvalue         =  0          !计算并显示刚度矩阵K的特征值(不支持稀疏矩阵求解器，不支持EBE-PCG求解器),默认关闭
Key_BLAS               =  0          !使用BLAS库(对于Intel Fortran可自动并行),但对结果有影响. 默认为0. 使用BLAS的相关子程序:
                                   !EBE_XFEM_PCG_3D_with_K.f90
!材料参数相关
Material_Interface(1:2) = ZR              !夹杂的界面材料参数:1-St,2-KIc
!非线性分析相关(2018-01-19)
NL_TIMS(1,1:5) = [ZR,ONE,ZP1,ZR,ONE]      !每行代表一个载荷步设置,各列(起始时间,结束时间,时间增量,初始载荷因子,终止载荷因子)
NL_TIMS(2:1000,1:5) = ZR
NL_ITRA        = 30                       !N-R最大迭代数目
NL_ATOL        = 1.0D8                    !允许的最大残差的norm2模
NL_NTOL        = 6                        !允许的最大载荷二分数目
NL_TOL         = 1.0D-6                   !N-R迭代收敛容差(1.0D-6)
!NL_NLOAD       = 1                       !载荷步数
!HYPLAS非线性分析相关(2021-07-19)
!NL_Time_Steps(1,1:5) = [1.0D0,1.0D0,1.0D-6,20.0D0,0.0D0]  !每行代表:(1)时间增量、(2)载荷因子增量、(3)收敛容差、(4)最大迭代次数、(5)Blank
!NL_Time_Steps(2:1000,1:5) = ZR

!场问题相关
Key_Fd_Body_Source = 0
!损伤材料标记
Key_Damage =0
Material_Dam2Frac_Value   = TWO           !损伤形成裂缝的极限损伤值(损伤超过该值即生成裂缝)
Crack_Gen_from_Damage =0                  !损伤生成的裂缝数目

!非零位移边界条件相关
Num_Boux_nonzero = 0
Num_Bouy_nonzero = 0
Num_Bouz_nonzero = 0
penalty_k_bou_nonzero = 1.0D15            !非零位移边界条件罚函数法的罚参数

!生死单元
Key_EKILL  = 0
Ele_Killed_Each_Load_Step(1:Max_Step,1:1000)  =0  !每个载荷步杀死的单元编号,最多支持1000个单元
EKILL_Weaken_Factor = 1.0D-6              !生死单元弱化系数

!稀疏矩阵存储K
Key_K_Sparse =0

!热应力
Key_Thermal_Stress =0
Key_Scheme_Thermal_Stress = 1    !用于设定温度应力计算方法. = 1，根据绝对温度计算热应力; = 2，根据温差计算热应力. 2023-03-13.
Key_Initial_Temperature   = 1    !初始温度给定方法. =1, 根据材料号. =2, 从文件读入（暂时不可用）. 2023-03-13.
Thermal_Str_Temper(1:100) = ZR     !绝对温度

!定义一个矩阵，存储12条棱边的节点编号Ele_3D_Edges_Node(12,2),2022-04-14
!局部编号
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

!其他
file_Sparse_K_Location_COO_bin = .False.  !用于标记Sparse_K_Location_COO.bin二进制文件是否存在
file_Sparse_K_Mask_COO_bin     = .False.  !用于标记Sparse_K_Mask_COO.bin二进制文件是否存在
Key_Schollmann_Twist = 0                  !Schollmann’s criterion准则是否考虑裂缝面的扭曲
Key_Ave_Stress    = 1                     !加权平均应力的计算方法: =1, SHI Fang's Formation; =2, Ordinary Formation; =3, single point
S1_Point_Factor   = 0.2D0                 !加权应力计算点移动系数(偏离裂缝形心往外移动; defaut to 0.2)
num_Gauss_S1      = 216                   !加权平均主拉应力计算单元高斯点数目(default to 6^3=216)
Key_Adj_Prp_Step_3D = 0                   !3D裂缝扩展控制参数(default:0); =1则根据应力或应力强度因子控制裂缝扩展步长的实际大小
Prp_3D_Factor_m     = 0.3333D0            !delta_l  = delta_L_Max*(S1/St)^m公式中的系数m(default:1/3). 该参数的影响见\changelog.src\2022-05-09-01.webp
!Adj_Prp_Step_3D_Max = 3.0D0               !3D裂缝扩展控制参数(default:3.0),Key_Adj_Prp_Step_3D = 1时起作用,3D裂缝扩展最大步长系数.
Adj_Prp_Step_3D_Max = 1.5D0               !3D裂缝扩展控制参数(default:1.5),Key_Adj_Prp_Step_3D = 1时起作用,3D裂缝扩展最大步长系数.IMPROV2024022701.
!Prp_Bisec_Factor_3D = 1.5D0              !3D裂缝扩展控制参数:边界剖分系数,若3D裂缝离散单元边界长度>Prp_Bisec_Factor_3D*Ave_Elem_L_Enrich,则二分
Prp_Bisec_Factor_3D = 2.0D0               !2022-09-28.
Key_Smooth_Front    = 0                   !3D裂缝扩展控制参数:(default:0),>=1则对裂缝前缘进行光滑处理
Key_Smooth_Front_Twice =0                 !=1时则执行两次Smooth操作，然后两次处理后的坐标取平均值
!Key_Smooth_Pressure_Curve = 0             !压力曲线光滑处理,2次光滑处理. 2022-10-14.
Key_3D_FluEle_Triang= 1                   !对3D流体单元进行拆分,确保每个流体单元都是三角形单元(default: 1)
Key_Crack_Inner_Pressure = 0              !默认无缝内流体压力
Key_Multi_TipEnrNode=0                    !允许多个裂尖增强节点(Key_TipEnrich_Radius_Factor确定的圆内的全部节点都进行裂尖增强)
Key_Junction_Enrich         =  0     !是否开启Junction增强(默认为0)，适用于3D. 2022-08-25.
Key_TipEnrich_Radius_Factor =  2.0D0      !Key_Multi_TipEnrNode=1时起作用,Key_TipEnrich_Radius_Factor确定的圆内的全部节点都进行裂尖增强
Num_Check_T_Matrix   = 180                !Tool_ThetaX_ThetaY_ThetaZ_3D_rotation.f遍历法计算局部坐标系转角的划分份数(默认等于180;越小计算越快)
Key_Pre_Conditioner  =0                   !激活Pre_Conditioner,用于减小刚度矩阵的条件数，从而降低方程组病态特性、减小计算量;目前适用于3D
Lis_Number_Iteration = 5000               !Lis求解器的迭代次数(默认5000)
MDOF_2D              = 200                !程序内部变量,Max_number_of_DOFs, 2D问题单元刚度矩阵最大自由度数目(default to 200;勿改动).
MDOF_3D              = 156                !程序内部变量,Max_number_of_DOFs, 3D问题单元刚度矩阵最大自由度数目(default to 156;勿改动).
Key_Initiation       = 0                  !默认不生成初始裂缝
Key_Ini_Rule         = 1                  !生成初始裂缝遵循的准则:=1,最大拉应力准则(默认)
Key_FS_Seque_Coupling= 0                  !Sequential coupling of solid and field problems (only for 2D problem and Key_Analysis_Type = 1)
Key_TS_Seque_Coupling= 0                  !热固耦合计算(2D静态+瞬态温度场计算) (only for 2D problem AND key_analysis_type = 1), date: 2021-11-03
Key_SIFs_DIM_Points  = 2                  !位移插值法计算应力强度因子的提取点的数目(2或3,默认为2)
Key_SIFs_DIM_Method  = 1                  !=1,根据平面应力公式;=2,根据平面应变公式. 2023-03-19.
Factor_Propagation   = 1.5                !关键字：裂纹扩展步长系数,default to 1.5
Propagation_Length   = -99.0D0            !裂缝扩展长度,若给定该参数且>0,则Factor_Propagation不起作用,default to -99.0
Key_Local_Mesh_Refine= 0                  !网格局部优化, =0,关闭；=1，加密全部增强节点；=2，仅加密裂尖增强节点
Key_Large_Deform     = 0                  !开启大变形(1), 仅在Key_Analysis_Type = 8时有效
Key_Front_Segmentation = 0                !Allow fracture front segmentation (Avaliable only for 3D)
Number_front_Segments  =2                 !Number of Segments(default:2).
Key_Adj_Ini_Crack_3D = 0                  !Adjust initail crack. For 3D only. default: 0.
Key_Check_and_Adjust_Cracks_3D =  0       !根据各节点的符号距离调整裂缝面坐标. 2022-08-01.
Adjust_Cracks_Delta_Angle   =  45.0D0     !方位调整分辨率. Key_Check_and_Adjust_Cracks_3D =  2和3时用到. 90.0D0, 60.0D0, 45.0D0, 30.0D0, 22.5D0
Adjust_Cracks_Resolution    =  6          !方位调整分辨率. 默认为6.
num_Suspended_Point  = 0                  !用于Fracture Front Segmentation,历史的抑制的点的数目(2021-08-20)
key_tip_fluid_element_3d = 1              !允许3D裂尖单元附近激活流体单元(接触单元)
allocate(Suspended_Points(1000,3))
Suspended_Points(1:1000,1:3)=-TEN_15      !用于Fracture Front Segmentation,历史的抑制的点的坐标(2021-08-20)
Flag_Local_Refined   = .false.            !用于标记是否已经局部加密
Ave_Elem_L_Enrich_Unlocalrefined = ZR     !局部加密前的增强单元特征长度(2021-08-22)
Key_Ini_Cr_3D_Type   =1                   !生成3D初始裂缝的类型(仅在3D问题时需要):=1,圆形;=2,矩形(暂时不可用)
Picard_Alpha         = 0.25D0             !水力压裂Picard迭代系数(3D水力压裂)(2021-12-05)
Picard_Tol           = 0.01               !水力压裂Picard迭代收敛容差(0.01)(2021-12-05)
Max_Picard_Iter      = 50                 !水力压裂Picard迭代次数(50)
Key_Block_Model      = 0                  !是否为块体模型
Num_Elem_Block_Bou   = 0                  !块体模型边界上的单元数目
Key_InPlane_Growth   = 0                  !用于3D XFEM，激活后裂缝仅在原平面内扩展(2022-04-18).
Key_Stop_Outside_Crack =0                 !一旦裂缝扩展到模型外部，即标记为不再扩展(2022-10-02). NEWFTU2022100201.
!---Wellbore井筒相关(2022-04-19)----
Key_HF_Multistage_3D = 0                  !是否是3D分段压裂分析
num_Wellbore         = 0                  !井筒数目，最多5个井筒，默认0个
num_Points_WB(1:20)  = 2                  !每个井筒的坐标点数目,最多20个井筒
Wellbore_Coors(1:20,1:20,1:3) = ZR        !井筒的点的坐标,每个井筒最多20个点
num_Stages_Wellbores(1:20)    = 1         !每个井筒的分段数
num_Crs_Stages_Wellbores(1:20,1:20) = 1!每个井筒每个分段的分段簇数
Key_Gen_Ini_Crack_Wellbores   = 0         !是否自动生成初始裂缝（注:自动生成的初始裂缝垂直于井筒）
Size_Ini_Crack_Wellbores      = 5.0D0     !自动生成的初始裂缝尺寸(正方形)
Num_Poly_Edges_NaCr_WB        = 6         !多边形初始裂缝的边数

Wellbores_Start_Point(1:20,1:3) = -TEN_15 !井筒上分段压裂的起点坐标和终点坐标(注:由于裂缝在各段均匀分布，所以初始裂缝不一定在端点位置)
Wellbores_End_Point(1:20,1:3)   = -TEN_15 !井筒上分段压裂的起点坐标和终点坐标(注:由于裂缝在各段均匀分布，所以初始裂缝不一定在端点位置)
Injection_Q_Stages_Wellbores(1:20,1:20)= -TEN_15       !每个井筒每个分段的压裂液流量,单位：m^3/s
Injection_T_Stages_Wellbores(1:20,1:20)= -TEN_15       !每个井筒每个分段的压裂液注入时间,单位：s
!----3D裂缝前缘S1或K光滑处理-(2022-04-25)----
Key_Denoise_Vertex_Value =0               !3D裂缝前缘S1或K等数据进行去噪操作.
Key_Smooth_Vertex_Value  =0               !对3D裂缝前缘S1或K等数据进行光滑处理, =1, 滑动平均法,把前后时刻的一共2n+1个值做平均.
Smooth_Vertex_n          =4               !Key_Smooth_Vertex_Method= 1时用到. 滑动平均法的n. 默认值为4.
Key_Smooth_Vertex_Value2 =0               !对3D裂缝前缘S1或K等数据进行光滑处理（二次处理）, =1, 滑动平均法,把前后时刻的一共2n+1个值做平均.
Smooth_Vertex_n2         =3               !Key_Smooth_Vertex_Method= 1时用到（二次处理）. 滑动平均法的n.
!-------3D裂缝前缘Theta光滑处理（CFCP=3时可用)-------Added on 2022-07-14. （后放弃）
Key_Denoise_Theta_Value= 0 !3D裂缝前缘S1或K等数据进行去噪操作. 默认值为0.
Key_Smooth_Theta_Value = 0 !对3D裂缝前缘S1或K等数据进行光滑处理. 默认值为0.!=1, 滑动平均法,把前后时刻的一共2n+1个值做平均.
Smooth_Theta_n         = 2 !Key_Smooth_Vertex_Method= 1时用到. 滑动平均法的n. 默认值为4.
Key_Smooth_Theta_Value2= 0 !对3D裂缝前缘S1或K等数据进行光滑处理(二次处理). 默认值为0.!=1, 滑动平均法,把前后时刻的一共2n+1个值做平均.
Smooth_Theta_n2        = 2 !二次处理:Key_Smooth_Vertex_Method= 1时用到. 滑动平均法的n. 默认值为3.
!-------3D裂缝前缘GrowthFactor光滑处理（CFCP=3时可用)-------Added on 2022-07-14.
Key_Denoise_GF_Value= 0 !3D裂缝前缘S1或K等数据进行去噪操作. 默认值为0.
Key_Smooth_GF_Value = 0 !对3D裂缝前缘S1或K等数据进行光滑处理. 默认值为0.!=1, 滑动平均法,把前后时刻的一共2n+1个值做平均.
Smooth_GF_n         = 2 !Key_Smooth_Vertex_Method= 1时用到. 滑动平均法的n. 默认值为4.
Key_Smooth_GF_Value2= 0 !对3D裂缝前缘S1或K等数据进行光滑处理(二次处理). 默认值为0.!=1, 滑动平均法,把前后时刻的一共2n+1个值做平均.
Smooth_GF_n2        = 2 !二次处理:Key_Smooth_Vertex_Method= 1时用到. 滑动平均法的n. 默认值为3.
!----压剪裂缝相关----
Key_CS_Crack(1:Max_Num_Cr) = 0            !初始化为非压剪裂缝
allocate(Cracks_Initial_Meshed(Max_Num_Cr_3D))
Cracks_Initial_Meshed(1:Max_Num_Cr_3D) =0         !用于标记每个裂缝是否被初始化离散过了.
allocate(Cracks_Initial_Adjusted(Max_Num_Cr_3D))
Cracks_Initial_Adjusted(1:Max_Num_Cr_3D)=0        !用于标记每个裂缝是否被调整过了.
allocate(Cracks_Checked_and_Adjusted(Max_Num_Cr_3D))
Cracks_Checked_and_Adjusted(1:Max_Num_Cr_3D)=0    !用于标记每个裂缝是否被检查且调整过了. 2022-08-01.

Key_EBE_Precondition      = 1                     !EBE求解器预处理关键字.
Key_EBE_Condition_Number  = 0                     !=1时则计算每个增强单元的条件数，存入全局变量EBE_Condition_Number(Num_Elem). NEWFTU2022070901.
Key_EBE_Sym_Storage_K     = 0                     !对称存储单元刚度矩阵storK_FEM. 2022-11-10. NEWFTU2022111001.
!用于标记裂缝类型和裂缝状态. 2022-06-02.
allocate(Crack_Type_Status_3D(Max_Num_Cr_3D,10))
Crack_Type_Status_3D(1:Max_Num_Cr_3D,1)=1   !第1列(裂缝类型)：               =1，HF裂缝；=2，天然裂缝
Crack_Type_Status_3D(1:Max_Num_Cr_3D,2)=1   !第2列(裂缝状态)：               =1，HF裂缝压裂未完成；=2，HF裂缝压裂完毕
Crack_Type_Status_3D(1:Max_Num_Cr_3D,3)=1   !第3列(裂缝能否继续扩展)：       =1，能；=0；不能
Crack_Type_Status_3D(1:Max_Num_Cr_3D,4)=0   !第4列(裂缝是否已获得流体节点):  =1，是; =0; 否
Crack_Type_Status_3D(1:Max_Num_Cr_3D,5)=0   !第5列(上一步裂缝是否发生了扩展):=1，是; =0; 否
Flag_HF_3D = 0                              !3D水力压裂分析标记(包含常水压)
Key_Cpp_Call_Fortran_Lib = 0                !2023-03-23.

!以下变量用于快速定义初始应力场（不分区域）. 2022-06-03.
InSitu_S1_3D = ZR
InSitu_S2_3D = ZR
InSitu_S3_3D = ZR
InSitu_S1_nv_3D = [1.0D0,0.0D0,0.0D0]
InSitu_S2_nv_3D = [0.0D0,1.0D0,0.0D0]
InSitu_S3_nv_3D = [0.0D0,0.0D0,1.0D0]
!非均匀初始应力场(x,y,z方向). 2022-07-06. NEWFTU2022070601.
Key_Nonuniform_InSitu_X_with_Z          = 0              !X方向给定不均匀初始应力场.
InSitu_Sx_3D_Seg_Strs_X_with_Z(1:100)   = ZR             !分成n段.
InSitu_Sx_3D_Seg_Loca_X_with_Z(1:100)   = ZR             !坐标位置n+1.
Key_Nonuniform_InSitu_X_with_Y          = 0              !X方向给定不均匀初始应力场.
InSitu_Sx_3D_Seg_Strs_X_with_Y(1:100)   = ZR             !分成n段.
InSitu_Sx_3D_Seg_Loca_X_with_Y(1:100)   = ZR             !坐标位置n+1.
Key_Nonuniform_InSitu_Y_with_Z          = 0              !X方向给定不均匀初始应力场.
InSitu_Sy_3D_Seg_Strs_Y_with_Z(1:100)   = ZR             !分成n段.
InSitu_Sy_3D_Seg_Loca_Y_with_Z(1:100)   = ZR             !坐标位置n+1.
Key_Nonuniform_InSitu_Y_with_X          = 0              !X方向给定不均匀初始应力场.
InSitu_Sy_3D_Seg_Strs_Y_with_X(1:100)   = ZR             !分成n段.
InSitu_Sy_3D_Seg_Loca_Y_with_X(1:100)   = ZR             !坐标位置n+1.
Key_Nonuniform_InSitu_Z_with_X          = 0              !X方向给定不均匀初始应力场.
InSitu_Sz_3D_Seg_Strs_Z_with_X(1:100)   = ZR             !分成n段.
InSitu_Sz_3D_Seg_Loca_Z_with_X(1:100)   = ZR             !坐标位置n+1.
Key_Nonuniform_InSitu_Z_with_Y          = 0              !X方向给定不均匀初始应力场.
InSitu_Sz_3D_Seg_Strs_Z_with_Y(1:100)   = ZR             !分成n段.
InSitu_Sz_3D_Seg_Loca_Z_with_Y(1:100)   = ZR             !坐标位置n+1.
allocate(Cracks_FluidEle_CalP_Glo_Insitu(Max_Num_Cr_3D*Max_Max_N_CalP_3D))
Cracks_FluidEle_CalP_Glo_Insitu(:)  = ZR    !全局编号流体节点的地应力(垂直于裂缝面方向), 2022-06-04.
!3D天然裂缝相关
Key_NaCr_Type_3D       = 1                  !初始天然裂缝类型:=1,矩形;=2,圆形
Num_Poly_Edges_NaCr    = 6                  !多边形初始天然裂缝的边数，默认为6
Key_NaCr_Cross         = 0                  !是否允许初始裂缝交叉(默认为0,不允许)
Key_NaCr_Growth        = 0                  !是否允许初始天然裂缝扩展(被HF沟通前)(默认为0,不允许)
NaCr_3D_n_Vector(1:3)  = [ONE,ZR,ZR]        !初始裂缝的法线方向
NaCr_3D_n_Vector_Delta = ZR                 !法线方向的波动幅度(单位为度,默认为0)
NaCr_3D_Size           = -999.0D0           !对于矩形裂缝指的是边长,对于圆形裂缝指的是半径
NaCr_3D_Sz_Delta       = ZR                 !尺寸的波动幅度(默认为0)
NaCr_3D_Rect_Longside_Vector = ZR           !狭长矩形的长边方向向量.
NaCr_3D_Rect_L         = -999.0D0           !狭长矩形的长边长度.
NaCr_3D_Rect_W         = -999.0D0           !狭长矩形的短边长度.
NaCr_3D_Rect_L_Delta   = ZR                 !狭长矩形的长边长度波动幅度.
NaCr_3D_Rect_W_Delta   = ZR                 !狭长矩形的短边长度波动幅度.
NaCr_3D_Rect_Longside_Vector_Delta = ZR     !狭长矩形的长边方向向量波动幅度(单位为度,默认为0).
num_XA_Input_Cracks    = 0                  !新奥传入的天然裂缝数目. 2023-03-23.
XA_Min_Frac_Radius     = ZR                 !天然裂缝半径阈值. 2023-03-23.
!3D裂缝交叉状态
!allocate(Cracks_3D_Inter_Status(Max_Num_Cr_3D,Max_Num_Cr_3D) )
!Cracks_3D_Inter_Status(1:Max_Num_Cr_3D,1:Max_Num_Cr_3D) = 0
!-------3D天然裂缝激活算法（用于3D）-----------2023-01-07
Key_NaCr_Active_Scheme_3D  =  1  !用于控制3D天然裂缝激活算法.
Size_Factor_of_Active_NaCr = 6.0D0 !被沟通的天然裂缝的直径(Size_Factor_of_Active_NaCr*交点所在单元的特征尺寸),Key_NaCr_Active_Scheme_3D = 3时用到. 2023-01-11.
KIc_NaCr                   = ZR     !天然裂缝的断裂韧度. 2023-01-12.
St_NaCr                    = ZR     !天然裂缝的抗拉强度. 2024-02-22.
Key_CFCP_3_Type   = 1                      !CFCP=3准则裂缝扩展行为, 1: 大于断裂韧度KIc才能扩展(默认)
                                           !                        2: 大于零即可扩展(Ref: Tang_2019_Analysis of stress interference among_Eq.16)
Key_3D_Cr_Update_Scheme = 2                !3D裂缝面更新算法(NEWFTU2022071201), 1: 不论是否扩展，都扩展一个步长，对于不扩展的裂尖，增加一个微小量.
                                           !                                    2: 非扩展裂尖不扩展，扩展很小步长的裂尖更新坐标(default).
Schollm_Max_Theta = 55.0D0                 !允许的3D Schollmann’s criterion的最大Theta偏转角(单位为度,建议小于75,默认55). NEWFTU2022071001.

!内存优化参差数组相关. 2022-09-04.
!Solid_El_Arrays_Objects_Created  = .False.        !逻辑变量，用于标记Solid_El相关参差数组是否已经生成对象
!Crack3D_Meshed_Arrays_Objects_Created = .False.         !逻辑变量，用于标记Crack3D_Meshed相关参差数组是否已经生成对象

!面载荷相关.
Num_Surface_Loads = 0         !面载荷数目. 2023-01-21.
Surface_Pressure(1:100) = ZR  !面载荷大小. 2023-01-21.

!其他参数分配内存. 2022-09-04.
allocate(Cracks_FluidEle_num_3D(Max_Num_Cr_3D))
Cracks_FluidEle_num_3D(1:Max_Num_Cr_3D) = 0
allocate(Cracks_Real_CalP_Num_3D(Max_Num_Cr_3D))
Cracks_Real_CalP_Num_3D(1:Max_Num_Cr_3D)= 0
allocate(Cracks_Volume(Max_Num_Cr_3D))                    !每条裂缝的体积
Cracks_Volume     = ZR
allocate(Cracks_Volume_Old(Max_Num_Cr_3D))                !每条裂缝的体积(上一步的)
Cracks_Volume_Old = ZR
allocate(Cracks_FluidEle_CalP_Glo_Info(Max_Num_Cr_3D*Max_Max_N_CalP_3D,3))
Cracks_FluidEle_CalP_Glo_Info(1:Max_Num_Cr_3D*Max_Max_N_CalP_3D,1:3)  = 0
allocate(Crack3D_Centroid(Max_Num_Cr_3D,3))
Crack3D_Centroid(1:Max_Num_Cr_3D,1:3) = ZR
allocate(Crack3D_Meshed_Outline_num(Max_Num_Cr_3D))
Crack3D_Meshed_Outline_num(1:Max_Num_Cr_3D) = 0
allocate(KI_3D(Max_Num_Cr_3D))    !参差数组对象
allocate(KII_3D(Max_Num_Cr_3D))    !参差数组对象
allocate(KIII_3D(Max_Num_Cr_3D))    !参差数组对象
allocate(KI_eq_3D(Max_Num_Cr_3D)) !参差数组对象

!-----------------------------------------------------------------------
!2022-10-09. IMPROV2022100901. 以下是原来在PhiPsi_Read_Input.f在的变量.
!-----------------------------------------------------------------------
Key_Junction_Check= 1
Key_Propa_Type    = 1       !扩展类型:=1,固定步长扩展;=2,实际步长扩展(用于疲劳分析和动态分析,自动逻辑修正)
Max_MNR_Iter      = 30        !总的迭代次数,30                     (++++++++++++++++)
Max_Num_Lnsrch    = 20       !每个NR迭代步最多执行的线搜索和回溯次数
Prop_Angle_Alowed = 180.0D0   !允许的裂缝扩展偏转角(0到180之间的数)
Key_Unit_System = 1         !单位制为国际单位制
k_tt = ZR                !切线刚度,1.0D15
k_nn = 1.0D16               !法向刚度,1.0D15
a_Ave_Shi         = 5.0     !我提出的加权平均计算算法中的参数a(1-100,越大越趋于平均)
Factor_Ave_R      = 0.7     !加权平均主拉应力计算区域半径系数(0.1-1.5)
Key_HF_Multistage = 0       !暂不支持分段压裂
Delta_Factor_SIFs = ZP1   !位移插值法计算应力强度因子时从裂尖往后偏置的量,dela_L = factor_SIFs_DIM*平均单元长度
Key_Tip_Pres_Zero = 1       !对于水力压裂分析而言,是否设置裂尖水压为0(默认裂尖水压为零:Key_Tip_Pres_Zero=1)
Key_HF_Del_Ne_Pres= 0
Coff_a_AMPT       = ZP1   !关键字：加权平均最大主拉应力准则控制参数
Water_Pressure    = 1.0D6   !分析类型4的均匀水压大小
Key_Leakoff       = 0       !是否考虑泄露
Coeff_Leak        = 1.0D-5  !泄露系数1.0D-5
Key_IniPre_PassOn = 0       !各个破裂步初始迭代时是否继承上一破裂步最后的水压、开度等
                          !                    0:不继承,total_time置零,裂缝开度从0开始迭代(默认)
                          !                    1:继承,思路详见我的笔记,V3_P78(不支持4号PNR迭代方法)
Key_Cal_deltaTime = 1       !线性HF单元计算时间增量的准则(二次单元不需要)
                          !          1:矩形法(default),delta_V=delta_L*delta_w1;
                          !            算出的delta_V偏小,于是delta_Time偏小;
                          !            Picard迭代必须采用矩形法,否则算出来的水压是线性下降到0的
                          !          2:梯形法,delta_L*0.5*(w1+w2);
                          !            算出的delta_V偏小,于是delta_Time偏大
SOR_factor        = 0.75D0  !NR迭代和割线法迭代逐次超松弛迭代系数，默认值为0.75
Key_Visco_Type    = 1       !压裂液粘度计算方式, 1: 粘度保持不变(静态粘度)
                          !                    2: 粘度随支撑剂的浓度变化(动态粘度)
Viscosity_Par_m   = TWO   !动态粘度指数m(1< m <3).
Max_c             = ZP6                  !允许的最大浓度
Factor_Check_Dis  = 3.0     !内部参数:裂缝交叉检测长度系数(很关键,太短了容易引起计算bug,如V5-P50)
Factor_L_Jun_NewCr= 5.23    !HF和NF相遇后形成的新裂缝的长度系数
Key_Kink_Point    = 1       !是否给拐点划分计算点(默认考虑,即Key_Kink_Point=1)
First_XFEM_Step   = 0       !新生成的初始裂缝激活的第一个XFEM步. 2023-01-23.
!------------------
Desired_KIc  = 2.0D6        !水力压裂目标层的K_C,用于PhiPsi3D_Static_HF_SlipWater. 2023-02-12.


!------------------
Key_Ele_Max_Related_Cracks = 10 !用于指定单元最多可以关联的裂缝数目. NEWFTU2023022501.

St_KIc_Conversion = 6.88D0      !抗拉强度和断裂韧度换算系数. KIc = St/St_KIc_Conversion. 2023-03-25.
!------------------
XA_Step_Count =  0              !用于标记总的破裂步数的全局变量. 2023-04-03.

!--------水力压裂实验仿真相关----------
HFE_Initial_Try_Pressure = ZR        !给定的初始孔壁压力. 2023-04-18.
HFE_Initial_Pressure_Step_Size = 0.1D6  !起裂分析压力加载增量.

!------------
Key_Allow_3D_Outside_Crack = 0

Key_3D_HF_Time_Step_Method = 1 !2023-05-18. 3D清水压裂分析时间步迭代方法:=1,NR迭代(default);=2,二分法.


Key_3D_HF_SlipWater_fk_Type =1 !2023-08-08. 3D清水压裂裂尖fk函数计算类型. NEWFTU2023080801.

!IMPROV2023081001.
SIFs_DIM_3D_Offset_Delta_Factor = 0.5D0
!SIFs_DIM_3D_r_1_Factor          = 0.1D0
!SIFs_DIM_3D_r_2_Factor          = 0.2D0
!2023-08-22.
SIFs_DIM_3D_r_1_Factor          = 1.0D0
SIFs_DIM_3D_r_2_Factor          = 2.0D0
SIFs_DIM_3D_r_k_Factor          = 1.5D0

Key_Crack_Aperture_Method =2 !裂缝开度计算方法, =1:根据公式计算; =2:根据偏置点计算(default). 2023-08-12.

!2023-08-22.
Key_Print_SIFs_to_Screen = 0

!2023-08-27.
Crack_Max_Min_Aperture(1:Max_Num_Cr,1:3) = -999.0D0   !保存每条裂缝的最大最小平均开度.

!2024-02-15.
Key_Save_Crack_Radius=0      !用于保存裂缝开度. 2023-02-15.

!3D圆形等效为多边形的分辨率. 2024-02-26.
Circle_3D_Eqv_Polygon_Resolution = 21 !3D圆形等效为多边形的分辨率. 2024-02-26.

!3D SlipWater NR迭代时间步和压力步最大迭代次数. IMPROV2024022801.
SlipWater_Max_Time_Steps_3D   = 20  
SlipWater_Max_Pres_Steps_3D   = 10 
SlipWater_Time_Step_Conv_Check= 1   !3D SlipWater NR迭代时间步收敛检测(默认为0，即进行检测). 
SlipWater_Pres_Step_Conv_Check= 1   !3D SlipWater NR迭代时间步收敛检测(默认为0，即进行检测). 

!SlipWater_Max_Time_Steps_3D   = 3  
!SlipWater_Max_Pres_Steps_3D   = 3 
!SlipWater_Time_Step_Conv_Check= 0   !3D SlipWater NR迭代时间步收敛检测(默认为0，即进行检测). 
!SlipWater_Pres_Step_Conv_Check= 0   !3D SlipWater NR迭代时间步收敛检测(默认为0，即进行检测). 
!Max_Contact_Iter              = 1

!2024-03-09.
Key_Random          = 1
Inject_Crack_Num    = 1
Key_TipEnrich       = 1
Key_CFCP_3_Type     = 1
Key_FD_Tipenrich    = 1

!2024-03-13.
MAT_ALLOW_CRACK_Initiation(1:Max_Materials) = 1  !是否允许某种材料生成初始裂缝. 默认全部允许.
Key_Max_Num_Initiation_Cracks               = 1  !最多允许萌生的初始裂缝数目.
Num_Initiation_Cracks                   = 0  !萌生的初始裂缝数目.

print *," "

RETURN
END SUBROUTINE Initialize
