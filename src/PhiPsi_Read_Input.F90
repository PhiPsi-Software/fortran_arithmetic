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

program PhiPsi_Read_Input
!子程序功能:从关键字文件读入

!读取公共变量模块
use Global_Float_Type
use Global_Common
use Global_Filename
use Global_Dynamic
use Global_Crack
use Global_Crack_Common
use Global_Crack_3D
use Global_HF
use Global_POST
use Global_Inclusion
use Global_Model
use Global_Cohesive
use Global_NonLinear
use Global_Field_Problem
use Global_Stress
use Global_Surface_Load
use Global_Read_kpp
use Global_3D_HF_Experiment
use omp_lib

implicit none
character(len=256) c_File_name
character(len=256) c_line_string,c_m_line_string
character(len=256) Upper_c_line_string
!character(len=256) INPUT_INFO(2000)
!character(len=256) Instructions_before_Upper(2000)
!character(len=256) Tool_String_to_Upper
integer stat_read,ls1,ls2,i_Char,num_Input_Info,i_Info
real(kind=FT) t_Crack_Coors(100),t_Hole_Coors(100)
integer num_Data,i_P,i,itry
logical Yes_Even,alive
character(len=256) arg
character(len=2)  para     !用来记录参数的种类 例如“-i,-o”等
character(len=256 ) Name_of_Keywords
real(kind=FT) c_Temp_Double
real(kind=FT) Real_Tem_Date(2000)
integer i_DATA
integer num_Parameter_Lines,equal_sign_location
integer Tool_chrpak_ch_index_first
integer Num_Instruction_line
integer comment_sign_location
character(len=10) String_Num_Threads_Input
integer Num_Threads_Input,c_max_nthreads

!character(len=256) INPUT_INFO(2000)
!character(len=256) Instructions_before_Upper(2000)
!character(len=256) Instructions(2000)
!character(len=256) Instructions_before_Upper(2000)

character(len=256),ALLOCATABLE::INPUT_INFO(:)
character(len=256),ALLOCATABLE::Input_Info_before_Upper(:)
character(len=256),ALLOCATABLE::Instructions(:)
character(len=256),ALLOCATABLE::Instructions_before_Upper(:)
character(len=256),ALLOCATABLE::Parameter_lines(:)
character(len=256),ALLOCATABLE::Parameter_lines_value(:)
character(len=256),ALLOCATABLE::Parameter_lines_par(:)

integer,ALLOCATABLE::Parameter_lines_par_size(:)
integer,ALLOCATABLE::Parameter_lines_value_size(:)
!character(len=256) Parameter_lines(2000)
integer I_Instruction
integer i_charc
integer I_Par,I_Ins

!2024-02-24. 用于四则运算.
integer Sign_Location
integer Num_Digits,Tool_chrpak_s_digits_count
integer ierror
integer Read_Value_length
integer, parameter :: rk = kind ( 1.0E+00 )
real(kind=rk) Read_Value_After,Read_Value_Before,Value_Try_1,Value_Try_2,Result_Value
integer i_Try
logical Yes_Value_1,Yes_Value_2
integer Index_Start
integer Value_Before_Start_Index,Value_Before_End_Index
integer Value_After_Start_Index,Value_After_End_Index
logical Yes_Found_Value_Before,Yes_Found_Value_After
character(len=256) Result_String
integer i_Calculate
character(len=1) first_Charc
integer string_length,num_added_char
character(len=256) Tem_String
character(len=1) Previous_Char
integer Sign_Location_plus,Sign_Location_minus
logical logical_plus,logical_minus
integer Sign_Location_minus_Old,Sign_Location_plus_Old
integer j_Try
integer Tem_Location
logical Yes_i,Yes_n
integer tem_ierror,tem_length
integer c_nthreads
integer Tool_chrpak_ch_index_last

character(len=256) sub1,sub2
integer sub1_index,Tool_chrpak_s_indexi
integer Sign_Location_minus_2,Sign_Location_minus_temp
integer next_comma_index

integer,ALLOCATABLE::lines_comma_number(:)
integer,ALLOCATABLE::lines_comma_index(:,:)
integer c_comma_Location,start_index
integer i_data_segment,DataSeg_start_index,DataSeg_end_index
character(len=256) CURRENT_Instructions,Temp_Instruction
integer result_index_start,result_index_end
character(200) c_File_name_1

!2024-02-28.
integer i_arg

call Initialize
c_File_name_1   =  'output.txt'
!*********************************************************
!如果该文件已存在(上一次计算生成的),则删除该文件
!*********************************************************
inquire(file=c_File_name_1, exist=alive)  !如果存在，alive赋值为True
if(alive.EQV..True.)then
    OPEN  (UNIT=105, FILE=c_File_name_1, STATUS='OLD')
    CLOSE (UNIT=105, STATUS='DELETE')
endif
open(301,file=c_File_name_1,status='unknown',position='append',action='write')

1002 FORMAT('     Number of crack coordinates data must be even!')

print *, " >> Pre-Processing...."
print *, "    Reading keywords file...."

!***********************************************************
!获得cmd调用exe时的传入参数(关键字文件),若无传入参数
!(如直接打开程序),则需用户输入
!参考资料:http://blog.sciencenet.cn/blog-51026-622520.html
!PhiPsi带参数调用实例:PhiPsi.exe -i kw_exa_tension.txt
!***********************************************************
Yes_i = .false.
Yes_n = .false.
do i_arg= 1,20
    call getarg(i_arg,arg)       !读取输入参数的第一个
    read(arg,'(A2)') para
    !............................
    !检查标识参数是否是"-i".
    !............................
    if(para =='-i') then
        call getarg(i_arg+1,arg)   !提取关键字文件名字及路径Name_of_Keywords
        read(arg,'(A256)') Name_of_Keywords
        c_File_name = adjustL(trim(Name_of_Keywords))
        !print *,"len(c_File_name):",len_trim(c_File_name)
        if(len_trim(c_File_name) <=0) then
            print *, '    Error :: keyword file specified by -i is illegal!'
            print *, '             keyword file specified by -i: ',trim(c_File_name)
            call Warning_Message('S',Keywords_Blank)
        endif
        !检查kpp文件是否存在.
        inquire(file=c_File_name, exist=alive)  !如果存在，alive赋值为True
        !如共存在,则删除
        if(alive.EQV..True.)then
            Yes_i = .True.
        else
            print *, '    Error :: cannot find keyword file specified by -i!'
            print *, '             keyword file specified by -i: ',trim(c_File_name)
            call Warning_Message('S',Keywords_Blank)
        endif
    !........................................................
    !检查标识参数是否是"-n". NEWFTU2024022801. 2024-02-28.
    !........................................................
    elseif(para =='-n') then
        call getarg(i_arg+1,arg)
        read(arg,'(A10)') String_Num_Threads_Input
        String_Num_Threads_Input = adjustL(trim(String_Num_Threads_Input))
        !print *,"String_Num_Threads_Input:",String_Num_Threads_Input
        if(len_trim(String_Num_Threads_Input) <=0) then
            print *, '    Error :: number of threads specified by -n is illegal!'
            print *, '             number of threads specified by -n: ',trim(String_Num_Threads_Input)
            call Warning_Message('S',Keywords_Blank)
        endif
        !将字符串转换成整数.
        call Tool_chrpak_s_to_i4 (String_Num_Threads_Input,Num_Threads_Input, tem_ierror, tem_length)
!        c_max_nthreads = omp_get_max_threads()
!        !print *,"omp_get_max_threads()",omp_get_max_threads()
!        if(Num_Threads_Input > c_max_nthreads) then
!            print *, '    Error :: number of threads specified by -n is larger than max_nthreads!'
!            print *, '             max_nthreads: ',c_max_nthreads
!            call Warning_Message('S',Keywords_Blank)
!        elseif(Num_Threads_Input <= 0 ) then
!            print *, '    Error :: number of threads specified by -n is illegal!'
!            print *, '             number of threads specified by -n: ',Num_Threads_Input
!            call Warning_Message('S',Keywords_Blank)
!        else
!            Key_Num_Process = Num_Threads_Input
!            if(Key_Num_Process>=1) then
!                Yes_n = .True.
!            endif
!        endif
    !........................................................
    !检查标识参数是否是"-r". 为重启动分析预留. 2024-02-28.
    !........................................................
    elseif(para =='-r') then
        !To be done!
    else
        !print *, '    Error :: Unrecognized instruction:',para
        !call Warning_Message('S',Keywords_Blank)
    endif
enddo

if(Yes_i .eqv. .True.)then
    goto 1301
endif


!*********************
!主控文件所在路径.
!*********************
print *, "    Please enter the name of the keywords file."
#ifdef sffortran
print *, "    (For example: x:\phipsi_work\input.txt or input.txt)"
#endif
#ifdef sffortranmpi
print *, "    (For example: x:\phipsi_work\input.txt or input.txt)"
#endif
#ifdef cbfortran
print *, "    (For example: input.txt)"
#endif
#ifdef linux
print *, "    (For example: /mnt/x/PhiPsi_Project/input.txt or input.txt)"
#endif
#ifdef ifort
print *, "    (For example: x:\phipsi_work\input.txt or input.txt)"
#endif
#ifdef ifx
print *, "    (For example: x:\phipsi_work\input.txt or input.txt)"
#endif

!Try ten times.
do itry=1,10
  if(itry>=2)then
      print *,'    Please enter again:'
  endif
  read(*,'(A)') c_File_name
  c_File_name = adjustl(trim(c_File_name))

  !检查c_File_name是否包含引号. 2024-03-08. BUGFIX2024030801.
  if(Tool_chrpak_ch_index_first(c_File_name,'"')==1) then
    if(Tool_chrpak_ch_index_last(c_File_name,'"')==len_trim(c_File_name)) then
          !Delete '"'
          c_File_name(1:1)=' '
          c_File_name(len_trim(c_File_name):len_trim(c_File_name))=' '
          c_File_name = adjustl(trim(c_File_name))
    endif
  endif
  if(Tool_chrpak_ch_index_first(c_File_name,"'")==1) then
    if(Tool_chrpak_ch_index_last(c_File_name,"'")==len_trim(c_File_name)) then
          !Delete "'"
          c_File_name(1:1)=' '
          c_File_name(len_trim(c_File_name):len_trim(c_File_name))=' '
          c_File_name = adjustl(trim(c_File_name))
    endif
  endif

  inquire(file=c_File_name, exist=alive)  !如果存在，alive赋值为True
  !如共存在,则删除
  if(alive.EQV..True.)then
      !检查kpp文件后缀. 2024-03-08. NEWFTU2024030801.
      if(c_File_name(len_trim(c_File_name)-3:len_trim(c_File_name))/='.txt')then
          print *,'    ERROR :: please check keywords filename suffix (supposed to be *.txt)!'
          call Warning_Message('S',Keywords_Blank)
      endif

      goto 1301
  else
      print *, "    Keywords file was not found!"
  endif
enddo
print *,'    Failed to read the keywords file after 10 tries!'
call Warning_Message('S',Keywords_Blank)

1301 continue


!c_File_name='D:\PhiPsi work\kw_exa_inclusions.kpp'
!c_File_name='D:\PhiPsi work\kw_exa_tension.kpp'
!c_File_name='D:\PhiPsi work\kw_exa_earthquake.kpp'
!c_File_name='D:\PhiPsi work\kw_exa_hydraulic_fracturing.kpp'
!c_File_name='D:\PhiPsi work\kw_exa_3D_hollow_cylinder.kpp'
!c_File_name='X:\PhiPsi_Project\Crack_Meet_Hole.kpp'
!c_File_name='X:\PhiPsi_Project\kw_exa_static_temperature.kpp'
!c_File_name='X:\PhiPsi_Project\3D_MPI_TEST_TINY.kpp'
!c_File_name='X:\PhiPsi_Project\3D_MPI_TEST_MEDIUM.kpp'

!*****************************************
!统计kpp文件行数.
!*****************************************
open(1101,file=c_File_name,status='old')
num_Input_Info = 0
do
  read(1101,"(A256)",iostat=stat_read) c_line_string
  !如果读取完毕则退出do循环
  if(stat_read/=0) exit
  !如果该行非空
  if (len_trim(c_line_string) /= 0)then
       !-----------------
       ! 删除换行符
       !-----------------
       do i_charc = 1, len_trim(c_line_string)
            if (c_line_string(i_charc:i_charc) == achar(10)) then
                c_line_string(i_charc:i_charc) = ''
            end if
       end do
      !------------------------------------
      !删除各行字符串中的前后和中间的空格
      !------------------------------------
      c_line_string = adjustl(c_line_string)
      c_line_string = trim(c_line_string)
      !------------------------------------
      !如果该行非注释语句
      !------------------------------------
      if(c_line_string(1:1) /= '%')then
          num_Input_Info = num_Input_Info +1
      end if
  endif
end do
!print *,num_Input_Info
close(1101)

!*****************************************
!变量分配内存. NEWFTU2024020501.
!*****************************************
!print *,1001
allocate(INPUT_INFO(num_Input_Info))
allocate(Input_Info_before_Upper(num_Input_Info))
allocate(Instructions(num_Input_Info))
allocate(Instructions_before_Upper(num_Input_Info))
allocate(Parameter_lines(num_Input_Info))
allocate(Parameter_lines_par(num_Input_Info))
allocate(Parameter_lines_par_size(num_Input_Info))
allocate(Parameter_lines_value(num_Input_Info))
allocate(Parameter_lines_value_size(num_Input_Info))

!!character(len=256) INPUT_INFO(2000)
!character(len=256),ALLOCATABLE::Instructions_before_Upper(:)
!!character(len=256) Instructions_before_Upper(2000)
!character(len=256),ALLOCATABLE::Instructions(:)
!!character(len=256) Instructions(2000)
!character(len=256),ALLOCATABLE::Instructions_before_Upper(:)
!!character(len=256) Instructions_before_Upper(2000)
!character(len=256),ALLOCATABLE::Parameter_lines(:)
!character(len=256) Parameter_lines(2000)

!*****************************************
!打开并一行一行的读取文件,存入Input_Info.
!*****************************************
!print *,1002
open(1102,file=c_File_name,status='old')
num_Input_Info = 0
!print *,2001
do
  read(1102,"(A256)",iostat=stat_read) c_line_string
  !如果读取完毕则退出do循环
  if(stat_read/=0) exit
  !如果该行非空
  if (len_trim(c_line_string) /= 0)then
       !print *,trim(c_line_string)
       !-----------------
       ! 删除换行符.
       !-----------------
       do i_char = 1, len_trim(c_line_string)
            if (c_line_string(i_char:i_char) == achar(10)) then
                c_line_string(i_char:i_char) = ''
            end if
       end do

      !------------------------------------
      !删除各行字符串中的前后和中间的空格.
      !------------------------------------
       c_line_string = adjustl(c_line_string)
       c_line_string = trim(c_line_string)

      !------------------------------------
      !删除中间的任何空格.
      !------------------------------------
      if(c_line_string(2:2) /=':') then  !X:等不删除，因为路径中可能有空格.
          ls1 = len_trim(c_line_string)
          ls2 = 0
          do i_Char = 1,ls1
            if(c_line_string(i_Char:i_Char).ne.' ') then
              ls2 = ls2 + 1
              c_m_line_string(ls2:ls2)=c_line_string(i_Char:i_Char)
            endif
          enddo
      endif
      !print *,len(c_m_line_string),len_trim(c_m_line_string)!,c_m_line_string
      !call Tool_chrpak_s_blank_delete(c_line_string)  !Use chrpak to delete all spaces.

      !------------------------------------
      !如果该行非注释语句
      !------------------------------------
      if(c_line_string(1:1) /= '%')then
          num_Input_Info = num_Input_Info +1
          !转换成大写. IMPROV2022101001.
          !call Tool_String_to_Upper(c_line_string,c_line_string)

          !call Tool_chrpak_s_blank_delete(c_line_string)  !Use chrpak to delete all spaces.

          !检查该行是否有注释符号. 2024-02-24. BUGFIX2024022401.
          comment_sign_location = Tool_chrpak_ch_index_first(c_line_string,'%')
          if(comment_sign_location>=2) then
              !print *,'这是注释行-1...',c_line_string
              c_line_string(comment_sign_location:) = ''
              !print *,'这是注释行-2...',c_line_string
          endif

          call Tool_String_to_Upper(c_line_string,Upper_c_line_string)

          INPUT_INFO(num_Input_Info)=Upper_c_line_string

          Input_Info_before_Upper(num_Input_Info) = c_line_string  !IMPROV2023072401.
      end if
  endif
end do
!print *,num_Input_Info
close(1102)
!print *,1003


!!****************************************************
!!删除行后%注释. NEWFTU2024020501. 2023-02-05.
!!****************************************************
!DO I_INFO=1,NUM_INPUT_INFO
!    !call Tool_chrpak_s_blank_delete(INPUT_INFO(I_INFO))  !Use chrpak to delete all spaces.
!    !print *,Instructions(I_INFO)
!    !检查该行是否有注释符号.
!    comment_sign_location = Tool_chrpak_ch_index_first(INPUT_INFO(I_INFO),'%')
!    if(comment_sign_location>=2) then
!        print *,'这是注释行-1...',INPUT_INFO(I_INFO)
!        INPUT_INFO(I_INFO)(comment_sign_location:) = ''
!        print *,'这是注释行-2...',INPUT_INFO(I_INFO)
!    endif
!ENDDO

!**************************************************************
!删除Input_Info、Input_Info_before_Upper中的空格. 2024-02-05.
!**************************************************************
DO I_INFO=1,NUM_INPUT_INFO
    if(INPUT_INFO(I_INFO)(2:2) /=':') then  !X:等不删除，因为路径中可能有空格. 2024-03-10.
        call Tool_chrpak_s_blank_delete(INPUT_INFO(I_INFO))  !Use chrpak to delete all spaces.
        call Tool_chrpak_s_blank_delete(Input_Info_before_Upper(I_INFO))  !Use chrpak to delete all spaces.
        !print *,Instructions(I_INFO)
    endif
ENDDO

!print *,1004
!****************************************************
!NEWFTU2024020501.
!统计参数定义行的数目num_Parameter_Lines. 2024-02-05.
!存入Parameter_lines(2000)
!指令行存入Instruction_lines(2000)
!****************************************************
num_Parameter_Lines = 0
Num_Instruction_line= 0
DO I_INFO=1,NUM_INPUT_INFO
    equal_sign_location =  Tool_chrpak_ch_index_first(INPUT_INFO(I_INFO),'=') !Use chrpak to delete all spaces.
    !print *,"equal_sign_location:",equal_sign_location
    !print *,'len(INPUT_INFO(I_INFO)):',len_trim(INPUT_INFO(I_INFO))
    !参数定义行
    if(equal_sign_location>=2)then
        num_Parameter_Lines = num_Parameter_Lines + 1
        Parameter_lines(num_Parameter_Lines) = INPUT_INFO(I_INFO)
        !character(len=256),ALLOCATABLE::Parameter_lines_par(:)
        !integer,ALLOCATABLE::Parameter_lines_par_size(:)
        !character(len=256),ALLOCATABLE::Parameter_lines_value(:)
        !Parameter_lines_par(num_Parameter_Lines)= ' '
        Parameter_lines_par(num_Parameter_Lines)       = INPUT_INFO(I_INFO)(1:(equal_sign_location-1))
        Parameter_lines_par_size(num_Parameter_Lines)  = equal_sign_location-1

        !E或D开头的参数名必须至少4个字母.
        if(Parameter_lines_par(1)  == 'E' .or. Parameter_lines_par(1)  == 'D') then
            if(Parameter_lines_par_size(num_Parameter_Lines)<=3)then
                print *, '    Error :: Parameter starting with E(or D) must contain 4 letters at least!'
                print *, '             Parameter: ',INPUT_INFO(I_INFO)(1:len_trim(INPUT_INFO(I_INFO)))
                call Warning_Message('S',Keywords_Blank)
            endif
        endif

        Parameter_lines_value(num_Parameter_Lines)     =INPUT_INFO(I_INFO)(equal_sign_location+1:)
        Parameter_lines_value_size(num_Parameter_Lines)=len_trim(INPUT_INFO(I_INFO)) - equal_sign_location
        !print *,'Parameter_lines_par(num_Parameter_Lines):',Parameter_lines_par(num_Parameter_Lines)
        !print *,'Parameter_lines_par_size(num_Parameter_Lines):',Parameter_lines_par_size(num_Parameter_Lines)
        !print *,'Parameter_lines_value_size(num_Parameter_Lines):',Parameter_lines_value_size(num_Parameter_Lines)
    !指令行
    elseif(equal_sign_location==-1)then
        Num_Instruction_line = Num_Instruction_line + 1
        Instructions(Num_Instruction_line) = INPUT_INFO(I_INFO)
        Instructions_before_Upper(Num_Instruction_line) = Input_Info_before_Upper(I_INFO)
        !print *,Num_Instruction_line,Instructions(Num_Instruction_line)
    endif
ENDDO
!print *,"num_Parameter_Lines:", num_Parameter_Lines
!print *,"Num_Instruction_line:",Num_Instruction_line
!print *,1005

!****************************************************
!参数替换. 2024-02-05. NEWFTU2024020501.
!****************************************************
DO I_Par=1,num_Parameter_Lines
    !print *, Parameter_lines_par(I_Par)(1:Parameter_lines_par_size(I_Par))
    !print *, Parameter_lines_value(I_Par)(1:Parameter_lines_value_size(I_Par))
    sub1 = Parameter_lines_par(I_Par)(1:Parameter_lines_par_size(I_Par))
    sub2 = Parameter_lines_value(I_Par)(1:Parameter_lines_value_size(I_Par))
    DO I_Ins =1,Num_Instruction_line
        !print *,Instructions(I_Ins),Tool_chrpak_ch_index_first(Instructions(I_Ins),'*')
        !假如该行不含有*
        !if (Tool_chrpak_ch_index_first(Instructions(I_Ins),'*')==(-1)) then
        !假如该行不以*开头
        if (Instructions(I_Ins)(1:1) /='*') then
            !print *,"Instructions(I_Ins):",Instructions(I_Ins)
            !Tool_chrpak_s_replace_i函数每次只替换1个，因此需要执行多次.
            do i_Try=1,100
                sub1_index = Tool_chrpak_s_indexi(Instructions(I_Ins),adjustl((trim(sub1))))
                !print *,"sub1_index:",sub1_index
                if(sub1_index/=0) then
                    !将字符串s1中的sub1用sub2替代，传入s2
                    call Tool_chrpak_s_replace_i(Instructions(I_Ins),adjustl((trim(sub1))),adjustl((trim(sub2))))
                    !print *,'参数替换后:',I_Ins,Instructions(I_Ins)
                else
                    exit
                endif
            enddo
        endif
    ENDDO
ENDDO

!DO I_Ins =1,Num_Instruction_line
!    print *,I_Ins,Instructions(I_Ins)
!ENDDO



!*************************************************
!提取并记录逗号. 2024-03-15.
!*************************************************
allocate(lines_comma_number(Num_Instruction_line))
allocate(lines_comma_index(Num_Instruction_line,100))
lines_comma_number = 0
lines_comma_index  = 0
DO I_INFO=1,Num_Instruction_line
    !
    !print *,5555
    
    !print *,Instructions(I_INFO)
    
    !*开头的不处理.
    if(Instructions(I_INFO)(1:1)=='*') then
        cycle
    endif
    
    !print *,6666
    start_index = 1
    
    !查找逗号并保存.
    do i_Try=1,100
        
        !print *,"start_index:",start_index
        c_comma_Location = Tool_chrpak_ch_index_first(Instructions(I_INFO)(start_index:),',')
        !print *,"c_comma_Location:",c_comma_Location
        if(c_comma_Location==(-1)) then
            exit
        endif
        if(c_comma_Location>1)then
            lines_comma_number(I_INFO) = lines_comma_number(I_INFO) + 1
            lines_comma_index(I_INFO,lines_comma_number(I_INFO)) = start_index+c_comma_Location-1
        endif
        start_index = start_index+c_comma_Location
        if(start_index>=len_trim(Instructions(I_INFO))) then
            exit
        endif
    enddo
enddo

!DO I_Ins =1,Num_Instruction_line
!    if(lines_comma_number(I_Ins)>=1)then
!        print *,"I_Ins,lines_comma_number(I_Ins):",I_Ins,lines_comma_number(I_Ins)
!        print *,'lines_comma_index:',lines_comma_index(I_Ins,1:lines_comma_number(I_Ins))
!    endif
!ENDDO



!*************************************************
!处理四则运算. 2024-02-24. NEWFTU2024022401.
!先不考虑带括号的情况.
!*************************************************
DO I_INFO=1,Num_Instruction_line
  !*开头的不处理.
  if(Instructions(I_INFO)(1:1)=='*') then
      write(301, *) Instructions(I_INFO)(1:len_trim(Instructions(I_INFO)))
      cycle
  endif
  
  !////////////////////////////////////////
  !                                       /
  !                                       /
  !  根据逗号分成若干段进行处理.          /
  !  暂未完成. 2024-03-15.                /
  !                                       /
  !                                       /
  !////////////////////////////////////////
  !print *,"Instructions(I_INFO),lines_comma_number(I_INFO):",Instructions(I_INFO),lines_comma_number(I_INFO)
  !各个逗号段之间循环. 2024-03-15.
  !例如1,2,3：两个逗号，分成3段.
  
  
  Temp_Instruction(1:) = ' ' 
  result_index_start  = 0
    
  DO i_data_segment=1,lines_comma_number(I_INFO)+1
    !print *,"i_data_segment,lines_comma_number(I_INFO)+1:",i_data_segment,lines_comma_number(I_INFO)+1
    !如果没有逗号.
    if(lines_comma_number(I_INFO)==0) then
        !print *,1001
        DataSeg_start_index = 1
        DataSeg_end_index   = len_trim(Instructions(I_INFO))
    !有逗号.
    else
        !第1段.
        if(i_data_segment==1) then
            !print *,1002
            DataSeg_start_index = 1
            DataSeg_end_index = lines_comma_index(I_INFO,i_data_segment)-1
        !最后一段.
        elseif(i_data_segment == (lines_comma_number(I_INFO)+1))then
            !print *,1003
            DataSeg_start_index = lines_comma_index(I_INFO,lines_comma_number(I_INFO)) + 1
            DataSeg_end_index = len_trim(Instructions(I_INFO))
        else
            !print *,1004
            DataSeg_start_index = lines_comma_index(I_INFO,i_data_segment-1) + 1
            DataSeg_end_index = lines_comma_index(I_INFO,i_data_segment) - 1
        endif
        !print *,"i_data_segment,DataSeg_start_index,DataSeg_end_index:",i_data_segment,DataSeg_start_index,DataSeg_end_index
    endif
    
    
    
    CURRENT_Instructions = Instructions(I_INFO)(DataSeg_start_index:DataSeg_end_index)
    
    !print *,"I_INFO,i_data_segment,CURRENT_Instructions-四则运算前:",I_INFO,i_data_segment,":",CURRENT_Instructions
    
    !四则运算处理.
    call Tool_String_arithmetic(CURRENT_Instructions)
    
    !print *,"I_INFO,i_data_segment,CURRENT_Instructions-四则运算后:",I_INFO,i_data_segment,":",CURRENT_Instructions
   
    
    !result_index_end    = 0
    !如果没有逗号.
    if(lines_comma_number(I_INFO)==0) then
        Temp_Instruction(1: len_trim(CURRENT_Instructions)) = CURRENT_Instructions
    !有逗号.
    else
        !第1段.
        if(i_data_segment==1) then
            !print *,6666
            !print *,"len_trim(CURRENT_Instructions):",len_trim(CURRENT_Instructions)
            Temp_Instruction(1:len_trim(CURRENT_Instructions)) = CURRENT_Instructions(1:len_trim(CURRENT_Instructions))
            
            !print *,"CURRENT_Instructions", CURRENT_Instructions(1:len_trim(CURRENT_Instructions))
                
            Temp_Instruction(len_trim(CURRENT_Instructions)+1:len_trim(CURRENT_Instructions)+1) = ','
            
            result_index_start = result_index_start + len_trim(CURRENT_Instructions) + 1 + 1
        !最后一段.
        elseif(i_data_segment == (lines_comma_number(I_INFO)+1))then
            !print *,7777
            !print *,1003
            Temp_Instruction(result_index_start:result_index_start+len_trim(CURRENT_Instructions)) = &
                CURRENT_Instructions(1:len_trim(CURRENT_Instructions))
        else
            !print *,8888
            Temp_Instruction(result_index_start:result_index_start+len_trim(CURRENT_Instructions)) = &
                CURRENT_Instructions(1:len_trim(CURRENT_Instructions))
            
            Temp_Instruction(result_index_start+len_trim(CURRENT_Instructions)+1:&
                                 result_index_start+len_trim(CURRENT_Instructions)+1) = ','
            
            result_index_start = result_index_start + len_trim(CURRENT_Instructions) + 1 + 1
        endif
        !print *,"i_data_segment,DataSeg_start_index,DataSeg_end_index:",i_data_segment,DataSeg_start_index,DataSeg_end_index
    endif
    
    
  enddo
  
  print *,' '
  print *,"Instructions(I_INFO)_before:",Instructions(I_INFO)(1:len_trim(Instructions(I_INFO)))
  Instructions(I_INFO) = Temp_Instruction
  call Tool_chrpak_s_blank_delete(Instructions(I_INFO)) 
  print *,"Instructions(I_INFO)_after: ",Instructions(I_INFO)(1:len_trim(Instructions(I_INFO)))
  !IMPROV2022111402.
  write(301, *) Instructions(I_INFO)(1:len_trim(Instructions(I_INFO)))
  
enddo


!***************************************************
!给关键字赋特殊值,以便判断是否已经从主控文件读取.
!***************************************************
num_Na_Crack       = -99
Key_Dimension      = -99
Key_Analysis_Type  = -99
Key_Type_2D        = -99
Key_Data_Format    = -99
Key_Contact        = -99
fric_mu_Cont       = -99.0D0
Key_TipEnrich      = -99
Key_SIFs_Method    = -99
Key_SLOE           = -99
Key_Initiation     = -99
Key_Propagation    = -99
Factor_Propagation = -99.0D0
Key_Force_Control  = -99
Key_Gravity        = -99
Num_Substeps       = -99
CFCP               = -99
Num_Frac           = -99
Key_Proppant       = -99
Key_Propp_Trans    = -99
Key_Symm_HF        = -99
Viscosity          = -99.0D0
!粘聚裂缝相关
Coh_Constitutive_type = -99        !Constitutive model of the F-w curve,=1,双线性_先升后降(默认);=2,单线性下降(有时不收敛);=3,常量(一条水平线)
Coh_Width_Critical1   = -99.0D0    !极限拉伸开度1,此开度对应极限法向牵引力,Coh_Constitutive_type=1时可用
Coh_Width_Critical2   = -99.0D0    !极限拉伸开度2,此开度下法向牵引力为0
Coh_f_Ultimate        = -99.0D0    !最大牵引力,裂尖的法向牵引力
Coh_Tangential_Key    = -99        !是否考虑切向牵引力,若考虑,应给出对应的参数
Coh_Width_Critical1_T = -99.0D0    !极限拉伸开度1,此开度对应极限切向牵引力,Coh_Constitutive_type=1时可用
Coh_Width_Critical2_T = -99.0D0    !极限拉伸开度2,此开度下切向牵引力为0
Coh_f_Ultimate_T      = -99.0D0    !最大牵引力,裂尖的切向牵引力
!材料参数
Material_Para(1,1) = -99.0D0
!裂缝坐标
Crack_Coor(1,1,1)  = -99.0D0
!3D裂缝坐标
!Crack3D_Coor(1,1,1)  = -99.0D0
!Crack3D_Cir_Coor(1,1)= -99.0D0
Inject_Crack_Num   = -99
!注水时间
Inject_Q_Time(1)   = -99.0D0
!注水流量
Inject_Q_Val(1)    = -99.0D0
Cracks_Allow_Propa(1)= -99
Cracks_HF_State(1)   = -99
!后处理相关
!水力压裂接触设置
Key_HF_Cont_Scheme   = -99
!随机生成算法
Key_Random = -99
!水力压裂天然裂缝相关
Key_Na_Crack_Type = -99
!接触分析相关
!Max_Contact_Iter = -99

!**************************
!指令行数目必须是2的倍数.
!**************************
!num_Input_Info-num_Parameter_Lines必须是偶数(一行关键字跟一行数据)
!if(mod((num_Input_Info-num_Parameter_Lines),2)/=0)then
if(mod(Num_Instruction_line,2)/=0)then
  print *,'    Error:: number of instruction lines must be even!'
  print *,'            number of input keywords and its value is',num_Input_Info
  call Warning_Message('S',Keywords_Blank)
endif
!print *,"num_Crack:",num_Crack


!*********************
!各条指令行逐个循环.
!*********************
!DO I_INFO=1,NUM_INPUT_INFO
!DO I_Instruction=1,Num_Instruction_line
DO I_INFO=1,Num_Instruction_line
  !PRINT *,'    I_INFO',I_INFO,Instructions(I_INFO)(1:30)
  !PRINT *,'    I_INFO',I_INFO,Instructions(I_INFO)(1:30)
  !............................
  !读取单点数据(非数组型数据)
  !............................
  IF (Instructions(I_INFO)(1:9)=='*FILENAME' .and. Instructions(I_INFO)(10:10)==' ')THEN
      !FILENAME = Instructions(I_INFO+1)
      FILENAME = Instructions_before_Upper(I_INFO+1)   !2023-07-24. Linux区分大小写. IMPROV2023072401.
      !print *,"FILENAME:",FILENAME
  ELSEIF (Instructions(I_INFO)(1:15)=='*WORK_DIRECTORY' .and. Instructions(I_INFO)(16:16)==' ')THEN
      !WORK_DIRECTORY = Instructions(I_INFO+1)         !2023-07-24. Linux区分大小写. IMPROV2023072401.
      WORK_DIRECTORY = Instructions_before_Upper(I_INFO+1)
      !print *,"WORK_DIRECTORY:",WORK_DIRECTORY
  ELSEIF (Instructions(I_INFO)(1:10)=='*NUM_CRACK' .and. Instructions(I_INFO)(11:11)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_CRACK
      !PRINT *,"Instructions(I_INFO):",Instructions(I_INFO)
      !PRINT *,"NUM_CRACK00000:",NUM_CRACK
  ELSEIF (Instructions(I_INFO)(1:9) =='*NUM_HOLE' .and. Instructions(I_INFO)(10:10)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_HOLE
      !print *, 8888, NUM_HOLE
  ELSEIF (Instructions(I_INFO)(1:14) =='*NUM_CIRC_HOLE' .and. Instructions(I_INFO)(15:15)==' ')THEN   !2023-05-07.
      READ(Instructions(I_INFO+1) , * ) num_Circ_Hole
      !print *, 9999, num_Circ_Hole
  ELSEIF (Instructions(I_INFO)(1:14)=='*NUM_CIRC_INCL' .and. Instructions(I_INFO)(15:15)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_CIRC_INCL
  ELSEIF (Instructions(I_INFO)(1:13)=='*NUM_NA_CRACK' .and. Instructions(I_INFO)(14:14)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_NA_CRACK
  ELSEIF (Instructions(I_INFO)(1:14)=='*KEY_DIMENSION' .and. Instructions(I_INFO)(15:15)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_DIMENSION
  ELSEIF (Instructions(I_INFO)(1:18)=='*KEY_ANALYSIS_TYPE' .and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_ANALYSIS_TYPE
  ELSEIF (Instructions(I_INFO)(1:12)=='*KEY_TYPE_2D' .and. Instructions(I_INFO)(13:13)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_TYPE_2D
  ELSEIF (Instructions(I_INFO)(1:16)=='*KEY_DATA_FORMAT' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_DATA_FORMAT
  ENDIF
  !............
  !接触相关
  !............
  IF (Instructions(I_INFO)(1:12)=='*KEY_CONTACT' .and. Instructions(I_INFO)(13:13)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_CONTACT
  ELSEIF (Instructions(I_INFO)(1:13)=='*FRIC_MU_CONT' .and. Instructions(I_INFO)(14:14)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) FRIC_MU_CONT
  ELSEIF (Instructions(I_INFO)(1:16)=='*KN_CONT_PENALTY' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KN_CONT_PENALTY
  ELSEIF (Instructions(I_INFO)(1:16)=='*KT_CONT_PENALTY' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KT_CONT_PENALTY
  ELSEIF (Instructions(I_INFO)(1:18)=='*CONVE_TOL_PENALTY' .and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CONVE_TOL_PENALTY
  ELSEIF (Instructions(I_INFO)(1:14)=='*KEY_TIPENRICH' .and. Instructions(I_INFO)(15:15)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_TIPENRICH
  ELSEIF (Instructions(I_INFO)(1:16)=='*KEY_SIFS_METHOD' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_SIFS_METHOD
  ELSEIF (Instructions(I_INFO)(1:9)=='*KEY_SLOE' .and. Instructions(I_INFO)(10:10)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_SLOE
  ELSEIF (Instructions(I_INFO)(1:15)=='*KEY_INITIATION' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_INITIATION
  ELSEIF (Instructions(I_INFO)(1:16)=='*KEY_PROPAGATION' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_PROPAGATION
  ELSEIF (Instructions(I_INFO)(1:19)=='*FACTOR_PROPAGATION' .and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) FACTOR_PROPAGATION
  ELSEIF (Instructions(I_INFO)(1:18)=='*KEY_FORCE_CONTROL' .and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_FORCE_CONTROL
  ELSEIF (Instructions(I_INFO)(1:12)=='*KEY_GRAVITY' .and. Instructions(I_INFO)(13:13)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_GRAVITY
  ELSEIF (Instructions(I_INFO)(1:8)=='*G_X_Y_Z' .and. Instructions(I_INFO)(9:9)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) G_X_Y_Z(1:3)
  ELSEIF (Instructions(I_INFO)(1:13)=='*NUM_SUBSTEPS' .and. Instructions(I_INFO)(14:14)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_SUBSTEPS
  ELSEIF (Instructions(I_INFO)(1:5)=='*CFCP' .and. Instructions(I_INFO)(6:6)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CFCP
  ELSEIF (Instructions(I_INFO)(1:9)=='*NUM_FRAC' .and. Instructions(I_INFO)(10:10)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_FRAC
  ELSEIF (Instructions(I_INFO)(1:13)=='*KEY_PROPPANT' .and. Instructions(I_INFO)(14:14)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_PROPPANT
  ELSEIF (Instructions(I_INFO)(1:19)=='*KEY_HF_CONT_SCHEME' .and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_HF_CONT_SCHEME
  ELSEIF (Instructions(I_INFO)(1:16)=='*KEY_PROPP_TRANS' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_PROPP_TRANS
  ELSEIF (Instructions(I_INFO)(1:12)=='*KEY_SYMM_HF' .and. Instructions(I_INFO)(13:13)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_SYMM_HF
  ELSEIF (Instructions(I_INFO)(1:10)=='*VISCOSITY' .and. Instructions(I_INFO)(11:11)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) VISCOSITY
  ELSEIF (Instructions(I_INFO)(1:13)=='*KEY_K_SPARSE' .and. Instructions(I_INFO)(14:14)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_K_SPARSE
  ELSEIF (Instructions(I_INFO)(1:13)=='*SPARSE_RATIO' .and. Instructions(I_INFO)(14:14)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) SPARSE_RATIO
  ELSEIF (Instructions(I_INFO)(1:20)=='*SPARSE_STORE_METHOD' .and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) SPARSE_STORE_METHOD
  ELSEIF (Instructions(I_INFO)(1:10)=='*KEY_EKILL' .and. Instructions(I_INFO)(11:11)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_EKILL
  ELSEIF (Instructions(I_INFO)(1:20)=='*EKILL_WEAKEN_FACTOR' .and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) EKILL_WEAKEN_FACTOR
  ENDIF
  !............................
  !杀死的单元号(各个载荷步的).
  !............................
  IF (Instructions(I_INFO)(1:29)=='*ELE_KILLED_EACH_LOAD_STEP_01' .and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(1,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)=='*ELE_KILLED_EACH_LOAD_STEP_02'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(2,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)=='*ELE_KILLED_EACH_LOAD_STEP_03'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(3,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)=='*ELE_KILLED_EACH_LOAD_STEP_04'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(4,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)=='*ELE_KILLED_EACH_LOAD_STEP_05'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(5,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)=='*ELE_KILLED_EACH_LOAD_STEP_06'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(6,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)=='*ELE_KILLED_EACH_LOAD_STEP_07'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(7,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)=='*ELE_KILLED_EACH_LOAD_STEP_08'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*)  ELE_KILLED_EACH_LOAD_STEP(8,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)=='*ELE_KILLED_EACH_LOAD_STEP_09'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(9,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)=='*ELE_KILLED_EACH_LOAD_STEP_10'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(10,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)== '*ELE_KILLED_EACH_LOAD_STEP_11'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*)  ELE_KILLED_EACH_LOAD_STEP(11,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)=='*ELE_KILLED_EACH_LOAD_STEP_12'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(12,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)=='*ELE_KILLED_EACH_LOAD_STEP_13'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(13,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)==  '*ELE_KILLED_EACH_LOAD_STEP_14'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(14,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)==  '*ELE_KILLED_EACH_LOAD_STEP_15'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(15,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)=='*ELE_KILLED_EACH_LOAD_STEP_16'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(16,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)== '*ELE_KILLED_EACH_LOAD_STEP_17'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(17,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)=='*ELE_KILLED_EACH_LOAD_STEP_18'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(18,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)=='*ELE_KILLED_EACH_LOAD_STEP_19'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(19,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)=='*ELE_KILLED_EACH_LOAD_STEP_20'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(20,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)=='*ELE_KILLED_EACH_LOAD_STEP_21'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*)  ELE_KILLED_EACH_LOAD_STEP(21,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)== '*ELE_KILLED_EACH_LOAD_STEP_22'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(22,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)=='*ELE_KILLED_EACH_LOAD_STEP_23'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(23,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)== '*ELE_KILLED_EACH_LOAD_STEP_24'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(24,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)== '*ELE_KILLED_EACH_LOAD_STEP_25'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*)  ELE_KILLED_EACH_LOAD_STEP(25,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)== '*ELE_KILLED_EACH_LOAD_STEP_26'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(26,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)== '*ELE_KILLED_EACH_LOAD_STEP_27'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),  256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*)  ELE_KILLED_EACH_LOAD_STEP(27,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)== '*ELE_KILLED_EACH_LOAD_STEP_28'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*)   ELE_KILLED_EACH_LOAD_STEP(28,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)== '*ELE_KILLED_EACH_LOAD_STEP_29'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(29,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)=='*ELE_KILLED_EACH_LOAD_STEP_30'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(30,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)== '*ELE_KILLED_EACH_LOAD_STEP_31'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(31,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)== '*ELE_KILLED_EACH_LOAD_STEP_32'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(32,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)=='*ELE_KILLED_EACH_LOAD_STEP_33'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(33,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)== '*ELE_KILLED_EACH_LOAD_STEP_34'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(34,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)=='*ELE_KILLED_EACH_LOAD_STEP_35'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(35,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)== '*ELE_KILLED_EACH_LOAD_STEP_36'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*)  ELE_KILLED_EACH_LOAD_STEP(36,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)=='*ELE_KILLED_EACH_LOAD_STEP_37'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(37,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)== '*ELE_KILLED_EACH_LOAD_STEP_38'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*)  ELE_KILLED_EACH_LOAD_STEP(38,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)=='*ELE_KILLED_EACH_LOAD_STEP_39'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*) ELE_KILLED_EACH_LOAD_STEP(39,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)== '*ELE_KILLED_EACH_LOAD_STEP_40'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1) ,*)  ELE_KILLED_EACH_LOAD_STEP(40,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:29)== '*ELE_KILLED_EACH_LOAD_STEP_41'.and. Instructions(I_INFO)(30:30)==' ')THEN
      print *, '    Error :: input file does not support *ELE_KILLED_EACH_LOAD_STEP_41 or higher!'
      call Warning_Message('S',Keywords_Blank)
  ENDIF
  !..................
  !载荷位移曲线相关
  !..................
  IF (Instructions(I_INFO)(1:19)=='*KEY_SAVE_F_D_CURVE'.and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_SAVE_F_D_CURVE
  ELSEIF (Instructions(I_INFO)(1:19)=='*F_D_CURVE_NODE_NUM'.and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) F_D_CURVE_NODE_NUM
  ENDIF
  !..................
  !载荷COD曲线相关
  !..................
  IF (Instructions(I_INFO)(1:21)=='*KEY_SAVE_F_COD_CURVE'.and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_Save_f_COD_Curve
  ELSEIF (Instructions(I_INFO)(1:22)=='*F_COD_CURVE_CRACK_NUM'.and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) f_COD_Curve_Crack_Num
  ENDIF
  !...............
  !粘聚裂缝相关
  !...............
  IF(Instructions(I_INFO)(1:22)=='*COH_CONSTITUTIVE_TYPE'.and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) COH_CONSTITUTIVE_TYPE
  ELSEIF(Instructions(I_INFO)(1:20)=='*COH_WIDTH_CRITICAL1'.and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) COH_WIDTH_CRITICAL1
  ELSEIF(Instructions(I_INFO)(1:20)=='*COH_WIDTH_CRITICAL2'.and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) COH_WIDTH_CRITICAL2
  ELSEIF(Instructions(I_INFO)(1:15)=='*COH_F_ULTIMATE'.and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) COH_F_ULTIMATE
  ELSEIF(Instructions(I_INFO)(1:19)=='*COH_TANGENTIAL_KEY'.and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) COH_TANGENTIAL_KEY
  ELSEIF(Instructions(I_INFO)(1:22)=='*COH_WIDTH_CRITICAL1_T'.and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) COH_WIDTH_CRITICAL1_T
  ELSEIF(Instructions(I_INFO)(1:22)=='*COH_WIDTH_CRITICAL2_T'.and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) COH_WIDTH_CRITICAL2_T
  ELSEIF(Instructions(I_INFO)(1:17)=='*COH_F_ULTIMATE_T'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) COH_F_ULTIMATE_T
  ENDIF
  !.......................................
  !各个材料号的材料类型:最多支持70种材料.
  !.......................................
  IF (Instructions(I_INFO)(1:16)=='*MATERIAL_TYPE_1' .and. Instructions(I_INFO)(17:17)==' ' )THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(1)
  ELSEIF (Instructions(I_INFO)(1:16)=='*MATERIAL_TYPE_2' .and. Instructions(I_INFO)(17:17)==' ' )THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(2)
  ELSEIF (Instructions(I_INFO)(1:16)=='*MATERIAL_TYPE_3' .and. Instructions(I_INFO)(17:17)==' ' )THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(3)
  ELSEIF (Instructions(I_INFO)(1:16)=='*MATERIAL_TYPE_4' .and. Instructions(I_INFO)(17:17)==' ' )THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(4)
  ELSEIF (Instructions(I_INFO)(1:16)=='*MATERIAL_TYPE_5' .and. Instructions(I_INFO)(17:17)==' ' )THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(5)
  ELSEIF (Instructions(I_INFO)(1:16)=='*MATERIAL_TYPE_6' .and. Instructions(I_INFO)(17:17)==' ' )THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(6)
  ELSEIF (Instructions(I_INFO)(1:16)=='*MATERIAL_TYPE_7' .and. Instructions(I_INFO)(17:17)==' ' )THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(7)
  ELSEIF (Instructions(I_INFO)(1:16)=='*MATERIAL_TYPE_8' .and. Instructions(I_INFO)(17:17)==' ' )THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(8)
  ELSEIF (Instructions(I_INFO)(1:16)=='*MATERIAL_TYPE_9' .and. Instructions(I_INFO)(17:17)==' ' )THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(9)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_10'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(10)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_11'.and. Instructions(I_INFO)(18:18)==' ')THEN !IMPROV2023080902.
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(11)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_12'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(12)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_13'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(13)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_14'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(14)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_15'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(15)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_16'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(16)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_17'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(17)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_18'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(18)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_19'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(19)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_20'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(20)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_21'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(21)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_22'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(22)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_23'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(23)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_24'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(24)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_25'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(25)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_26'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(26)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_27'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(27)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_28'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(28)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_29'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(29)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_30'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(30)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_31'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(31)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_32'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(32)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_33'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(33)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_34'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(34)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_35'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(35)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_36'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(36)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_37'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(37)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_38'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(38)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_39'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(39)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_40'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(40)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_41'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(41)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_42'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(42)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_43'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(43)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_44'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(44)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_45'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(45)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_46'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(46)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_47'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(47)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_48'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(48)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_49'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(49)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_50'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(50)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_51'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(51)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_52'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(52)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_53'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(53)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_54'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(54)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_55'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(55)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_56'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(56)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_57'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(57)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_58'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(58)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_59'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(59)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_60'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(60)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_61'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(61)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_62'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(62)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_63'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(63)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_64'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(64)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_65'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(65)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_66'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(66)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_67'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(67)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_68'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(68)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_69'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(69)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_70'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) MATERIAL_TYPE(70)
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_TYPE_71'.and. Instructions(I_INFO)(18:18)==' ')THEN
      print *, '    Error :: input file does not support *MATERIAL_TYPE_71 or higher!'
      call Warning_Message('S',Keywords_Blank)
  ENDIF

  !....................
  !材料参数:支持70种.
  !....................
  IF (Instructions(I_INFO)(1:16)=='*MATERIAL_PARA_1' .and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(1,1:NUM_DATA)
      NUM_OF_MATERIAL = 1
  ELSEIF (Instructions(I_INFO)(1:16)=='*MATERIAL_PARA_2' .and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(2,1:NUM_DATA)
      NUM_OF_MATERIAL = 2
  ELSEIF (Instructions(I_INFO)(1:16)=='*MATERIAL_PARA_3' .and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(3,1:NUM_DATA)
      NUM_OF_MATERIAL = 3
  ELSEIF (Instructions(I_INFO)(1:16)=='*MATERIAL_PARA_4' .and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(4,1:NUM_DATA)
      NUM_OF_MATERIAL = 4
  ELSEIF (Instructions(I_INFO)(1:16)=='*MATERIAL_PARA_5' .and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(5,1:NUM_DATA)
      NUM_OF_MATERIAL = 5
  ELSEIF (Instructions(I_INFO)(1:16)=='*MATERIAL_PARA_6' .and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(6,1:NUM_DATA)
      NUM_OF_MATERIAL = 6
  ELSEIF (Instructions(I_INFO)(1:16)=='*MATERIAL_PARA_7' .and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(7,1:NUM_DATA)
      NUM_OF_MATERIAL = 7
  ELSEIF (Instructions(I_INFO)(1:16)=='*MATERIAL_PARA_8' .and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(8,1:NUM_DATA)
      NUM_OF_MATERIAL = 8
  ELSEIF (Instructions(I_INFO)(1:16)=='*MATERIAL_PARA_9' .and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(9,1:NUM_DATA)
      NUM_OF_MATERIAL = 9
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_10'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(10,1:NUM_DATA)
      NUM_OF_MATERIAL = 10
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_11'.and. Instructions(I_INFO)(18:18)==' ')THEN !IMPROV2023080902.
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(11,1:NUM_DATA)
      NUM_OF_MATERIAL = 11
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_12'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(12,1:NUM_DATA)
      NUM_OF_MATERIAL = 12
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_13'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(13,1:NUM_DATA)
      NUM_OF_MATERIAL = 13
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_14'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(14,1:NUM_DATA)
      NUM_OF_MATERIAL = 14
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_15'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(15,1:NUM_DATA)
      NUM_OF_MATERIAL = 15
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_16'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(16,1:NUM_DATA)
      NUM_OF_MATERIAL = 16
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_17'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(17,1:NUM_DATA)
      NUM_OF_MATERIAL = 17
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_18'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(18,1:NUM_DATA)
      NUM_OF_MATERIAL = 18
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_19'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(19,1:NUM_DATA)
      NUM_OF_MATERIAL = 19
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_20'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(20,1:NUM_DATA)
      NUM_OF_MATERIAL = 20
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_21'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(21,1:NUM_DATA)
      NUM_OF_MATERIAL = 21
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_22'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(22,1:NUM_DATA)
      NUM_OF_MATERIAL = 22
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_23'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(23,1:NUM_DATA)
      NUM_OF_MATERIAL = 23
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_24'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(24,1:NUM_DATA)
      NUM_OF_MATERIAL = 24
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_25'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(25,1:NUM_DATA)
      NUM_OF_MATERIAL = 25
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_26'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(26,1:NUM_DATA)
      NUM_OF_MATERIAL = 26
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_27'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(27,1:NUM_DATA)
      NUM_OF_MATERIAL = 27
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_28'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(28,1:NUM_DATA)
      NUM_OF_MATERIAL = 28
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_29'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(29,1:NUM_DATA)
      NUM_OF_MATERIAL = 29
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_30'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(30,1:NUM_DATA)
      NUM_OF_MATERIAL = 30
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_31'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(31,1:NUM_DATA)
      NUM_OF_MATERIAL = 31
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_32'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(32,1:NUM_DATA)
      NUM_OF_MATERIAL = 32
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_33'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(33,1:NUM_DATA)
      NUM_OF_MATERIAL = 33
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_34'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(34,1:NUM_DATA)
      NUM_OF_MATERIAL = 34
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_35'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(35,1:NUM_DATA)
      NUM_OF_MATERIAL = 35
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_36'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(36,1:NUM_DATA)
      NUM_OF_MATERIAL = 36
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_37'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(37,1:NUM_DATA)
      NUM_OF_MATERIAL = 37
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_38'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(38,1:NUM_DATA)
      NUM_OF_MATERIAL = 38
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_39'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(39,1:NUM_DATA)
      NUM_OF_MATERIAL = 39
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_40'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(40,1:NUM_DATA)
      NUM_OF_MATERIAL = 40
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_41'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(41,1:NUM_DATA)
      NUM_OF_MATERIAL = 41
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_42'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(42,1:NUM_DATA)
      NUM_OF_MATERIAL = 42
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_43'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(43,1:NUM_DATA)
      NUM_OF_MATERIAL = 43
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_44'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(44,1:NUM_DATA)
      NUM_OF_MATERIAL = 44
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_45'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(45,1:NUM_DATA)
      NUM_OF_MATERIAL = 45
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_46'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(46,1:NUM_DATA)
      NUM_OF_MATERIAL = 46
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_47'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(47,1:NUM_DATA)
      NUM_OF_MATERIAL = 47
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_48'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(48,1:NUM_DATA)
      NUM_OF_MATERIAL = 48
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_49'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(49,1:NUM_DATA)
      NUM_OF_MATERIAL = 49
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_50'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(50,1:NUM_DATA)
      NUM_OF_MATERIAL = 50
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_51'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(51,1:NUM_DATA)
      NUM_OF_MATERIAL = 51
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_52'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(52,1:NUM_DATA)
      NUM_OF_MATERIAL = 52
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_53'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(53,1:NUM_DATA)
      NUM_OF_MATERIAL = 53
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_54'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(54,1:NUM_DATA)
      NUM_OF_MATERIAL = 54
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_55'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(55,1:NUM_DATA)
      NUM_OF_MATERIAL = 55
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_56'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(56,1:NUM_DATA)
      NUM_OF_MATERIAL = 56
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_57'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(57,1:NUM_DATA)
      NUM_OF_MATERIAL = 57
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_58'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(58,1:NUM_DATA)
      NUM_OF_MATERIAL = 58
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_59'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(59,1:NUM_DATA)
      NUM_OF_MATERIAL = 59
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_60'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(60,1:NUM_DATA)
      NUM_OF_MATERIAL = 60
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_61'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(61,1:NUM_DATA)
      NUM_OF_MATERIAL = 61
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_62'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(62,1:NUM_DATA)
      NUM_OF_MATERIAL = 62
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_63'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(63,1:NUM_DATA)
      NUM_OF_MATERIAL = 63
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_64'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(64,1:NUM_DATA)
      NUM_OF_MATERIAL = 64
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_65'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(65,1:NUM_DATA)
      NUM_OF_MATERIAL = 65
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_66'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(66,1:NUM_DATA)
      NUM_OF_MATERIAL = 66
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_67'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(67,1:NUM_DATA)
      NUM_OF_MATERIAL = 67
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_68'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(68,1:NUM_DATA)
      NUM_OF_MATERIAL = 68
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_69'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(69,1:NUM_DATA)
      NUM_OF_MATERIAL = 69
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_70'.and. Instructions(I_INFO)(18:18)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA(70,1:NUM_DATA)
      NUM_OF_MATERIAL = 70
  ELSEIF (Instructions(I_INFO)(1:17)=='*MATERIAL_PARA_71'.and. Instructions(I_INFO)(18:18)==' ')THEN
      print *, '    Error :: input file does not support *MATERIAL_PARA_71 or higher!'
      call Warning_Message('S',Keywords_Blank)
  ENDIF
  !..............................
  !材料参数补充参数:支持10种.
  !..............................
  IF (Instructions(I_INFO)(1:22)=='*MATERIAL_PARA_ADDED_1' .and. Instructions(I_INFO)(23:23)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA_ADDED(1,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*MATERIAL_PARA_ADDED_2' .and. Instructions(I_INFO)(23:23)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA_ADDED(2,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*MATERIAL_PARA_ADDED_3' .and. Instructions(I_INFO)(23:23)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA_ADDED(3,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*MATERIAL_PARA_ADDED_4' .and. Instructions(I_INFO)(23:23)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA_ADDED(4,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*MATERIAL_PARA_ADDED_5' .and. Instructions(I_INFO)(23:23)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA_ADDED(5,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*MATERIAL_PARA_ADDED_6' .and. Instructions(I_INFO)(23:23)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA_ADDED(6,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*MATERIAL_PARA_ADDED_7' .and. Instructions(I_INFO)(23:23)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA_ADDED(7,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*MATERIAL_PARA_ADDED_8' .and. Instructions(I_INFO)(23:23)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA_ADDED(8,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*MATERIAL_PARA_ADDED_9' .and. Instructions(I_INFO)(23:23)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA_ADDED(9,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:23)=='*MATERIAL_PARA_ADDED_10'.and. Instructions(I_INFO)(24:24)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      READ(Instructions(I_INFO+1) ,*) MATERIAL_PARA_ADDED(10,1:NUM_DATA)
  endif
  !......................................
  !非线性分析的载荷步设置:支持30个载荷步.
  !......................................
  IF (Instructions(I_INFO)(1:10)=='*NL_TIMS_1' .and. Instructions(I_INFO)(11:11)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(1,:)
  ELSEIF (Instructions(I_INFO)(1:10)=='*NL_TIMS_2' .and. Instructions(I_INFO)(11:11)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(2,:)
  ELSEIF (Instructions(I_INFO)(1:10)=='*NL_TIMS_3' .and. Instructions(I_INFO)(11:11)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(3,:)
  ELSEIF (Instructions(I_INFO)(1:10)=='*NL_TIMS_4' .and. Instructions(I_INFO)(11:11)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(4,:)
  ELSEIF (Instructions(I_INFO)(1:10)=='*NL_TIMS_5' .and. Instructions(I_INFO)(11:11)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(5,:)
  ELSEIF (Instructions(I_INFO)(1:10)=='*NL_TIMS_6' .and. Instructions(I_INFO)(11:11)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(6,:)
  ELSEIF (Instructions(I_INFO)(1:10)=='*NL_TIMS_7' .and. Instructions(I_INFO)(11:11)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(7,:)
  ELSEIF (Instructions(I_INFO)(1:10)=='*NL_TIMS_8' .and. Instructions(I_INFO)(11:11)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(8,:)
  ELSEIF (Instructions(I_INFO)(1:10)=='*NL_TIMS_9' .and. Instructions(I_INFO)(11:11)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(9,:)
  ELSEIF (Instructions(I_INFO)(1:11)=='*NL_TIMS_10'.and. Instructions(I_INFO)(12:12)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(10,:)
  ELSEIF (Instructions(I_INFO)(1:11)=='*NL_TIMS_11'.and. Instructions(I_INFO)(12:12)==' ')THEN  !IMPROV2023080902.
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(11,:)
  ELSEIF (Instructions(I_INFO)(1:11)=='*NL_TIMS_12'.and. Instructions(I_INFO)(12:12)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(12,:)
  ELSEIF (Instructions(I_INFO)(1:11)=='*NL_TIMS_13'.and. Instructions(I_INFO)(12:12)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(13,:)
  ELSEIF (Instructions(I_INFO)(1:11)=='*NL_TIMS_14'.and. Instructions(I_INFO)(12:12)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(14,:)
  ELSEIF (Instructions(I_INFO)(1:11)=='*NL_TIMS_15'.and. Instructions(I_INFO)(12:12)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(15,:)
  ELSEIF (Instructions(I_INFO)(1:11)=='*NL_TIMS_16'.and. Instructions(I_INFO)(12:12)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(16,:)
  ELSEIF (Instructions(I_INFO)(1:11)=='*NL_TIMS_17'.and. Instructions(I_INFO)(12:12)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(17,:)
  ELSEIF (Instructions(I_INFO)(1:11)=='*NL_TIMS_18'.and. Instructions(I_INFO)(12:12)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(18,:)
  ELSEIF (Instructions(I_INFO)(1:11)=='*NL_TIMS_19'.and. Instructions(I_INFO)(12:12)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(19,:)
  ELSEIF (Instructions(I_INFO)(1:11)=='*NL_TIMS_20'.and. Instructions(I_INFO)(12:12)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(20,:)
  ELSEIF (Instructions(I_INFO)(1:11)=='*NL_TIMS_21'.and. Instructions(I_INFO)(12:12)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(21,:)
  ELSEIF (Instructions(I_INFO)(1:11)=='*NL_TIMS_22'.and. Instructions(I_INFO)(12:12)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(22,:)
  ELSEIF (Instructions(I_INFO)(1:11)=='*NL_TIMS_23'.and. Instructions(I_INFO)(12:12)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(23,:)
  ELSEIF (Instructions(I_INFO)(1:11)=='*NL_TIMS_24'.and. Instructions(I_INFO)(12:12)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(24,:)
  ELSEIF (Instructions(I_INFO)(1:11)=='*NL_TIMS_25'.and. Instructions(I_INFO)(12:12)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(25,:)
  ELSEIF (Instructions(I_INFO)(1:11)=='*NL_TIMS_26'.and. Instructions(I_INFO)(12:12)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(26,:)
  ELSEIF (Instructions(I_INFO)(1:11)=='*NL_TIMS_27'.and. Instructions(I_INFO)(12:12)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(27,:)
  ELSEIF (Instructions(I_INFO)(1:11)=='*NL_TIMS_28'.and. Instructions(I_INFO)(12:12)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(28,:)
  ELSEIF (Instructions(I_INFO)(1:11)=='*NL_TIMS_29'.and. Instructions(I_INFO)(12:12)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(29,:)
  ELSEIF (Instructions(I_INFO)(1:11)=='*NL_TIMS_30'.and. Instructions(I_INFO)(12:12)==' ')THEN
      READ(Instructions(I_INFO+1) ,*) NL_TIMS(30,:)
  ELSEIF (Instructions(I_INFO)(1:11)=='*NL_TIMS_31'.and. Instructions(I_INFO)(12:12)==' ')THEN
      print *, '    Error :: input file does not support *NL_TIMS_31 or higher!'
      call Warning_Message('S',Keywords_Blank)
  ENDIF
  !.....................
  !非线性分析控制参数
  !.....................
  IF (Instructions(I_INFO)(1:8)=='*NL_ITRA'.and. Instructions(I_INFO)(9:9)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NL_ITRA
  ELSEIF (Instructions(I_INFO)(1:8)=='*NL_ATOL'.and. Instructions(I_INFO)(9:9)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NL_ATOL
  ELSEIF (Instructions(I_INFO)(1:8)=='*NL_NTOL'.and. Instructions(I_INFO)(9:9)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NL_NTOL
  ELSEIF (Instructions(I_INFO)(1:7)=='*NL_TOL'.and. Instructions(I_INFO)(8:8)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NL_TOL
  ENDIF
  !............................................
  !读取初始裂缝坐标(每条裂缝可以有多个坐标点)
  !............................................
  IF ((Instructions(I_INFO)(1:8)=='*CRACK_1') .AND. (Instructions(I_INFO)(9:9)==''))THEN
      !PRINT *,Instructions(I_INFO)(9:9)
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
        CRACK_COOR(1,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
        CRACK_COOR(1,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_CR_POI_NUM(1)  = NUM_DATA/2
  ELSEIF ((Instructions(I_INFO)(1:8)=='*CRACK_2') .AND. (Instructions(I_INFO)(9:9)==''))THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
        CRACK_COOR(2,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
        CRACK_COOR(2,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_CR_POI_NUM(2)  = NUM_DATA/2
  ELSEIF ((Instructions(I_INFO)(1:8)=='*CRACK_3').AND. (Instructions(I_INFO)(9:9)==''))THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
        CRACK_COOR(3,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
        CRACK_COOR(3,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_CR_POI_NUM(3)  = NUM_DATA/2
  ELSEIF ((Instructions(I_INFO)(1:8)=='*CRACK_4').AND. (Instructions(I_INFO)(9:9)==''))THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
        CRACK_COOR(4,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
        CRACK_COOR(4,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_CR_POI_NUM(4)  = NUM_DATA/2
  ELSEIF ((Instructions(I_INFO)(1:8)=='*CRACK_5') .AND. (Instructions(I_INFO)(9:9)==''))THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
        CRACK_COOR(5,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
        CRACK_COOR(5,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_CR_POI_NUM(5)  = NUM_DATA/2
  ELSEIF ((Instructions(I_INFO)(1:8)=='*CRACK_6').AND. (Instructions(I_INFO)(9:9)==''))THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
        CRACK_COOR(6,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
        CRACK_COOR(6,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_CR_POI_NUM(6)  = NUM_DATA/2
  ELSEIF ((Instructions(I_INFO)(1:8)=='*CRACK_7').AND. (Instructions(I_INFO)(9:9)==''))THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
        CRACK_COOR(7,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
        CRACK_COOR(7,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF ((Instructions(I_INFO)(1:8)=='*CRACK_8').AND. (Instructions(I_INFO)(9:9)==''))THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
        CRACK_COOR(8,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
        CRACK_COOR(8,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_CR_POI_NUM(8)  = NUM_DATA/2
  ELSEIF ((Instructions(I_INFO)(1:8)=='*CRACK_9') .AND. (Instructions(I_INFO)(9:9)==''))THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
        CRACK_COOR(9,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
        CRACK_COOR(9,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_CR_POI_NUM(9)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_10'.AND. (Instructions(I_INFO)(10:10)==''))THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
        CRACK_COOR(10,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
        CRACK_COOR(10,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_CR_POI_NUM(10)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_11'.AND. (Instructions(I_INFO)(10:10)==''))THEN !IMPROV2023080902.
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
        CRACK_COOR(11,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
        CRACK_COOR(11,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_CR_POI_NUM(11)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_12'.AND. (Instructions(I_INFO)(10:10)==''))THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
        CRACK_COOR(12,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
        CRACK_COOR(12,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_CR_POI_NUM(12)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_13'.AND. (Instructions(I_INFO)(10:10)==''))THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
        CRACK_COOR(13,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
        CRACK_COOR(13,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_CR_POI_NUM(13)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_14'.AND. (Instructions(I_INFO)(10:10)==''))THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
        CRACK_COOR(14,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
        CRACK_COOR(14,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_CR_POI_NUM(14)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_15'.AND. (Instructions(I_INFO)(10:10)==''))THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
        CRACK_COOR(15,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
        CRACK_COOR(15,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_CR_POI_NUM(15)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_16'.AND. (Instructions(I_INFO)(10:10)==''))THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
        CRACK_COOR(16,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
        CRACK_COOR(16,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_CR_POI_NUM(16)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_17'.AND. (Instructions(I_INFO)(10:10)==''))THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
        CRACK_COOR(17,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
        CRACK_COOR(17,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_CR_POI_NUM(17)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_18'.AND. (Instructions(I_INFO)(10:10)==''))THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
        CRACK_COOR(18,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
        CRACK_COOR(18,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_CR_POI_NUM(18)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_19'.AND. (Instructions(I_INFO)(10:10)==''))THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
        CRACK_COOR(19,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
        CRACK_COOR(19,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_CR_POI_NUM(19)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_20'.AND. (Instructions(I_INFO)(10:10)==''))THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
        CRACK_COOR(20,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
        CRACK_COOR(20,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_CR_POI_NUM(20)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_21'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(21,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(21,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(11)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_22'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(22,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(22,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(12)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_23'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(23,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(23,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(23)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_24'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(24,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(24,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(24)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_25'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(25,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(25,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(25)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_26'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(26,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(26,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(26)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_27'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(27,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(27,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(27)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_28'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(28,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(28,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(28)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_29'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(29,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(29,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(29)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_30'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(30,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(30,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(30)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_31'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(31,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(31,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(31)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_32'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(32,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(32,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(32)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_33'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(33,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(33,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(33)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_34'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(34,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(34,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(34)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_35'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(35,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(35,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(35)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_36'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(36,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(36,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(36)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_37'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(37,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(37,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(37)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_38'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(38,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(38,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(38)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_39'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(39,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(39,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(39)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_40'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(40,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(40,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(40)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_41'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(41,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(41,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(41)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_42'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(42,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(42,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(42)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_43'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(43,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(43,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(43)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_44'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(44,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(44,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(44)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_45'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(45,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(45,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(45)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_46'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(46,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(46,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(46)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_47'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(47,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(47,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(47)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_48'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(48,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(48,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(48)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_49'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(49,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(49,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(49)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_50'.AND. (Instructions(I_INFO)(10:10)==''))THEN
     CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
     IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
         WRITE(*,1002)
         CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
     ENDIF
     READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
     DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       CRACK_COOR(50,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       CRACK_COOR(50,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
     ENDDO
     EACH_CR_POI_NUM(50)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:9)=='*CRACK_51'.AND. (Instructions(I_INFO)(10:10)==''))THEN
      print *, '    Error :: input file does not support *CRACK_51 or higher!'
      call Warning_Message('S',Keywords_Blank)
  ENDIF

  !..................................
  !水力压裂分析相关
  !..................................
  IF (Instructions(I_INFO)(1:18)=='*KEY_NA_CRACK_TYPE')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_NA_CRACK_TYPE
  ELSEIF (Instructions(I_INFO)(1:8)=='*MNR_TOL')THEN
      READ(Instructions(I_INFO+1) , * ) MNR_TOL
  ELSEIF (Instructions(I_INFO)(1:18)=='*KEY_HF_CONV_CRITE')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_HF_CONV_CRITE
  ELSEIF (Instructions(I_INFO)(1:18)=='*KEY_HF_LINESEARCH')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_HF_LINESEARCH
  ENDIF

  !..............................................................
  !读取初始天然裂缝坐标(每条裂缝可以有多个坐标点),最多支持50个.
  !..............................................................
  IF (Instructions(I_INFO)(1:11)=='*NA_CRACK_1' .and. Instructions(I_INFO)(12:12)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(1,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(1,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(1)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:11)=='*NA_CRACK_2'.and. Instructions(I_INFO)(12:12)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(2,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(2,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(2)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:11)=='*NA_CRACK_3'.and. Instructions(I_INFO)(12:12)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(3,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(3,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(3)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:11)=='*NA_CRACK_4'.and. Instructions(I_INFO)(12:12)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(4,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(4,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(4)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:11)=='*NA_CRACK_5'.and. Instructions(I_INFO)(12:12)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(5,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(5,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(5)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:11)=='*NA_CRACK_6'.and. Instructions(I_INFO)(12:12)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(6,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(6,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(6)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:11)=='*NA_CRACK_7'.and. Instructions(I_INFO)(12:12)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(7,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(7,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:11)=='*NA_CRACK_8'.and. Instructions(I_INFO)(12:12)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(8,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(8,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:11)=='*NA_CRACK_9'.and. Instructions(I_INFO)(12:12)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(9,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(9,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_10'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(10,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(10,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_11'.and. Instructions(I_INFO)(13:13)==' ')THEN  !IMPROV2023080902.
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(11,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(11,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_12'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(12,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(12,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_13'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(13,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(13,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_14'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(14,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(14,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_15'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(15,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(15,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_16'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(16,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(16,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_17'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(17,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(17,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_18'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(18,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(18,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_19'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(19,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(19,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_20'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(20,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(20,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_21'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(21,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(21,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_22'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(22,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(22,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_23'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(23,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(23,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_24'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(24,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(24,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_25'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(25,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(25,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_26'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(26,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(26,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_27'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(27,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(27,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_28'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(28,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(28,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_29'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(29,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(29,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_30'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(30,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(30,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_31'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(31,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(31,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_32'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(32,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(32,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_33'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(33,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(33,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_34'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(34,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(34,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_35'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(35,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(35,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_36'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(36,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(36,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_37'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(37,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(37,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_38'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(38,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(38,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_39'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(39,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(39,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_40'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(40,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(40,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_41'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(41,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(41,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_42'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(42,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(42,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_43'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(43,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(43,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_44'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(44,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(44,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_45'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(45,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(45,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_46'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(46,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(46,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_47'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(47,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(47,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_48'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(48,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(48,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_49'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(49,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(49,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_50'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA/2-1  !裂缝片段循环
       NA_CRACK_COOR(50,I,1:2)=T_CRACK_COORS(2*I-1:2*I)
       NA_CRACK_COOR(50,I+1,1:2)=T_CRACK_COORS(2*(I+1)-1:2*(I+1))
      ENDDO
      EACH_NA_CR_POI_NUM(7)  = NUM_DATA/2
  ELSEIF (Instructions(I_INFO)(1:12)=='*NA_CRACK_31')THEN
      print *, '    Error :: input file does not support *NA_CRACK_51 or higher!'
      call Warning_Message('S',Keywords_Blank)
  ENDIF

  !.............................................................
  !读取3D初始裂缝坐标(每条裂缝可以有多个坐标点),最多支持50个.
  !.............................................................
  IF (Instructions(I_INFO)(1:15)=='*CRACK3D_COOR_1' .and. Instructions(I_INFO)(16:16)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      !print *,Instructions(I_INFO+1)
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(1,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(1)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:15)=='*CRACK3D_COOR_2'.and. Instructions(I_INFO)(16:16)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(2,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(2)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:15)=='*CRACK3D_COOR_3'.and. Instructions(I_INFO)(16:16)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(3,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(3)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:15)=='*CRACK3D_COOR_4'.and. Instructions(I_INFO)(16:16)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(4,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(4)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:15)=='*CRACK3D_COOR_5'.and. Instructions(I_INFO)(16:16)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(5,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(5)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:15)=='*CRACK3D_COOR_6'.and. Instructions(I_INFO)(16:16)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(6,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(6)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:15)=='*CRACK3D_COOR_7'.and. Instructions(I_INFO)(16:16)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(7,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(7)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:15)=='*CRACK3D_COOR_8'.and. Instructions(I_INFO)(16:16)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(8,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(8)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:15)=='*CRACK3D_COOR_9'.and. Instructions(I_INFO)(16:16)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(9,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(9)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_10'.and. Instructions(I_INFO)(17:17)==' ')THEN !IMPROV2023080902.
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(10,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(10)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_11'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(11,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(11)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_12'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(12,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(12)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_13'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(13,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(13)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_14'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(14,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(14)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_15'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(15,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(15)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_16'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(16,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(16)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_17'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(17,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(17)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_18'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(18,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(18)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_19'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(19,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(19)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_20'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(20,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(20)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_21'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(21,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(21)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_22'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(22,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(22)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_23'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(23,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(23)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_24'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(24,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(24)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_25'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(25,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(25)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_26'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(26,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(26)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_27'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(27,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(27)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_28'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(28,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(28)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_29'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(29,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(29)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_30'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(30,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(30)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_31'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(31,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(31)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_32'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(32,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(32)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_33'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(33,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(33)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_34'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(34,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(34)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_35'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(35,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(35)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_36'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(36,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(36)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_37'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(37,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(37)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_38'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(38,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(38)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_39'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(39,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(39)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_40'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(40,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(40)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_41'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(41,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(41)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_42'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(42,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(42)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_43'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(43,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(43)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_44'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(44,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(44)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_45'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(45,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(45)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_46'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(46,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(46)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_47'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(47,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(47)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_48'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(48,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(48)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_49'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(49,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(49)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_50'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        CRACK3D_COOR(50,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      EACH_CR_POI_NUM(50)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACK3D_COOR_51'.and. Instructions(I_INFO)(17:17)==' ')THEN
      print *, '    Error :: input file does not support *CRACK3D_COOR_51 or higher!'
      call Warning_Message('S',Keywords_Blank)
  ENDIF

  !........................................
  !读取3D初始圆形裂缝坐标, 最多30个.
  !........................................
  IF (Instructions(I_INFO)(1:19)=='*CRACK3D_CIR_COOR_1' .and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(1,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:19)=='*CRACK3D_CIR_COOR_2'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(2,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:19)=='*CRACK3D_CIR_COOR_3'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(3,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:19)=='*CRACK3D_CIR_COOR_4'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(4,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:19)=='*CRACK3D_CIR_COOR_5'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(5,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:19)=='*CRACK3D_CIR_COOR_6'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(6,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:19)=='*CRACK3D_CIR_COOR_7'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(7,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:19)=='*CRACK3D_CIR_COOR_8'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(8,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:19)=='*CRACK3D_CIR_COOR_9'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(9,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:20)=='*CRACK3D_CIR_COOR_10'.and. Instructions(I_INFO)(21:21)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(10,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:20)=='*CRACK3D_CIR_COOR_11'.and. Instructions(I_INFO)(21:21)==' ')THEN !IMPROV2023080902.
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(11,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:20)=='*CRACK3D_CIR_COOR_12'.and. Instructions(I_INFO)(21:21)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(12,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:20)=='*CRACK3D_CIR_COOR_13'.and. Instructions(I_INFO)(21:21)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(13,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:20)=='*CRACK3D_CIR_COOR_14'.and. Instructions(I_INFO)(21:21)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(14,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:20)=='*CRACK3D_CIR_COOR_15'.and. Instructions(I_INFO)(21:21)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(15,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:20)=='*CRACK3D_CIR_COOR_16'.and. Instructions(I_INFO)(21:21)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(16,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:20)=='*CRACK3D_CIR_COOR_17'.and. Instructions(I_INFO)(21:21)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(17,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:20)=='*CRACK3D_CIR_COOR_18'.and. Instructions(I_INFO)(21:21)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(18,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:20)=='*CRACK3D_CIR_COOR_19'.and. Instructions(I_INFO)(21:21)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(19,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:20)=='*CRACK3D_CIR_COOR_20'.and. Instructions(I_INFO)(21:21)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(20,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:20)=='*CRACK3D_CIR_COOR_21'.and. Instructions(I_INFO)(21:21)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(21,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:20)=='*CRACK3D_CIR_COOR_22'.and. Instructions(I_INFO)(21:21)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(22,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:20)=='*CRACK3D_CIR_COOR_23'.and. Instructions(I_INFO)(21:21)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(23,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:20)=='*CRACK3D_CIR_COOR_24'.and. Instructions(I_INFO)(21:21)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(24,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:20)=='*CRACK3D_CIR_COOR_25'.and. Instructions(I_INFO)(21:21)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(25,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:20)=='*CRACK3D_CIR_COOR_26'.and. Instructions(I_INFO)(21:21)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(26,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:20)=='*CRACK3D_CIR_COOR_27'.and. Instructions(I_INFO)(21:21)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(27,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:20)=='*CRACK3D_CIR_COOR_28'.and. Instructions(I_INFO)(21:21)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(28,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:20)=='*CRACK3D_CIR_COOR_29'.and. Instructions(I_INFO)(21:21)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(29,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:20)=='*CRACK3D_CIR_COOR_30'.and. Instructions(I_INFO)(21:21)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) CRACK3D_CIR_COOR(30,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:20)=='*CRACK3D_CIR_COOR_31'.and. Instructions(I_INFO)(21:21)==' ')THEN
    print *, '    Error :: input file does not support *CRACK3D_CIR_COOR_31 or higher!'
    call Warning_Message('S',Keywords_Blank)
  ENDIF

  !....................................
  !读取3D初始椭圆裂缝坐标, 最多30个.
  !....................................
  IF (Instructions(I_INFO)(1:21)=='*CRACK3D_ELLIP_COOR_1' .and. Instructions(I_INFO)(22:22)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(1,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:21)=='*CRACK3D_ELLIP_COOR_2'.and. Instructions(I_INFO)(22:22)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(2,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:21)=='*CRACK3D_ELLIP_COOR_3'.and. Instructions(I_INFO)(22:22)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(3,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:21)=='*CRACK3D_ELLIP_COOR_4'.and. Instructions(I_INFO)(22:22)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(4,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:21)=='*CRACK3D_ELLIP_COOR_5'.and. Instructions(I_INFO)(22:22)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(5,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:21)=='*CRACK3D_ELLIP_COOR_6'.and. Instructions(I_INFO)(22:22)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(6,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:21)=='*CRACK3D_ELLIP_COOR_7'.and. Instructions(I_INFO)(22:22)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(7,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:21)=='*CRACK3D_ELLIP_COOR_8'.and. Instructions(I_INFO)(22:22)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(8,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:21)=='*CRACK3D_ELLIP_COOR_9'.and. Instructions(I_INFO)(22:22)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(9,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*CRACK3D_ELLIP_COOR_10'.and. Instructions(I_INFO)(23:23)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(10,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*CRACK3D_ELLIP_COOR_11'.and. Instructions(I_INFO)(23:23)==' ')THEN !IMPROV2023080902.
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(11,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*CRACK3D_ELLIP_COOR_12'.and. Instructions(I_INFO)(23:23)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(12,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*CRACK3D_ELLIP_COOR_13'.and. Instructions(I_INFO)(23:23)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(13,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*CRACK3D_ELLIP_COOR_14'.and. Instructions(I_INFO)(23:23)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(14,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*CRACK3D_ELLIP_COOR_15'.and. Instructions(I_INFO)(23:23)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(15,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*CRACK3D_ELLIP_COOR_16'.and. Instructions(I_INFO)(23:23)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(16,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*CRACK3D_ELLIP_COOR_17'.and. Instructions(I_INFO)(23:23)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(17,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*CRACK3D_ELLIP_COOR_18'.and. Instructions(I_INFO)(23:23)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(18,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*CRACK3D_ELLIP_COOR_19'.and. Instructions(I_INFO)(23:23)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(19,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*CRACK3D_ELLIP_COOR_20'.and. Instructions(I_INFO)(23:23)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(20,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*CRACK3D_ELLIP_COOR_21'.and. Instructions(I_INFO)(23:23)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(21,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*CRACK3D_ELLIP_COOR_22'.and. Instructions(I_INFO)(23:23)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(22,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*CRACK3D_ELLIP_COOR_23'.and. Instructions(I_INFO)(23:23)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(23,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*CRACK3D_ELLIP_COOR_24'.and. Instructions(I_INFO)(23:23)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(24,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*CRACK3D_ELLIP_COOR_25'.and. Instructions(I_INFO)(23:23)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(25,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*CRACK3D_ELLIP_COOR_26'.and. Instructions(I_INFO)(23:23)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(26,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*CRACK3D_ELLIP_COOR_27'.and. Instructions(I_INFO)(23:23)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(27,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*CRACK3D_ELLIP_COOR_28'.and. Instructions(I_INFO)(23:23)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(28,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*CRACK3D_ELLIP_COOR_29'.and. Instructions(I_INFO)(23:23)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(29,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*CRACK3D_ELLIP_COOR_30'.and. Instructions(I_INFO)(23:23)==' ')THEN
   CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
   READ(Instructions(I_INFO+1),*) CRACK3D_ELLIP_COOR(30,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*CRACK3D_ELLIP_COOR_31'.and. Instructions(I_INFO)(23:23)==' ')THEN
    print *, '    Error :: input file does not support *CRACK3D_ELLIP_COOR_31 or higher!'
    call Warning_Message('S',Keywords_Blank)
  ENDIF

  !.........................
  !其他裂缝相关数据的读取
  !.........................
  IF (Instructions(I_INFO)(1:14)=='*INJECT_Q_TIME'.and. Instructions(I_INFO)(15:15)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)!由逗号隔开的字符串数据中数据的个数,"逗号数加1"
      READ(Instructions(I_INFO+1) ,*) INJECT_Q_TIME(1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:13)=='*INJECT_Q_VAL'.and. Instructions(I_INFO)(14:14)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)!由逗号隔开的字符串数据中数据的个数,"逗号数加1"
      READ(Instructions(I_INFO+1),*) INJECT_Q_VAL(1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:17)=='*INJECT_CRACK_NUM'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) INJECT_CRACK_NUM
  ELSEIF (Instructions(I_INFO)(1:19)=='*CRACKS_ALLOW_PROPA'.and. Instructions(I_INFO)(20:20)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(NUM_DATA < NUM_CRACK)THEN
          PRINT *,'    ERROR:: WRONG *CRACKS_ALLOW_PROPA, ALL CRACKS SHOULD BE GIVEN!'
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1),*)CRACKS_ALLOW_PROPA(1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:16)=='*CRACKS_HF_STATE'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(NUM_DATA < NUM_CRACK)THEN
          PRINT *,'    ERROR:: WRONG *CRACKS_HF_STATE, ALL CRACKS SHOULD BE GIVEN!'
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1),*)CRACKS_HF_STATE(1:NUM_DATA)
      !PRINT *,CRACKS_HF_STATE(1:NUM_DATA)
  ENDIF

  !...........................
  !初始HOLE相关,最多支持30个.
  !...........................
  IF (Instructions(I_INFO)(1:12)=='*HOLE_COOR_1' .and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(1,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:12)=='*HOLE_COOR_2'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(2,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:12)=='*HOLE_COOR_3'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(3,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:12)=='*HOLE_COOR_4'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(4,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:12)=='*HOLE_COOR_5'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(5,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:12)=='*HOLE_COOR_6'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(6,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:12)=='*HOLE_COOR_7'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(7,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:12)=='*HOLE_COOR_8'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(8,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:12)=='*HOLE_COOR_9'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(9,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:13)=='*HOLE_COOR_10'.and. Instructions(I_INFO)(14:14)==' ')THEN !IMPROV2023080902.
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(10,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:13)=='*HOLE_COOR_11'.and. Instructions(I_INFO)(14:14)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(11,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:13)=='*HOLE_COOR_12'.and. Instructions(I_INFO)(14:14)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(12,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:13)=='*HOLE_COOR_13'.and. Instructions(I_INFO)(14:14)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(13,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:13)=='*HOLE_COOR_14'.and. Instructions(I_INFO)(14:14)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(14,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:13)=='*HOLE_COOR_15'.and. Instructions(I_INFO)(14:14)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(15,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:13)=='*HOLE_COOR_16'.and. Instructions(I_INFO)(14:14)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(16,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:13)=='*HOLE_COOR_17'.and. Instructions(I_INFO)(14:14)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(17,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:13)=='*HOLE_COOR_18'.and. Instructions(I_INFO)(14:14)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(18,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:13)=='*HOLE_COOR_19'.and. Instructions(I_INFO)(14:14)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(19,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:13)=='*HOLE_COOR_20'.and. Instructions(I_INFO)(14:14)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(20,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:13)=='*HOLE_COOR_21'.and. Instructions(I_INFO)(14:14)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(21,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:13)=='*HOLE_COOR_22'.and. Instructions(I_INFO)(14:14)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(22,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:13)=='*HOLE_COOR_23'.and. Instructions(I_INFO)(14:14)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(23,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:13)=='*HOLE_COOR_24'.and. Instructions(I_INFO)(14:14)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(24,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:13)=='*HOLE_COOR_25'.and. Instructions(I_INFO)(14:14)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(25,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:13)=='*HOLE_COOR_26'.and. Instructions(I_INFO)(14:14)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(26,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:13)=='*HOLE_COOR_27'.and. Instructions(I_INFO)(14:14)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(27,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:13)=='*HOLE_COOR_28'.and. Instructions(I_INFO)(14:14)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(28,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:13)=='*HOLE_COOR_29'.and. Instructions(I_INFO)(14:14)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(29,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:13)=='*HOLE_COOR_30'.and. Instructions(I_INFO)(14:14)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        HOLE_COOR(30,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:13)=='*HOLE_COOR_31'.and. Instructions(I_INFO)(14:14)==' ')THEN
      print *, '    Error :: input file does not support *HOLE_COOR_31 or higher!'
      call Warning_Message('S',Keywords_Blank)
  ENDIF

  !........................................
  !初始圆形INCLUSION坐标相关,最多支持30个.
  !........................................
  IF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_COOR_1' .and. Instructions(I_INFO)(19:19)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(1,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_COOR_2'.and. Instructions(I_INFO)(19:19)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(2,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_COOR_3'.and. Instructions(I_INFO)(19:19)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(3,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_COOR_4'.and. Instructions(I_INFO)(19:19)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(4,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_COOR_5'.and. Instructions(I_INFO)(19:19)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(5,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_COOR_6'.and. Instructions(I_INFO)(19:19)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(6,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_COOR_7'.and. Instructions(I_INFO)(19:19)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(7,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_COOR_8'.and. Instructions(I_INFO)(19:19)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(8,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_COOR_9'.and. Instructions(I_INFO)(19:19)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(9,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:19)=='*CIRC_INCLU_COOR_10'.and. Instructions(I_INFO)(20:20)==' ')THEN  !IMPROV2023080902.
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(10,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:19)=='*CIRC_INCLU_COOR_11'.and. Instructions(I_INFO)(20:20)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(11,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:19)=='*CIRC_INCLU_COOR_12'.and. Instructions(I_INFO)(20:20)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(12,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:19)=='*CIRC_INCLU_COOR_13'.and. Instructions(I_INFO)(20:20)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(13,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:19)=='*CIRC_INCLU_COOR_14'.and. Instructions(I_INFO)(20:20)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(14,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:19)=='*CIRC_INCLU_COOR_15'.and. Instructions(I_INFO)(20:20)==' ')THEN  !IMPROV2023080902.
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(15,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:19)=='*CIRC_INCLU_COOR_16'.and. Instructions(I_INFO)(20:20)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(16,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:19)=='*CIRC_INCLU_COOR_17'.and. Instructions(I_INFO)(20:20)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(17,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:19)=='*CIRC_INCLU_COOR_18'.and. Instructions(I_INFO)(20:20)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(18,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:19)=='*CIRC_INCLU_COOR_19'.and. Instructions(I_INFO)(20:20)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(19,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:19)=='*CIRC_INCLU_COOR_20'.and. Instructions(I_INFO)(20:20)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(20,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:19)=='*CIRC_INCLU_COOR_21'.and. Instructions(I_INFO)(20:20)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(21,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:19)=='*CIRC_INCLU_COOR_22'.and. Instructions(I_INFO)(20:20)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(22,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:19)=='*CIRC_INCLU_COOR_23'.and. Instructions(I_INFO)(20:20)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(10,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:23)=='*CIRC_INCLU_COOR_10'.and. Instructions(I_INFO)(20:20)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(10,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:19)=='*CIRC_INCLU_COOR_24'.and. Instructions(I_INFO)(20:20)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(24,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:19)=='*CIRC_INCLU_COOR_25'.and. Instructions(I_INFO)(20:20)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(25,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:19)=='*CIRC_INCLU_COOR_26'.and. Instructions(I_INFO)(20:20)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(26,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:19)=='*CIRC_INCLU_COOR_27'.and. Instructions(I_INFO)(20:20)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(27,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:19)=='*CIRC_INCLU_COOR_28'.and. Instructions(I_INFO)(20:20)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(28,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:19)=='*CIRC_INCLU_COOR_29'.and. Instructions(I_INFO)(20:20)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(29,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:19)=='*CIRC_INCLU_COOR_30'.and. Instructions(I_INFO)(20:20)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_HOLE_COORS(1:NUM_DATA)
      DO I = 1,NUM_DATA
        CIRC_INCLU_COOR(30,I) =  T_HOLE_COORS(I)
      ENDDO
  ELSEIF (Instructions(I_INFO)(1:19)=='*CIRC_INCLU_COOR_31'.and. Instructions(I_INFO)(20:20)==' ')THEN
      print *, '    Error :: input file does not support *CIRC_INCLU_COOR_31 or higher!'
      call Warning_Message('S',Keywords_Blank)
  ENDIF

  !................................
  !初始圆形INCLUSION材料号相关.
  !................................
  IF (Instructions(I_INFO)(1:17)=='*CIRC_INCLU_MAT_1' .and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(1)
  ELSEIF (Instructions(I_INFO)(1:17)=='*CIRC_INCLU_MAT_2'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(2)
  ELSEIF (Instructions(I_INFO)(1:17)=='*CIRC_INCLU_MAT_3'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(3)
  ELSEIF (Instructions(I_INFO)(1:17)=='*CIRC_INCLU_MAT_4'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(4)
  ELSEIF (Instructions(I_INFO)(1:17)=='*CIRC_INCLU_MAT_5'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(5)
  ELSEIF (Instructions(I_INFO)(1:17)=='*CIRC_INCLU_MAT_6'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(6)
  ELSEIF (Instructions(I_INFO)(1:17)=='*CIRC_INCLU_MAT_7'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(7)
  ELSEIF (Instructions(I_INFO)(1:17)=='*CIRC_INCLU_MAT_8'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(8)
  ELSEIF (Instructions(I_INFO)(1:17)=='*CIRC_INCLU_MAT_9'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(9)
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_MAT_10'.and. Instructions(I_INFO)(19:19)==' ')THEN !IMPROV2023080902.
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(10)
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_MAT_11'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(11)
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_MAT_12'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(12)
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_MAT_13'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(13)
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_MAT_14'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(14)
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_MAT_15'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(15)
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_MAT_16'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(16)
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_MAT_17'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(17)
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_MAT_18'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(18)
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_MAT_19'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(19)
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_MAT_20'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(20)
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_MAT_21'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(21)
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_MAT_22'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(22)
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_MAT_23'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(23)
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_MAT_24'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(24)
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_MAT_25'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(25)
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_MAT_26'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(26)
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_MAT_27'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(27)
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_MAT_28'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(28)
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_MAT_29'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(29)
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_MAT_30'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CIRC_INCLU_MAT_NUM(30)
  ELSEIF (Instructions(I_INFO)(1:18)=='*CIRC_INCLU_MAT_31'.and. Instructions(I_INFO)(19:19)==' ')THEN
      print *, '    Error :: input file does not support *CIRC_INCLU_MAT_31 or higher!'
      call Warning_Message('S',Keywords_Blank)
  ENDIF
  !............
  !后处理相关
  !............
  IF (Instructions(I_INFO)(1:19)=='*KEY_POST_CS_N_STRS'.and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_POST_CS_N_STRS
      !PRINT *,'KEY_POST_CS_N_STRS:',KEY_POST_CS_N_STRS
  ENDIF
  !..................
  !随机生成裂缝相关
  !..................
  IF (Instructions(I_INFO)(1:16)=='*KEY_RANDOM_NACR'.and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_RANDOM_NACR
  ELSEIF (Instructions(I_INFO)(1:18)=='*KEY_RANDOM_SCHEME'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_RANDOM
  ELSEIF (Instructions(I_INFO)(1:5)=='*SEED'.and. Instructions(I_INFO)(6:6)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) SEED
  ELSEIF (Instructions(I_INFO)(1:18)=='*NUM_RAND_NA_CRACK'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_RAND_NA_CRACK
      !重要程序行,生成的裂缝,定义每个随机生成裂缝有两个坐标点
      !PRINT *,9999,NUM_CRACK
      EACH_CR_POI_NUM(NUM_CRACK+1:NUM_CRACK+NUM_RAND_NA_CRACK) = 2
  ELSEIF(Instructions(I_INFO)(1:17)=='*NACR_ORIENTATION'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NACR_ORIENTATION
  ELSEIF (Instructions(I_INFO)(1:15)=='*NACR_ORI_DELTA'.and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NACR_ORI_DELTA
  ELSEIF (Instructions(I_INFO)(1:12)=='*NACR_LENGTH'.and. Instructions(I_INFO)(13:13)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NACR_LENGTH
  ELSEIF (Instructions(I_INFO)(1:15)=='*NACR_LEN_DELTA'.and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NACR_LEN_DELTA
  ENDIF
  !..................
  !随机生成夹杂相关
  !..................
  IF (Instructions(I_INFO)(1:19)=='*KEY_RAND_CIRC_INCL'.and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_RAND_CIRC_INCL
  ELSEIF (Instructions(I_INFO)(1:19)=='*NUM_RAND_CIRC_INCL'.and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_RAND_CIRC_INCL
  ELSEIF (Instructions(I_INFO)(1:17)=='*RAND_CIRC_INCL_R'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) RAND_CIRC_INCL_R
  ELSEIF(Instructions(I_INFO)(1:22)=='*RAND_CIRC_INC_R_DELTA'.and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) RAND_CIRC_INC_R_DELTA
  ELSEIF (Instructions(I_INFO)(1:19)=='*KEY_RAND_POLY_INCL'.and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_RAND_POLY_INCL
  ELSEIF (Instructions(I_INFO)(1:19)=='*NUM_RAND_POLY_INCL'.and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_RAND_POLY_INCL
  ELSEIF (Instructions(I_INFO)(1:19)=='*NUM_VERT_POLY_INCL'.and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_VERT_POLY_INCL
  ELSEIF(Instructions(I_INFO)(1:17)=='*RAND_POLY_INCL_R'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) RAND_POLY_INCL_R
  ELSEIF(Instructions(I_INFO)(1:22)=='*RAND_POLY_INC_DELTA_R'.and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) RAND_POLY_INC_R_DELTA
  ENDIF
  !..................
  !随机生成孔洞相关
  !..................
  IF (Instructions(I_INFO)(1:16)=='*KEY_RANDOM_HOLE'.and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_RANDOM_HOLE
  ELSEIF (Instructions(I_INFO)(1:14)=='*NUM_RAND_HOLE'.and. Instructions(I_INFO)(15:15)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_RAND_HOLE
  ELSEIF (Instructions(I_INFO)(1:12)=='*RAND_HOLE_R'.and. Instructions(I_INFO)(13:13)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) RAND_HOLE_R
  ELSEIF (Instructions(I_INFO)(1:18)=='*RAND_HOLE_DELTA_R'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) RAND_HOLE_R_DELTA
  ENDIF
  !.....................
  !节点自由度耦合相关
  !.....................
  IF(Instructions(I_INFO)(1:15)=='*NUM_CP_X_NODES'.and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_CP_X_NODES
  ELSEIF(Instructions(I_INFO)(1:15)=='*NUM_CP_Y_NODES'.and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_CP_Y_NODES
  ELSEIF(Instructions(I_INFO)(1:11)=='*CP_X_NODES'.and. Instructions(I_INFO)(12:12)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)!由逗号隔开的字符串数据中数据的个数,"逗号数加1"
      READ(Instructions(I_INFO+1) ,*) CP_X_NODES(1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:11)=='*CP_Y_NODES'.and. Instructions(I_INFO)(12:12)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)!由逗号隔开的字符串数据中数据的个数,"逗号数加1"
      READ(Instructions(I_INFO+1) ,*) CP_Y_NODES(1:NUM_DATA)
  ENDIF
  !................
  !动态分析相关
  !................
  IF(Instructions(I_INFO)(1:15)=='*IDY_NUM_ITERAS'.and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) IDY_NUM_ITERAS
  ELSEIF(Instructions(I_INFO)(1:18)=='*IDY_NUM_FORCE_ITR'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) IDY_NUM_FORCE_ITR
  ELSEIF(Instructions(I_INFO)(1:15)=='*FACTOR_PROP_DY'.and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) FACTOR_PROP_DY
  ELSEIF(Instructions(I_INFO)(1:18)=='*DELT_TIME_NEWMARK'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) DELT_TIME_NEWMARK
  ENDIF
  !地震加速度相关数据读取
  IF(Instructions(I_INFO)(1:7)=='*KEY_EQ'.and. Instructions(I_INFO)(8:8)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_EQ
  ELSEIF(Instructions(I_INFO)(1:15)=='*EQ_AC_TIME_GAP'.and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) EQ_AC_TIME_GAP
  ELSEIF(Instructions(I_INFO)(1:16)=='*NUM_EQ_AC_NODES'.and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_EQ_AC_NODES
  ELSEIF(Instructions(I_INFO)(1:12)=='*EQ_AC_NODES'.and. Instructions(I_INFO)(13:13)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN) !由逗号隔开的字符串数据中数据的个数,"逗号数加1"
      READ(Instructions(I_INFO+1) ,*) EQ_AC_NODES(1:NUM_DATA)
  ENDIF
  !正弦加速度激励相关数据读取
  IF(Instructions(I_INFO)(1:14)=='*KEY_SIN_ACCEL'.and. Instructions(I_INFO)(15:15)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_SIN_ACCEL
  ELSEIF(Instructions(I_INFO)(1:15)=='*SIN_ACCEL_DIRE'.and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) SIN_ACCEL_DIRE
  ELSEIF(Instructions(I_INFO)(1:12)=='*SIN_ACCEL_A'.and. Instructions(I_INFO)(13:13)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) SIN_ACCEL_A
  ELSEIF(Instructions(I_INFO)(1:12)=='*SIN_ACCEL_T'.and. Instructions(I_INFO)(13:13)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) SIN_ACCEL_T
  ELSEIF(Instructions(I_INFO)(1:20)=='*SIN_ACCEL_NUM_NODES'.and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) SIN_ACCEL_NUM_NODES
  ELSEIF(Instructions(I_INFO)(1:16)=='*SIN_ACCEL_NODES'.and. Instructions(I_INFO)(17:17)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)!由逗号隔开的字符串数据中数据的个数,"逗号数加1"
      READ(Instructions(I_INFO+1) ,*) SIN_ACCEL_NODES(1:NUM_DATA)
  ENDIF
  !.......
  !其他
  !.......
  IF(Instructions(I_INFO)(1:17)=='*KEY_CLOSE_WINDOW'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_CLOSE_WINDOW
  ELSEIF(Instructions(I_INFO)(1:16)=='*KEY_NUM_PROCESS'.and. Instructions(I_INFO)(17:17)==' ')THEN
      !READ(Instructions(I_INFO+1) , * ) KEY_NUM_PROCESS
      if(Yes_n .eqv. .false.) then  !2024-02-28.
          READ(Instructions(I_INFO+1) , * ) KEY_NUM_PROCESS
      endif
  ELSEIF(Instructions(I_INFO)(1:15)=='*KEY_WINDOW_LOG'.and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_WINDOW_LOG
  ELSEIF(Instructions(I_INFO)(1:14)=='*KEY_CLEAR_ALL'.and. Instructions(I_INFO)(15:15)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_CLEAR_ALL
  ELSEIF(Instructions(I_INFO)(1:21)=='*KEY_VISIT_PHIPSI_TOP'.and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_VISIT_PHIPSI_TOP
  ELSEIF(Instructions(I_INFO)(1:19)=='*KEY_POST_S_TANDISP'.and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_POST_S_TANDISP
  ELSEIF(Instructions(I_INFO)(1:21)=='*KEY_CONTA_INTG_POINT'.and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CONTA_INTEG_POINT
  ELSEIF(Instructions(I_INFO)(1:24)=='*KEY_HOLE_CRACK_GENERATE'.and. Instructions(I_INFO)(25:25)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_HOLE_CRACK_GENERATE
  ELSEIF(Instructions(I_INFO)(1:26)=='*KEY_NUM_CR_HOLE_GENERATED'.and. Instructions(I_INFO)(27:27)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_CRACK_HOLE_GENERATED
  ELSEIF(Instructions(I_INFO)(1:16)=='*KEY_PLAY_SOUNDS'.and. Instructions(I_INFO)(17:17)==' ') THEN
      READ(Instructions(I_INFO+1) , * ) KEY_PLAY_SOUNDS
  ELSEIF(Instructions(I_INFO)(1:17)=='*KEY_FD_TIPENRICH'.and. Instructions(I_INFO)(18:18)==' ') THEN
      READ(Instructions(I_INFO+1) , * ) KEY_FD_TIPENRICH
  ELSEIF(Instructions(I_INFO)(1:19)=='*KEY_FD_BODY_SOURCE'.and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_FD_BODY_SOURCE
  ELSEIF(Instructions(I_INFO)(1:15)=='*WATER_PRESSURE'.and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) WATER_PRESSURE
  ELSEIF(Instructions(I_INFO)(1:20)=='*TYPE_WATER_PRESSURE'.and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) TYPE_WATER_PRESSURE
  ELSEIF(Instructions(I_INFO)(1:20)=='*KEY_INSITU_STRATEGY'.and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_INSITU_STRATEGY
  ENDIF

  !缝内水压控制关键字, ADDED ON 2021-11-06
  IF (Instructions(I_INFO)(1:25)=='*KEY_CRACK_INNER_PRESSURE'.and. Instructions(I_INFO)(26:26)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_CRACK_INNER_PRESSURE
  ENDIF

  !缝内水压
  IF (Instructions(I_INFO)(1:21)=='*INI_CRACK_PRESSURE_1' .and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(1)
  ELSEIF(Instructions(I_INFO)(1:21)=='*INI_CRACK_PRESSURE_2' .and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(2)
  ELSEIF(Instructions(I_INFO)(1:21)=='*INI_CRACK_PRESSURE_3' .and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(3)
  ELSEIF(Instructions(I_INFO)(1:21)=='*INI_CRACK_PRESSURE_4' .and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(4)
  ELSEIF(Instructions(I_INFO)(1:21)=='*INI_CRACK_PRESSURE_5' .and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(5)
  ELSEIF(Instructions(I_INFO)(1:21)=='*INI_CRACK_PRESSURE_6' .and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(6)
  ELSEIF(Instructions(I_INFO)(1:21)=='*INI_CRACK_PRESSURE_7' .and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(7)
  ELSEIF(Instructions(I_INFO)(1:21)=='*INI_CRACK_PRESSURE_8' .and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(8)
  ELSEIF(Instructions(I_INFO)(1:21)=='*INI_CRACK_PRESSURE_9' .and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(9)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_10' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(10)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_11' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(11)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_12' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(12)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_13' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(13)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_14' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(14)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_15' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(15)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_16' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(16)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_17' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(17)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_18' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(18)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_19' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(19)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_20' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(20)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_21' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(21)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_22' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(22)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_23' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(23)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_24' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(24)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_25' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(25)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_26' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(26)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_27' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(27)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_28' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(28)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_29' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(29)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_30' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(30)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_31' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(31)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_32' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(32)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_33' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(33)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_34' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(34)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_35' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(35)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_36' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(36)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_37' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(37)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_38' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(38)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_39' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(39)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_40' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(40)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_41' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(41)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_42' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(42)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_43' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(43)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_44' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(44)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_45' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(45)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_46' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(46)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_47' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(47)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_48' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(48)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_49' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(49)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_50' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) CRACK_PRESSURE(50)
  ELSEIF(Instructions(I_INFO)(1:22)=='*INI_CRACK_PRESSURE_51' .and. Instructions(I_INFO)(23:23)==' ')THEN
      print *, '    Error :: input file does not support *INI_CRACK_PRESSURE_51 or higher!'
      call Warning_Message('S',Keywords_Blank)
  ENDIF

  IF (Instructions(I_INFO)(1:19)=='*PROPAGATION_LENGTH'.and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) PROPAGATION_LENGTH
  ENDIF
  IF (Instructions(I_INFO)(1:17)=='*KEY_SMOOTH_FRONT'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_SMOOTH_FRONT
  ENDIF
  !IF (Instructions(I_INFO)(1:26)=='*KEY_SMOOTH_PRESSURE_CURVE')THEN
  !    READ(Instructions(I_INFO+1) , * ) KEY_SMOOTH_PRESSURE_CURVE
  !ENDIF
  IF (Instructions(I_INFO)(1:23)=='*KEY_SMOOTH_FRONT_TWICE'.and. Instructions(I_INFO)(24:24)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_SMOOTH_FRONT_TWICE
  ENDIF
  IF (Instructions(I_INFO)(1:19)=='*KEY_POST_CS_N_STRA'.and. Instructions(I_INFO)(20:20)==' ')THEN   !输出节点位移
      READ(Instructions(I_INFO+1) , * ) KEY_POST_CS_N_STRA
  ENDIF
  IF (Instructions(I_INFO)(1:22)=='*KEY_LOCAL_MESH_REFINE'.and. Instructions(I_INFO)(23:23)==' ')THEN !局部网格加密
      READ(Instructions(I_INFO+1) , * ) KEY_LOCAL_MESH_REFINE
  ENDIF
  IF (Instructions(I_INFO)(1:13)=='*KEY_INI_RULE'.and. Instructions(I_INFO)(14:14)==' ')THEN          !初始裂缝生成规则
      READ(Instructions(I_INFO+1) , * ) KEY_INI_RULE
  ENDIF
  IF (Instructions(I_INFO)(1:19)=='*KEY_INI_CR_3D_TYPE'.and. Instructions(I_INFO)(20:20)==' ')THEN !初始3D裂缝的形状
      READ(Instructions(I_INFO+1) , * ) KEY_INI_CR_3D_TYPE
  ENDIF
  IF (Instructions(I_INFO)(1:19)=='*KEY_THERMAL_STRESS'.and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_THERMAL_STRESS
  ENDIF
  IF (Instructions(I_INFO)(1:21)=='*KEY_INSTRESS_FOR_MAT'.and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_INSTRESS_FOR_MAT
  ENDIF

  !NEWFTU2022100901. 2022-10-09.
  IF (Instructions(I_INFO)(1:16)=='*KEY_CFCP_3_TYPE'.and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_CFCP_3_TYPE
  ENDIF
  IF (Instructions(I_INFO)(1:24)=='*KEY_3D_CR_UPDATE_SCHEME'.and. Instructions(I_INFO)(25:25)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_3D_CR_UPDATE_SCHEME
  ENDIF
  IF (Instructions(I_INFO)(1:19)=='*KEY_INPLANE_GROWTH'.and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_INPLANE_GROWTH
  ENDIF
  IF (Instructions(I_INFO)(1:24)=='*KEY_SMOOTH_VERTEX_VALUE'.and. Instructions(I_INFO)(25:25)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_SMOOTH_VERTEX_VALUE
  ENDIF
  IF (Instructions(I_INFO)(1:16)=='*SMOOTH_VERTEX_N'.and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) SMOOTH_VERTEX_N
  ENDIF
  IF (Instructions(I_INFO)(1:25)=='*KEY_SMOOTH_VERTEX_VALUE2'.and. Instructions(I_INFO)(26:26)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_SMOOTH_VERTEX_VALUE2
  ENDIF
  IF (Instructions(I_INFO)(1:17)=='*SMOOTH_VERTEX_N2'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) SMOOTH_VERTEX_N2
  ENDIF
  IF (Instructions(I_INFO)(1:20)=='*KEY_SMOOTH_GF_VALUE'.and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_SMOOTH_GF_VALUE
  ENDIF
  IF (Instructions(I_INFO)(1:12)=='*SMOOTH_GF_N'.and. Instructions(I_INFO)(13:13)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) SMOOTH_GF_N
  ENDIF
  IF (Instructions(I_INFO)(1:21)=='*KEY_SMOOTH_GF_VALUE2'.and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_SMOOTH_GF_VALUE2
  ENDIF
  IF (Instructions(I_INFO)(1:13)=='*SMOOTH_GF_N2'.and. Instructions(I_INFO)(14:14)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) SMOOTH_GF_N2
  ENDIF
  IF (Instructions(I_INFO)(1:21)=='*KEY_EBE_PRECONDITION'.and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_EBE_PRECONDITION
  ENDIF
  IF (Instructions(I_INFO)(1:17)=='*KEY_SAVE_NOTHING'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_SAVE_NOTHING
  ENDIF
  !2022-10-09.
  IF (Instructions(I_INFO)(1:16)=='*KEY_SIMPLE_POST'.and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_SIMPLE_POST
  ENDIF
  IF (Instructions(I_INFO)(1:19)=='*KEY_POST_CS_N_STRS'.and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_POST_CS_N_STRS
  ENDIF
  IF (Instructions(I_INFO)(1:19)=='*KEY_POST_CS_G_COOR'.and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_POST_CS_G_COOR
  ENDIF
  IF (Instructions(I_INFO)(1:19)=='*KEY_POST_CS_G_DISP'.and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_POST_CS_G_DISP
  ENDIF
  IF (Instructions(I_INFO)(1:19)=='*KEY_POST_CS_G_STRS'.and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_POST_CS_G_STRS
  ENDIF
  IF (Instructions(I_INFO)(1:17)=='*KEY_POST_S_DOF_F'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_POST_S_DOF_F
  ENDIF
  IF (Instructions(I_INFO)(1:21)=='*KEY_POST_CRACKED_ELE'.and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_POST_CRACKED_ELE
  ENDIF
  IF (Instructions(I_INFO)(1:15)=='*KEY_NODE_VALUE'.and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_NODE_VALUE
  ENDIF
  IF (Instructions(I_INFO)(1:13)=='*KEY_SAVE_VTK'.and. Instructions(I_INFO)(14:14)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_SAVE_VTK
  ENDIF
  !2022-10-09.
  IF (Instructions(I_INFO)(1:21)=='*KEY_HF_MULTISTAGE_3D'.and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_HF_MULTISTAGE_3D
  ENDIF
  IF (Instructions(I_INFO)(1:13)=='*NUM_WELLBORE'.and. Instructions(I_INFO)(14:14)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_WELLBORE
  ENDIF
  !2022-10-09.
  IF (Instructions(I_INFO)(1:16)=='*NUM_POINTS_WB_1' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_POINTS_WB(1)
  ELSEIF(Instructions(I_INFO)(1:16)=='*NUM_POINTS_WB_2'.and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_POINTS_WB(2)
  ELSEIF(Instructions(I_INFO)(1:16)=='*NUM_POINTS_WB_3'.and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_POINTS_WB(3)
  ELSEIF(Instructions(I_INFO)(1:16)=='*NUM_POINTS_WB_4'.and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_POINTS_WB(4)
  ELSEIF(Instructions(I_INFO)(1:16)=='*NUM_POINTS_WB_5'.and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_POINTS_WB(5)
  ELSEIF(Instructions(I_INFO)(1:16)=='*NUM_POINTS_WB_6'.and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_POINTS_WB(6)
  ELSEIF(Instructions(I_INFO)(1:16)=='*NUM_POINTS_WB_7'.and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_POINTS_WB(7)
  ELSEIF(Instructions(I_INFO)(1:16)=='*NUM_POINTS_WB_8'.and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_POINTS_WB(8)
  ELSEIF(Instructions(I_INFO)(1:16)=='*NUM_POINTS_WB_9'.and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_POINTS_WB(9)
  ELSEIF(Instructions(I_INFO)(1:17)=='*NUM_POINTS_WB_10'.and. Instructions(I_INFO)(18:18)==' ')THEN !IMPROV2023080902.
      READ(Instructions(I_INFO+1) , * ) NUM_POINTS_WB(10)
  ELSEIF(Instructions(I_INFO)(1:17)=='*NUM_POINTS_WB_11'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_POINTS_WB(11)
  ELSEIF(Instructions(I_INFO)(1:17)=='*NUM_POINTS_WB_12'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_POINTS_WB(12)
  ELSEIF(Instructions(I_INFO)(1:17)=='*NUM_POINTS_WB_13'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_POINTS_WB(13)
  ELSEIF(Instructions(I_INFO)(1:17)=='*NUM_POINTS_WB_14'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_POINTS_WB(14)
  ELSEIF(Instructions(I_INFO)(1:17)=='*NUM_POINTS_WB_15'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_POINTS_WB(15)
  ELSEIF(Instructions(I_INFO)(1:17)=='*NUM_POINTS_WB_16'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_POINTS_WB(16)
  ELSEIF(Instructions(I_INFO)(1:17)=='*NUM_POINTS_WB_17'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_POINTS_WB(17)
  ELSEIF(Instructions(I_INFO)(1:17)=='*NUM_POINTS_WB_18'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_POINTS_WB(18)
  ELSEIF(Instructions(I_INFO)(1:17)=='*NUM_POINTS_WB_19'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_POINTS_WB(19)
  ELSEIF(Instructions(I_INFO)(1:17)=='*NUM_POINTS_WB_20'.and. Instructions(I_INFO)(18:18)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) NUM_POINTS_WB(20)
  ELSEIF(Instructions(I_INFO)(1:17)=='*NUM_POINTS_WB_21'.and. Instructions(I_INFO)(18:18)==' ')THEN
      print *, '    Error :: input file does not support *NUM_POINTS_WB_21 or higher!'
      call Warning_Message('S',Keywords_Blank)
  ENDIF
  !2022-10-09.
  IF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_1_1'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(1,1,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_1_2'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(1,2,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_1_3'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(1,3,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_1_4'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(1,4,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_1_5'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(1,5,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_1_6'.and. Instructions(I_INFO)(20:20)==' ')THEN
    print *, '    Error :: input file does not support *WELLBORE_COORS_1_6 or higher!'
    call Warning_Message('S',Keywords_Blank)
  ENDIF
  !2022-10-09.
  IF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_2_1'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(2,1,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_2_2'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(2,2,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_2_3'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(2,3,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_2_4'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(2,4,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_2_5'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(2,5,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_2_6'.and. Instructions(I_INFO)(20:20)==' ')THEN
    print *, '    Error :: input file does not support *WELLBORE_COORS_2_6 or higher!'
    call Warning_Message('S',Keywords_Blank)
  ENDIF
  !2022-10-09.
  IF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_3_1'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(3,1,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_3_2'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(3,2,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_3_3'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(3,3,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_3_4'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(3,4,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_3_5'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(3,5,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_3_6'.and. Instructions(I_INFO)(20:20)==' ')THEN
    print *, '    Error :: input file does not support *WELLBORE_COORS_3_6 or higher!'
    call Warning_Message('S',Keywords_Blank)
  ENDIF
  !2022-10-09.
  IF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_4_1'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(4,1,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_4_2'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(4,2,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_4_3'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(4,3,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_4_4'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(4,4,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_4_5'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(4,5,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_4_6'.and. Instructions(I_INFO)(20:20)==' ')THEN
    print *, '    Error :: input file does not support *WELLBORE_COORS_4_6 or higher!'
    call Warning_Message('S',Keywords_Blank)
  ENDIF
  !2022-10-09.
  IF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_5_1'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(5,1,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_5_2'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(5,2,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_5_3'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(5,3,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_5_4'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1), 256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(5,4,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_5_5'.and. Instructions(I_INFO)(20:20)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORE_COORS(5,5,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:19)=='*WELLBORE_COORS_5_6'.and. Instructions(I_INFO)(20:20)==' ')THEN
    print *, '    Error :: input file does not support *WELLBORE_COORS_5_6 or higher!'
    call Warning_Message('S',Keywords_Blank)
  ENDIF
  !2022-10-09.
  IF(Instructions(I_INFO)(1:24)=='*WELLBORES_START_POINT_1'.and. Instructions(I_INFO)(25:25)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORES_START_POINT(1,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:24)=='*WELLBORES_START_POINT_2'.and. Instructions(I_INFO)(25:25)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORES_START_POINT(2,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:24)=='*WELLBORES_START_POINT_3'.and. Instructions(I_INFO)(25:25)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORES_START_POINT(3,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:24)=='*WELLBORES_START_POINT_4'.and. Instructions(I_INFO)(25:25)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORES_START_POINT(4,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:24)=='*WELLBORES_START_POINT_5'.and. Instructions(I_INFO)(25:25)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORES_START_POINT(5,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:24)=='*WELLBORES_START_POINT_6'.and. Instructions(I_INFO)(25:25)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORES_START_POINT(6,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:24)=='*WELLBORES_START_POINT_7'.and. Instructions(I_INFO)(25:25)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORES_START_POINT(7,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:24)=='*WELLBORES_START_POINT_8'.and. Instructions(I_INFO)(25:25)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORES_START_POINT(8,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:24)=='*WELLBORES_START_POINT_9'.and. Instructions(I_INFO)(25:25)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORES_START_POINT(9,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:25)=='*WELLBORES_START_POINT_10'.and. Instructions(I_INFO)(25:25)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORES_START_POINT(10,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:25)=='*WELLBORES_START_POINT_11'.and. Instructions(I_INFO)(25:25)==' ')THEN
    print *, '    Error :: input file does not support *WELLBORES_START_POINT_11 or higher!'
    call Warning_Message('S',Keywords_Blank)
  ENDIF
  IF(Instructions(I_INFO)(1:24)=='*WELLBORES_END_POINT_1'.and. Instructions(I_INFO)(25:25)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORES_END_POINT(1,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:24)=='*WELLBORES_END_POINT_2'.and. Instructions(I_INFO)(25:25)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORES_END_POINT(2,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:24)=='*WELLBORES_END_POINT_3'.and. Instructions(I_INFO)(25:25)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORES_END_POINT(3,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:24)=='*WELLBORES_END_POINT_4'.and. Instructions(I_INFO)(25:25)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORES_END_POINT(4,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:24)=='*WELLBORES_END_POINT_5'.and. Instructions(I_INFO)(25:25)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORES_END_POINT(5,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:24)=='*WELLBORES_END_POINT_6'.and. Instructions(I_INFO)(25:25)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORES_END_POINT(6,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:24)=='*WELLBORES_END_POINT_7'.and. Instructions(I_INFO)(25:25)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORES_END_POINT(7,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:24)=='*WELLBORES_END_POINT_8'.and. Instructions(I_INFO)(25:25)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORES_END_POINT(8,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:24)=='*WELLBORES_END_POINT_9'.and. Instructions(I_INFO)(25:25)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORES_END_POINT(9,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:25)=='*WELLBORES_END_POINT_10'.and. Instructions(I_INFO)(26:26)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) WELLBORES_END_POINT(10,1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:25)=='*WELLBORES_END_POINT_11'.and. Instructions(I_INFO)(26:26)==' ')THEN
    print *, '    Error :: input file does not support *WELLBORES_END_POINT_11 or higher!'
    call Warning_Message('S',Keywords_Blank)
  ENDIF
  !2022-10-09.
  IF(Instructions(I_INFO)(1:23)=='*NUM_STAGES_WELLBORES_1' .and. Instructions(I_INFO)(24:24)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_STAGES_WELLBORES(1)
  ELSEIF(Instructions(I_INFO)(1:23)=='*NUM_STAGES_WELLBORES_2'.and. Instructions(I_INFO)(24:24)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_STAGES_WELLBORES(2)
  ELSEIF(Instructions(I_INFO)(1:23)=='*NUM_STAGES_WELLBORES_3'.and. Instructions(I_INFO)(24:24)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_STAGES_WELLBORES(3)
  ELSEIF(Instructions(I_INFO)(1:23)=='*NUM_STAGES_WELLBORES_4'.and. Instructions(I_INFO)(24:24)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_STAGES_WELLBORES(4)
  ELSEIF(Instructions(I_INFO)(1:23)=='*NUM_STAGES_WELLBORES_5'.and. Instructions(I_INFO)(24:24)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_STAGES_WELLBORES(5)
  ELSEIF(Instructions(I_INFO)(1:23)=='*NUM_STAGES_WELLBORES_6'.and. Instructions(I_INFO)(24:24)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_STAGES_WELLBORES(6)
  ELSEIF(Instructions(I_INFO)(1:23)=='*NUM_STAGES_WELLBORES_7'.and. Instructions(I_INFO)(24:24)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_STAGES_WELLBORES(7)
  ELSEIF(Instructions(I_INFO)(1:23)=='*NUM_STAGES_WELLBORES_8'.and. Instructions(I_INFO)(24:24)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_STAGES_WELLBORES(8)
  ELSEIF(Instructions(I_INFO)(1:23)=='*NUM_STAGES_WELLBORES_9'.and. Instructions(I_INFO)(24:24)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_STAGES_WELLBORES(9)
  ELSEIF(Instructions(I_INFO)(1:24)=='*NUM_STAGES_WELLBORES_10'.and. Instructions(I_INFO)(25:25)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_STAGES_WELLBORES(10)
  ELSEIF(Instructions(I_INFO)(1:24)=='*NUM_STAGES_WELLBORES_11'.and. Instructions(I_INFO)(25:25)==' ')THEN
    print *, '    Error :: input file does not support *NUM_STAGES_WELLBORES_11 or higher!'
    call Warning_Message('S',Keywords_Blank)
  ENDIF
  !2022-10-09.
  IF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_1_1'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(1,1)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_1_2'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(1,2)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_1_3'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(1,3)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_1_4'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(1,4)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_1_5'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(1,5)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_2_1'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(2,1)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_2_2'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(2,2)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_2_3'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(2,3)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_2_4'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(2,4)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_2_5'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(2,5)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_3_1'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(3,1)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_3_2'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(3,2)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_3_3'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(3,3)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_3_4'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(3,4)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_3_5'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(3,5)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_4_1'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(4,1)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_4_2'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(4,2)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_4_3'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(4,3)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_4_4'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(4,4)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_4_5'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(4,5)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_5_1'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(5,1)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_5_2'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(5,2)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_5_3'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(5,3)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_5_4'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(5,4)
  ELSEIF(Instructions(I_INFO)(1:32)=='*NUM_CRS_STAGES_WELLBORES_5_5'.and. Instructions(I_INFO)(33:33)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_CRS_STAGES_WELLBORES(5,5)
  ENDIF
  IF(Instructions(I_INFO)(1:28)=='*KEY_GEN_INI_CRACK_WELLBORES'.and. Instructions(I_INFO)(29:29)==' ')THEN
    READ(Instructions(I_INFO+1),*) KEY_GEN_INI_CRACK_WELLBORES
  ENDIF
  IF(Instructions(I_INFO)(1:23)=='*NUM_POLY_EDGES_NACR_WB'.and. Instructions(I_INFO)(24:24)==' ')THEN
    READ(Instructions(I_INFO+1),*) NUM_POLY_EDGES_NACR_WB
  ENDIF
  IF(Instructions(I_INFO)(1:25)=='*SIZE_INI_CRACK_WELLBORES'.and. Instructions(I_INFO)(26:26)==' ')THEN
    READ(Instructions(I_INFO+1),*) SIZE_INI_CRACK_WELLBORES
  ENDIF
  !2022-10-09.
  IF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_1_1'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(1,1)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_1_2'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(1,2)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_1_3'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(1,3)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_1_4'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(1,4)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_1_5'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(1,5)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_1_6'.and. Instructions(I_INFO)(34:34)==' ')THEN
    print *, '    Error :: input file does not support *INJECTION_Q_STAGES_WELLBORES_1_6 or higher!'
    call Warning_Message('S',Keywords_Blank)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_2_1'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(2,1)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_2_2'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(2,2)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_2_3'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(2,3)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_2_4'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(2,4)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_2_5'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(2,5)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_2_6'.and. Instructions(I_INFO)(34:34)==' ')THEN
    print *, '    Error :: input file does not support *INJECTION_Q_STAGES_WELLBORES_2_6 or higher!'
    call Warning_Message('S',Keywords_Blank)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_3_1'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(3,1)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_3_2'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(3,2)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_3_3'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(3,3)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_3_4'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(3,4)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_3_5'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(3,5)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_3_6'.and. Instructions(I_INFO)(34:34)==' ')THEN
    print *, '    Error :: input file does not support *INJECTION_Q_STAGES_WELLBORES_3_6 or higher!'
    call Warning_Message('S',Keywords_Blank)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_4_1'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(4,1)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_4_2'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(4,2)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_4_3'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(4,3)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_4_4'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(4,4)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_4_5'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(4,5)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_4_6'.and. Instructions(I_INFO)(34:34)==' ')THEN
    print *, '    Error :: input file does not support *INJECTION_Q_STAGES_WELLBORES_4_6 or higher!'
    call Warning_Message('S',Keywords_Blank)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_5_1'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(5,1)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_5_2'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(5,2)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_5_3'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(5,3)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_5_4'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(5,4)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_5_5'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_Q_STAGES_WELLBORES(5,5)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_Q_STAGES_WELLBORES_5_6'.and. Instructions(I_INFO)(34:34)==' ')THEN
    print *, '    Error :: input file does not support *INJECTION_Q_STAGES_WELLBORES_5_6 or higher!'
    call Warning_Message('S',Keywords_Blank)
  ENDIF
  !2022-10-09.
  IF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_1_1'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(1,1)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_1_2'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(1,2)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_1_3'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(1,3)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_1_4'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(1,4)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_1_5'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(1,5)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_1_6'.and. Instructions(I_INFO)(34:34)==' ')THEN
    print *, '    Error :: input file does not support *INJECTION_T_STAGES_WELLBORES_1_6 or higher!'
    call Warning_Message('S',Keywords_Blank)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_2_1'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(2,1)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_2_2'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(2,2)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_2_3'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(2,3)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_2_4'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(2,4)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_2_5'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(2,5)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_2_6'.and. Instructions(I_INFO)(34:34)==' ')THEN
    print *, '    Error :: input file does not support *INJECTION_T_STAGES_WELLBORES_2_6 or higher!'
    call Warning_Message('S',Keywords_Blank)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_3_1'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(3,1)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_3_2'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(3,2)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_3_3'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(3,3)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_3_4'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(3,4)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_3_5'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(3,5)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_3_6'.and. Instructions(I_INFO)(34:34)==' ')THEN
    print *, '    Error :: input file does not support *INJECTION_T_STAGES_WELLBORES_3_6 or higher!'
    call Warning_Message('S',Keywords_Blank)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_4_1'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(4,1)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_4_2'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(4,2)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_4_3'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(4,3)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_4_4'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(4,4)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_4_5'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(4,5)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_4_6'.and. Instructions(I_INFO)(34:34)==' ')THEN
    print *, '    Error :: input file does not support *INJECTION_T_STAGES_WELLBORES_4_6 or higher!'
    call Warning_Message('S',Keywords_Blank)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_5_1'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(5,1)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_5_2'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(5,2)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_5_3'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(5,3)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_5_4'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(5,4)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_5_5'.and. Instructions(I_INFO)(34:34)==' ')THEN
    READ(Instructions(I_INFO+1),*) INJECTION_T_STAGES_WELLBORES(5,5)
  ELSEIF(Instructions(I_INFO)(1:33)=='*INJECTION_T_STAGES_WELLBORES_5_6'.and. Instructions(I_INFO)(34:34)==' ')THEN
    print *, '    Error :: input file does not support *INJECTION_T_STAGES_WELLBORES_5_6 or higher!'
    call Warning_Message('S',Keywords_Blank)
  ENDIF
  !2022-10-10.
  IF(Instructions(I_INFO)(1:13)=='*INSITU_S1_3D'.and. Instructions(I_INFO)(14:14)==' ')THEN
    READ(Instructions(I_INFO+1),*) INSITU_S1_3D
  ELSEIF(Instructions(I_INFO)(1:13)=='*INSITU_S2_3D'.and. Instructions(I_INFO)(14:14)==' ')THEN
    READ(Instructions(I_INFO+1),*) INSITU_S2_3D
  ELSEIF(Instructions(I_INFO)(1:13)=='*INSITU_S3_3D'.and. Instructions(I_INFO)(14:14)==' ')THEN
    READ(Instructions(I_INFO+1),*) INSITU_S3_3D
  ENDIF
  !2022-10-10.
  IF(Instructions(I_INFO)(1:16)=='*INSITU_S1_NV_3D'.and. Instructions(I_INFO)(17:17)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) INSITU_S1_NV_3D(1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:16)=='*INSITU_S2_NV_3D'.and. Instructions(I_INFO)(17:17)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) INSITU_S2_NV_3D(1:NUM_DATA)
  ELSEIF(Instructions(I_INFO)(1:16)=='*INSITU_S3_NV_3D'.and. Instructions(I_INFO)(17:17)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) INSITU_S3_NV_3D(1:NUM_DATA)
  ENDIF
  !2022-10-10.
  IF(Instructions(I_INFO)(1:17)=='*KEY_NACR_TYPE_3D'.and. Instructions(I_INFO)(18:18)==' ')THEN
    READ(Instructions(I_INFO+1),*) KEY_NACR_TYPE_3D
  ENDIF
  IF(Instructions(I_INFO)(1:15)=='*KEY_NACR_CROSS')THEN
    READ(Instructions(I_INFO+1),*) KEY_NACR_CROSS
  ENDIF
  IF(Instructions(I_INFO)(1:16)=='*KEY_NACR_GROWTH'.and. Instructions(I_INFO)(17:17)==' ')THEN
    READ(Instructions(I_INFO+1),*) KEY_NACR_GROWTH
  ENDIF
  !IF(Instructions(I_INFO)(1:17)=='*NACR_3D_N_VECTOR')THEN
  IF(Instructions(I_INFO)(1:17)=='*NACR_3D_N_VECTOR' .and. Instructions(I_INFO)(18:18)==' ')THEN  !2024-02-26. BUGFIX2024022602.
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) NACR_3D_N_VECTOR(1:NUM_DATA)
    !print *,"NaCr_3D_n_Vector(1:3):",NaCr_3D_n_Vector(1:3)
  ENDIF
  IF(Instructions(I_INFO)(1:23)=='*NACR_3D_N_VECTOR_DELTA'.and. Instructions(I_INFO)(24:24)==' ')THEN
    READ(Instructions(I_INFO+1),*) NACR_3D_N_VECTOR_DELTA
  ENDIF
  IF(Instructions(I_INFO)(1:13)=='*NACR_3D_SIZE'.and. Instructions(I_INFO)(14:14)==' ')THEN
    READ(Instructions(I_INFO+1),*) NACR_3D_SIZE
  ENDIF
  IF(Instructions(I_INFO)(1:19)=='*NACR_3D_SZ_DELTA'.and. Instructions(I_INFO)(20:20)==' ')THEN
    READ(Instructions(I_INFO+1),*) NACR_3D_SZ_DELTA
  ENDIF
  IF(Instructions(I_INFO)(1:17)=='*NACR_3D_CHECK_R'.and. Instructions(I_INFO)(18:18)==' ')THEN
    READ(Instructions(I_INFO+1),*) NACR_3D_CHECK_R
  ENDIF
  !2023-03-01.
  IF(Instructions(I_INFO)(1:29)=='*NACR_3D_RECT_LONGSIDE_VECTOR'.and. Instructions(I_INFO)(30:30)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) NaCr_3D_Rect_Longside_Vector(1:NUM_DATA)
  ENDIF
  IF(Instructions(I_INFO)(1:31)=='*NACR_3D_RECT_LONG_VECTOR_DELTA'.and. Instructions(I_INFO)(32:32)==' ')THEN    !注意不一致.
    READ(Instructions(I_INFO+1),*) NaCr_3D_Rect_Longside_Vector_Delta
  ENDIF
  IF(Instructions(I_INFO)(1:15)=='*NACR_3D_RECT_L'.and. Instructions(I_INFO)(16:16)==' ')THEN
    READ(Instructions(I_INFO+1),*) NaCr_3D_Rect_L
  ENDIF
  IF(Instructions(I_INFO)(1:15)=='*NACR_3D_RECT_W'.and. Instructions(I_INFO)(16:16)==' ')THEN
    READ(Instructions(I_INFO+1),*) NaCr_3D_Rect_W
  ENDIF
  IF(Instructions(I_INFO)(1:20)=='*NACR_3D_REC_L_DELTA'.and. Instructions(I_INFO)(21:21)==' ')THEN   !注意不一致.
    READ(Instructions(I_INFO+1),*) NaCr_3D_Rect_L_Delta
  ENDIF
  IF(Instructions(I_INFO)(1:20)=='*NACR_3D_REC_W_DELTA'.and. Instructions(I_INFO)(21:21)==' ')THEN   !注意不一致.
    READ(Instructions(I_INFO+1),*) NaCr_3D_Rect_W_Delta
  ENDIF
  !.................................
  !破裂区域相关. 2022-10-10修正.
  !.................................
  IF(Instructions(I_INFO)(1:18)=='*KEY_FRACTURE_ZONE'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_FRACTURE_ZONE
  ENDIF
  IF(Instructions(I_INFO)(1:15)=='*FRAC_ZONE_MINX'.and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) FRAC_ZONE_MINX
      !PRINT *,'FRAC_ZONE_MINX:',FRAC_ZONE_MINX
  ENDIF
  IF(Instructions(I_INFO)(1:15)=='*FRAC_ZONE_MAXY'.and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) FRAC_ZONE_MAXX
  ENDIF

  IF(Instructions(I_INFO)(1:15)=='*FRAC_ZONE_MINY'.and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) FRAC_ZONE_MINY
  ENDIF
  IF(Instructions(I_INFO)(1:15)=='*FRAC_ZONE_MAXY'.and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) FRAC_ZONE_MAXY
  ENDIF
  IF(Instructions(I_INFO)(1:15)=='*FRAC_ZONE_MINZ'.and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) FRAC_ZONE_MINZ
  ENDIF
  IF(Instructions(I_INFO)(1:15)=='*FRAC_ZONE_MAXZ'.and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) FRAC_ZONE_MAXZ
  ENDIF
  !2022-10-10.
  IF(Instructions(I_INFO)(1:7)=='*KEY_XA'.and. Instructions(I_INFO)(8:8)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_XA
  ENDIF
  !PRINT *,Instructions(I_INFO)(1:30)
  IF(Instructions(I_INFO)(1:31)=='*KEY_CHECK_AND_ADJUST_CRACKS_3D'.and. Instructions(I_INFO)(32:32)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_Check_and_Adjust_Cracks_3D
  ENDIF
  IF(Instructions(I_INFO)(1:25)=='*KEY_TIP_FLUID_ELEMENT_3D'.and. Instructions(I_INFO)(26:26)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_TIP_FLUID_ELEMENT_3D
  ENDIF
  IF(Instructions(I_INFO)(1:23)=='*KEY_STOP_OUTSIDE_CRACK'.and. Instructions(I_INFO)(24:24)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_Stop_Outside_Crack
  ENDIF
  !2022-11-09.
  IF(Instructions(I_INFO)(1:25)=='*KEY_DENOISE_VERTEX_VALUE'.and. Instructions(I_INFO)(26:26)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_Denoise_Vertex_Value
  ENDIF

  IF(Instructions(I_INFO)(1:22)=='*KEY_CS_NATURAL_CRACK'.and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_CS_Natural_Crack
  ENDIF

  !2022-11-10.
  IF(Instructions(I_INFO)(1:22)=='*KEY_EBE_SYM_STORAGE_K'.and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_EBE_Sym_Storage_K
  ENDIF
  !2022-11-14.
  IF(Instructions(I_INFO)(1:18)=='*MAX_CONTACT_ITER'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Max_Contact_Iter
  ENDIF
  !2022-11-26.
  IF(Instructions(I_INFO)(1:21)=='*KEY_GET_PERMEABILITY'.and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_Get_Permeability
  ENDIF
  !2022-12-21.
  IF(Instructions(I_INFO)(1:34)=='*KEY_READ_INITIAL_NODE_STRESS_FILE'.and. Instructions(I_INFO)(35:35)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_Read_Initial_Node_Stress_File
  ENDIF
  !2023-01-07.
  IF(Instructions(I_INFO)(1:26)=='*KEY_NACR_ACTIVE_SCHEME_3D'.and. Instructions(I_INFO)(27:27)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_NaCr_Active_Scheme_3D
  ENDIF
  !2023-02-12.
  IF(Instructions(I_INFO)(1:12)=='*DESIRED_KIC'.and. Instructions(I_INFO)(13:13)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Desired_KIc
  ENDIF
  !2023-02-12.
  IF(Instructions(I_INFO)(1:26)=='*KEY_SCHEME_THERMAL_STRESS'.and. Instructions(I_INFO)(27:27)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_Scheme_Thermal_Stress
  ENDIF
  IF(Instructions(I_INFO)(1:24)=='*KEY_INITIAL_TEMPERATURE'.and. Instructions(I_INFO)(25:25)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_Initial_Temperature
  ENDIF
  !2023-05-07.
  IF(Instructions(I_INFO)(1:27)=='*KEY_ALLOW_3D_OUTSIDE_CRACK'.and. Instructions(I_INFO)(28:28)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_Allow_3D_Outside_Crack
  ENDIF
  !2023-05-16.
  IF(Instructions(I_INFO)(1:9)=='*KIC_NACR' .and. Instructions(I_INFO)(10:10)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) c_Temp_Double
      KIc_NaCr = c_Temp_Double
  ENDIF
  !2023-05-17.
  IF(Instructions(I_INFO)(1:31)=='*KEY_NONUNIFORM_INSITU_X_WITH_Z'.and. Instructions(I_INFO)(32:32)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_NONUNIFORM_INSITU_X_WITH_Z
  ENDIF
  IF(Instructions(I_INFO)(1:31)=='*INSITU_SX_3D_SEG_STRS_X_WITH_Z'.and. Instructions(I_INFO)(32:32)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) InSitu_Sx_3D_Seg_Strs_X_with_Z(1:NUM_DATA)
  ENDIF
  IF(Instructions(I_INFO)(1:31)=='*INSITU_SX_3D_SEG_LOCA_X_WITH_Z'.and. Instructions(I_INFO)(32:32)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) InSitu_Sx_3D_Seg_Loca_X_with_Z(1:NUM_DATA)
  ENDIF
  IF(Instructions(I_INFO)(1:31)=='*KEY_NONUNIFORM_INSITU_X_WITH_Y'.and. Instructions(I_INFO)(32:32)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_NONUNIFORM_INSITU_X_WITH_Y
  ENDIF
  IF(Instructions(I_INFO)(1:31)=='*INSITU_SX_3D_SEG_STRS_X_WITH_Y'.and. Instructions(I_INFO)(32:32)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) InSitu_Sx_3D_Seg_Strs_X_with_Y(1:NUM_DATA)
  ENDIF
  IF(Instructions(I_INFO)(1:31)=='*INSITU_SX_3D_SEG_LOCA_X_WITH_Y'.and. Instructions(I_INFO)(32:32)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) InSitu_Sx_3D_Seg_Loca_X_with_Y(1:NUM_DATA)
  ENDIF
  IF(Instructions(I_INFO)(1:31)=='*KEY_NONUNIFORM_INSITU_Y_WITH_Z'.and. Instructions(I_INFO)(32:32)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_NONUNIFORM_INSITU_Y_WITH_Z
  ENDIF
  IF(Instructions(I_INFO)(1:31)=='*INSITU_SY_3D_SEG_STRS_Y_WITH_Z'.and. Instructions(I_INFO)(32:32)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) InSitu_Sy_3D_Seg_Strs_Y_with_Z(1:NUM_DATA)
  ENDIF
  IF(Instructions(I_INFO)(1:31)=='*INSITU_SY_3D_SEG_LOCA_Y_WITH_Z'.and. Instructions(I_INFO)(32:32)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) InSitu_SY_3D_Seg_Loca_Y_with_Z(1:NUM_DATA)
  ENDIF
  IF(Instructions(I_INFO)(1:31)=='*KEY_NONUNIFORM_INSITU_Y_WITH_X'.and. Instructions(I_INFO)(32:32)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_NONUNIFORM_INSITU_Y_WITH_X
  ENDIF
  IF(Instructions(I_INFO)(1:31)=='*INSITU_SY_3D_SEG_STRS_Y_WITH_X'.and. Instructions(I_INFO)(32:32)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) InSitu_Sy_3D_Seg_Strs_Y_with_X(1:NUM_DATA)
  ENDIF
  IF(Instructions(I_INFO)(1:31)=='*INSITU_SY_3D_SEG_LOCA_Y_WITH_X'.and. Instructions(I_INFO)(32:32)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) InSitu_SY_3D_Seg_Loca_Y_with_X(1:NUM_DATA)
  ENDIF
  IF(Instructions(I_INFO)(1:31)=='*KEY_NONUNIFORM_INSITU_Z_WITH_X'.and. Instructions(I_INFO)(32:32)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_NONUNIFORM_INSITU_Z_WITH_X
  ENDIF
  IF(Instructions(I_INFO)(1:31)=='*INSITU_SZ_3D_SEG_STRS_Z_WITH_X'.and. Instructions(I_INFO)(32:32)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) InSitu_Sz_3D_Seg_Strs_Z_with_X(1:NUM_DATA)
  ENDIF
  IF(Instructions(I_INFO)(1:31)=='*INSITU_SZ_3D_SEG_LOCA_Z_WITH_X'.and. Instructions(I_INFO)(32:32)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) InSitu_Sz_3D_Seg_Loca_Z_with_X(1:NUM_DATA)
  ENDIF
  IF(Instructions(I_INFO)(1:31)=='*KEY_NONUNIFORM_INSITU_Z_WITH_Y'.and. Instructions(I_INFO)(32:32)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_NONUNIFORM_INSITU_Z_WITH_Y
  ENDIF
  IF(Instructions(I_INFO)(1:31)=='*INSITU_SZ_3D_SEG_STRS_Z_WITH_Y'.and. Instructions(I_INFO)(32:32)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) InSitu_Sz_3D_Seg_Strs_Z_with_Y(1:NUM_DATA)
  ENDIF
  IF(Instructions(I_INFO)(1:31)=='*INSITU_SZ_3D_SEG_LOCA_Z_WITH_Y'.and. Instructions(I_INFO)(32:32)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) InSitu_Sz_3D_Seg_Loca_Z_with_Y(1:NUM_DATA)
  ENDIF
  !2023-05-18.
  IF(Instructions(I_INFO)(1:27)=='*KEY_3D_HF_TIME_STEP_METHOD'.and. Instructions(I_INFO)(28:28)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_3D_HF_Time_Step_Method
  ENDIF
  !2023-08-08. NEWFTU2023080801.
  IF(Instructions(I_INFO)(1:28)=='*KEY_3D_HF_SLIPWATER_FK_TYPE'.and. Instructions(I_INFO)(29:29)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_3D_HF_SlipWater_fk_Type
  ENDIF
  !2023-08-10. IMPROV2023081001.
  IF(Instructions(I_INFO)(1:32)=='*SIFS_DIM_3D_OFFSET_DELTA_FACTOR'.and. Instructions(I_INFO)(33:33)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) SIFs_DIM_3D_Offset_Delta_Factor
  ENDIF
  IF(Instructions(I_INFO)(1:20)=='*KEY_SIFS_DIM_POINTS'.and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_SIFs_DIM_Points
  ENDIF
  IF(Instructions(I_INFO)(1:20)=='*KEY_SIFS_DIM_METHOD'.and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_SIFs_DIM_Method
  ENDIF
  IF(Instructions(I_INFO)(1:23)=='*SIFS_DIM_3D_R_1_FACTOR'.and. Instructions(I_INFO)(24:24)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) SIFs_DIM_3D_r_1_Factor
  ENDIF
  IF(Instructions(I_INFO)(1:23)=='*SIFS_DIM_3D_R_2_FACTOR'.and. Instructions(I_INFO)(24:24)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) SIFs_DIM_3D_r_2_Factor
  ENDIF
  !2023-08-12.
  IF(Instructions(I_INFO)(1:26)=='*KEY_CRACK_APERTURE_METHOD'.and. Instructions(I_INFO)(27:27)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_Crack_Aperture_Method
  ENDIF
  !2023-08-21. Surface Load.
  IF(Instructions(I_INFO)(1:18)=='*NUM_SURFACE_LOADS'.and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Num_Surface_Loads
  ENDIF
  IF(Instructions(I_INFO)(1:20)=='*FILE_SURFACE_LOAD_1' .and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) File_Surface_Load(1)
  elseif(Instructions(I_INFO)(1:20)=='*FILE_SURFACE_LOAD_2' .and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) File_Surface_Load(2)
  elseif(Instructions(I_INFO)(1:20)=='*FILE_SURFACE_LOAD_3' .and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) File_Surface_Load(3)
  elseif(Instructions(I_INFO)(1:20)=='*FILE_SURFACE_LOAD_4' .and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) File_Surface_Load(4)
  elseif(Instructions(I_INFO)(1:20)=='*FILE_SURFACE_LOAD_5' .and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) File_Surface_Load(5)
  elseif(Instructions(I_INFO)(1:20)=='*FILE_SURFACE_LOAD_6' .and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) File_Surface_Load(6)
  elseif(Instructions(I_INFO)(1:20)=='*FILE_SURFACE_LOAD_7' .and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) File_Surface_Load(7)
  elseif(Instructions(I_INFO)(1:20)=='*FILE_SURFACE_LOAD_8' .and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) File_Surface_Load(8)
  elseif(Instructions(I_INFO)(1:20)=='*FILE_SURFACE_LOAD_9' .and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) File_Surface_Load(9)
  elseif(Instructions(I_INFO)(1:21)=='*FILE_SURFACE_LOAD_10'.and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) File_Surface_Load(10)
  elseif(Instructions(I_INFO)(1:21)=='*FILE_SURFACE_LOAD_11'.and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) File_Surface_Load(11)
  elseif(Instructions(I_INFO)(1:21)=='*FILE_SURFACE_LOAD_12'.and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) File_Surface_Load(12)
  elseif(Instructions(I_INFO)(1:21)=='*FILE_SURFACE_LOAD_13'.and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) File_Surface_Load(13)
  elseif(Instructions(I_INFO)(1:21)=='*FILE_SURFACE_LOAD_14'.and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) File_Surface_Load(14)
  elseif(Instructions(I_INFO)(1:21)=='*FILE_SURFACE_LOAD_15'.and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) File_Surface_Load(15)
  elseif(Instructions(I_INFO)(1:21)=='*FILE_SURFACE_LOAD_16'.and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) File_Surface_Load(16)
  elseif(Instructions(I_INFO)(1:21)=='*FILE_SURFACE_LOAD_17'.and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) File_Surface_Load(17)
  elseif(Instructions(I_INFO)(1:21)=='*FILE_SURFACE_LOAD_18'.and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) File_Surface_Load(18)
  elseif(Instructions(I_INFO)(1:21)=='*FILE_SURFACE_LOAD_19'.and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) File_Surface_Load(19)
  elseif(Instructions(I_INFO)(1:21)=='*FILE_SURFACE_LOAD_20'.and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) File_Surface_Load(20)
  elseif(Instructions(I_INFO)(1:21)=='*FILE_SURFACE_LOAD_21'.and. Instructions(I_INFO)(22:22)==' ')THEN
      print *, '    Error :: input file does not support *FILE_SURFACE_LOAD_21 or higher!'
      call Warning_Message('S',Keywords_Blank)
  ENDIF
  IF(Instructions(I_INFO)(1:19)=='*SURFACE_PRESSURE_1' .and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Surface_Pressure(1)
  ELSEIF(Instructions(I_INFO)(1:19)=='*SURFACE_PRESSURE_2' .and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Surface_Pressure(2)
  ELSEIF(Instructions(I_INFO)(1:19)=='*SURFACE_PRESSURE_3' .and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Surface_Pressure(3)
  ELSEIF(Instructions(I_INFO)(1:19)=='*SURFACE_PRESSURE_4' .and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Surface_Pressure(4)
  ELSEIF(Instructions(I_INFO)(1:19)=='*SURFACE_PRESSURE_5' .and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Surface_Pressure(5)
  ELSEIF(Instructions(I_INFO)(1:19)=='*SURFACE_PRESSURE_6' .and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Surface_Pressure(6)
  ELSEIF(Instructions(I_INFO)(1:19)=='*SURFACE_PRESSURE_7' .and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Surface_Pressure(7)
  ELSEIF(Instructions(I_INFO)(1:19)=='*SURFACE_PRESSURE_8' .and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Surface_Pressure(8)
  ELSEIF(Instructions(I_INFO)(1:19)=='*SURFACE_PRESSURE_9' .and. Instructions(I_INFO)(20:20)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Surface_Pressure(9)
  ELSEIF(Instructions(I_INFO)(1:20)=='*SURFACE_PRESSURE_10'.and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Surface_Pressure(10)
  ELSEIF(Instructions(I_INFO)(1:20)=='*SURFACE_PRESSURE_11'.and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Surface_Pressure(11)
  ELSEIF(Instructions(I_INFO)(1:20)=='*SURFACE_PRESSURE_12'.and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Surface_Pressure(12)
  ELSEIF(Instructions(I_INFO)(1:20)=='*SURFACE_PRESSURE_13'.and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Surface_Pressure(13)
  ELSEIF(Instructions(I_INFO)(1:20)=='*SURFACE_PRESSURE_14'.and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Surface_Pressure(14)
  ELSEIF(Instructions(I_INFO)(1:20)=='*SURFACE_PRESSURE_15'.and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Surface_Pressure(15)
  ELSEIF(Instructions(I_INFO)(1:20)=='*SURFACE_PRESSURE_16'.and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Surface_Pressure(16)
  ELSEIF(Instructions(I_INFO)(1:20)=='*SURFACE_PRESSURE_17'.and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Surface_Pressure(17)
  ELSEIF(Instructions(I_INFO)(1:20)=='*SURFACE_PRESSURE_18'.and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Surface_Pressure(18)
  ELSEIF(Instructions(I_INFO)(1:20)=='*SURFACE_PRESSURE_19'.and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Surface_Pressure(19)
  ELSEIF(Instructions(I_INFO)(1:20)=='*SURFACE_PRESSURE_20'.and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Surface_Pressure(20)
  ELSEIF(Instructions(I_INFO)(1:21)=='*SURFACE_PRESSURE_21'.and. Instructions(I_INFO)(21:21)==' ')THEN
      print *, '    Error :: input file does not support *SURFACE_PRESSURE_21 or higher!'
      call Warning_Message('S',Keywords_Blank)
  ENDIF
  !2023-08-22.
  IF(Instructions(I_INFO)(1:25)=='*KEY_PRINT_SIFS_TO_SCREEN'.and. Instructions(I_INFO)(26:26)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_Print_SIFs_to_Screen
  ENDIF
  IF(Instructions(I_INFO)(1:23)=='*SIFS_DIM_3D_R_K_FACTOR'.and. Instructions(I_INFO)(24:24)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) SIFs_DIM_3D_r_k_Factor
  ENDIF
!  !2023-08-24.
!  IF(Instructions(I_INFO)(1:24)=='*SET_NATURAL_CRACKS_TYPE')THEN
!      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
!      IF(NUM_DATA < NUM_CRACK)THEN
!          PRINT *,'    ERROR:: WRONG *SET_NATURAL_CRACKS_TYPE, ALL CRACKS SHOULD BE GIVEN!'
!          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
!      ENDIF
!      READ(Instructions(I_INFO+1),*) Real_Tem_Date(1:NUM_DATA)
!      do i_DATA = 1,NUM_DATA
!          if(Real_Tem_Date(i_DATA)==1) then
!              Crack_Type_Status_3D(i_DATA,1) = 2
!          endif
!      enddo
!      !print *,'Crack_Type_Status_3D(1:3,1):',Crack_Type_Status_3D(1:3,1)
!  ENDIF
!  IF(Instructions(I_INFO)(1:26)=='*SET_CS_FOR_NATURAL_CRACKS')THEN
!      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
!      IF(NUM_DATA < NUM_CRACK)THEN
!          PRINT *,'    ERROR:: WRONG *SET_CS_FOR_NATURAL_CRACKS, ALL CRACKS SHOULD BE GIVEN!'
!          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
!      ENDIF
!      READ(Instructions(I_INFO+1),*) Real_Tem_Date(1:NUM_DATA)
!      do i_DATA = 1,NUM_DATA
!          if(Real_Tem_Date(i_DATA)==1) then
!              Key_CS_Crack(i_DATA) = 1
!          endif
!      enddo
!      !print *,'Key_CS_Crack(1:3):',Key_CS_Crack(1:3)
!  ENDIF
  IF (Instructions(I_INFO)(1:18)=='*Na_CRACK3D_COOR_1' .and. Instructions(I_INFO)(19:19)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        Read_kpp_Na_CRACK3D_COOR(1,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      Read_kpp_Each_NaCr3D_Poi_Num(1)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:18)=='*Na_CRACK3D_COOR_2'.and. Instructions(I_INFO)(19:19)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        Na_CRACK3D_COOR(2,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      Read_kpp_Each_NaCr3D_Poi_Num(2)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:18)=='*Na_CRACK3D_COOR_3'.and. Instructions(I_INFO)(19:19)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        Na_CRACK3D_COOR(3,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      Read_kpp_Each_NaCr3D_Poi_Num(3)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:18)=='*Na_CRACK3D_COOR_4'.and. Instructions(I_INFO)(19:19)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        Na_CRACK3D_COOR(4,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      Read_kpp_Each_NaCr3D_Poi_Num(4)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:18)=='*Na_CRACK3D_COOR_5'.and. Instructions(I_INFO)(19:19)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        Na_CRACK3D_COOR(5,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      Read_kpp_Each_NaCr3D_Poi_Num(5)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:18)=='*Na_CRACK3D_COOR_6'.and. Instructions(I_INFO)(19:19)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        Na_CRACK3D_COOR(6,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      Read_kpp_Each_NaCr3D_Poi_Num(6)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:18)=='*Na_CRACK3D_COOR_7'.and. Instructions(I_INFO)(19:19)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        Na_CRACK3D_COOR(7,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      Read_kpp_Each_NaCr3D_Poi_Num(7)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:18)=='*Na_CRACK3D_COOR_8'.and. Instructions(I_INFO)(19:19)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        Na_CRACK3D_COOR(8,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      Read_kpp_Each_NaCr3D_Poi_Num(8)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:18)=='*Na_CRACK3D_COOR_9'.and. Instructions(I_INFO)(19:19)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        Na_CRACK3D_COOR(9,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      Read_kpp_Each_NaCr3D_Poi_Num(9)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:19)=='*Na_CRACK3D_COOR_10'.and. Instructions(I_INFO)(20:20)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
      IF(YES_EVEN.EQV..FALSE.) THEN !如果数据个数是奇数,则退出
          WRITE(*,1002)
          CALL WARNING_MESSAGE('S',KEYWORDS_BLANK)
      ENDIF
      READ(Instructions(I_INFO+1) ,*) T_CRACK_COORS(1:NUM_DATA)
      DO I = 1,4  !裂缝片段循环
        Na_CRACK3D_COOR(10,I,1:3)= T_CRACK_COORS(3*I-2:3*I)
      ENDDO
      Read_kpp_Each_NaCr3D_Poi_Num(10)  = NUM_DATA/3
  ELSEIF (Instructions(I_INFO)(1:19)=='*Na_CRACK3D_COOR_11'.and. Instructions(I_INFO)(20:20)==' ')THEN
      print *, '    Error :: input file does not support *Na_CRACK3D_COOR_11 or higher!'
      call Warning_Message('S',Keywords_Blank)
  ENDIF
  IF (Instructions(I_INFO)(1:22)=='*NA_CRACK3D_CIR_COOR_1' .and. Instructions(I_INFO)(23:23)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) Read_kpp_Na_Crack3D_Cir_Coor(1,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*NA_CRACK3D_CIR_COOR_2'.and. Instructions(I_INFO)(23:23)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) Read_kpp_Na_Crack3D_Cir_Coor(2,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*NA_CRACK3D_CIR_COOR_3'.and. Instructions(I_INFO)(23:23)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) Read_kpp_Na_Crack3D_Cir_Coor(3,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*NA_CRACK3D_CIR_COOR_4'.and. Instructions(I_INFO)(23:23)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) Read_kpp_Na_Crack3D_Cir_Coor(4,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*NA_CRACK3D_CIR_COOR_5'.and. Instructions(I_INFO)(23:23)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) Read_kpp_Na_Crack3D_Cir_Coor(5,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*NA_CRACK3D_CIR_COOR_6'.and. Instructions(I_INFO)(23:23)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) Read_kpp_Na_Crack3D_Cir_Coor(6,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*NA_CRACK3D_CIR_COOR_7'.and. Instructions(I_INFO)(23:23)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) Read_kpp_Na_Crack3D_Cir_Coor(7,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*NA_CRACK3D_CIR_COOR_8'.and. Instructions(I_INFO)(23:23)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) Read_kpp_Na_Crack3D_Cir_Coor(8,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:22)=='*NA_CRACK3D_CIR_COOR_9'.and. Instructions(I_INFO)(23:23)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) Read_kpp_Na_Crack3D_Cir_Coor(9,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:23)=='*NA_CRACK3D_CIR_COOR_10'.and. Instructions(I_INFO)(24:24)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) Read_kpp_Na_Crack3D_Cir_Coor(10,1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:23)=='*NA_CRACK3D_CIR_COOR_11'.and. Instructions(I_INFO)(24:24)==' ')THEN
      print *, '    Error :: input file does not support *NA_CRACK3D_CIR_COOR_11 or higher!'
      call Warning_Message('S',Keywords_Blank)
  ENDIF
  IF(Instructions(I_INFO)(1:25)=='*PENALTY_CS_NATURAL_CRACK' .and. Instructions(I_INFO)(26:26)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) penalty_CS_Natural_Crack
  ENDIF
  IF(Instructions(I_INFO)(1:18)=='*KEY_CONTA_CONCRIT' .and. Instructions(I_INFO)(19:19)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_Conta_ConCrit
  ENDIF
  !2023-08-27.
  !Key_Penalty_CS_Method=1则罚函数控制l、m、n位移，=2(default)则仅罚函数控制n方向位移，即法线方向位移. NEWFTU2023082701.
  IF(Instructions(I_INFO)(1:22)=='*KEY_PENALTY_CS_METHOD' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_Penalty_CS_Method
  ENDIF
  !2023-09-16.
  IF(Instructions(I_INFO)(1:20)=='*ADJ_PRP_STEP_3D_MAX' .and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Adj_Prp_Step_3D_Max
  ENDIF
  !2023-09-19.
  IF(Instructions(I_INFO)(1:20)=='*KEY_JUNCTION_ENRICH' .and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_Junction_Enrich
  ENDIF
  !2024-02-15.
  IF(Instructions(I_INFO)(1:22)=='*KEY_SAVE_CRACK_RADIUS' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_Save_Crack_Radius
  ENDIF
  !2024-02-22. NEWFTU2024022201.
  IF(Instructions(I_INFO)(1:15)=='*KIC_NA_CRACK_1' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(1)
  ELSEIF(Instructions(I_INFO)(1:15)=='*KIC_NA_CRACK_2' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(2)
  ELSEIF(Instructions(I_INFO)(1:15)=='*KIC_NA_CRACK_3' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(3)
  ELSEIF(Instructions(I_INFO)(1:15)=='*KIC_NA_CRACK_4' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(4)
  ELSEIF(Instructions(I_INFO)(1:15)=='*KIC_NA_CRACK_5' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(5)
  ELSEIF(Instructions(I_INFO)(1:15)=='*KIC_NA_CRACK_6' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(6)
  ELSEIF(Instructions(I_INFO)(1:15)=='*KIC_NA_CRACK_7' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(7)
  ELSEIF(Instructions(I_INFO)(1:15)=='*KIC_NA_CRACK_8' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(8)
  ELSEIF(Instructions(I_INFO)(1:15)=='*KIC_NA_CRACK_9' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(9)
  ELSEIF(Instructions(I_INFO)(1:16)=='*KIC_NA_CRACK_10' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(10)
  ELSEIF(Instructions(I_INFO)(1:16)=='*KIC_NA_CRACK_11' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(11)
  ELSEIF(Instructions(I_INFO)(1:16)=='*KIC_NA_CRACK_12' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(12)
  ELSEIF(Instructions(I_INFO)(1:16)=='*KIC_NA_CRACK_13' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(13)
  ELSEIF(Instructions(I_INFO)(1:16)=='*KIC_NA_CRACK_14' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(14)
  ELSEIF(Instructions(I_INFO)(1:16)=='*KIC_NA_CRACK_15' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(15)
  ELSEIF(Instructions(I_INFO)(1:16)=='*KIC_NA_CRACK_16' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(16)
  ELSEIF(Instructions(I_INFO)(1:16)=='*KIC_NA_CRACK_17' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(17)
  ELSEIF(Instructions(I_INFO)(1:16)=='*KIC_NA_CRACK_18' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(18)
  ELSEIF(Instructions(I_INFO)(1:16)=='*KIC_NA_CRACK_19' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(19)
  ELSEIF(Instructions(I_INFO)(1:16)=='*KIC_NA_CRACK_20' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(20)
  ELSEIF(Instructions(I_INFO)(1:16)=='*KIC_NA_CRACK_21' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(21)
  ELSEIF(Instructions(I_INFO)(1:16)=='*KIC_NA_CRACK_22' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(22)
  ELSEIF(Instructions(I_INFO)(1:16)=='*KIC_NA_CRACK_23' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(23)
  ELSEIF(Instructions(I_INFO)(1:16)=='*KIC_NA_CRACK_24' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(24)
  ELSEIF(Instructions(I_INFO)(1:16)=='*KIC_NA_CRACK_25' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(25)
  ELSEIF(Instructions(I_INFO)(1:16)=='*KIC_NA_CRACK_26' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(26)
  ELSEIF(Instructions(I_INFO)(1:16)=='*KIC_NA_CRACK_27' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(27)
  ELSEIF(Instructions(I_INFO)(1:16)=='*KIC_NA_CRACK_28' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(28)
  ELSEIF(Instructions(I_INFO)(1:16)=='*KIC_NA_CRACK_29' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(29)
  ELSEIF(Instructions(I_INFO)(1:16)=='*KIC_NA_CRACK_30' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KIc_NaCr(30)
  ELSEIF(Instructions(I_INFO)(1:16)=='*KIC_NA_CRACK_31' .and. Instructions(I_INFO)(17:17)==' ')THEN
      print *, '    Error :: input file does not support *KIC_NA_CRACK_31 or higher!'
      call Warning_Message('S',Keywords_Blank)
  ENDIF
  !2024-02-22. NEWFTU2024022202.
  IF(Instructions(I_INFO)(1:8)=='*ST_NACR' .and. Instructions(I_INFO)(9:9)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) c_Temp_Double
      St_NaCr = c_Temp_Double
  ENDIF
  !2024-02-22. NEWFTU2024022202.
  IF(Instructions(I_INFO)(1:14)=='*ST_NA_CRACK_1' .and. Instructions(I_INFO)(15:15)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(1)
  ELSEIF(Instructions(I_INFO)(1:14)=='*ST_NA_CRACK_2' .and. Instructions(I_INFO)(15:15)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(2)
  ELSEIF(Instructions(I_INFO)(1:14)=='*ST_NA_CRACK_3' .and. Instructions(I_INFO)(15:15)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(3)
  ELSEIF(Instructions(I_INFO)(1:14)=='*ST_NA_CRACK_4' .and. Instructions(I_INFO)(15:15)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(4)
  ELSEIF(Instructions(I_INFO)(1:14)=='*ST_NA_CRACK_5' .and. Instructions(I_INFO)(15:15)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(5)
  ELSEIF(Instructions(I_INFO)(1:14)=='*ST_NA_CRACK_6' .and. Instructions(I_INFO)(15:15)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(6)
  ELSEIF(Instructions(I_INFO)(1:14)=='*ST_NA_CRACK_7' .and. Instructions(I_INFO)(15:15)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(7)
  ELSEIF(Instructions(I_INFO)(1:14)=='*ST_NA_CRACK_8' .and. Instructions(I_INFO)(15:15)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(8)
  ELSEIF(Instructions(I_INFO)(1:14)=='*ST_NA_CRACK_9' .and. Instructions(I_INFO)(15:15)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(9)
  ELSEIF(Instructions(I_INFO)(1:15)=='*ST_NA_CRACK_10' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(10)
  ELSEIF(Instructions(I_INFO)(1:15)=='*ST_NA_CRACK_11' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(11)
  ELSEIF(Instructions(I_INFO)(1:15)=='*ST_NA_CRACK_12' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(12)
  ELSEIF(Instructions(I_INFO)(1:15)=='*ST_NA_CRACK_13' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(13)
  ELSEIF(Instructions(I_INFO)(1:15)=='*ST_NA_CRACK_14' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(14)
  ELSEIF(Instructions(I_INFO)(1:15)=='*ST_NA_CRACK_15' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(15)
  ELSEIF(Instructions(I_INFO)(1:15)=='*ST_NA_CRACK_16' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(16)
  ELSEIF(Instructions(I_INFO)(1:15)=='*ST_NA_CRACK_17' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(17)
  ELSEIF(Instructions(I_INFO)(1:15)=='*ST_NA_CRACK_18' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(18)
  ELSEIF(Instructions(I_INFO)(1:15)=='*ST_NA_CRACK_19' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(19)
  ELSEIF(Instructions(I_INFO)(1:15)=='*ST_NA_CRACK_20' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(20)
  ELSEIF(Instructions(I_INFO)(1:15)=='*ST_NA_CRACK_21' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(21)
  ELSEIF(Instructions(I_INFO)(1:15)=='*ST_NA_CRACK_22' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(22)
  ELSEIF(Instructions(I_INFO)(1:15)=='*ST_NA_CRACK_23' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(23)
  ELSEIF(Instructions(I_INFO)(1:15)=='*ST_NA_CRACK_24' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(24)
  ELSEIF(Instructions(I_INFO)(1:15)=='*ST_NA_CRACK_25' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(25)
  ELSEIF(Instructions(I_INFO)(1:15)=='*ST_NA_CRACK_26' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(26)
  ELSEIF(Instructions(I_INFO)(1:15)=='*ST_NA_CRACK_27' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(27)
  ELSEIF(Instructions(I_INFO)(1:15)=='*ST_NA_CRACK_28' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(28)
  ELSEIF(Instructions(I_INFO)(1:15)=='*ST_NA_CRACK_29' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(29)
  ELSEIF(Instructions(I_INFO)(1:15)=='*ST_NA_CRACK_30' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) St_NaCr(30)
  ELSEIF(Instructions(I_INFO)(1:15)=='*ST_NA_CRACK_31' .and. Instructions(I_INFO)(16:16)==' ')THEN
      print *, '    Error :: input file does not support *ST_NA_CRACK_31 or higher!'
      call Warning_Message('S',Keywords_Blank)
  ENDIF
  !2024-02-22. 3D水力压裂实验相关. NEWFTU2024022203.
  IF(Instructions(I_INFO)(1:21)=='*HFE_SURFACE_LOAD_NUM'.and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) HFE_Surface_Load_Num
  ENDIF
  IF(Instructions(I_INFO)(1:27)=='*HFE_INITIAL_INJECTION_RATE'.and. Instructions(I_INFO)(28:28)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) HFE_Initial_Injection_Rate
  ENDIF
  IF(Instructions(I_INFO)(1:20)=='*HFE_HOLE_MAT_NUMBER'.and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) HFE_Hole_Mat_Number
  ENDIF
  IF(Instructions(I_INFO)(1:25)=='*HFE_INITIAL_TRY_PRESSURE'.and. Instructions(I_INFO)(26:26)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) HFE_Initial_Try_Pressure
  ENDIF
  IF(Instructions(I_INFO)(1:31)=='*HFE_INITIAL_PRESSURE_STEP_SIZE'.and. Instructions(I_INFO)(32:32)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) HFE_Initial_Pressure_Step_Size
  ENDIF
  IF(Instructions(I_INFO)(1:15)=='*SIZE_INI_CRACK' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Size_Ini_Crack
  ENDIF
  !2024-02-26. IMPROV2024022602.
  IF(Instructions(I_INFO)(1:20)=='*NUM_POLY_EDGES_NACR' .and. Instructions(I_INFO)(21:21)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Num_Poly_Edges_NaCr
  ENDIF
  !2024-02-28.
  IF(Instructions(I_INFO)(1:28)=='*SLIPWATER_MAX_TIME_STEPS_3D' .and. Instructions(I_INFO)(29:29)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) SlipWater_Max_Time_Steps_3D
  ENDIF
  IF(Instructions(I_INFO)(1:28)=='*SLIPWATER_MAX_PRES_STEPS_3D' .and. Instructions(I_INFO)(29:29)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) SlipWater_Max_Pres_Steps_3D
  ENDIF
  IF(Instructions(I_INFO)(1:31)=='*SLIPWATER_TIME_STEP_CONV_CHECK' .and. Instructions(I_INFO)(32:32)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) SlipWater_Time_Step_Conv_Check
  ENDIF
  IF(Instructions(I_INFO)(1:31)=='*SLIPWATER_PRES_STEP_CONV_CHECK' .and. Instructions(I_INFO)(32:32)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) SlipWater_Pres_Step_Conv_Check
  ENDIF
  !2024-03-09.
  IF(Instructions(I_INFO)(1:16)=='*VISCOSITY_PAR_M' .and. Instructions(I_INFO)(17:17)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Viscosity_Par_m
  ENDIF
  IF(Instructions(I_INFO)(1:16)=='*KEY_VISCO_TYPE' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_Visco_Type
  ENDIF  
  IF(Instructions(I_INFO)(1:13)=='*KEY_LEAKOFF' .and. Instructions(I_INFO)(14:14)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_Leakoff
  ENDIF 
  IF(Instructions(I_INFO)(1:12)=='*COEFF_LEAK' .and. Instructions(I_INFO)(13:13)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Coeff_Leak
  ENDIF   
  IF (Instructions(I_INFO)(1:14)=='*INJECT_C_TIME'.and. Instructions(I_INFO)(15:15)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)!由逗号隔开的字符串数据中数据的个数,"逗号数加1"
      READ(Instructions(I_INFO+1) ,*) INJECT_C_TIME(1:NUM_DATA)
  ELSEIF (Instructions(I_INFO)(1:13)=='*INJECT_C_VAL'.and. Instructions(I_INFO)(14:14)==' ')THEN
      CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)!由逗号隔开的字符串数据中数据的个数,"逗号数加1"
      READ(Instructions(I_INFO+1),*) INJECT_C_VAL(1:NUM_DATA)
  ENDIF
  IF(Instructions(I_INFO)(1:15)=='*INJ_POINT_LOC'.and. Instructions(I_INFO)(16:16)==' ')THEN
    CALL TOOL_GET_NUM_DATA_FROM_A_STRING(Instructions(I_INFO+1),256,NUM_DATA,YES_EVEN)
    READ(Instructions(I_INFO+1),*) Inj_Point_Loc(1:NUM_DATA)
  ENDIF
  !2024-03-11.
  IF (Instructions(I_INFO)(1:11)=='*KEY_RANDOM'.and. Instructions(I_INFO)(12:12)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) KEY_RANDOM
  ENDIF
  !2024-03-13.
  IF(Instructions(I_INFO)(1:21)=='*THERMAL_STR_TEMPER_1' .and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(1)
  ELSEIF(Instructions(I_INFO)(1:21)=='*THERMAL_STR_TEMPER_2' .and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(2)
  ELSEIF(Instructions(I_INFO)(1:21)=='*THERMAL_STR_TEMPER_3' .and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(3)
  ELSEIF(Instructions(I_INFO)(1:21)=='*THERMAL_STR_TEMPER_4' .and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(4)
  ELSEIF(Instructions(I_INFO)(1:21)=='*THERMAL_STR_TEMPER_5' .and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(5)
  ELSEIF(Instructions(I_INFO)(1:21)=='*THERMAL_STR_TEMPER_6' .and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(6)
  ELSEIF(Instructions(I_INFO)(1:21)=='*THERMAL_STR_TEMPER_7' .and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(7)
  ELSEIF(Instructions(I_INFO)(1:21)=='*THERMAL_STR_TEMPER_8' .and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(8)
  ELSEIF(Instructions(I_INFO)(1:21)=='*THERMAL_STR_TEMPER_9' .and. Instructions(I_INFO)(22:22)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(9)
  ELSEIF(Instructions(I_INFO)(1:22)=='*THERMAL_STR_TEMPER_10' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(10)
  ELSEIF(Instructions(I_INFO)(1:22)=='*THERMAL_STR_TEMPER_11' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(11)
  ELSEIF(Instructions(I_INFO)(1:22)=='*THERMAL_STR_TEMPER_12' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(12)
  ELSEIF(Instructions(I_INFO)(1:22)=='*THERMAL_STR_TEMPER_13' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(13)      
  ELSEIF(Instructions(I_INFO)(1:22)=='*THERMAL_STR_TEMPER_14' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(14)
  ELSEIF(Instructions(I_INFO)(1:22)=='*THERMAL_STR_TEMPER_15' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(15)
  ELSEIF(Instructions(I_INFO)(1:22)=='*THERMAL_STR_TEMPER_16' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(16)
  ELSEIF(Instructions(I_INFO)(1:22)=='*THERMAL_STR_TEMPER_17' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(17)      
  ELSEIF(Instructions(I_INFO)(1:22)=='*THERMAL_STR_TEMPER_18' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(18)
  ELSEIF(Instructions(I_INFO)(1:22)=='*THERMAL_STR_TEMPER_19' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(19)
  ELSEIF(Instructions(I_INFO)(1:22)=='*THERMAL_STR_TEMPER_20' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(20)
  ELSEIF(Instructions(I_INFO)(1:22)=='*THERMAL_STR_TEMPER_21' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(21)      
  ELSEIF(Instructions(I_INFO)(1:22)=='*THERMAL_STR_TEMPER_22' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(22)
  ELSEIF(Instructions(I_INFO)(1:22)=='*THERMAL_STR_TEMPER_23' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(23)
  ELSEIF(Instructions(I_INFO)(1:22)=='*THERMAL_STR_TEMPER_24' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(24)
  ELSEIF(Instructions(I_INFO)(1:22)=='*THERMAL_STR_TEMPER_25' .and. Instructions(I_INFO)(23:23)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Thermal_Str_Temper(25)            
  ELSEIF(Instructions(I_INFO)(1:22)=='*THERMAL_STR_TEMPER_26' .and. Instructions(I_INFO)(23:23)==' ')THEN
      print *, '    Error :: input file does not support *THERMAL_STR_TEMPER_26 or higher!'
      call Warning_Message('S',Keywords_Blank)
  ENDIF
  !2024-03-13.
  IF(Instructions(I_INFO)(1:25)=='*MAT_NUMBER_OF_INSTRESS_1' .and. Instructions(I_INFO)(26:26)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Mat_Number_of_InStress(1)
  ELSEIF(Instructions(I_INFO)(1:25)=='*MAT_NUMBER_OF_INSTRESS_2' .and. Instructions(I_INFO)(26:26)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Mat_Number_of_InStress(2)  
  ELSEIF(Instructions(I_INFO)(1:25)=='*MAT_NUMBER_OF_INSTRESS_3' .and. Instructions(I_INFO)(26:26)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Mat_Number_of_InStress(3)  
  ELSEIF(Instructions(I_INFO)(1:25)=='*MAT_NUMBER_OF_INSTRESS_4' .and. Instructions(I_INFO)(26:26)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Mat_Number_of_InStress(4)  
  ELSEIF(Instructions(I_INFO)(1:25)=='*MAT_NUMBER_OF_INSTRESS_5' .and. Instructions(I_INFO)(26:26)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Mat_Number_of_InStress(5)  
  ELSEIF(Instructions(I_INFO)(1:25)=='*MAT_NUMBER_OF_INSTRESS_6' .and. Instructions(I_INFO)(26:26)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Mat_Number_of_InStress(6)  
  ELSEIF(Instructions(I_INFO)(1:25)=='*MAT_NUMBER_OF_INSTRESS_7' .and. Instructions(I_INFO)(26:26)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Mat_Number_of_InStress(7)        
  ELSEIF(Instructions(I_INFO)(1:25)=='*MAT_NUMBER_OF_INSTRESS_8' .and. Instructions(I_INFO)(26:26)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Mat_Number_of_InStress(8)        
  ELSEIF(Instructions(I_INFO)(1:25)=='*MAT_NUMBER_OF_INSTRESS_9' .and. Instructions(I_INFO)(26:26)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Mat_Number_of_InStress(9)        
  ELSEIF(Instructions(I_INFO)(1:26)=='*MAT_NUMBER_OF_INSTRESS_10' .and. Instructions(I_INFO)(27:27)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Mat_Number_of_InStress(10)        
  ELSEIF(Instructions(I_INFO)(1:26)=='*MAT_NUMBER_OF_INSTRESS_11' .and. Instructions(I_INFO)(27:27)==' ')THEN
      print *, '    Error :: input file does not support *MAT_NUMBER_OF_INSTRESS_11 or higher!'
      call Warning_Message('S',Keywords_Blank)
  ENDIF
  !2024-03-13.
  IF(Instructions(I_INFO)(1:15)=='*MAT_INSTRESS_X' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) MAT_INSTRESS_X
  ELSEIF(Instructions(I_INFO)(1:15)=='*MAT_INSTRESS_Y' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) MAT_INSTRESS_Y 
  ELSEIF(Instructions(I_INFO)(1:15)=='*MAT_INSTRESS_Z' .and. Instructions(I_INFO)(16:16)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) MAT_INSTRESS_Z  
  ENDIF
  
  !2024-03-13.
  IF (Instructions(I_INFO)(1:29)=='*MAT_ALLOW_CRACK_INITIATION_1'.and. Instructions(I_INFO)(30:30)==' ')THEN
      READ(Instructions(I_INFO+1),*) MAT_ALLOW_CRACK_Initiation(1)
  ELSEIF (Instructions(I_INFO)(1:29)=='*MAT_ALLOW_CRACK_INITIATION_2'.and. Instructions(I_INFO)(30:30)==' ')THEN
      READ(Instructions(I_INFO+1),*) MAT_ALLOW_CRACK_Initiation(2)     
  ELSEIF (Instructions(I_INFO)(1:29)=='*MAT_ALLOW_CRACK_INITIATION_3'.and. Instructions(I_INFO)(30:30)==' ')THEN
      READ(Instructions(I_INFO+1),*) MAT_ALLOW_CRACK_Initiation(3)   
  ELSEIF (Instructions(I_INFO)(1:29)=='*MAT_ALLOW_CRACK_INITIATION_4'.and. Instructions(I_INFO)(30:30)==' ')THEN
      READ(Instructions(I_INFO+1),*) MAT_ALLOW_CRACK_Initiation(4)   
  ELSEIF (Instructions(I_INFO)(1:29)=='*MAT_ALLOW_CRACK_INITIATION_5'.and. Instructions(I_INFO)(30:30)==' ')THEN
      READ(Instructions(I_INFO+1),*) MAT_ALLOW_CRACK_Initiation(5)   
  ELSEIF (Instructions(I_INFO)(1:29)=='*MAT_ALLOW_CRACK_INITIATION_6'.and. Instructions(I_INFO)(30:30)==' ')THEN
      READ(Instructions(I_INFO+1),*) MAT_ALLOW_CRACK_Initiation(6)   
  ELSEIF (Instructions(I_INFO)(1:29)=='*MAT_ALLOW_CRACK_INITIATION_7'.and. Instructions(I_INFO)(30:30)==' ')THEN
      READ(Instructions(I_INFO+1),*) MAT_ALLOW_CRACK_Initiation(7)   
  ELSEIF (Instructions(I_INFO)(1:29)=='*MAT_ALLOW_CRACK_INITIATION_8'.and. Instructions(I_INFO)(30:30)==' ')THEN
      READ(Instructions(I_INFO+1),*) MAT_ALLOW_CRACK_Initiation(8)   
  ELSEIF (Instructions(I_INFO)(1:29)=='*MAT_ALLOW_CRACK_INITIATION_9'.and. Instructions(I_INFO)(30:30)==' ')THEN
      READ(Instructions(I_INFO+1),*) MAT_ALLOW_CRACK_Initiation(9)   
  ELSEIF (Instructions(I_INFO)(1:30)=='*MAT_ALLOW_CRACK_INITIATION_10'.and. Instructions(I_INFO)(31:31)==' ')THEN
      READ(Instructions(I_INFO+1),*) MAT_ALLOW_CRACK_Initiation(10)   
  ELSEIF (Instructions(I_INFO)(1:30)=='*MAT_ALLOW_CRACK_INITIATION_11'.and. Instructions(I_INFO)(31:31)==' ')THEN
      READ(Instructions(I_INFO+1),*) MAT_ALLOW_CRACK_Initiation(11)   
  ELSEIF (Instructions(I_INFO)(1:30)=='*MAT_ALLOW_CRACK_INITIATION_12'.and. Instructions(I_INFO)(31:31)==' ')THEN
      READ(Instructions(I_INFO+1),*) MAT_ALLOW_CRACK_Initiation(12)   
  ELSEIF (Instructions(I_INFO)(1:30)=='*MAT_ALLOW_CRACK_INITIATION_13'.and. Instructions(I_INFO)(31:31)==' ')THEN
      READ(Instructions(I_INFO+1),*) MAT_ALLOW_CRACK_Initiation(13) 
  ELSEIF (Instructions(I_INFO)(1:30)=='*MAT_ALLOW_CRACK_INITIATION_14'.and. Instructions(I_INFO)(31:31)==' ')THEN
      READ(Instructions(I_INFO+1),*) MAT_ALLOW_CRACK_Initiation(14)   
  ELSEIF (Instructions(I_INFO)(1:30)=='*MAT_ALLOW_CRACK_INITIATION_15'.and. Instructions(I_INFO)(31:31)==' ')THEN
      READ(Instructions(I_INFO+1),*) MAT_ALLOW_CRACK_Initiation(15) 
  ELSEIF (Instructions(I_INFO)(1:30)=='*MAT_ALLOW_CRACK_INITIATION_16'.and. Instructions(I_INFO)(31:31)==' ')THEN
      READ(Instructions(I_INFO+1),*) MAT_ALLOW_CRACK_Initiation(16)   
  ELSEIF (Instructions(I_INFO)(1:30)=='*MAT_ALLOW_CRACK_INITIATION_17'.and. Instructions(I_INFO)(31:31)==' ')THEN
      READ(Instructions(I_INFO+1),*) MAT_ALLOW_CRACK_Initiation(17) 
  ELSEIF (Instructions(I_INFO)(1:30)=='*MAT_ALLOW_CRACK_INITIATION_18'.and. Instructions(I_INFO)(31:31)==' ')THEN
      READ(Instructions(I_INFO+1),*) MAT_ALLOW_CRACK_Initiation(18)   
  ELSEIF (Instructions(I_INFO)(1:30)=='*MAT_ALLOW_CRACK_INITIATION_19'.and. Instructions(I_INFO)(31:31)==' ')THEN
      READ(Instructions(I_INFO+1),*) MAT_ALLOW_CRACK_Initiation(19) 
  ELSEIF (Instructions(I_INFO)(1:30)=='*MAT_ALLOW_CRACK_INITIATION_20'.and. Instructions(I_INFO)(31:31)==' ')THEN
      READ(Instructions(I_INFO+1),*) MAT_ALLOW_CRACK_Initiation(20)   
  ELSEIF (Instructions(I_INFO)(1:30)=='*MAT_ALLOW_CRACK_INITIATION_21'.and. Instructions(I_INFO)(31:31)==' ')THEN
      READ(Instructions(I_INFO+1),*) MAT_ALLOW_CRACK_Initiation(21) 
  ELSEIF (Instructions(I_INFO)(1:30)=='*MAT_ALLOW_CRACK_INITIATION_22'.and. Instructions(I_INFO)(31:31)==' ')THEN
      print *, '    Error :: kpp file does not support *MAT_ALLOW_CRACK_INITIATION_22 or higher!'
      call Warning_Message('S',Keywords_Blank)
  ENDIF
  !2024-03-13.
  IF(Instructions(I_INFO)(1:30)=='*KEY_MAX_NUM_INITIATION_CRACKS' .and. Instructions(I_INFO)(31:31)==' ')THEN
      READ(Instructions(I_INFO+1) , * ) Key_Max_Num_Initiation_Cracks
  ENDIF
ENDDO

!print*,CRACK_PRESSURE(1:10)
!print *,"num_Crack:",num_Crack
!print *,CP_x_nodes(1:30)


!*****************************
!Full_Pathname路径.
!*****************************
!WINDOWS
!Full_Pathname =  trim(Work_Directory)//String_Connector//trim(Filename) !全路径
!LINUX
!Full_Pathname =  trim(Work_Directory)//'/'//trim(Filename) !全路径

if(Operation_System_Type==1)then      !Linux
    Full_Pathname =  trim(Work_Directory)//'/'//trim(Filename)
elseif(Operation_System_Type==2)then  !MAC OS
    Full_Pathname =  trim(Work_Directory)//'/'//trim(Filename)
elseif(Operation_System_Type==3)then  !Windows
    full_pathname =  trim(work_directory)//string_connector//trim(filename)
elseif(Operation_System_Type==0)then
    full_pathname =  trim(work_directory)//string_connector//trim(filename)
endif

close(301)

print *," >> All done."

!*****************************************
!清空内存.
!*****************************************
!print *,1001
deallocate(INPUT_INFO)
deallocate(Input_Info_before_Upper)
deallocate(Instructions)
deallocate(Instructions_before_Upper)
deallocate(Parameter_lines)
deallocate(Parameter_lines_par)
deallocate(Parameter_lines_par_size)
deallocate(Parameter_lines_value)
deallocate(Parameter_lines_value_size)

RETURN
END program PhiPsi_Read_Input
