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
 
recursive subroutine Tool_String_arithmetic(Instruction)
!�ݹ��ӳ���.
!�ַ����������㴦��.

!2024-03-15.

use Global_Float_Type
use Global_Elem_Area_Vol
use Global_Common

implicit none
character(len=256),intent(inout)::Instruction

!2024-02-24. ������������.
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

character(len=256) Input_Instruction

integer Tool_chrpak_ch_index_first
integer Sign_Location_minus_2,next_comma_index,Sign_Location_minus_temp
integer Sign_Location_minus_real
integer Tool_chrpak_ch_index_first_for_plus_sign,Tool_chrpak_ch_index_first_for_minus_sign

character(len=256) Output_String_for_parentheses,parentheses_result
logical parentheses_found
integer left_index, right_index
integer size_Input_Instruction
integer I_Try_Parentheses
integer num_char_left,num_char_right

Input_Instruction = Instruction



!////////////////////////////////////////
!                                       /
!                                       /
!              ��������.                /
!             2024-03-16.               /
!                                       /
!////////////////////////////////////////
call Tool_String_count_char_occurrences(Input_Instruction,'(', num_char_left)
call Tool_String_count_char_occurrences(Input_Instruction,')', num_char_right)
if(num_char_left/=num_char_right) then
    print *, '    Error :: number of "C" differs from number of  ")" in the *kpp file!'
    print *, "             Input_Instruction:",Input_Instruction
    print *, "             num_char_left:    ",num_char_left
    print *, "             num_char_right:   ",num_char_right
    call Warning_Message('S',Keywords_Blank)
endif
!���֧��50������Ƕ��.
do I_Try_Parentheses =1,50
    call Tool_String_extract_content_in_parentheses(Input_Instruction,Output_String_for_parentheses,&
                                                    left_index, right_index,parentheses_found) 
    if(parentheses_found) then
        !print *,"Input_Instruction:            ",Input_Instruction
        size_Input_Instruction = len_trim(Input_Instruction)
        !print *,"left_index, right_index:      ",left_index, right_index
        !print *,"Output_String_for_parentheses:",Output_String_for_parentheses
        !���������ڵ�����.
        call Tool_String_arithmetic(Output_String_for_parentheses)
        parentheses_result = Output_String_for_parentheses
        !print *,"parentheses_result:           ",parentheses_result
        
        !parentheses_result = '01234567890099877'
        
        !���������ݽ����滻.
        Tem_String = Input_Instruction
        Input_Instruction(left_index:left_index) = ''
        Input_Instruction(left_index+1:left_index+len_trim(parentheses_result)) = parentheses_result(1:len_trim(parentheses_result))
        Input_Instruction(left_index+len_trim(parentheses_result)+1:left_index+len_trim(parentheses_result)+1) = ''
        Input_Instruction(left_index+len_trim(parentheses_result)+2:) = Tem_String(right_index+1:)
        
        !ɾ���ո�.
        call Tool_chrpak_s_blank_delete(Input_Instruction) 
        !print *,"Input_Instruction�����:      ",Input_Instruction
    endif
enddo

!////////////////////////////////////////
!                                       /
!                                       /
!              ����"*"��.               /
!                                       /
!                                       /
!////////////////////////////////////////
!�������100��"*"��.
do i_Calculate= 1,100
    Yes_Found_Value_After = .false.
    !�������Ƿ���"*"����(��һ�γ��ֵ�λ��).
    Sign_Location = Tool_chrpak_ch_index_first(Input_Instruction,'*')
    !print *,'Sign_Location:',Sign_Location
    !�����"*"�ſ�ͷ��Ϊ�ؼ�����.
    if(Sign_Location==1)then
        exit
    !û��"*"�Ż�����"*"��.
    elseif(Sign_Location<=0) then
        exit
    endif

    if(Sign_Location>=2) then
!        !�������������*Work_Directory����*Filename�����˳�.
!        if(Instructions(I_INFO-1)(1:9)=='*FILENAME') then
!            exit
!        endif
!        if(Instructions(I_INFO-1)(1:15)=='*WORK_DIRECTORY') then
!            exit
!        endif

        !print *,'Sign_Location:',Sign_Location
        !print *,'Instructions(I_INFO):',Instructions(I_INFO)
        
        !��ȡ"*"���ź���ĵ�1����ֵ.
        !print *,'Instructions(I_INFO)(Sign_Location+1:):',Instructions(I_INFO)(Sign_Location+1:)
        call Tool_chrpak_s_to_r4(Input_Instruction(Sign_Location+1:),Read_Value_After, ierror, Read_Value_length)

        !print *,"IERROR:           ", IERROR
        !print *,"Read_Value_After: ", Read_Value_After
        !print *,"Read_Value_length:", Read_Value_length

        !����Ƿ��ҵ���"*��"�������ֵ.
        if(Read_Value_length<=0)then
            print *, '    Error :: Cannot find value after sign "*"!'
            call Warning_Message('S',Keywords_Blank)
        else
            Yes_Found_Value_After = .True.
            Value_After_Start_Index = Sign_Location+1
            Value_After_End_Index   = Sign_Location+1+Read_Value_length-1
            !�����������:','.
            if(Input_Instruction(Value_After_End_Index:Value_After_End_Index)==",")then
                Value_After_End_Index =  Value_After_End_Index -1
            endif
        endif

        !���"*"����ǰ�������λ�Ƿ�����ֵ.
        Yes_Found_Value_Before = .false.
        do i_Try=1,100
            Index_Start = Sign_Location-(i_Try)
            !print *,"Index_Start:",Index_Start
            !print *,"Instructions(I_INFO)(Index_Start:Sign_Location-1):",Instructions(I_INFO)(Index_Start:Sign_Location-1)
            if (Index_Start <=0) exit
            call Tool_chrpak_s_is_r(Input_Instruction(Index_Start:Sign_Location-1), Value_Try_1,Yes_Value_1)
            Value_Before_Start_Index = Index_Start
            Value_Before_End_Index   = Sign_Location-1
            !print *,"Yes_Value_1:",Yes_Value_1
            !print *,"Value_Try_1:",Value_Try_1
            !print *,"Value_Before_Start_Index:",Value_Before_Start_Index
            !print *,"Value_Before_End_Index:",Value_Before_End_Index
            !first_Charc = Instructions(I_INFO)(Index_Start:Index_Start)
            !if(Yes_Value_1 .and. (first_Charc /='+') .and. (first_Charc /='-')) then
            if(Yes_Value_1) then
                if(Index_Start==1)then
                    Read_Value_Before = Value_Try_1
                    Yes_Found_Value_Before = .True.
                    !print *,7777
                    exit
                else
                    !����ǰ����1λ������Ͳ�������ֵ�ˣ�˵����ֵ��ȷ��.
                    !�����ǰ����1λǡ����+����-�ţ���ô��ֵ����Value_Try_1.
                    first_Charc = Input_Instruction(Index_Start-1:Index_Start-1)
                    if((first_Charc =='+') .or. (first_Charc =='-')) then
                        !print *,8888
                        Read_Value_Before = Value_Try_1
                        Yes_Found_Value_Before = .True.
                        exit
                    endif
                    !print *,9999
                    call Tool_chrpak_s_is_r(Input_Instruction(Index_Start-1:Sign_Location-1), Value_Try_2,Yes_Value_2)
                    !print *,"Yes_Value_2:",Yes_Value_2
                    !print *,"Value_Try_2:",Value_Try_2
                    if(Yes_Value_2 .eqv. .false.) then
                        Read_Value_Before = Value_Try_1
                        Yes_Found_Value_Before = .True.
                        exit
                    endif
                endif
            endif
        enddo

        !print *,"Read_Value_Before:       ",Read_Value_Before
        !print *,"Value_Before_Start_Index:",Value_Before_Start_Index
        !print *,"Value_Before_End_Index:  ",Value_Before_End_Index

        !print *,"Read_Value_After:        ",Read_Value_After
        !print *,"Value_After_Start_Index: ",Value_After_Start_Index
        !print *,"Value_After_End_Index:   ",Value_After_End_Index

        !����Ƿ��ҵ���"*"�ź������ֵ.
        if(Yes_Found_Value_Before .eqv. .false.)then
            print *, '    Error :: Cannot find value before sign "*"!'
            call Warning_Message('S',Keywords_Blank)
        endif

        !������ҵ��ˣ�����м��㣬���ü���ֵ�滻ԭ���ʽ.
        if(Yes_Found_Value_Before .and. Yes_Found_Value_After) then
            !ִ�м���.
            Result_Value = Read_Value_Before*Read_Value_After
            !תΪ�ַ���.
            call Tool_chrpak_r4_to_s_left (Result_Value,Result_String)
            Result_String = trim(adjustl(Result_String))
            !print *,"Result_String:",Result_String
!                !�滻.
!                Instructions(I_INFO)(Value_Before_Start_Index:Value_After_End_Index) = Result_String
!                Instructions_before_Upper(I_INFO)(Value_Before_Start_Index:Value_After_End_Index) = Result_String

            !�滻.
            string_length = Value_After_End_Index- Value_Before_Start_Index +1
            !print *,'string_length:',string_length
            !print *,'len_trim(Result_String):',len_trim(Result_String)

            !������ɵĽ���ַ�������С��ԭ���ȣ���ֱ���滻����.
            if(len_trim(Result_String) <string_length) then
                Input_Instruction(Value_Before_Start_Index:Value_After_End_Index) = Result_String
                !Instructions_before_Upper(I_INFO)(Value_Before_Start_Index:Value_After_End_Index) = Result_String
            !������ɵĽ���ַ������ȴ��ڴ��滻��ԭ���ȣ�����Ҫƽ�ƺ�����ַ���.
            else
                num_added_char = len_trim(Result_String) - string_length
                Tem_String = Input_Instruction
                Input_Instruction(Value_Before_Start_Index:Value_After_End_Index+num_added_char) = Result_String
                Input_Instruction(Value_After_End_Index+num_added_char+1:) = Tem_String(Value_After_End_Index+1:)
                !Tem_String = Instructions_before_Upper(I_INFO)
                !Instructions_before_Upper(I_INFO)(Value_Before_Start_Index:Value_After_End_Index+num_added_char) = Result_String
                !Instructions_before_Upper(I_INFO)(Value_After_End_Index+num_added_char+1:) = &
                !                                                                Tem_String(Value_After_End_Index+1:)
            endif

            !ɾ��ȫ���ո�.
            call Tool_chrpak_s_blank_delete(Input_Instruction)  !Use chrpak to delete all spaces.
            !call Tool_chrpak_s_blank_delete(Instructions_before_Upper(I_INFO))  !Use chrpak to delete all spaces.
            !print *,"INPUT_INFO(I_INFO)_�滻*��:",Instructions(I_INFO)

        endif
    endif
enddo

!////////////////////////////////////////
!                                       /
!                                       /
!              ����"/"��.               /
!                                       /
!                                       /
!////////////////////////////////////////
!�������100��"*"��.
do i_Calculate= 1,100
    Yes_Found_Value_After = .false.
    !�������Ƿ���"*"����(��һ�γ��ֵ�λ��).
    Sign_Location = Tool_chrpak_ch_index_first(Input_Instruction,'/')
    !�����"/"�ſ�ͷ�򱨴�(Linuxϵͳ������������).
    if(Sign_Location==1)then
!        !�������������*Work_Directory����*Filename�����˳�.
!        if(Instructions(I_INFO-1)(1:9)=='*FILENAME') then
!            exit
!        endif
!        if(Instructions(I_INFO-1)(1:15)=='*WORK_DIRECTORY') then
!            exit
!        endif
        exit
!        print *, '    Error :: keywords file cannot starts with sign "/"!'
!        call Warning_Message('S',Keywords_Blank)
    !û��"/"�Ż�����"/"��.
    elseif(Sign_Location<=0) then
        exit
    endif

    if(Sign_Location>=2) then
!        !�������������*Work_Directory����*Filename�����˳�.
!        if(Instructions(I_INFO-1)(1:9)=='*FILENAME') then
!            exit
!        endif
!        if(Instructions(I_INFO-1)(1:15)=='*WORK_DIRECTORY') then
!            exit
!        endif

        !print *,'Sign_Location:',Sign_Location
        !print *,'Instructions(I_INFO):',Instructions(I_INFO)
        !��ȡ"/"���ź���ĵ�1����ֵ.
        !print *,'Instructions(I_INFO)(Sign_Location+1:):',Instructions(I_INFO)(Sign_Location+1:)

        call Tool_chrpak_s_to_r4 (Input_Instruction(Sign_Location+1:),Read_Value_After, ierror, Read_Value_length)

        !print *,"IERROR:           ", IERROR
        !print *,"Read_Value_After: ", Read_Value_After
        !print *,"Read_Value_length:", Read_Value_length

        !����Ƿ��ҵ���"*��"�������ֵ.
        if(Read_Value_length<=0)then
            print *, '    Error :: Cannot find value after sign "/"!'
            call Warning_Message('S',Keywords_Blank)
        else
            Yes_Found_Value_After = .True.
            Value_After_Start_Index = Sign_Location+1
            Value_After_End_Index   = Sign_Location+1+Read_Value_length-1
            !�����������:','.
            if(Input_Instruction(Value_After_End_Index:Value_After_End_Index)==",")then
                Value_After_End_Index =  Value_After_End_Index -1
            endif
        endif

        !���"* "����ǰ�������λ�Ƿ�����ֵ.
        Yes_Found_Value_Before = .false.
        do i_Try=1,100
            Index_Start = Sign_Location-(i_Try)
            !print *,"Index_Start:",Index_Start
            !print *,"Instructions(I_INFO)(Index_Start:Sign_Location-1):",Instructions(I_INFO)(Index_Start:Sign_Location-1)
            if (Index_Start <=0) exit
            call Tool_chrpak_s_is_r(Input_Instruction(Index_Start:Sign_Location-1), Value_Try_1,Yes_Value_1)
            Value_Before_Start_Index = Index_Start
            Value_Before_End_Index   = Sign_Location-1
            !print *,"Yes_Value_1:",Yes_Value_1
            !print *,"Value_Try_1:",Value_Try_1
            if(Yes_Value_1) then
                if(Index_Start==1)then
                    Read_Value_Before = Value_Try_1
                    Yes_Found_Value_Before = .True.
                    exit
                else
                    !����ǰ����1λ������Ͳ�������ֵ�ˣ�˵����ֵ��ȷ��.
                    !�����ǰ����1λǡ����+����-�ţ���ô��ֵ����Value_Try_1.
                    first_Charc = Input_Instruction(Index_Start-1:Index_Start-1)
                    !if((first_Charc /='+') .or. (first_Charc /='-')) then
                    if((first_Charc =='+') .or. (first_Charc =='-')) then
                        Read_Value_Before = Value_Try_1
                        Yes_Found_Value_Before = .True.
                        exit
                    endif
                    call Tool_chrpak_s_is_r(Input_Instruction(Index_Start-1:Sign_Location-1), Value_Try_2,Yes_Value_2)
                    !print *,"Yes_Value_2:",Yes_Value_2
                    !print *,"Value_Try_2:",Value_Try_2
                    if(Yes_Value_2 .eqv. .false.) then
                        Read_Value_Before = Value_Try_1
                        Yes_Found_Value_Before = .True.
                        exit
                    endif
                endif
            endif
        enddo

        !print *,"Read_Value_Before:       ",Read_Value_Before
        !print *,"Value_Before_Start_Index:",Value_Before_Start_Index
        !print *,"Value_Before_End_Index:  ",Value_Before_End_Index

        !print *,"Read_Value_After:        ",Read_Value_After
        !print *,"Value_After_Start_Index: ",Value_After_Start_Index
        !print *,"Value_After_End_Index:   ",Value_After_End_Index

        !����Ƿ��ҵ���"/"�ź������ֵ.
        if(Yes_Found_Value_Before .eqv. .false.)then
            print *, '    Error :: Cannot find value before sign "/"!'
            call Warning_Message('S',Keywords_Blank)
        endif

        !������ҵ��ˣ�����м��㣬���ü���ֵ�滻ԭ���ʽ.
        if(Yes_Found_Value_Before .and. Yes_Found_Value_After) then
            !ִ�м���.
            Result_Value = Read_Value_Before/Read_Value_After
            !תΪ�ַ���.
            call Tool_chrpak_r4_to_s_left (Result_Value,Result_String)
            Result_String = trim(adjustl(Result_String))
            !print *,"Result_String:",Result_String
!                !�滻.
!                Instructions(I_INFO)(Value_Before_Start_Index:Value_After_End_Index) = Result_String
!                Instructions_before_Upper(I_INFO)(Value_Before_Start_Index:Value_After_End_Index) = Result_String

            !�滻.
            string_length = Value_After_End_Index- Value_Before_Start_Index +1
            !print *,'string_length:',string_length
            !������ɵĽ���ַ�������С��ԭ���ȣ���ֱ���滻����.
            if(len_trim(Result_String) <string_length) then
                Input_Instruction(Value_Before_Start_Index:Value_After_End_Index) = Result_String
                !Instructions_before_Upper(I_INFO)(Value_Before_Start_Index:Value_After_End_Index) = Result_String
            !������ɵĽ���ַ������ȴ��ڴ��滻��ԭ���ȣ�����Ҫƽ�ƺ�����ַ���.
            else
                num_added_char = len_trim(Result_String) - string_length
                Tem_String = Input_Instruction
                Input_Instruction(Value_Before_Start_Index:Value_After_End_Index+num_added_char) = Result_String
                Input_Instruction(Value_After_End_Index+num_added_char+1:) = Tem_String(Value_After_End_Index+1:)
                !Tem_String = Instructions_before_Upper(I_INFO)
                !Instructions_before_Upper(I_INFO)(Value_Before_Start_Index:Value_After_End_Index+num_added_char) = Result_String
                !Instructions_before_Upper(I_INFO)(Value_After_End_Index+num_added_char+1:) = &
                !                                                                Tem_String(Value_After_End_Index+1:)
            endif

            !ɾ��ȫ���ո�.
            call Tool_chrpak_s_blank_delete(Input_Instruction)  !Use chrpak to delete all spaces.
            !call Tool_chrpak_s_blank_delete(Instructions_before_Upper(I_INFO))  !Use chrpak to delete all spaces.
            !print *,"INPUT_INFO(I_INFO)_�滻/��:",Instructions(I_INFO)

        endif
    endif
enddo

!////////////////////////////////////////
!                                       /
!                                       /
!           ����"+"�ź�"-"��.           /
!           ��ǰ�������δ���.           /
!                                       /
!                                       /
!////////////////////////////////////////
!�������100��"+"��.
do i_Calculate= 1,100
    !print *, " "
    !print *, "i_Calculate:",i_Calculate
    !print *,'----:',Input_Instruction(1:len_trim(Input_Instruction))

    Yes_Found_Value_After = .false.
    !�������Ƿ���"+"����(��һ�γ��ֵ�λ��).
    !Sign_Location_plus = Tool_chrpak_ch_index_first(Input_Instruction,'+')
    Sign_Location_plus = Tool_chrpak_ch_index_first_for_plus_sign(Input_Instruction)
    
    !�������Ƿ���"-"����(��һ�γ��ֵ�λ��).
    !Sign_Location_minus= Tool_chrpak_ch_index_first(Input_Instruction,'-')
    Sign_Location_minus=  Tool_chrpak_ch_index_first_for_minus_sign(Input_Instruction)
    
    !print *,"Sign_Location_plus-��ʼ: ",Sign_Location_plus
    !print *,"Sign_Location_minus-��ʼ:",Sign_Location_minus

    !˭��ǰ���ȴ���˭.
    logical_plus  = .False.
    logical_minus = .False.
    
    !print *,"Sign_Location_plus: ",Sign_Location_plus
    !print *,"Sign_Location_minus:",Sign_Location_minus

    !��Ϊ-1. ˵��������.
    if(Sign_Location_plus==Sign_Location_minus) then
        exit
    endif

    !���Ϊ�෴������һ�����ڣ�һ��������.
    if(Sign_Location_plus<=0 .and. Sign_Location_minus >0)then
        logical_minus = .True.
        Sign_Location = Sign_Location_minus
        !goto 10
    endif

    if(Sign_Location_plus > 0 .and. Sign_Location_minus<=0)then
        logical_plus  = .True.
        Sign_Location = Sign_Location_plus
        !goto 10
    endif

    !���������������.
    if(Sign_Location_plus > 0 .and. Sign_Location_minus>0) then
        if(Sign_Location_plus < Sign_Location_minus) then
            logical_plus  = .True.
            Sign_Location = Sign_Location_plus
        elseif(Sign_Location_plus > Sign_Location_minus) then
            logical_minus = .True.
            Sign_Location = Sign_Location_minus
        endif
    endif
    
    !������У�����-������-��ͷ������Ҫ�����,���磺-1+2. 2024-03-15.
    if(Sign_Location_plus > 0 .and. Sign_Location_minus>0) then
        !�����-��ͷ.
        if(Sign_Location_minus==1)then
            logical_plus  = .True.
            Sign_Location = Sign_Location_plus
        endif
    endif
    
    !�����"-"��ͷ�����еڸ�2��-��(Sign_Location_minus_2)������Ҫ�����,���磺-1-2. 2024-03-15.
    if(Sign_Location_minus==1 .and. Sign_Location_minus_2 >0) then
        !print *,8888
        logical_minus  = .True.
        Sign_Location = Sign_Location_minus_2
    endif

!        !����Ǵ���"-"������"+"����"-"��ǰ����E��D�ȷ��ţ�����"+".
!        Previous_Char = Instructions(I_INFO)(Sign_Location-1:Sign_Location-1)
!        if(Previous_Char=="E") exit
!        if(Previous_Char=="e") exit
!        if(Previous_Char=="D") exit
!        if(Previous_Char=="d") exit

    !print *,"Sign_Location: ",Sign_Location
    !print *,"logical_plus:  ",logical_plus
    !print *,"logical_minus: ",logical_minus

    !�����"+"�Ż���"-"��ͷ.
    if(Sign_Location==1)then
        !print *, '    Error :: keywords file cannot starts with sign "+"!'
        !call Warning_Message('S',Keywords_Blank)
        !exit
    !û��"+"�Ż�����"+"��.
    elseif(Sign_Location<=0) then
        exit
    endif
    
    !print *,"logical_plus",logical_plus
    !print *,"logical_minus",logical_minus
    !print *,"Sign_Location",Sign_Location
    
    if(Sign_Location>=2) then
!        !�������������*Work_Directory����*Filename�����˳�.
!        if(Instructions(I_INFO-1)(1:9)=='*FILENAME') then
!            exit
!        endif
!        if(Instructions(I_INFO-1)(1:15)=='*WORK_DIRECTORY') then
!            exit
!        endif

!            !���ǰ��E����e����D����d�����˳�.
!            Previous_Char = Instructions(I_INFO)(Sign_Location-1:Sign_Location-1)
!            if(Previous_Char=="E") exit
!            if(Previous_Char=="e") exit
!            if(Previous_Char=="D") exit
!            if(Previous_Char=="d") exit

        !print *,'Sign_Location:',Sign_Location
        !print *,'Instructions(I_INFO):',Instructions(I_INFO)
        !��ȡ"+"���ź���ĵ�1����ֵ.
        !print *,'Input_Instruction(Sign_Location+1:):',Input_Instruction(Sign_Location+1:)

        call Tool_chrpak_s_to_r4(Input_Instruction(Sign_Location+1:),Read_Value_After, ierror, Read_Value_length)

        !print *,"IERROR:           ", IERROR
        !print *,"Read_Value_After: ", Read_Value_After
        !print *,"Read_Value_length:", Read_Value_length

        !����Ƿ��ҵ���"+�ź������ֵ".
        if(Read_Value_length<=0)then
            if(logical_plus) then
                print *, '    Error :: Cannot find value after sign "+"!'
            elseif(logical_minus)then
                print *, '    Error :: Cannot find value after sign "-"!'
            endif
            call Warning_Message('S',Keywords_Blank)
        else
            Yes_Found_Value_After = .True.
            Value_After_Start_Index = Sign_Location+1
            Value_After_End_Index   = Sign_Location+1+Read_Value_length-1
            !�����������:','.
            if(Input_Instruction(Value_After_End_Index:Value_After_End_Index)==",")then
                Value_After_End_Index =  Value_After_End_Index -1
            endif
        endif

        !���"+"����ǰ�������λ�Ƿ�����ֵ.
        Yes_Found_Value_Before = .false.
        do i_Try=1,100
            Index_Start = Sign_Location-(i_Try)
            !print *,"Index_Start:",Index_Start
            !print *,"Instructions(I_INFO)(Index_Start:Sign_Location-1):",Instructions(I_INFO)(Index_Start:Sign_Location-1)
            if (Index_Start <=0) exit
            call Tool_chrpak_s_is_r(Input_Instruction(Index_Start:Sign_Location-1), Value_Try_1,Yes_Value_1)
            Value_Before_Start_Index = Index_Start
            Value_Before_End_Index   = Sign_Location-1
            !print *,"Yes_Value_1:",Yes_Value_1
            !print *,"Value_Try_1:",Value_Try_1
            if(Yes_Value_1) then
                if(Index_Start==1)then
                    Read_Value_Before = Value_Try_1
                    Yes_Found_Value_Before = .True.
                    exit
                else
                    !����ǰ����1λ������Ͳ�������ֵ�ˣ�˵����ֵ��ȷ��.
                    call Tool_chrpak_s_is_r(Input_Instruction(Index_Start-1:Sign_Location-1), Value_Try_2,Yes_Value_2)
                    !print *,"Yes_Value_2:",Yes_Value_2
                    !print *,"Value_Try_2:",Value_Try_2
                    if(Yes_Value_2 .eqv. .false.) then
                        Read_Value_Before = Value_Try_1
                        Yes_Found_Value_Before = .True.
                        exit
                    endif
                endif
            endif
        enddo

        !print *,"Read_Value_Before:       ",Read_Value_Before
        !print *,"Value_Before_Start_Index:",Value_Before_Start_Index
        !print *,"Value_Before_End_Index:  ",Value_Before_End_Index

        !print *,"Read_Value_After:        ",Read_Value_After
        !print *,"Value_After_Start_Index: ",Value_After_Start_Index
        !print *,"Value_After_End_Index:   ",Value_After_End_Index

        !����Ƿ��ҵ���"+"�ź������ֵ.
        if(Yes_Found_Value_Before .eqv. .false.)then
            if(logical_plus) then
                print *, '    Error :: Cannot find value before sign "+"!'
                print *, "             Input_Instruction:",Input_Instruction(1:len_trim(Input_Instruction))
            elseif(logical_minus)then
                print *, '    Error :: Cannot find value before sign "-"!'
                print *, "             Input_Instruction:",Input_Instruction(1:len_trim(Input_Instruction))
            endif
            call Warning_Message('S',Keywords_Blank)
        endif

        !������ҵ��ˣ�����м��㣬���ü���ֵ�滻ԭ���ʽ.
        if(Yes_Found_Value_Before .and. Yes_Found_Value_After) then
            !ִ�м���.
            if(logical_plus) then
                Result_Value = Read_Value_Before + Read_Value_After
            elseif(logical_minus)then
                Result_Value = Read_Value_Before - Read_Value_After
            endif
            !print *,"Result_Value:   ",Result_Value
            !תΪ�ַ���.
            call Tool_chrpak_r4_to_s_left (Result_Value,Result_String)
            !print *,"Result_String:",Result_String
            Result_String = trim(adjustl(Result_String))
            !print *,"Result_String:",Result_String
            !print *,'len(Result_String):',len_trim(Result_String)

            !�滻.
            string_length = Value_After_End_Index- Value_Before_Start_Index +1
            !print *,'string_length:',string_length

            !������ɵĽ���ַ�������С��ԭ���ȣ���ֱ���滻����.
            if(len_trim(Result_String) <string_length) then
                Input_Instruction(Value_Before_Start_Index:Value_After_End_Index) = Result_String
                !Instructions_before_Upper(I_INFO)(Value_Before_Start_Index:Value_After_End_Index) = Result_String
            !������ɵĽ���ַ������ȴ��ڴ��滻��ԭ���ȣ�����Ҫƽ�ƺ�����ַ���.
            else
                num_added_char = len_trim(Result_String) - string_length
                Tem_String = Input_Instruction
                Input_Instruction(Value_Before_Start_Index:Value_After_End_Index+num_added_char) = Result_String
                Input_Instruction(Value_After_End_Index+num_added_char+1:) = Tem_String(Value_After_End_Index+1:)
                !Tem_String = Instructions_before_Upper(I_INFO)
                !Instructions_before_Upper(I_INFO)(Value_Before_Start_Index:Value_After_End_Index+num_added_char) = Result_String
                !Instructions_before_Upper(I_INFO)(Value_After_End_Index+num_added_char+1:) = &
                !                                                                Tem_String(Value_After_End_Index+1:)
            endif

            !ɾ��ȫ���ո�.
            call Tool_chrpak_s_blank_delete(Input_Instruction)  !Use chrpak to delete all spaces.
            !call Tool_chrpak_s_blank_delete(Instructions_before_Upper(I_INFO))  !Use chrpak to delete all spaces.

!                if(logical_plus) then
!                    print *,"INPUT_INFO(I_INFO)_�滻+��:",Instructions(I_INFO)
!                elseif(logical_minus)then
!                    print *,"INPUT_INFO(I_INFO)_�滻-��:",Instructions(I_INFO)
!                endif

        endif
    endif
enddo

Instruction = Input_Instruction

return 
end SUBROUTINE Tool_String_arithmetic                       
