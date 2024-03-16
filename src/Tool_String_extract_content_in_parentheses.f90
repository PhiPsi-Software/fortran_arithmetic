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
 
recursive subroutine Tool_String_extract_content_in_parentheses(Input_String,Output_String,left_index,right_index,found) 
!字符串中提取括号内容.
!递归子程序.
!2024-03-15.

use Global_Float_Type
use Global_Elem_Area_Vol
use Global_Common

!以下代码由ChatGPT 3.5生成.

implicit none  

character(len=*), intent(in) :: input_string
character(len=*), intent(out) :: output_string
integer, intent(out) :: left_index, right_index
logical, intent(out) :: found

integer :: i, len_input, stack_count
character(len=1) :: current_char
character(len=256) :: stack
character(len=256) :: Temp_string,Temp_output_string
integer Temp_left_index,Temp_right_index
logical Temp_found

len_input = len_trim(input_string)
stack = ''
stack_count = 0
found = .false.

do i = 1, len_input
    current_char = input_string(i:i)
    if (current_char == '(') then
        if (stack_count == 0) then
            left_index = i
        endif
        stack = trim(stack) // '('
        stack_count = stack_count + 1
    elseif (current_char == ')') then
        if (stack_count > 0) then
            stack_count = stack_count - 1
            if (stack_count == 0) then
                stack = adjustl(stack)
                right_index = i
                found = .true.
                exit
            else
                stack = trim(stack) // ')'
            endif
        endif
    elseif (stack_count > 0) then
        stack = trim(stack) // current_char
    endif
end do

if (found) then
    output_string = input_string(left_index + 1:right_index - 1)
    !递归. Written by Fang Shi.
    Temp_string(1:len_trim(output_string)) = output_string(1:len_trim(output_string))
    call Tool_String_extract_content_in_parentheses(Temp_string,Temp_Output_String,Temp_left_index,Temp_right_index,Temp_found) 
    if(Temp_found) then
        left_index  = left_index  + Temp_left_index
        right_index = left_index + (Temp_right_index-Temp_left_index)
        found = Temp_found
        !output_string(1:len_trim(Temp_Output_String)) = Temp_Output_String(1:len_trim(Temp_Output_String))
        !output_string(1:len_trim(Temp_Output_String)) = Temp_Output_String(1:len_trim(Temp_Output_String))
        output_string = input_string(left_index + 1:right_index - 1)
    endif
    
else
    output_string = ''
endif


return 
end SUBROUTINE Tool_String_extract_content_in_parentheses                   
