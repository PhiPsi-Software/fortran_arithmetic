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
 
      subroutine Tool_Get_num_Data_from_a_String(c_String,num_Char,
     &                                           num_Data,Yes_Even)
       
      !由逗号隔开的字符串数据中数据的个数,"num_Data = 逗号数加1"
      use Global_Float_Type
      IMPLICIT NONE
      integer,intent(in)::num_Char
      character(len=num_Char),intent(in)::c_String
      integer,intent(out)::num_Data
      logical,intent(out)::Yes_Even    !检查num_Data是否是偶数
      integer i_Char,num_Comma
      
      num_Comma = 0
      do i_Char = 1,num_Char
          if(c_String(i_Char:i_Char).eq.',')then
              num_Comma = num_Comma +1
          endif
      enddo
      num_Data = num_Comma + 1
      
      Yes_Even = .False.
      if(mod(num_Data,1)==0) Yes_Even = .True.
      
      RETURN
      end subroutine Tool_Get_num_Data_from_a_String