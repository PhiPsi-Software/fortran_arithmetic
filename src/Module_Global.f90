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

module Global_Float_Type
  implicit none
  save
  !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  !The following is single precision:
  !---------------------------
  !单精度用于计算水力压裂问题时所得结果极不准确
  !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  !integer,PARAMETER::FT = 4                           !single=4;double=8.
  !REAL(kind=FT)::ZR   = 0.0e0
  !REAL(kind=FT)::pi   = 3.1415926e0  !PI
  !REAL(kind=FT)::Cn_H = 1.0e2
  !REAL(kind=FT)::Cn_K = 1.0e3
  !REAL(kind=FT)::Cn_M = 1.0e6
  !REAL(kind=FT)::Cn_G = 1.0e9
  !REAL(kind=FT)::HLF  = 0.5e0
  !REAL(kind=FT)::ONE  = 1.0e0
  !REAL(kind=FT)::TWO  = 2.0e0
  !REAL(kind=FT)::THR  = 3.0e0
  !REAL(kind=FT)::FOU  = 4.0e0
  !REAL(kind=FT)::FIV  = 5.0e0
  !REAL(kind=FT)::SIX  = 6.0e0
  !REAL(kind=FT)::SEV  = 7.0e0
  !REAL(kind=FT)::EIG  = 8.0e0
  !REAL(kind=FT)::NIN  = 9.0e0
  !REAL(kind=FT)::TEN  = 1.0e1
  !REAL(kind=FT)::ZP1  = 0.1e0
  !REAL(kind=FT)::ZP2  = 0.2e0
  !REAL(kind=FT)::ZP3  = 0.3e0
  !REAL(kind=FT)::ZP4  = 0.4e0
  !REAL(kind=FT)::ZP5  = 0.5e0
  !REAL(kind=FT)::ZP6  = 0.6e0
  !REAL(kind=FT)::ZP7  = 0.7e0
  !REAL(kind=FT)::ZP8  = 0.8e0
  !REAL(kind=FT)::ZP9  = 0.9e0
  !REAL(kind=FT)::ZPZ1 = 0.01e0
  !REAL(kind=FT)::ZPZZ1= 0.001e0
  !REAL(kind=FT)::ZPZZZ1= 0.0001e0
  !REAL(kind=FT)::ZP25 = 0.25e0
  !REAL(kind=FT)::ONEP5= 1.5e0
  !其他
  !REAL(kind=FT)::Time_year  = 365.0e0*30.416e0*24.0e0*3600.0e0
  !REAL(kind=FT)::Time_month = 30.416e0*24.0e0*3600.0e0
  !REAL(kind=FT)::Time_week  = 7.0e0*24.0e0*3600.0e0
  !REAL(kind=FT)::Time_day   = 24.0e0*3600.0e0
  !REAL(kind=FT)::Time_hour  = 3600.0e0
  !REAL(kind=FT)::Time_min   = 60.0e0
  !REAL(kind=FT)::Tol_4      = 1.0e-4
  !REAL(kind=FT)::Tol_5      = 1.0e-5
  !REAL(kind=FT)::Tol_6      = 1.0e-6
  !REAL(kind=FT)::Tol_7      = 1.0e-7
  !REAL(kind=FT)::Tol_8      = 1.0e-8
  !REAL(kind=FT)::Tol_9      = 1.0e-9
  !REAL(kind=FT)::Tol_10     = 1.0e-10
  !REAL(kind=FT)::Tol_12     = 1.0e-12
  !REAL(kind=FT)::Tol_13     = 1.0e-13
  !REAL(kind=FT)::Tol_15     = 1.0e-15
  !REAL(kind=FT)::Tol_20     = 1.0e-20
  !REAL(kind=FT)::Tol_25     = 1.0e-25
  !REAL(kind=FT)::Tol_30     = 1.0e-30
  !REAL(kind=FT)::Tol_35     = 1.0e-35
  !REAL(kind=FT)::Tol_40     = 1.0e-40
  !REAL(kind=FT)::Con_30     = 30.0e0
  !REAL(kind=FT)::Con_90     = 90.0e0
  !REAL(kind=FT)::Con_180    = 180.0e0
  !REAL(kind=FT)::Con_240    = 240.0e0
  !REAL(kind=FT)::Con_270    = 270.0e0
  !REAL(kind=FT)::Con_360    = 360.0e0
  !REAL(kind=FT)::Con_Big_15 = 1.0e15
  !REAL(kind=FT)::Con_Big_20 = 1.0e20
  !REAL(kind=FT)::TEN_2      = 1.0e2
  !REAL(kind=FT)::TEN_3      = 1.0e3
  !REAL(kind=FT)::TEN_4      = 1.0e4
  !REAL(kind=FT)::TEN_5      = 1.0e5
  !REAL(kind=FT)::TEN_6      = 1.0e6
  !REAL(kind=FT)::TEN_7      = 1.0e7
  !REAL(kind=FT)::TEN_8      = 1.0e8
  !REAL(kind=FT)::TEN_9      = 1.0e9
  !REAL(kind=FT)::TEN_10     = 1.0e10
  !REAL(kind=FT)::TEN_11     = 1.0e11
  !REAL(kind=FT)::TEN_12     = 1.0e12
  !REAL(kind=FT)::TEN_13     = 1.0e13
  !REAL(kind=FT)::TEN_14     = 1.0e14
  !REAL(kind=FT)::TEN_15     = 1.0e15
  !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  !The following is double precision:
  !------------------------------
  !注意:不要忘记修改Tool_Function_GAMMA.f中的DGAMMA函数!!!!!!
  !     后来证明可以不修改,2019-04-27
  !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  integer,PARAMETER::FT = 8                           !single=4;double=8.
  REAL(kind=FT)::ZR   = 0.0D0
  REAL(kind=FT)::pi   = 3.14159265358979323846264D0  !PI
  REAL(kind=FT)::Cn_H = 1.0D2
  REAL(kind=FT)::Cn_K = 1.0D3
  REAL(kind=FT)::Cn_M = 1.0D6
  REAL(kind=FT)::Cn_G = 1.0D9
  REAL(kind=FT)::HLF  = 0.5D0
  REAL(kind=FT)::ONE  = 1.0D0
  REAL(kind=FT)::TWO  = 2.0D0
  REAL(kind=FT)::THR  = 3.0D0
  REAL(kind=FT)::FOU  = 4.0D0
  REAL(kind=FT)::FIV  = 5.0D0
  REAL(kind=FT)::SIX  = 6.0D0
  REAL(kind=FT)::SEV  = 7.0D0
  REAL(kind=FT)::EIG  = 8.0D0
  REAL(kind=FT)::NIN  = 9.0D0
  REAL(kind=FT)::TEN  = 1.0D1
  REAL(kind=FT)::ZP1  = 0.1D0
  REAL(kind=FT)::ZP2  = 0.2D0
  REAL(kind=FT)::ZP3  = 0.3D0
  REAL(kind=FT)::ZP4  = 0.4D0
  REAL(kind=FT)::ZP5  = 0.5D0
  REAL(kind=FT)::ZP6  = 0.6D0
  REAL(kind=FT)::ZP7  = 0.7D0
  REAL(kind=FT)::ZP8  = 0.8D0
  REAL(kind=FT)::ZP9  = 0.9D0
  REAL(kind=FT)::ZPZ1 = 0.01D0
  REAL(kind=FT)::ZPZZ1= 0.001D0
  REAL(kind=FT)::ZPZZZ1= 0.0001D0
  REAL(kind=FT)::ZP25 = 0.25D0
  REAL(kind=FT)::ONEP5= 1.5D0
  !其他
  REAL(kind=FT)::Time_year  = 365.0D0*30.416D0*24.0D0*3600.0D0
  REAL(kind=FT)::Time_month = 30.416D0*24.0D0*3600.0D0
  REAL(kind=FT)::Time_week  = 7.0D0*24.0D0*3600.0D0
  REAL(kind=FT)::Time_day   = 24.0D0*3600.0D0
  REAL(kind=FT)::Time_hour  = 3600.0D0
  REAL(kind=FT)::Time_min   = 60.0D0
  REAL(kind=FT)::Tol_2      = 1.0D-2
  REAL(kind=FT)::Tol_3      = 1.0D-3
  REAL(kind=FT)::Tol_4      = 1.0D-4
  REAL(kind=FT)::Tol_5      = 1.0D-5
  REAL(kind=FT)::Tol_6      = 1.0D-6
  REAL(kind=FT)::Tol_7      = 1.0D-7
  REAL(kind=FT)::Tol_8      = 1.0D-8
  REAL(kind=FT)::Tol_9      = 1.0D-9
  REAL(kind=FT)::Tol_10     = 1.0D-10
  REAL(kind=FT)::Tol_11     = 1.0D-11
  REAL(kind=FT)::Tol_12     = 1.0D-12
  REAL(kind=FT)::Tol_13     = 1.0D-13
  REAL(kind=FT)::Tol_15     = 1.0D-15
  REAL(kind=FT)::Tol_20     = 1.0D-20
  REAL(kind=FT)::Tol_25     = 1.0D-25
  REAL(kind=FT)::Tol_30     = 1.0D-30
  REAL(kind=FT)::Tol_35     = 1.0D-35
  REAL(kind=FT)::Tol_40     = 1.0D-40
  REAL(kind=FT)::Con_30     = 30.0D0
  REAL(kind=FT)::Con_60     = 60.0D0
  REAL(kind=FT)::Con_90     = 90.0D0
  REAL(kind=FT)::Con_180    = 180.0D0
  REAL(kind=FT)::Con_240    = 240.0D0
  REAL(kind=FT)::Con_270    = 270.0D0
  REAL(kind=FT)::Con_360    = 360.0D0
  REAL(kind=FT)::Con_Big_15 = 1.0D15
  REAL(kind=FT)::Con_Big_20 = 1.0D20
  REAL(kind=FT)::TEN_2      = 1.0D2
  REAL(kind=FT)::TEN_3      = 1.0D3
  REAL(kind=FT)::TEN_4      = 1.0D4
  REAL(kind=FT)::TEN_5      = 1.0D5
  REAL(kind=FT)::TEN_6      = 1.0D6
  REAL(kind=FT)::TEN_7      = 1.0D7
  REAL(kind=FT)::TEN_8      = 1.0D8
  REAL(kind=FT)::TEN_9      = 1.0D9
  REAL(kind=FT)::TEN_10     = 1.0D10
  REAL(kind=FT)::TEN_11     = 1.0D11
  REAL(kind=FT)::TEN_12     = 1.0D12
  REAL(kind=FT)::TEN_13     = 1.0D13
  REAL(kind=FT)::TEN_14     = 1.0D14
  REAL(kind=FT)::TEN_15     = 1.0D15
end module Global_Float_Type

!------------------------------------------
!   01. 参差数组类(Real). 2022-09-02.
!------------------------------------------
!Ref: https://en.wikipedia.org/wiki/Jagged_array
!Ref: https://stackoverflow.com/questions/18316592/multidimensional-array-with-different-lengths
!Ref: https://stackoverflow.com/questions/14857366/are-there-any-problems-with-using-jagged-arrays-in-fortran-with-multiple-levels
module Global_Ragged_Array_Real_Classs
  use Global_Float_Type
  implicit none
  save
  !private
  !public::Ragged_Array_1D,Ragged_Array_2D,Ragged_Array_3D,Ragged_Array_4D

  !//////////////
  !  1D Array
  !//////////////
  type :: Ragged_Array_1D
      real(kind=FT), allocatable :: row(:)
  end type Ragged_Array_1D

  !Usage:
  !type(Ragged_Array_1D) :: Ragged(3)
  !Ragged(1)%row = [1]
  !Ragged(2)%row = [1,2]
  !Ragged(3)%row = [1,2,3]

  !or

  !type(ragged_array),Ragged(:)
  !allocate(Ragged_Array_2D_1(3))
  !allocate(Ragged_Array_2(1)%row(1))
  !allocate(Ragged_Array_2(2)%row(2))
  !allocate(Ragged_Array_2(3)%row(3))
  !Ragged_Array_2(1)%row(:) = 1.0D0
  !Ragged_Array_2(2)%row(:) = 2.0D0
  !Ragged_Array_2(3)%row(:) = 3.0D0

  !//////////////
  !  2D Array
  !//////////////
  type :: Ragged_Array_2D
      real(kind=FT), allocatable :: row(:,:)
  end type Ragged_Array_2D

  !Usage:
  !allocate(Ragged_Array_2D_1(3))
  !allocate(Ragged_Array_2D_1(1)%row(1,5))
  !allocate(Ragged_Array_2D_1(2)%row(2,5))
  !allocate(Ragged_Array_2D_1(3)%row(3,5))
  !Ragged_Array_2D_1(1)%row(:,:) = 1.0D0
  !Ragged_Array_2D_1(2)%row(:,:) = 2.0D0
  !Ragged_Array_2D_1(3)%row(:,:) = 3.0D0
  !print *,'Ragged_Array_2D_1(1)%row(1,1:5):',Ragged_Array_2D_1(1)%row(1,1:5)
  !print *,'Ragged_Array_2D_1(2)%row(1,1:5):',Ragged_Array_2D_1(2)%row(1,1:5)
  !print *,'Ragged_Array_2D_1(3)%row(1,1:5):',Ragged_Array_2D_1(3)%row(1,1:5)

  !//////////////
  !  3D Array
  !//////////////
  type :: Ragged_Array_3D
      real(kind=FT), allocatable :: row(:,:,:)
  end type Ragged_Array_3D

  !//////////////
  !  4D Array
  !//////////////
  type :: Ragged_Array_4D
      real(kind=FT), allocatable :: row(:,:,:,:)
  end type Ragged_Array_4D


end module Global_Ragged_Array_Real_Classs

!------------------------------------------
!   02. 参差数组类(Int). 2022-09-03.
!------------------------------------------
!Ref: https://en.wikipedia.org/wiki/Jagged_array
!Ref: https://stackoverflow.com/questions/18316592/multidimensional-array-with-different-lengths
!Ref: https://stackoverflow.com/questions/14857366/are-there-any-problems-with-using-jagged-arrays-in-fortran-with-multiple-levels
module Global_Ragged_Array_Int_Classs
  !use Global_Float_Type
  implicit none
  save
  !private
  !public::Ragged_Int_Array_1D,Ragged_Int_Array_2D,Ragged_Int_Array_3D,Ragged_Int_Array_4D

  !//////////////
  !  1D Array
  !//////////////
  type :: Ragged_Int_Array_1D
      integer, allocatable :: row(:)
  end type Ragged_Int_Array_1D

  !//////////////
  !  2D Array
  !//////////////
  type :: Ragged_Int_Array_2D
      integer, allocatable :: row(:,:)
  end type Ragged_Int_Array_2D


  !//////////////
  !  3D Array
  !//////////////
  type :: Ragged_Int_Array_3D
      integer, allocatable :: row(:,:,:)
  end type Ragged_Int_Array_3D

  !//////////////
  !  4D Array
  !//////////////
  type :: Ragged_Int_Array_4D
      integer, allocatable :: row(:,:,:,:)
  end type Ragged_Int_Array_4D


end module Global_Ragged_Array_Int_Classs

!------------------------------------------
!   03.1 稀疏矩阵模块. 1D整型. 2022-09-14.
!------------------------------------------
!module sparse_matrix_1DInt
!    !Ref: https://stackoverflow.com/questions/62255387/sparse-array-in-fortran
!    !Rewritten by Fang Shi. HYIT. 2022-09-14.
!    implicit none
!
!    private
!    public :: SparseElement_1DInt
!    public :: Sparse_1DInt
!
!
!    type SparseElement_1DInt
!        integer  :: irow
!        integer :: val
!    end type
!
!    type Sparse_1DInt
!        ! Only the first no_elements elements will be in use.
!        integer                          :: no_elements
!        type(SparseElement_1DInt), allocatable :: elements_(:)
!    contains
!        procedure, public :: elements
!        procedure, public :: add_element
!    end type
!
!    interface Sparse_1DInt
!      module procedure new_Sparse
!    end interface
!
!    contains
!
!    !//////////////////////////////////////////////////////////////////////////
!    !function new_Sparse: Initialise the Sparse_1DInt matrix to an empty matrix.
!    !//////////////////////////////////////////////////////////////////////////
!    function new_Sparse() result(this)
!        implicit none
!
!        type(Sparse_1DInt) :: this
!
!        this%no_elements = 0
!        allocate(this%elements_(0))
!    end function
!
!    !/////////////////////////////////////////////
!    ! Return only the elements which are in use.
!    !/////////////////////////////////////////////
!    function elements(this) result(output)
!        implicit none
!
!        class(Sparse_1DInt), intent(in)        :: this
!        type(SparseElement_1DInt), allocatable :: output(:)
!
!        output = this%elements_(:this%no_elements)
!    end function
!
!    !/////////////////////////////////////////////////////////////////////////
!    ! Add an element to the array, automatically allocating memory if needed.
!    !/////////////////////////////////////////////////////////////////////////
!    subroutine add_element(this,element)
!        implicit none
!
!        class(Sparse_1DInt),       intent(inout) :: this
!        type(SparseElement_1DInt), intent(in)    :: element
!
!        type(SparseElement_1DInt), allocatable :: temp(:)
!
!        this%no_elements = this%no_elements+1
!
!        ! This is where memory is added.
!        ! If this%elements_ would overflow, its length is doubled.
!        if (this%no_elements>size(this%elements_)) then
!            allocate(temp(2*this%no_elements))
!            print *,'size of temp:',size(temp)
!            temp(:this%no_elements-1) = this%elements_
!            this%elements_ = temp
!        endif
!
!        this%elements_(this%no_elements) = element
!    end subroutine
!
!end module sparse_matrix_1DInt

!!------------------------------------------
!!   03.2 稀疏矩阵模块. 2D整型. 2022-09-14.
!!------------------------------------------
!module sparse_matrix_2DInt
!    !Ref: https://stackoverflow.com/questions/62255387/sparse-array-in-fortran
!    implicit none
!
!    private
!    public :: SparseElement_2DInt
!    public :: Sparse_2DInt
!
!
!    type SparseElement_2DInt
!        integer  :: irow
!        integer  :: icol
!        integer :: val
!    end type
!
!    type Sparse_2DInt
!        ! Only the first no_elements elements will be in use.
!        integer                          :: no_elements
!        type(SparseElement_2DInt), allocatable :: elements_(:)
!    contains
!        procedure, public :: elements
!        procedure, public :: add_element
!    end type
!
!    interface Sparse_2DInt
!      module procedure new_Sparse
!    end interface
!
!    contains
!
!    !//////////////////////////////////////////////////////////////////////////
!    !function new_Sparse: Initialise the Sparse_2DInt matrix to an empty matrix.
!    !//////////////////////////////////////////////////////////////////////////
!    function new_Sparse() result(this)
!        implicit none
!
!        type(Sparse_2DInt) :: this
!
!        this%no_elements = 0
!        allocate(this%elements_(0))
!    end function
!
!    !/////////////////////////////////////////////
!    ! Return only the elements which are in use.
!    !/////////////////////////////////////////////
!    function elements(this) result(output)
!        implicit none
!
!        class(Sparse_2DInt), intent(in)        :: this
!        type(SparseElement_2DInt), allocatable :: output(:)
!
!        output = this%elements_(:this%no_elements)
!    end function
!
!    !/////////////////////////////////////////////////////////////////////////
!    ! Add an element to the array, automatically allocating memory if needed.
!    !/////////////////////////////////////////////////////////////////////////
!    subroutine add_element(this,element)
!        implicit none
!
!        class(Sparse_2DInt),       intent(inout) :: this
!        type(SparseElement_2DInt), intent(in)    :: element
!
!        type(SparseElement_2DInt), allocatable :: temp(:)
!
!        this%no_elements = this%no_elements+1
!
!        ! This is where memory is added.
!        ! If this%elements_ would overflow, its length is doubled.
!        if (this%no_elements>size(this%elements_)) then
!            allocate(temp(2*this%no_elements))
!            !print *,'size of temp:',size(temp)
!            temp(:this%no_elements-1) = this%elements_
!            this%elements_ = temp
!        endif
!
!        this%elements_(this%no_elements) = element
!    end subroutine
!
!end module sparse_matrix_2DInt

!!------------------------------------------
!!   03.3 稀疏矩阵模块. 3D整型. 2022-09-14.
!!------------------------------------------
!module sparse_matrix_3DInt
!    !Ref: https://stackoverflow.com/questions/62255387/sparse-array-in-fortran
!    !Rewritten by Fang Shi. HYIT. 2022-09-14.
!    implicit none
!
!    private
!    public :: SparseElement_3DInt
!    public :: Sparse_3DInt
!
!    type SparseElement_3DInt
!        integer  :: irow
!        integer  :: icol
!        integer  :: ilay  !ilayer
!        integer :: val
!    end type
!
!    type Sparse_3DInt
!        ! Only the first no_elements elements will be in use.
!        integer                          :: no_elements
!        type(SparseElement_3DInt), allocatable :: elements_(:)
!    contains
!        procedure, public :: elements
!        procedure, public :: add_element
!    end type
!
!    interface Sparse_3DInt
!      module procedure new_Sparse
!    end interface
!
!    contains
!
!    ! Initialise the Sparse_2DInt matrix to an empty matrix.
!    function new_Sparse() result(this)
!    implicit none
!
!    type(Sparse_3DInt) :: this
!
!    this%no_elements = 0
!    allocate(this%elements_(0))
!    end function
!
!    ! Return only the elements which are in use.
!    function elements(this) result(output)
!    implicit none
!
!    class(Sparse_3DInt), intent(in)        :: this
!    type(SparseElement_3DInt), allocatable :: output(:)
!
!    output = this%elements_(:this%no_elements)
!    end function
!
!    ! Add an element to the array, automatically allocating memory if needed.
!    subroutine add_element(this,element)
!    implicit none
!
!    class(Sparse_3DInt),       intent(inout) :: this
!    type(SparseElement_3DInt), intent(in)    :: element
!
!    type(SparseElement_3DInt), allocatable :: temp(:)
!
!    this%no_elements = this%no_elements+1
!
!    ! This is where memory is added.
!    ! If this%elements_ would overflow, its length is doubled.
!    if (this%no_elements>size(this%elements_)) then
!        allocate(temp(2*this%no_elements))
!        temp(:this%no_elements-1) = this%elements_
!        this%elements_ = temp
!    endif
!
!    this%elements_(this%no_elements) = element
!    end subroutine
!end module sparse_matrix_3DInt

!!------------------------------------------
!!   03.4 稀疏矩阵模块. 4D整型. 2022-09-14.
!!------------------------------------------
!module sparse_matrix_4DInt
!    !Ref: https://stackoverflow.com/questions/62255387/sparse-array-in-fortran
!    !Rewritten by Fang Shi. HYIT. 2022-09-14.
!    implicit none
!
!    private
!    public :: SparseElement_4DInt
!    public :: Sparse_4DInt
!
!    type SparseElement_4DInt
!        integer  :: irow
!        integer  :: icol
!        integer  :: ilay  !ilayer
!        integer  :: iflo  !ifloor
!        integer :: val
!    end type
!
!    type Sparse_4DInt
!        ! Only the first no_elements elements will be in use.
!        integer                          :: no_elements
!        type(SparseElement_4DInt), allocatable :: elements_(:)
!    contains
!        procedure, public :: elements
!        procedure, public :: add_element
!    end type
!
!    interface Sparse_4DInt
!      module procedure new_Sparse
!    end interface
!
!    contains
!
!    ! Initialise the Sparse_2DInt matrix to an empty matrix.
!    function new_Sparse() result(this)
!    implicit none
!
!    type(Sparse_4DInt) :: this
!
!    this%no_elements = 0
!    allocate(this%elements_(0))
!    end function
!
!    ! Return only the elements which are in use.
!    function elements(this) result(output)
!    implicit none
!
!    class(Sparse_4DInt), intent(in)        :: this
!    type(SparseElement_4DInt), allocatable :: output(:)
!
!    output = this%elements_(:this%no_elements)
!    end function
!
!    ! Add an element to the array, automatically allocating memory if needed.
!    subroutine add_element(this,element)
!    implicit none
!
!    class(Sparse_4DInt),       intent(inout) :: this
!    type(SparseElement_4DInt), intent(in)    :: element
!
!    type(SparseElement_4DInt), allocatable :: temp(:)
!
!    this%no_elements = this%no_elements+1
!
!    ! This is where memory is added.
!    ! If this%elements_ would overflow, its length is doubled.
!    if (this%no_elements>size(this%elements_)) then
!        allocate(temp(2*this%no_elements))
!        temp(:this%no_elements-1) = this%elements_
!        this%elements_ = temp
!    endif
!
!    this%elements_(this%no_elements) = element
!    end subroutine
!end module sparse_matrix_4DInt

!!------------------------------------------
!!  03.5 稀疏矩阵模块. 1D Real. 2022-09-14.
!!------------------------------------------
!module sparse_matrix_1DReal
!    !Ref: https://stackoverflow.com/questions/62255387/sparse-array-in-fortran
!    !Rewritten by Fang Shi. HYIT. 2022-09-14.
!    use Global_Float_Type
!    implicit none
!
!    private
!    public :: SparseElement_1DReal
!    public :: Sparse_1DReal
!
!
!    type SparseElement_1DReal
!        integer  :: irow
!        !real(kind=8) :: val
!        real(kind=FT) :: val
!    end type
!
!    type Sparse_1DReal
!        ! Only the first no_elements elements will be in use.
!        integer                          :: no_elements
!        type(SparseElement_1DReal), allocatable :: elements_(:)
!    contains
!        procedure, public :: elements
!        procedure, public :: add_element
!    end type
!
!    interface Sparse_1DReal
!      module procedure new_Sparse
!    end interface
!
!    contains
!
!    !//////////////////////////////////////////////////////////////////////////
!    !function new_Sparse: Initialise the Sparse_1DReal matrix to an empty matrix.
!    !//////////////////////////////////////////////////////////////////////////
!    function new_Sparse() result(this)
!        implicit none
!
!        type(Sparse_1DReal) :: this
!
!        this%no_elements = 0
!        allocate(this%elements_(0))
!    end function
!
!    !/////////////////////////////////////////////
!    ! Return only the elements which are in use.
!    !/////////////////////////////////////////////
!    function elements(this) result(output)
!        implicit none
!
!        class(Sparse_1DReal), intent(in)        :: this
!        type(SparseElement_1DReal), allocatable :: output(:)
!
!        output = this%elements_(:this%no_elements)
!    end function
!
!    !/////////////////////////////////////////////////////////////////////////
!    ! Add an element to the array, automatically allocating memory if needed.
!    !/////////////////////////////////////////////////////////////////////////
!    subroutine add_element(this,element)
!        implicit none
!
!        class(Sparse_1DReal),       intent(inout) :: this
!        type(SparseElement_1DReal), intent(in)    :: element
!
!        type(SparseElement_1DReal), allocatable :: temp(:)
!
!        this%no_elements = this%no_elements+1
!
!        ! This is where memory is added.
!        ! If this%elements_ would overflow, its length is doubled.
!        if (this%no_elements>size(this%elements_)) then
!            allocate(temp(2*this%no_elements))
!            print *,'size of temp:',size(temp)
!            temp(:this%no_elements-1) = this%elements_
!            this%elements_ = temp
!        endif
!
!        this%elements_(this%no_elements) = element
!    end subroutine
!end module sparse_matrix_1DReal
!
!!------------------------------------------
!!  03.6 稀疏矩阵模块. 2D Real. 2022-09-14.
!!------------------------------------------
!module sparse_matrix_2DReal
!    !Ref: https://stackoverflow.com/questions/62255387/sparse-array-in-fortran
!    use Global_Float_Type
!    implicit none
!
!    private
!    public :: SparseElement_2DReal
!    public :: Sparse_2DReal
!
!
!    type SparseElement_2DReal
!        integer  :: irow
!        integer  :: icol
!        !real(kind=8) :: val
!        real(kind=FT) :: val
!    end type
!
!    type Sparse_2DReal
!        ! Only the first no_elements elements will be in use.
!        integer                          :: no_elements
!        type(SparseElement_2DReal), allocatable :: elements_(:)
!    contains
!        procedure, public :: elements
!        procedure, public :: add_element
!    end type
!
!    interface Sparse_2DReal
!      module procedure new_Sparse
!    end interface
!
!    contains
!
!    !//////////////////////////////////////////////////////////////////////////
!    !function new_Sparse: Initialise the Sparse_2DReal matrix to an empty matrix.
!    !//////////////////////////////////////////////////////////////////////////
!    function new_Sparse() result(this)
!        implicit none
!
!        type(Sparse_2DReal) :: this
!
!        this%no_elements = 0
!        allocate(this%elements_(0))
!    end function
!
!    !/////////////////////////////////////////////
!    ! Return only the elements which are in use.
!    !/////////////////////////////////////////////
!    function elements(this) result(output)
!        implicit none
!
!        class(Sparse_2DReal), intent(in)        :: this
!        type(SparseElement_2DReal), allocatable :: output(:)
!
!        output = this%elements_(:this%no_elements)
!    end function
!
!    !/////////////////////////////////////////////////////////////////////////
!    ! Add an element to the array, automatically allocating memory if needed.
!    !/////////////////////////////////////////////////////////////////////////
!    subroutine add_element(this,element)
!        implicit none
!
!        class(Sparse_2DReal),       intent(inout) :: this
!        type(SparseElement_2DReal), intent(in)    :: element
!
!        type(SparseElement_2DReal), allocatable :: temp(:)
!
!        this%no_elements = this%no_elements+1
!
!        ! This is where memory is added.
!        ! If this%elements_ would overflow, its length is doubled.
!        if (this%no_elements>size(this%elements_)) then
!            allocate(temp(2*this%no_elements))
!            !print *,'size of temp:',size(temp)
!            temp(:this%no_elements-1) = this%elements_
!            this%elements_ = temp
!        endif
!
!        this%elements_(this%no_elements) = element
!    end subroutine
!
!end module sparse_matrix_2DReal
!
!!------------------------------------------
!!  03.7 稀疏矩阵模块. 3D Real. 2022-09-14.
!!------------------------------------------
!module sparse_matrix_3DReal
!    !Ref: https://stackoverflow.com/questions/62255387/sparse-array-in-fortran
!    !Rewritten by Fang Shi. HYIT. 2022-09-14.
!    use Global_Float_Type
!    implicit none
!
!    private
!    public :: SparseElement_3DReal
!    public :: Sparse_3DReal
!
!    type SparseElement_3DReal
!        integer  :: irow
!        integer  :: icol
!        integer  :: ilay  !ilayer
!        !real(kind=8) :: val
!        real(kind=FT) :: val
!    end type
!
!    type Sparse_3DReal
!        ! Only the first no_elements elements will be in use.
!        integer                          :: no_elements
!        type(SparseElement_3DReal), allocatable :: elements_(:)
!    contains
!        procedure, public :: elements
!        procedure, public :: add_element
!    end type
!
!    interface Sparse_3DReal
!      module procedure new_Sparse
!    end interface
!
!    contains
!
!    ! Initialise the Sparse_2DReal matrix to an empty matrix.
!    function new_Sparse() result(this)
!    implicit none
!
!    type(Sparse_3DReal) :: this
!
!    this%no_elements = 0
!    allocate(this%elements_(0))
!    end function
!
!    ! Return only the elements which are in use.
!    function elements(this) result(output)
!    implicit none
!
!    class(Sparse_3DReal), intent(in)        :: this
!    type(SparseElement_3DReal), allocatable :: output(:)
!
!    output = this%elements_(:this%no_elements)
!    end function
!
!    ! Add an element to the array, automatically allocating memory if needed.
!    subroutine add_element(this,element)
!    implicit none
!
!    class(Sparse_3DReal),       intent(inout) :: this
!    type(SparseElement_3DReal), intent(in)    :: element
!
!    type(SparseElement_3DReal), allocatable :: temp(:)
!
!    this%no_elements = this%no_elements+1
!
!    ! This is where memory is added.
!    ! If this%elements_ would overflow, its length is doubled.
!    if (this%no_elements>size(this%elements_)) then
!        allocate(temp(2*this%no_elements))
!        temp(:this%no_elements-1) = this%elements_
!        this%elements_ = temp
!    endif
!
!    this%elements_(this%no_elements) = element
!    end subroutine
!end module sparse_matrix_3DReal
!
!!------------------------------------------
!!  03.8 稀疏矩阵模块. 4D Real. 2022-09-14.
!!------------------------------------------
!module sparse_matrix_4DReal
!    !Ref: https://stackoverflow.com/questions/62255387/sparse-array-in-fortran
!    !Rewritten by Fang Shi. HYIT. 2022-09-14.
!    use Global_Float_Type
!    implicit none
!
!    private
!    public :: SparseElement_4DReal
!    public :: Sparse_4DReal
!
!    type SparseElement_4DReal
!        integer  :: irow
!        integer  :: icol
!        integer  :: ilay  !ilayer
!        integer  :: iflo  !ifloor
!        !real(kind=8) :: val
!        real(kind=FT) :: val
!    end type
!
!    type Sparse_4DReal
!        ! Only the first no_elements elements will be in use.
!        integer                          :: no_elements
!        type(SparseElement_4DReal), allocatable :: elements_(:)
!    contains
!        procedure, public :: elements
!        procedure, public :: add_element
!    end type
!
!    interface Sparse_4DReal
!      module procedure new_Sparse
!    end interface
!
!    contains
!
!    ! Initialise the Sparse_2DReal matrix to an empty matrix.
!    function new_Sparse() result(this)
!    implicit none
!
!    type(Sparse_4DReal) :: this
!
!    this%no_elements = 0
!    allocate(this%elements_(0))
!    end function
!
!    ! Return only the elements which are in use.
!    function elements(this) result(output)
!    implicit none
!
!    class(Sparse_4DReal), intent(in)        :: this
!    type(SparseElement_4DReal), allocatable :: output(:)
!
!    output = this%elements_(:this%no_elements)
!    end function
!
!    ! Add an element to the array, automatically allocating memory if needed.
!    subroutine add_element(this,element)
!    implicit none
!
!    class(Sparse_4DReal),       intent(inout) :: this
!    type(SparseElement_4DReal), intent(in)    :: element
!
!    type(SparseElement_4DReal), allocatable :: temp(:)
!
!    this%no_elements = this%no_elements+1
!
!    ! This is where memory is added.
!    ! If this%elements_ would overflow, its length is doubled.
!    if (this%no_elements>size(this%elements_)) then
!        allocate(temp(2*this%no_elements))
!        temp(:this%no_elements-1) = this%elements_
!        this%elements_ = temp
!    endif
!
!    this%elements_(this%no_elements) = element
!    end subroutine
!end module sparse_matrix_4DReal

!----------------------------
!     1.通用全局变量
!----------------------------
module Global_Common
  use Global_Float_Type
  implicit none
  save
  character(120) :: PhiPsi_Version,PhiPsi_Release_Date,Compiler
  integer Operation_System_Type  !=1, Linux;    =2, MAC OS; = 3, Windows. 2023-08-10.
  integer Compiler_Type          !=1, gfortran; =2, ifort;  = 3, IFX.     2024-01-27.
  integer(8) S_time
  integer*8 Label_number
  integer*4 system_bit
  real(kind=FT) Delta_Factor_Aper,Delta_Factor_Junc,Delta_Factor_Edge
  real(kind=FT) Factor_Propagation                  !裂纹扩展步长系数.
  real(kind=FT) Propagation_Length                  !裂缝扩展长度,若给定该参数且>0,则Factor_Propagation不起作用,default to -99.0
  integer Key_Propa_Type                            !扩展类型:=1,固定步长扩展;=2,实际步长扩展(用于疲劳分析和动态分析,自动逻辑修正)
  integer Max_Step,Max_Itera
  integer max_nthreads                              !最大总的线程数目
  character(80) :: mac_Address
  character(80) :: CPU_ID,CPU_Type,DISK_ID,MAC_ID
  character(1) :: String_Connector  !"/" for Linux or "\" for windows.
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !cccccccccccccccccc         程序内部控制参数        ccccccccccccccccccccccc
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  parameter (Delta_Factor_Edge    = 0.001D0 )       !偏移量控制参数，边缘裂纹的偏置量,千分之一
  parameter (Delta_Factor_Aper    = 0.001D0 )       !偏移量控制参数，计算点的偏置,用于计算裂缝开度,万分之一
                                                    !0.001*单元平均长度
  parameter (Delta_Factor_Junc    = 0.001D0 )       !偏移量控制参数，Junction处的计算点需要偏移一点,否则计算出来的开度会混乱
                                                    !0.001*单元平均长度
  parameter (Max_Step        = 1000  )              !最大载荷步数, 默认:1000
  parameter (Max_Itera       = 10000 )              !最大迭代次数，10000
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !cccccccccccccccccc         程序内部控制参数        ccccccccccccccccccccccc
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  integer Key_Heaviside_Value
  integer Key_Cond_Number                           !计算并显示条件数,仅用于Lapack求解器,默认关闭
  integer Key_Determinant                           !计算并显示刚度矩阵K的行列式(不支持稀疏矩阵求解器，不支持EBE-PCG求解器),默认关闭
  integer Key_Eigenvalue                            !计算并显示刚度矩阵K的特征值(不支持稀疏矩阵求解器，不支持EBE-PCG求解器),默认关闭
  integer Key_BLAS                                  !使用BLAS库(对于Intel Fortran可自动并行),但对结果有影响. 默认为0. 使用BLAS的相关子程序:
                                                    !EBE_XFEM_PCG_3D_with_K.f90
  integer Key_Hole_Value
  real(kind=FT) Delta_Factor_SIFs
  integer Key_Tip_Pres_Zero                         !水力压裂分析是否设置裂尖水压为0
  integer Num_Gauss_Points,Num_Substeps,Key_Integral_Sol
  integer Num_Sub_Quads                             !子四边形剖分四边形数目(Key_Integral_Sol= 3时设置),默认16,不能取奇数
  integer Num_Sub_3D_Cubes                          !3D子立方体剖分分块数
  integer Num_Gau_Points_3D_Cube                    !3D子立方体剖分分块高斯积分点数目
  integer Num_Gau_Points_3D,Num_Gauss_P_FEM_3D
  integer Num_Gau_Points_3D_MC                      !含3个以上裂缝的3D增强单元的高斯积分点数: 1000=10^3(default);3375=15^3;5832=18^3;8000=20^3
  integer Num_Gauss_P_Inc
  integer Num_Gauss_P_FEM
  integer Num_Gau_P_SubInteg_6                      !3D分块积分6面体单元积分点数目. 2022-07-28.
  integer Num_Gau_P_SubInteg_4                      !3D分块积分4面体单元积分点数目. 2022-07-28.
  integer Key_Initiation,Key_Propagation,Key_Type_2D,CFCP
  integer Key_CFCP_3_Type                           !CFCP=3准则裂缝扩展行为,2022-07-02.
  integer Key_3D_Cr_Update_Scheme                   !3D裂缝面更新算法, 1: 不论是否扩展，都扩展一个步长，对于不扩展的裂尖，增加一个微小量.
                                                    !                  2: 非扩展裂尖不扩展，扩展很小步长的裂尖更新坐标(default).
  integer Key_Large_Deform                          !开启大变形, 仅用于Key_Analysis_Type = 8时
  integer Key_Ini_Rule                              !生成初始裂缝遵循的准则:=1,最大拉应力准则(默认)
  integer Key_Ini_Cr_3D_Type                        !生成初始裂缝的类型(仅在3D问题时需要):=1,圆形;=2,矩形
  real(kind=FT) Size_Ini_Crack                      !生成的初始裂缝的尺寸(对于3D圆形初始裂缝,表示直径;对于3D矩形初始裂缝,表示边长;对于2D裂缝,表示长度)
  integer Key_Schollmann_Twist
  real(kind=FT) Schollm_Max_Theta                   !允许的3D Schollmann’s criterion的最大Theta偏转角(单位为度,建议小于75,默认55). NEWFTU2022071001.
  integer Key_Adj_Prp_Step_3D                       !3D裂缝扩展控制参数(default:0); =1则根据应力或应力强度因子控制裂缝扩展步长的实际大小
  real(kind=FT) Adj_Prp_Step_3D_Max                 !3D裂缝扩展控制参数(default:3.0),Key_Adj_Prp_Step_3D = 1时起作用,3D裂缝扩展最大步长
  real(kind=FT) Prp_3D_Factor_m                     !delta_l  = delta_L_Max*(S1/St)^m公式中的系数m. 默认值为1/3即0.333333.
  real(kind=FT) Prp_Bisec_Factor_3D                 !3D裂缝扩展控制参数:边界剖分系数,若3D裂缝离散单元边界长度>Prp_Bisec_Factor_3D*Ave_Elem_L_Enrich,则二分
  integer Key_Smooth_Front                          !3D裂缝扩展控制参数:(default:0),>=1则对裂缝前缘进行光滑处理
  integer Key_Smooth_Front_Twice                    !=1时则执行两次Smooth操作，然后两次处理后的坐标取平均值
  !integer Key_Smooth_Pressure_Curve                 !压力曲线光滑处理,2次光滑处理. 2022-10-14.
  !integer Key_OpenMP                               !是否开启OpenMP
  integer Key_Num_Process                           !并行计算使用的核心数目(若取为99,则使用全部核心)

  integer Key_Ave_Stress                            !加权平均应力的计算方法
  real(kind=FT) S1_Point_Factor                     !加权应力计算点移动系数(偏离裂缝形心往外移动; defaut to 0.2)
  integer Key_Ave_Half_HF                           !对于水力压裂分析,取不包括水压作用的右边一半计算加权平均应力
  real(kind=FT) a_Ave_Shi                           !我提出的加权平均计算算法中的参数a
  real(kind=FT) Factor_Ave_R                        !加权平均主拉应力计算区域半径系数(0.1-1.5)
  integer num_Gauss_S1                              !加权平均主拉应力计算单元高斯点数目(default to 6^3=216)
  real(kind=FT) Prop_Angle_Alowed                   !允许的裂缝扩展偏转角(0到180之间的数)
  integer Key_HF_Del_Ne_Pres                        !水力压裂分析中负的水压转换成0
  real(kind=FT) Coff_a_AMPT
  integer,ALLOCATABLE::Material_Type(:)                         !材料类型
  real(kind=FT),ALLOCATABLE:: Material_Para(:,:)                !材料参数
  real(kind=FT),ALLOCATABLE:: Material_Para_Added(:,:)          !复合材料参数
  real(kind=FT),ALLOCATABLE:: Mat_Cartesian_Coor_Vector_x(:,:)  !复合材料正交材料坐标系x轴向量
  real(kind=FT),ALLOCATABLE:: Mat_Cartesian_Coor_Vector_y(:,:)  !复合材料正交材料坐标系y轴向量
  real(kind=FT),ALLOCATABLE:: Mat_cylinder_Coor_Center(:,:)     !复合材料圆柱材料坐标系原点
  real(kind=FT),ALLOCATABLE:: Mat_cylinder_Coor_Vector_z(:,:)   !复合材料圆柱材料坐标系z轴向量
  real(kind=FT) Desired_KIc                                     !水力压裂目标层的K_C,用于PhiPsi3D_Static_HF_SlipWater. 2023-02-12.

  real(kind=FT) Material_Interface(2)               !夹杂的界面材料参数:1-St,2-KIc
  integer Key_SIFs_Method,Key_Force_Control,Key_Analysis_Type
  integer Key_SIFs_DIM_Points                       !位移插值法计算应力强度因子的提取点的数目(1、2或3,默认为2)
  integer Key_SIFs_DIM_Method                       !=1,根据平面应力公式;=2,根据平面应变公式. 2023-03-19.
  integer Key_FS_Seque_Coupling                     !Sequential coupling of solid and field problems (only for 2D problem and Key_Analysis_Type = 1)
  integer Key_TS_Seque_Coupling                     !热固耦合计算(2D静态+瞬态温度场计算) (only for 2D problem AND key_analysis_type = 1)
  integer Num_Force_Divs                            !Key_Force_Control=3时需要
  integer Key_Save_f_d_Curve                        !是否导出载荷位移曲线数据
  integer f_d_Curve_node_num                        !导出载荷位移曲线数据的节点号
  integer Key_Save_f_COD_Curve                      !导出给定裂缝的载荷-裂缝张开位移(COD)曲线,保存到fccu文件中. 2022-10-15.
  integer f_COD_Curve_Crack_Num                     !载荷-裂缝张开位移(COD)曲线裂缝号. 2022-10-15.
  integer num_Crack_Log(Max_Itera)                  !每一步的裂纹的数目
  integer Key_Gravity                               !重力开关
  real(kind=FT) g_X_Y_Z(3)                          !各个方向上的重力加速度
  integer Key_Geo_Stress                            !地应力开关,若=1则需要进行地应力平衡
  integer Key_SLOE                                  !线性方程组求解方法
  integer Key_EBE_Precondition                      !EBE求解器预处理关键字, 2022-06-02.
  integer Key_EBE_Condition_Number                  !=1时则计算每个增强单元的条件数，存入全局变量EBE_Condition_Number(Num_Elem). NEWFTU2022070901.
  integer Key_EBE_Sym_Storage_K                     !对称存储单元刚度矩阵storK_FEM. 2022-11-10. NEWFTU2022111001.
  real(kind=FT),ALLOCATABLE::EBE_Condition_Number(:)!增强单元的条件数.
  integer Lis_Number_Iteration                      !Lis求解器的迭代次数(默认5000)
  integer MDOF_2D                                   !程序内部变量,Max_number_of_DOFs, 2D问题单元刚度矩阵最大自由度数目(default to 200;勿改动).
  integer MDOF_3D                                   !程序内部变量,Max_number_of_DOFs, 3D问题单元刚度矩阵最大自由度数目(default to 200;勿改动).
  integer Old_MDOF_3D                               !上一步MDOF_3D. 2023-01-11.
  integer Key_Kink_Point                            !是否给拐点划分计算点
  integer Key_Data_Format                           !数据保存格式，1：有格式的；2：二进制的
  integer Key_XA                                    !新奥相关. 2022-09-10.
  integer Key_K_Sparse                              !是否以稀疏矩阵的形式保存刚度矩阵
  real(kind=FT) Sparse_Ratio                        !构建稀疏矩阵时假定的最大稀疏比
  integer Sparse_Store_Method                       !中间变量(Location_COO)数据存储方式, 2:内存;1:硬盘二进制文件(默认)
  !integer Key_Asem_Sparse_Scheme                   !稀疏矩阵组集算法,1:快速,占内存;2:慢速,省内存
  !real(kind=FT) Spa_Work_Ratio                     !累加之前的工作区最大稀疏比
  integer Key_Unit_System                           !单位制,       1: 国际单位制(默认);2: mm-MPa单位制
  integer Key_Dimension                             !2D或3D问题,   2: 二维问题;3: 三维问题
  integer Key_Contact                               !不允许裂纹面的互相嵌入(默认开启,即=1)
  real(kind=FT) Contact_Aper_Tol                    !接触检测容差.
  !integer Key_Contact_Reduce                       !缩减的接触算法(仅适用于罚函数法(1))
  integer Key_TipEnrich                             !裂尖增强方案, 1: 标准裂尖增强(4项,默认值)
                                                    !              2: 仅保留第一项(对于动态分析,此为默认)
                                                    !              3: Heaviside光滑过度方案(详见我的博士论文)
                                                    !              4: Cohesive
  integer Key_CS_Natural_Crack                      !考虑压剪天然裂缝罚函数处理. 2022-10-22.
  real(kind=FT) Penalty_CS_Natural_Crack            !压剪型裂缝的罚参数. 默认1.0D10. 2022-12-23.
  integer Key_Penalty_CS_Method                     !=1(default)则罚函数控制l、m、n位移，=2
                                                    !则仅罚函数控制n方向位移，即法线方向位移. NEWFTU2023082701.
  integer Num_F_Functions                           !裂尖增强F函数的数目(内部参数-不需要设置)
  integer Key_Junction_Enrich                       !是否开启Junction增强(默认为0)，适用于3D. 2022-08-25.
  integer Key_Tip_Fluid_Element_3D                  !允许3D裂尖增强单元内激活流体单元
  integer Key_Multi_TipEnrNode                      !多裂尖增强
  integer Key_Pre_Conditioner                       !激活Pre_Conditioner,用于减小刚度矩阵的条件数，从而降低方程组病态特性、减小计算量;目前适用于3D
  real(kind=FT) Key_TipEnrich_Radius_Factor         !多裂尖增强节点检测半径系数
  integer Key_Local_Mesh_Refine                     !网格局部优化, =0,关闭；=1，加密全部增强节点；=2，仅加密裂尖增强节点
  logical Flag_Local_Refined                        !全局变量,用于标记是否已经局部加密
  integer Key_Front_Segmentation                    !Allow 3D fracture front segmentation
  integer Number_front_Segments                     !Number of Segments(default:2).

  integer Key_Adj_Ini_Crack_3D                      !Adjust initail crack. For 3D only.
  integer Key_Check_and_Adjust_Cracks_3D            !根据裂缝面和单元的相交情况调整裂缝面位置. 2022-08-01.
  real(kind=FT) Adjust_Cracks_Delta_Angle           !方位调整分辨率. Key_Check_and_Adjust_Cracks_3D =  2和3时用到. 90.0D0, 60.0D0, 45.0D0, 30.0D0, 22.5D0
  integer Adjust_Cracks_Resolution                  !方位调整分辨率. 2022-08-02.
  real(kind=FT) k_tt                                !防止裂纹面嵌入及模拟HF支撑剂切线刚度.
  real(kind=FT) k_nn                                !防止裂纹面嵌入及模拟HF支撑剂法向刚度.
  integer Key_Min_Aperture                          !是否限制最小开度(仅用于水力压裂分析).
  real(kind=FT) Min_Aperture                        !闭合裂缝的最小开度.
  !接触分析相关
  real(kind=FT) fric_mu_Cont                        !接触分析摩擦系数
  integer Max_Contact_Iter                          !裂纹面接触(含支撑剂问题)的最大迭代次数
  integer Key_Conta_ConCrit                         !接触分析收敛准则(1:通过残差确定;2:通过位移确定)
  real(kind=FT) Aper_Tol_Factor                     !裂缝接触开度检测系数,此参数对接触迭代的收敛有非常大的影响(越大越容易收敛,默认:1.0D-4*Ave_Elem_L_Enrich)
  real(kind=FT) kn_Cont_Penalty,kt_Cont_Penalty     !接触分析罚函数法接触刚度
  real(kind=FT) Conve_Tol_Penalty                   !接触分析罚函数法收敛容差
  !粘聚裂尖和粘聚裂缝相关
  integer Max_Cohesive_Iter                         !粘聚裂缝的最大迭代次数 >=50
  integer Coh_Integ_Point                           !粘聚裂缝积分点数目(1或2)
  integer Key_Coh_ConCrit                           !粘聚裂缝迭代收敛准则(1:通过残差确定;2:通过位移确定)
  integer Key_Play_Sounds                           !是否播放声音
  integer Key_Memo_Moni                             !是否监控内存消耗量
  integer Key_Clear_All                             !删除全部计算结果数据
  integer Key_Junction_Check                        !是否对Junction点进行检查,保证交叉点只需要一个Junction增强单元
  real(kind=FT) Factor_Check_Dis                    !内部参数:裂缝交叉检测长度(很关键,太短了容易引起计算bug,如V5-P50)
  real(kind=FT) Factor_L_Jun_NewCr                  !HF和NF相遇后形成的新裂缝的长度系数
  !----------------------------------------
  real(kind=FT) HF_Ini_Pressure                     !水力压裂分析初始压力
  !----------------------------------------
  !integer Num_Gauss_P_FEM_3D                       !普通3D单元的高斯积分点数目
  integer Key_Post_CS_N_Strs                        !每个载荷步(或破裂步)计算应力
  integer Key_Post_CS_N_Stra                        !是否计算并保存Node点应变(默认值为0)
  integer Key_Save_avol_file                        !保存裂缝体积和文件. 2022-12-18. 默认为0.
  integer Key_Post_CS_N_Stra_Cylindr                !是否计算并保存Node点应变(默认值为0)柱坐标系
  integer Key_Post_CS_N_Strs_Cylindr                !是否计算并保存Node点应力(默认值为0)柱坐标系
  integer Key_Post_CS_N_Disp_Cylindr                !是否计算并保存Node点位移(默认值为0)柱坐标系
  !若计算圆柱坐标系的应变（环向应变等)，需要给定以下参数
  real(kind=FT) Post_cylinder_Coor_Center(3)        !圆柱材料坐标系原点
  real(kind=FT) Post_cylinder_Coor_Vector_x(3)      !圆柱材料坐标系x轴向量
  real(kind=FT) Post_cylinder_Coor_Vector_y(3)      !圆柱材料坐标系y轴向量
  real(kind=FT) Post_cylinder_Coor_Vector_z(3)      !圆柱材料坐标系z轴向量
  real(kind=FT),ALLOCATABLE::Theta_Cartesian_to_Cylinder(:,:)!笛卡尔坐标到柱坐标的角度theta(单元，节点).
  real(kind=FT),ALLOCATABLE::Theta_Cartesian_to_Cylinder_Node(:)!笛卡尔坐标到柱坐标的角度theta(节点).
  real(kind=FT) Water_Pressure                      !分析类型4的均匀水压大小
  integer Type_Water_Pressure                       !水压类型:=1,常压强;=2,线性压强(裂尖为0);=3,二次压强
  real(kind=FT) Num_Div_Elment                      !非水力压裂分析时计算点划分密度
  integer Key_InSitu_Method                         !考虑地应力的方法(用于HF分析的5号迭代器)
  integer Key_InSitu_Strategy                       !考虑地应力的方法(用于HF分析的6号和8号迭代器)
  integer Key_Read_Initial_Node_Stress_File         !从初始应力文件(*.istn,节点应力文件)读入初始应力场数据.
  real(kind=FT) InSitu_x                            !x方向地应力
  real(kind=FT) InSitu_y                            !y方向地应力
  real(kind=FT) InSitu_z                            !z方向地应力
  real(kind=FT) InSitu_xy,InSitu_yz,InSitu_xz
  integer Key_InStress_for_Mat                      !给特定材料施加预应力
  integer Mat_Number_of_InStress(100)               !施加预应力的材料号
  real(kind=FT) Mat_InStress_x                      !x方向预应力
  real(kind=FT) Mat_InStress_y                      !y方向预应力
  real(kind=FT) Mat_InStress_z                      !z方向预应力

  integer Key_HF_Secant_TS                          !水力压裂分析中是否通过割线法确定时间步长
  integer Key_HF_LineSearch                         !HF分析是否开启线搜索
  integer Key_HF_Multistage                         !是否是水力压裂分段压裂
  integer Key_HF_Multistage_3D                      !是否是水力压裂分段压裂(3D)
  integer Key_HF_MS_Contc_Check                     !分段压裂是否进行含支撑剂裂缝面的接触迭代
  integer Key_HF_Cont_Scheme                        !水力压裂分析接触检测方案:=1,仅在第一次迭代确定接触状态,之后保持不变(默认);
                                                    !                         =2,仅在前两次流固迭代执行接触检测;
                                                    !                         =3,每次流固耦合迭代均执行接触迭代.
  character*1  Keywords_Blank
  parameter (Keywords_Blank  = ' ')
  character*4 Space_4
  parameter  (Space_4  = '    ')
  character*8 Space_8
  parameter  (Space_8  = '        ')
  !针对特定论文优化相关
  integer Key_Paper1
  integer Type_Cal_Propped                          !标记当前计算为支撑剂支撑步计算
  !integer Key_No_Tip_Enrich                        !是否进行裂尖增强
  !地应力相关
  integer State_InSitu
  !---------疲劳分析相关--------------
  integer Key_Static_Fatigue                        !Fatigue static analysis,定义是否是准静态疲劳分析
  integer Key_Fatigue_Cri                           !疲劳分析准则
  integer Num_Fatigue_N                             !疲劳循环次数
  real(kind=FT) Fatigue_Paris_C,Fatigue_Paris_m     !Paris准则相关变量
  !---------初始孔隙压力相关----------
  integer Key_PoreP                                 !是否有初始孔隙水压力
  real(kind=FT) Initial_PoreP                       !初始孔隙压的大小
  real(kind=FT) Initial_Biot_Coeff                  !初始比奥系数. 2023-03-19.
  !-------以下用于支撑裂缝开度计算,Key_Propped_Width=1------------------
  integer Key_Propped_Width                         !是否计算支撑裂缝的开度(Newton-Raphson迭代计算),Paper03,需要输入
                                                    !各裂缝计算点的W_P_0(wpnp文件),仅用于分析类型1
  real(kind=FT)Prop_Size                            !支撑剂尺寸
  real(kind=FT)Prop_Yita                            !支撑剂的铺置密度
  real(kind=FT)Prop_Elas                            !支撑剂弹性模量
  real(kind=FT)Prop_Poss                            !支撑剂的泊松比
  real(kind=FT)Prop_D                               !板的厚度
  !real(kind=FT)Prop_C1,Prop_C2                      !本构公式中的C1和C2,详见Paper03
  logical Yes_XFEM                                  !检查是否是XFEM分析
  !热应力计算相关
  integer Key_Thermal_Stress                        !是否考虑热应力
  integer Key_Initial_Temperature                   !初始温度给定方法. =1, 根据材料号. =2, 从文件读入（暂时不可用）. 2023-03-13.
  integer Key_Scheme_Thermal_Stress                 !用于设定温度应力计算方法. = 1，根据绝对温度计算热应力; = 2，根据相对温度（温差）计算热应力.
  real(kind=FT) Thermal_Str_Temper(100)             !热应力计算的温度值(不同材料号的)
  integer Key_Random                              !随机生成方案 0:系统自带的自动生成函数random_number,每次生成的完全相同且唯一
                          !             1:系统自带的自动生成函数random_number,每次生成的不同
                          !             2:Handbook of Simulation: Principles, Methodology, Advances, Applications
                          !              一书中的撒种子方案,每次生成情况由种子控制
  real(kind=FT) Inject_Prop_c
  integer Key_Plasticity
  !其他
  integer Key_Close_Window                          !计算结束后是否关闭DOS窗口
  integer Key_Visit_PhiPsi_top                      !访问PhiPsi网站信息并上传数据
  integer Key_Window_Log                            !保存窗口记录
  character(200) Window_log_filename
  character(200) PhiPsi_Current_Directory           !PhiPsi所在路径
  integer Seed                                      !用于随机生成变量的种子
  integer Key_Damage
  real(kind=FT),ALLOCATABLE:: Ele_Damage(:,:)       !每个单元每个Gauss点的损伤状态变量
  real(kind=FT),ALLOCATABLE:: Damage_Gauss(:)       !每个单元每个Gauss点的损伤状态变量(用于后处理)
  real(kind=FT) Material_Dam2Frac_Value             !损伤形成裂缝的极限损伤值(损伤超过该值即生成裂缝)
  integer Crack_Gen_from_Damage                     !损伤生成的裂缝数目
  !real(kind=FT),ALLOCATABLE:: Ele_Damage_D(:,:,:,:)!每个单元每个高斯点考虑损伤之后的D矩阵
  logical file_Sparse_K_Location_COO_bin            !用于标记Sparse_K_Location_COO.bin二进制文件是否存在
  logical file_Sparse_K_Mask_COO_bin                !用于标记Sparse_K_Mask_COO.bin二进制文件是否存在

  integer Key_InPlane_Growth                        !用于3D XFEM，激活后裂缝仅在原平面内扩展(2022-04-18).
  integer Key_Stop_Outside_Crack                    !一旦裂缝扩展到模型外部，即标记为不再扩展(2022-10-02). NEWFTU2022100201.
  integer Key_3D_HF_Time_Step_Method                !3D清水压裂分析时间步迭代方法:=1,NR迭代(default);=2,二分法.
  !******************
  !生死单元相关
  !******************
  integer Key_EKILL
  real(kind=FT) EKILL_Weaken_Factor              !生死单元弱化系数
  integer Ele_Killed_Each_Load_Step(Max_Step,1000)   !每个载荷步杀死的单元编号,最多支持1000个单元
  !ALLOCATA和DEALLOCATA错误反馈相关
  integer allo_STAT             !not used
  CHARACTER(len=80)::err_msg    !not used
  !***********************
  !About elements break.
  !***********************
  integer Key_Element_Break
  integer Key_Element_Break_Rule
  integer Break_Mat_Num
  !***********************************
  !About EBE-PCG solver, 2021-07-31.
  !***********************************
  real(kind=FT) cg_tol           !Tolerance of the PCG solver.
  integer       max_cg           !The max iteration number of CG solver.
  !parameter (cg_tol = 1.0D-7 )   !Default: 1.0D-7
  parameter (cg_tol = 1.0D-6 )   !Default: 1.0D-7
  !parameter (cg_tol = 1.0D-5 )   !Default: 1.0D-7
  parameter (max_cg = 5000)    !Default: 10000
  !*****************************
  !3D裂缝前缘S1或K光滑处理.
  !*****************************
  integer Key_Denoise_Vertex_Value   !3D裂缝前缘S1或K等数据进行去噪操作.
  integer Key_Smooth_Vertex_Value    !对3D裂缝前缘S1或K等数据进行光滑处理, =1, 滑动平均法,把前后时刻的一共2n+1个值做平均.
  integer Smooth_Vertex_n      !Key_Smooth_Vertex_Method= 1时用到. 滑动平均法的n.
  integer Key_Smooth_Vertex_Value2    !对3D裂缝前缘S1或K等数据进行光滑处理（二次处理）, =1, 滑动平均法,把前后时刻的一共2n+1个值做平均.
  integer Smooth_Vertex_n2      !Key_Smooth_Vertex_Method= 1时用到（二次处理）. 滑动平均法的n.
  !*****************************************
  !3D裂缝前缘Theta光滑处理. 2022-07-14.
  !*****************************************
  integer Key_Denoise_Theta_Value !3D裂缝前缘S1或K等数据进行去噪操作. 默认值为0.
  integer Key_Smooth_Theta_Value  !对3D裂缝前缘S1或K等数据进行光滑处理. 默认值为0.!=1, 滑动平均法,把前后时刻的一共2n+1个值做平均.
  integer Smooth_Theta_n         !Key_Smooth_Vertex_Method= 1时用到. 滑动平均法的n. 默认值为4.
  integer Key_Smooth_Theta_Value2 !对3D裂缝前缘S1或K等数据进行光滑处理(二次处理). 默认值为0.!=1, 滑动平均法,把前后时刻的一共2n+1个值做平均.
  integer Smooth_Theta_n2         !二次处理:Key_Smooth_Vertex_Method= 1时用到. 滑动平均法的n. 默认值为3.
  !*********************************************
  !3D裂缝前缘GrowthFactor光滑处理. 2022-07-14.
  !*********************************************
  integer Key_Denoise_GF_Value !3D裂缝前缘S1或K等数据进行去噪操作. 默认值为0.
  integer Key_Smooth_GF_Value  !对3D裂缝前缘S1或K等数据进行光滑处理. 默认值为0.!=1, 滑动平均法,把前后时刻的一共2n+1个值做平均.
  integer Smooth_GF_n          !Key_Smooth_Vertex_Method= 1时用到. 滑动平均法的n. 默认值为4.
  integer Key_Smooth_GF_Value2 !对3D裂缝前缘S1或K等数据进行光滑处理(二次处理). 默认值为0.!=1, 滑动平均法,把前后时刻的一共2n+1个值做平均.
  integer Smooth_GF_n2        !二次处理:Key_Smooth_Vertex_Method= 1时用到. 滑动平均法的n. 默认值为3.
  !******************
  !其他
  !******************
  integer Key_Crack_Inner_Pressure   !是否存在缝内流体压力
  integer Key_Block_Model            !是否为块体模型
  integer Flag_HF_3D                 !3D水力压裂分析标记. 2022-06-02.
  integer Key_Cpp_Call_Fortran_Lib   !2023-03-23.
  integer XA_Step_Count              !用于标记总的破裂步数的全局变量. 2023-04-03.
  integer Key_Save_Crack_Radius      !用于保存裂缝开度. 2023-02-15.
  integer Circle_3D_Eqv_Polygon_Resolution !3D圆形等效为多边形的分辨率(默认为21边形). 2024-02-26.
end module Global_Common

!----------------------------
!     2.模型相关全局变量
!----------------------------
module Global_Model
  use Global_Float_Type
  use Global_Ragged_Array_Real_Classs
  use Global_Ragged_Array_Int_Classs
  implicit none
  save
  integer Num_Node, Num_Elem, num_of_Material
  integer Num_Bou_x,Num_Bou_y,Num_Bou_z,Num_Foc_x,Num_Foc_y,Num_Foc_z
  integer Num_Boux_nonzero,Num_Bouy_nonzero,Num_Bouz_nonzero
  real(kind=FT) Min_X_Coor,Max_X_Coor,Min_Y_Coor,Max_Y_Coor,Min_Z_Coor,Max_Z_Coor
  !Model coordinates range.
  real(kind=FT) Model_X_Range,Model_Y_Range,Model_Z_Range
  real(kind=FT) Max_Model_Range,Min_Model_Range
  real(kind=FT),ALLOCATABLE::Coor(:,:)
  integer Max_Materials
  integer Model_Nx,Model_Ny,Model_Nz   !XA, 2023-03-22.
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !cccccccccccccccccc         程序规模控制参数        ccccccccccccccccccccccc
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  parameter (Max_Materials     = 100)          !最多100种材料
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !cccccccccccccccccc         程序规模控制参数        ccccccccccccccccccccccc
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  integer,ALLOCATABLE::Bou_x(:)
  integer,ALLOCATABLE::Bou_y(:)
  integer,ALLOCATABLE::Bou_z(:)
  real(kind=FT),ALLOCATABLE::Foc_x(:,:)
  real(kind=FT),ALLOCATABLE::Foc_y(:,:)
  real(kind=FT),ALLOCATABLE::Foc_z(:,:)
  real(kind=FT),ALLOCATABLE::Bou_x_nonzero(:,:)
  real(kind=FT),ALLOCATABLE::Bou_y_nonzero(:,:)
  real(kind=FT),ALLOCATABLE::Bou_z_nonzero(:,:)
  integer,ALLOCATABLE::Elem_Node(:,:)         !每个单元的四个节点
  integer,ALLOCATABLE::Elem_Node_Num(:,:)     !number of nodes of each element, for 2D and 3D.
  integer,ALLOCATABLE::Node_Elements(:,:)     !每个节点周围的单元.
  !integer,ALLOCATABLE::Node_Elements(:,:)     !每个节点周围的单元.
  type(Ragged_Int_Array_1D),allocatable::Node_Elements_3D(:)   !每个节点周围的单元. IMPROV2023061401. 参差数值.
  integer,ALLOCATABLE::num_Node_Elements(:)   !每个节点周围的单元个数
  integer,ALLOCATABLE::Ele_Elements(:,:)      !每个单元周围的单元个数
  integer,ALLOCATABLE::num_Ele_Eles(:)        !每个单元周围的单元个数
  integer,ALLOCATABLE::Element_Edges(:,:,:)   !每个单元的12条棱
  integer Max_Diff_Elem_Num                   !单元节点最大编号差
  integer Max_Half_Band_Width                 !按照公式计算出的最大半带宽,节点自由度数*(单元节点最大编号差+1)
  integer,ALLOCATABLE::Elem_Mat(:)
  integer,ALLOCATABLE::Outline(:,:)   !模型外边界(用于2D和3D)
  integer,ALLOCATABLE::OutArea(:,:)   !模型外表面(用于3D)
  integer Num_Surface_Nodes             !模型外表面节点数目(用于3D),2021-09-08
  integer,ALLOCATABLE::Surface_Nodes(:)   !模型外表面节点号(用于3D),2021-09-08
  integer,ALLOCATABLE:: G_NN(:,:)
  real(kind=FT),ALLOCATABLE:: G_X_NODES(:,:),G_Y_NODES(:,:),G_Z_NODES(:,:)
  real(kind=FT),ALLOCATABLE:: Elem_Area(:),Elem_Vol(:),Elem_Max_L(:),Elem_Min_L(:),Elem_Ave_L(:)
  real(kind=FT),ALLOCATABLE:: Node_Max_L(:)   !每个节点周围单元的最大单元长度. 2023-02-22. IMPROV2023022203.
  logical,ALLOCATABLE:: Elem_Break(:)                !用于标记各个单元是否破坏
  real(kind=FT),ALLOCATABLE:: Elem_Ave_Gauss_Stress(:,:)  !每个单元的平均Gauss应力
  real(kind=FT),ALLOCATABLE:: Elem_Ave_Gauss_S1(:)  !每个单元的平均Gauss最大主应力
  real(kind=FT),ALLOCATABLE:: Elem_Centroid(:,:)     !各个单元的质心
  integer,ALLOCATABLE:: Ele_yes_FEM_asemd(:)         !用于标记该单元的FEM单元刚度是否已经组集到总刚中
  logical*1,ALLOCATABLE:: EleGaus_yes_FEM_asemd(:,:)   !用于标记该单元该高斯点的FEM单元刚度是否已经组集到总刚中
  integer Total_FD,Usual_Freedom,Enrich_Freedom
  integer Total_FD_P,Total_FD_U
  integer,ALLOCATABLE:: Flag_FreeDOF(:)             !用于标记自由度是否是自由的. 2022-06-12.
  integer,ALLOCATABLE:: Location_FreeDOF(:)         !用于标记自由度对应的自由自由度位置. 2022-10-25.
  integer,ALLOCATABLE:: FreeDOF_for_Det(:)          !用于行列式分析和计算的FreeDOF. 2022-09-25.
  real(kind=FT) k_PenaltyStiffness
  !-------------Gauss点编号相关---------------
  !以下两个全局变量在组集刚度矩阵的过程中获得
  integer,ALLOCATABLE::num_GP_Elem(:)           !每个单元的Gauss点数目
  integer,ALLOCATABLE::Ele_GP_Start_Num(:)      !每个单元的Gauss点起始编号
  !-------------Gauss点坐标---------------
  real(kind=FT),ALLOCATABLE:: Gauss_CoorX(:)
  real(kind=FT),ALLOCATABLE:: Gauss_CoorY(:)
  !-------------破裂区---------
  integer Key_Fracture_Zone              !定义矩形的破裂区域(仅允许在该范围内扩展),若不定义,则在全模型内扩展
  real(kind=FT) Frac_Zone_MinX           !破裂区的x坐标范围
  real(kind=FT) Frac_Zone_MaxX           !破裂区的x坐标范围
  real(kind=FT) Frac_Zone_MinY           !破裂区的y坐标范围
  real(kind=FT) Frac_Zone_MaxY           !破裂区的y坐标范围
  real(kind=FT) Frac_Zone_MinZ           !破裂区的z坐标范围
  real(kind=FT) Frac_Zone_MaxZ           !破裂区的z坐标范围
  !-------------随机天然裂缝---------
  integer Key_Random_NaCr                !是否随机生成天然裂缝
  integer num_Rand_Na_Crack              !随机生成的天然裂缝数目
  real(kind=FT) NaCr_Orientation         !天然裂缝平均方位(度)
  real(kind=FT) NaCr_Ori_Delta           !天然裂缝方位的波动幅度(度)
  real(kind=FT) NaCr_Length              !天然裂缝的平均长度
  real(kind=FT) NaCr_Len_Delta           !天然裂缝的平均长度的波动幅度
  real(kind=FT) Random_NaCr_Rad_Factor   !天然裂缝的生成的检测半径系数(默认:1.5)
  !-------------模型的初始地应力
  !real(kind=FT) Insitu_x,Insitu_y
  !-------------节点耦合相关
  !option1-通过关键字文件直接定义耦合节点(这种方式每个方向只能定义一组耦合)
  integer num_CP_x_nodes,num_CP_y_nodes
  integer CP_x_nodes(1:5000)         !要耦合的x方向自由度节点列表,最多支持5000个节点
  integer CP_y_nodes(1:5000)         !要耦合的y方向自由度节点列表,最多支持5000个节点
  !option2-通过dofx,dofy,dofz文件定义耦合节点(这种方式每个方向只能定义多组耦合)
  integer num_CP_set_x,num_CP_set_y  !每个方向最多10组
  integer num_nodes_CP_set_x(10),num_nodes_CP_set_y(10)
  integer CP_nodes_x(10,5000),CP_nodes_y(10,5000)

  !-------------每个单元x,y,z坐标的范围--------
  real(kind=FT),ALLOCATABLE:: x_max_Elements(:)
  real(kind=FT),ALLOCATABLE:: x_min_Elements(:)
  real(kind=FT),ALLOCATABLE:: y_max_Elements(:)
  real(kind=FT),ALLOCATABLE:: y_min_Elements(:)
  real(kind=FT),ALLOCATABLE:: z_max_Elements(:)
  real(kind=FT),ALLOCATABLE:: z_min_Elements(:)
  real(kind=FT) penalty_k_bou_nonzero             !非零位移边界条件罚函数法的罚参数

  integer D3_nband_FEM  !3D问题半带宽(不考虑增强节点)

  integer Num_Elem_Block_Bou         !块体模型边界上的单元数目
  integer,ALLOCATABLE::Elems_Block_Bou(:)   !块体模型边界上的单元,大小为Num_Elem_Block_Bou
  integer Ele_3D_Edges_Node(12,2)           !定义一个矩阵，存储12条棱边的节点编号
  !-------------高斯积分相关--------
  integer,ALLOCATABLE::Elements_Gauss_Num(:)  !2022-07-16. 每个单元的高斯积分点数目.
  integer,ALLOCATABLE::Elems_Integration_Type(:,:)      !积分类型. 2022-07-27.
  integer,ALLOCATABLE::Elems_Num_SubEles(:,:)           !子单元数目
  integer,ALLOCATABLE::Elems_Type_SubEles(:,:)          !子单元类型, =1表示6面体单元,类型=2表示4面体单元
  integer,ALLOCATABLE::Elems_SubEles_Index(:,:)         !分块单元在SubEles_Coors矩阵中的编号.
  integer num_SubEles                                   !总的分块单元数目.
  integer,ALLOCATABLE::SubEles_Integ_Num(:)             !分块单元的积分点数目.
  real(kind=FT),ALLOCATABLE::SubEles_Integ_Coors(:,:,:)  !专门用于存放分块单元的积分点坐标点. 1:分块单元号; 2:积分点号;3:积分点局部坐标及权重.
  integer Num_Max_3D_gauss                              !3D单元可能最大积分点数目.
  !--------------
  !integer Ele_Num_Cache_by_Coors_3D                     !缓存，保存最近的通过坐标获得的单元号. 2022-09-24.
  !--------------
  !2022-11-21.
  !将模型根据坐标分成8个区域. 保存8个区域的单元编号. 便于根据坐标查找单元编号. IMPROV2022112101.
  real(kind=FT) Model_Center_x,Model_Center_y,Model_Center_z
  integer Domain_Elements_Num(8)
  integer,ALLOCATABLE::Ele_Domain_ID(:)
  integer,ALLOCATABLE::Domain_Elements(:,:)
  integer First_XFEM_Step             !新生成的初始裂缝激活的第一个XFEM步. 2023-01-23.
  !每种材料对应的单元列表. 2023-01-24. NEWFTU2023012401.
  type(Ragged_Int_Array_1D),allocatable::List_Elements_Mat(:)
  integer Elements_Num_Mat(Max_Materials)    !每种材料的单元数目.
  !单元的初始温度和当前温度. 2023-03-13.
  real(kind=FT),ALLOCATABLE:: Elem_Initial_T(:),Elem_Current_T(:),Elem_T_for_Stress(:)
  !核心全局变量. Size_Local_3D, All_Local_3D. 2023-03-15. IMPROV2023031501.
  integer,ALLOCATABLE::Size_Local_3D(:)
  integer,ALLOCATABLE::All_Local_3D(:,:)
  !
  !单元的初始孔隙压力、当前孔隙压力、比奥系数. 2023-03-19. IMPROV2023031901.
  real(kind=FT),ALLOCATABLE::Elem_Initial_PoreP(:),Elem_Current_PoreP(:),Elem_Biots(:)
  !单元的弹性模量、泊松比、热膨胀系数、断裂韧度、抗拉强度. 2023-03-19. IMPROV2023031903.
  real(kind=FT),ALLOCATABLE::Elem_E_XA(:),Elem_Mu_XA(:),Elem_TEC_XA(:),Elem_KIc_XA(:),Elem_St_XA(:)
  !单元的D矩阵. 2023-03-19. IMPROV2023031904.
  real(kind=FT),ALLOCATABLE::Elem_D_XA(:,:,:)
  !是否运行裂缝位于模型外部（边界裂缝）. 默认为0. NEWFTU2023050701.
  integer Key_Allow_3D_Outside_Crack
  !每个单元的独立节点数目. 2023-06-14. IMPROV2023061402.
  integer,ALLOCATABLE::Elem_Uniqued_Nodes(:)
  logical,ALLOCATABLE::Yes_Degenarated_Elem(:)
  integer Num_Degenarated_Elems
  integer MAT_ALLOW_CRACK_Initiation(Max_Materials)  !是否允许某种材料生成初始裂缝. 默认全部允许.
  !real(kind=FT) f_t_concrete
  integer Key_Max_Num_Initiation_Cracks              !最多允许萌生的初始裂缝数目.
  integer Num_Initiation_Cracks 
end module Global_Model

!--------------------------------
! 2.1 增强单元相关. 2022-06-24.
!--------------------------------
module Global_XFEM_Elements
  !---------------FEM单元列表和XFEM增强单元列表，2022-04-16------------
  integer num_FEM_Elem,num_XFEM_Elem !FEM单元数目和XFEM单元数目
  integer,ALLOCATABLE::FEM_Elem_List(:)   !FEM单元列表
  integer,ALLOCATABLE::XFEM_Elem_List(:)  !XFEM增强单元列表
  integer,ALLOCATABLE::Elem_XFEM_Flag(:)  !用于标记单元是否是增强单元
  integer,ALLOCATABLE::Elem_Location(:,:) !用于反向存储单元号在List中的位置，第一列为XFEM，第2列为FEM
  integer,ALLOCATABLE::Elem_New_XFEM_Flag(:)     !用于标记单元是否是新增增强单元. 2022-06-24.
  integer,ALLOCATABLE::Elem_Update_XFEM_Flag(:)  !用于标记单元是否是需要更新单元刚度矩阵的增强单元. 2022-06-24.
  integer,ALLOCATABLE::Elem_XFEM_Flag_Old(:)  !用于标记单元是否是增强单元
  integer,ALLOCATABLE::Elem_Location_Old(:,:) !用于反向存储单元号在List中的位置，第一列为XFEM，第2列为FEM
  integer,ALLOCATABLE::size_local_Old(:)   !用于保存上一步的各个单元刚度矩阵的大小.

  !BUGFIX2022092602.
  integer Must_Gauss_Number_3D        !刚度矩阵组集需要的裂缝面两侧高斯点数目,裂缝面两侧都至少有Must_Gauss_Number_3D个Gauss点，否则删除Heaviside增强节点.
  parameter (Must_Gauss_Number_3D = 50)!核心参数. 默认值20.
  !2023-02-15.
  integer,ALLOCATABLE::Rollbacked_FEM_Elements(:) !退化形成的FEM单元列表.
  integer Num_Rollbacked_FEM_Elements             !退化形成的FEM单元数目.
end module Global_XFEM_Elements


!----------------------------
!   3.文件名相关全局变量
!----------------------------
module Global_Filename
  implicit none
  save
  character(256) Filename,Work_Directory
  character(256) Full_Pathname
  character(256) Python_Directory
  character(256) PhiPsi_Directory
end module Global_Filename

!---------------------------------
! 4.动态分析相关(包括隐式和显式)
!---------------------------------
module Global_Dynamic
  use Global_Float_Type
  implicit none
  save
  integer Num_Ivex                              !x方向初始速度数目
  integer Num_Ivey                              !y方向初始速度数目
  integer Num_Ivez                              !z方向初始速度数目
  real(kind=FT),ALLOCATABLE::Ive_x(:,:)         !x方向初始速度
  real(kind=FT),ALLOCATABLE::Ive_y(:,:)         !y方向初始速度
  real(kind=FT),ALLOCATABLE::Ive_z(:,:)         !z方向初始速度
  integer Num_Iacx                              !x方向初始加速度数目
  integer Num_Iacy                              !y方向初始加速度数目
  integer Num_Iacz                              !z方向初始加速度数目
  real(kind=FT),ALLOCATABLE::Iac_x(:,:)         !x方向初始加速度
  real(kind=FT),ALLOCATABLE::Iac_y(:,:)         !y方向初始加速度
  real(kind=FT),ALLOCATABLE::Iac_z(:,:)         !z方向初始加速度
  integer IDy_Num_Iteras                        !Total number of iterations
  integer IDy_Num_force_Itr                     !Number of iterations with force applied
  real(kind=FT) delt_time_NewMark               !Delta time of Newmark
  integer Key_EQ                                !是否是地震分析,若是,则需要读入地震加速度值
  real(kind=FT)  EQ_Ac_Time_Gap                 !地震加速度数据时间间隔
  real(kind=FT),ALLOCATABLE::EQ_Accel_data(:)   !地震加速度数据
  integer num_EQ_Accel                   !地震加速度数据个数
  integer num_EQ_Ac_nodes                       !地震加速度施加到的节点数目
  integer EQ_Ac_nodes(5000)                     !地震加速度施加到的节点列表(最多5000个节点上)
  !正弦加速度激励相关
  integer Key_Sin_Accel                         !是否激活正弦加速度激励
  integer  Sin_Accel_Dire                       !正弦加速度激励的方向:=1,x;=2,y
  real(kind=FT) Sin_Accel_A                     !正弦加速度激励的振幅
  real(kind=FT) Sin_Accel_T                     !正弦加速度激励的周期
  integer  Sin_Accel_num_Nodes                  !正弦加速度激励的节点数目
  integer Sin_Accel_Nodes(5000)                 !正弦加速度激励的节点列表

  real(kind=FT) Factor_Prop_Dy                  !动态分析允许的裂缝扩展步长上限
  integer EDy_Num_Iteras                        !Total number of iterations(显式)
  integer EDy_Num_force_Itr                     !Number of iterations with force applieds(显式)
  real(kind=FT) Delt_Time_Explicit              !时间增量大小
  integer Key_Mass_Lumped                       !是否采用集中质量矩阵(对角质量矩阵,默认开启)
  real(kind=FT) Explicit_time_inc               !显式动态分析最小时间步长


  real(kind=FT),ALLOCATABLE::EDy_DISP(:),EDy_VELC(:),EDy_ACCL(:)
  real(kind=FT),ALLOCATABLE::IDy_DISP(:),IDy_VELC(:),IDy_ACCL(:)
end module Global_Dynamic

!------------------------------------------------------
!  5.2D裂缝相关，包括增强节点，计算点，裂纹连通关系
!  程序中数据量最大的全局变量等;此外还包括孔洞相关变量
!------------------------------------------------------
module Global_Crack
  use Global_Float_Type
  implicit none
  save
  integer  Max_Num_Cr,Max_Num_Arc_Cr,Max_Num_Cr_P
  integer  Max_Num_Cr_CalP,Max_Num_Seg_CalP,Max_Num_Cone_Cr
  integer  Max_Num_Ele_QuadHF,Max_Num_Ele_CalP,Max_Num_Hl
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !cccccccccccccccccc         程序规模控制参数        ccccccccccccccccccccccc
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !若要gfortran支持150条裂缝,需要:-fopenmp -fno-align-commons -fno-range-check -fmax-stack-var-size=100000    !100000调的更高支持更多裂缝(2021-08-19)
  !parameter (Max_Num_Cr        = 150   )      !最多150条裂纹,100条gfortran没问题,Intel Fortran有问题; 50条都没问题
  !parameter (Max_Num_Cr        = 100   )      !最多150条裂纹,100条gfortran没问题,Intel Fortran有问题; 50条都没问题
  parameter (Max_Num_Cr        = 50   )      !最多150条裂纹,100条gfortran没问题,Intel Fortran有问题; 50条都没问题
  parameter (Max_Num_Cr_P      = 200   )      !每个裂纹最多100个裂纹点
  parameter (Max_Num_Cr_CalP   = 300 )        !每条裂纹最多包含300个计算点(注,不能过大,否则内存会出错,但原因不明)
  parameter (Max_Num_Seg_CalP  = 200  )       !每个裂纹片段最多包含200个计算点
  parameter (Max_Num_Cone_Cr   = 10   )       !每个裂纹最多与10条其他的裂纹相连
  !---------
  parameter (Max_Num_Arc_Cr    = 100   )      !最多100条弧形裂纹
  !---------
  parameter (Max_Num_Ele_QuadHF= 100  )       !每个裂纹最多允许的高阶(二次)HF单元数目
  !---------
  parameter (Max_Num_Ele_CalP  = 5   )        !每个单元内最多允许存在的计算点数目
  parameter (Max_Num_Hl        = 100   )      !最多100个孔洞
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !cccccccccccccccccc         程序规模控制参数        ccccccccccccccccccccccc
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !********************************************************
  !增强节点相关，变量含义见子程序Determine_Enriched_Nodes
  !********************************************************
  real(kind=FT) Crack_Coor(Max_Num_Cr,Max_Num_Cr_P,2)           !裂缝号，裂纹坐标点，(x,y)
  real(kind=FT) Arc_Crack_Coor(Max_Num_Cr,Max_Num_Cr_P-1,11)    !定义裂缝的哪一个片段是弧形的,并给定对应的圆心坐标(
                                                                !x,y,Direction(1为逆时针,-1为顺时针),r,Radian_Start,Radian_End,Radian,
                                                                !Point_Start_x,Point_Start_y,Point_End_x,Point_End_y;
                                                                !输入时仅需输入圆心坐标(x,y)以及方向
                                                                !4,5,6,7,8,9,10,11号变量将通过Tool_Cal_Arc_r_and_Radian_Given_Coors子程序计算得到
                                                                !注意:起始弧角Radian_Start,Radian_End以3点钟为0度,0-360度逆时针
  !integer Arc_Crack_Passed_Ele(Max_Num_Cr,Max_Num_Cr_P-1,1000) !弧形裂缝穿过的单元号


  real(kind=FT) Hole_Coor(Max_Num_Hl,3)                         !圆形孔洞坐标(x,y,r)
  real(kind=FT) Ellip_Hole_Coor(Max_Num_Hl,5)                   !椭圆形孔洞坐标(x,y,r)
  real(kind=FT) Na_Crack_Coor(Max_Num_Cr,Max_Num_Cr_P,2)        !天然裂缝号，裂纹坐标点号，(x,y)
  real(kind=FT) Cr_First_Tip(Max_Num_Cr,2),Cr_Second_Tip(Max_Num_Cr,2)                  !各条裂纹的裂尖坐标
  real(kind=FT) Cr_First_Tip_Ori(Max_Num_Cr),Cr_Second_Tip_Ori(Max_Num_Cr)                !各条裂纹的裂尖所在裂纹片段的倾角


  !real(kind=FT) Penalty_CS
  integer,ALLOCATABLE:: Elem_Type(:,:)
  integer ,ALLOCATABLE:: c_POS(:,:)
  integer,ALLOCATABLE:: Enriched_Node_Type(:,:)
  real(kind=FT),ALLOCATABLE::Enriched_Node_Crack_n_Vector(:,:,:)!增强节点对应的裂缝面的外法线向量,added on 2022-05-12.
  integer,ALLOCATABLE:: Node_Jun_elem(:,:)                      !Junction增强节点对应的Junction单元号,added on 2016-07-10.
  integer,ALLOCATABLE:: Jun_Ele_Negative_Cr_Num(:,:)            !Juntion增强单元对应的被动裂缝号
  integer,ALLOCATABLE:: Elem_Type_Hl(:,:)                       !用于孔洞
  integer,ALLOCATABLE:: Enriched_Node_Type_Hl(:,:)              !用于孔洞
  integer ,ALLOCATABLE:: c_POS_Hl(:,:)                          !用于孔洞

  real(kind=FT),ALLOCATABLE::  Coors_Element_Crack(:,:,:)
  real(kind=FT),ALLOCATABLE::  Coors_Tip(:,:)
  real(kind=FT),ALLOCATABLE::  Coors_Vertex(:,:)
  real(kind=FT),ALLOCATABLE::  Coors_Junction(:,:,:)
  real(kind=FT),ALLOCATABLE::  x_cr_tip_nodes(:,:)
  real(kind=FT),ALLOCATABLE::  y_cr_tip_nodes(:,:)
  integer,ALLOCATABLE::  Ele_Num_Tip_Enriched_Node(:,:)
  integer Crack_Tip_Type(Max_Num_Cr,2)
  real(kind=FT) Crack_Arc_Tip_A_B_C_x(Max_Num_Cr,2,3)
  real(kind=FT) Crack_Arc_Tip_A_B_C_y(Max_Num_Cr,2,3)
  integer Crack_Jun_CrNum(Max_Num_Cr,2)
  integer Crack_Jun_HoleNum(Max_Num_Cr,2)
  integer,ALLOCATABLE:: Node_Jun_Hole(:,:)
  integer,ALLOCATABLE:: Ele_Jun_Hole(:,:)
  integer,ALLOCATABLE:: TipEle_Adjacent_Ele(:,:)      !各裂缝裂尖所在单元的临近单元(被该裂缝穿过的)
  integer Crack_Jun_Elem(Max_Num_Cr,2)
  real(kind=FT) Crack_Tip_Coor(Max_Num_Cr,2,2)        !各裂纹裂尖坐标
  real(kind=FT) Edge_Disposed_Crack(Max_Num_Cr,Max_Num_Cr_P,2)!边缘裂纹后处理用到
  logical Flag_Crack_Tip_Out_Mol(Max_Num_Cr,2)        !用于标记裂纹的两个裂尖是否超出模型范围
  logical Yes_Arc_Crack                               !是否包含弧形裂缝或裂缝段
  !***************************************************
  !裂缝计算点相关(所谓的计算点就是水力裂缝单元的节点)
  !对于非水力压裂问题,就是计算裂纹开度的坐标点
  !-----------
  !这一部分是对应当前破裂步的数据
  !***************************************************
  !以下改为Allocateble变量, 2022-09-02. IMPROV2022090201.
  !integer Cracks_CalP_Num(Max_Num_Cr)                 !每条裂缝计算点个数
  !real(kind=FT) Cracks_CalP_Coors(Max_Num_Cr,Max_Num_Cr_CalP,2)   !每条裂缝计算点坐标
  !real(kind=FT) Cracks_CalP_Orient(Max_Num_Cr,Max_Num_Cr_CalP)  !每条裂缝计算点对应的裂缝方位
  !integer  Cracks_CalP_Seg(Max_Num_Cr,Max_Num_Cr_CalP)  !每条裂缝计算点对应的裂纹片段号
  !integer  Cracks_CalP_Elem(Max_Num_Cr,Max_Num_Cr_CalP)  !每条裂缝计算点所在单元号
  !real(kind=FT) Cracks_CalP_Aper(Max_Num_Cr,Max_Num_Cr_CalP)  !每条裂缝计算点开度
  !real(kind=FT) Cracks_CalP_Pres(Max_Num_Cr,Max_Num_Cr_CalP)  !每条裂缝计算点水压
  !real(kind=FT) Cracks_CalP_Tractions(Max_Num_Cr,Max_Num_Cr_CalP,2)!转换到直角坐标系下的粘聚牵引力(用于粘聚裂缝)
  !real(kind=FT) Cracks_CalP_Pgra(Max_Num_Cr,Max_Num_Cr_CalP)  !每条裂缝计算点压力梯度(有方向)
  !real(kind=FT) Cracks_CalP_Velo(Max_Num_Cr,Max_Num_Cr_CalP)  !每条裂缝计算点流速(有方向)
  !real(kind=FT) Cracks_CalP_Quan(Max_Num_Cr,Max_Num_Cr_CalP)  !每条裂缝计算点流量(有方向)
  !real(kind=FT) Cracks_CalP_Conc(Max_Num_Cr,Max_Num_Cr_CalP)  !每条裂缝计算点浓度
  !real(kind=FT) Cracks_CalP_Remo_Strs(Max_Num_Cr,Max_Num_Cr_CalP)  !每条裂缝计算点远场应力
  !real(kind=FT) Cracks_CalP_Contact_Strs(Max_Num_Cr,Max_Num_Cr_CalP,2)!每条裂缝计算点法向和切向接触应力(用于地应力计算)
  !real(kind=FT) Cracks_CalP_Contact_Force_x(Max_Num_Cr,Max_Num_Cr_CalP)!每条裂缝计算点法向和切向接触应力(高斯点的平均值,真正的接触应力,单位为Pa)
  !real(kind=FT) Cracks_CalP_Contact_Force_y(Max_Num_Cr,Max_Num_Cr_CalP)!每条裂缝计算点法向和切向接触应力(高斯点的平均值,真正的接触应力,单位为Pa)
  !!-----2016-08-25新增--支撑裂缝开度和导流能力计算相关-------
  !real(kind=FT) Cracks_CalP_wpnp(Max_Num_Cr,Max_Num_Cr_CalP)     !每条裂缝计算点支撑裂缝初始开度
  !real(kind=FT) Cracks_CalP_wpor(Max_Num_Cr,Max_Num_Cr_CalP)     !每条裂缝计算点支撑裂缝初始开度
  !real(kind=FT) Cracks_CalP_wdeform(Max_Num_Cr,Max_Num_Cr_CalP)     !支撑裂缝开度的弹性变形量(小于支撑裂缝开度变化总量)
  !real(kind=FT) Cracks_CalP_Conductivity(Max_Num_Cr,Max_Num_Cr_CalP)     !每条裂缝计算点支撑裂缝的导流系数
  !real(kind=FT) Cracks_CalP_kf(Max_Num_Cr,Max_Num_Cr_CalP)     !每条裂缝计算点支撑裂缝的渗透系数
  !!real(kind=FT),ALLOCATABLE:: Cracks_CalP_Elem_CalP(:,:)!每个单元(相对于各裂缝而言)包含的计算点起始号(各裂缝局部编号)和数目
  !!-----------以下变量跟分段压裂相关----------
  !integer MS_Cracks_CalP_Num(20,Max_Num_Cr)              !各段压裂结束时每条裂缝计算点个数
  !real(kind=FT) MS_Cracks_CalP_Aper(20,Max_Num_Cr,Max_Num_Cr_CalP)        !各段压裂结束时每条裂缝计算点开度
  !real(kind=FT) MS_Cracks_CalP_Conc(10,Max_Num_Cr,Max_Num_Cr_CalP)  !各段压裂结束时每条裂缝计算点浓度
  !real(kind=FT) MS_CalP_Propped_Aper(Max_Num_Cr,Max_Num_Cr_CalP)    !各裂缝含支撑剂的闭合开度
  integer,ALLOCATABLE::  Cracks_CalP_Num(:)                 !每条裂缝计算点个数
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Coors(:,:,:)   !每条裂缝计算点坐标
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Orient(:,:)  !每条裂缝计算点对应的裂缝方位
  integer,ALLOCATABLE::   Cracks_CalP_Seg(:,:)  !每条裂缝计算点对应的裂纹片段号
  integer,ALLOCATABLE::   Cracks_CalP_Elem(:,:)  !每条裂缝计算点所在单元号
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Aper(:,:)  !每条裂缝计算点开度
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Pres(:,:)  !每条裂缝计算点水压
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Tractions(:,:,:)!转换到直角坐标系下的粘聚牵引力(用于粘聚裂缝)
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Pgra(:,:)  !每条裂缝计算点压力梯度(有方向)
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Velo(:,:)  !每条裂缝计算点流速(有方向)
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Quan(:,:)  !每条裂缝计算点流量(有方向)
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Conc(:,:)  !每条裂缝计算点浓度
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Remo_Strs(:,:)  !每条裂缝计算点远场应力
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Contact_Strs(:,:,:)!每条裂缝计算点法向和切向接触应力(用于地应力计算)
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Contact_Force_x(:,:)!每条裂缝计算点法向和切向接触应力(高斯点的平均值,真正的接触应力,单位为Pa)
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Contact_Force_y(:,:)!每条裂缝计算点法向和切向接触应力(高斯点的平均值,真正的接触应力,单位为Pa)
  !-----2016-08-25新增--支撑裂缝开度和导流能力计算相关-------
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_wpnp(:,:)     !每条裂缝计算点支撑裂缝初始开度
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_wpor(:,:)     !每条裂缝计算点支撑裂缝初始开度
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_wdeform(:,:)     !支撑裂缝开度的弹性变形量(小于支撑裂缝开度变化总量)
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Conductivity(:,:)     !每条裂缝计算点支撑裂缝的导流系数
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_kf(:,:)     !每条裂缝计算点支撑裂缝的渗透系数
  !real(kind=FT),ALLOCATABLE:: Cracks_CalP_Elem_CalP(:,:)!每个单元(相对于各裂缝而言)包含的计算点起始号(各裂缝局部编号)和数目
  !-----------以下变量跟分段压裂相关----------
  integer,ALLOCATABLE::  MS_Cracks_CalP_Num(:,:)              !各段压裂结束时每条裂缝计算点个数
  real(kind=FT),ALLOCATABLE::  MS_Cracks_CalP_Aper(:,:,:)        !各段压裂结束时每条裂缝计算点开度
  real(kind=FT),ALLOCATABLE::  MS_Cracks_CalP_Conc(:,:,:)  !各段压裂结束时每条裂缝计算点浓度
  real(kind=FT),ALLOCATABLE::  MS_CalP_Propped_Aper(:,:)    !各裂缝含支撑剂的闭合开度

  !***************************************************
  !这是对应于上一破裂步的裂缝计算点相关数据
  !先不指定内存空间,假如破裂步数>=2,程序再分配内存空间
  !***************************************************
  integer,ALLOCATABLE::       L_Cracks_CalP_Num(:)
  real(kind=FT),ALLOCATABLE:: L_Cracks_CalP_Pres(:,:)
  real(kind=FT),ALLOCATABLE:: L_Cracks_CalP_Aper(:,:)
  real(kind=FT),ALLOCATABLE:: L_Cracks_CalP_Coors(:,:,:)
  real(kind=FT),ALLOCATABLE:: L_Cracks_CalP_Orient(:,:)
  integer,ALLOCATABLE::       L_Cracks_CalP_Seg(:,:)
  integer,ALLOCATABLE::       L_Cracks_CalP_Elem(:,:)
  real(kind=FT),ALLOCATABLE:: L_Cracks_CalP_Pgra(:,:)
  real(kind=FT),ALLOCATABLE:: L_Cracks_CalP_Velo(:,:)
  real(kind=FT),ALLOCATABLE:: L_Cracks_CalP_Quan(:,:)
  real(kind=FT),ALLOCATABLE:: L_Cracks_CalP_Conc(:,:)
  real(kind=FT),ALLOCATABLE:: Map_L_Cracks_CalP_Conc(:,:)    !映射到新裂缝后的裂缝计算点浓度
  !----------
  integer Cracks_CalP_Type(Max_Num_Cr,Max_Num_Cr_CalP,2) !每条裂缝计算点的类型及相关联的裂纹号
                                                         !(1)计算点类型,Cracks_CalP_Type(i_C,i_CalP,1)
                                                         !     0 --- 普通
                                                         !     1 --- 裂纹(i_C)裂尖1与另一裂纹(other_C)的Junction共用计算点;
                                                         !     2 --- 裂纹(i_C)裂尖2与另一裂纹(other_C)的Junction共用计算点;
                                                         !     3 --- 裂纹(i_C)中部某点与另一裂纹(other_C)的Junction共用计算点;
                                                         !     4 --- 裂纹(i_C)中部某点与另一裂纹(other_C)的十字交叉共用计算点;
                                                         !     5 --- 裂纹(i_C)裂尖不与其他裂纹相连时的普通计算点,这种情况下
                                                         !           约束HF压力边界为0,详见子程序Boundary_Cond_HF
                                                         !(2)对应的裂纹号other_C,Cracks_CalP_Type(i_C,i_CalP,2)
                                                         !     说明: Cracks_CalP_Type(i_C,i_CalP,1)=5时不需要
  integer Cracks_TipJunCalpNum(Max_Num_Cr,2)             !每条裂缝裂尖Junction点对应的计算点号(当前裂纹的计算点号)
  integer Cracks_MidJunCalpNum(Max_Num_Cr,Max_Num_Cr_CalP) !每条裂缝非裂尖Junction点对应的计算点号(当前裂纹的计算点号)
  !----------计算点全局编号及反之对应的局部编号---------------------
  !既包括含水裂纹也包括不参与流固耦合的不含水裂纹
  integer Cracks_GloNumCalP(Max_Num_Cr,Max_Num_Cr_CalP)        !各条裂纹各计算点的全局编号
  integer Cracks_LocalNumCalP(Max_Num_Cr*Max_Num_Cr_CalP,2)    !全局计算点对应的裂纹号和局部计算点号
                                                               !1: 裂纹号
                                                               !2: 局部计算点号
  !既包括含水裂纹也包括不参与流固耦合的不含水裂纹
  integer Cracks_GloNumCalP_W(Max_Num_Cr,Max_Num_Cr_CalP)      !各条含水裂纹各计算点的全局编号
  integer Cracks_LocalNumCalP_W(Max_Num_Cr*Max_Num_Cr_CalP,2)  !全局计算点对应的裂纹号和局部计算点号
                                                               !1: 裂纹号
                                                               !2: 局部计算点号
  !----------
  integer Num_JunPair                                          !Junction点对数
  integer Cracks_JunPair(Max_Num_Cr,2)                         !Junction点对对应的计算点号(全局号码),1对应从裂纹,2对应主裂纹
  !*********************************
  !水力压裂高阶单元(二次单元)特有的
  !*********************************
  integer Ele_Nodes_QuadHF(Max_Num_Cr,Max_Num_Ele_QuadHF,3)   !各裂缝水力压裂二次单元的三个节点号码
  !********************
  !水力裂缝单元相关
  !********************
  real(kind=FT) Cracks_HF_Ele_L(Max_Num_Cr,Max_Num_Cr_CalP-1)         !每个水力裂缝单元的长度
  !*******************************************
  !裂缝连通关系相关 （仅在水力压裂时用到）
  !*******************************************
  integer  Cracks_Cone_Num(Max_Num_Cr)                        !每条裂缝直接相连的裂纹数目(包括裂尖和中部)
  integer  Cracks_Cone_Cr(Max_Num_Cr,Max_Num_Cone_Cr)         !每条裂缝直接相连的裂纹号(包括裂尖和中部)
  !--------------裂尖联通------------
  integer Cracks_Cone_NumTipCr(Max_Num_Cr)                    !每条裂缝的两个裂尖与其他裂纹的联通的数目,为1或者2
  integer Cracks_Cone_TipCrNum(Max_Num_Cr,2)                  !每条裂缝的两个裂尖与其他裂纹的联通关系:
                                                              !Cracks_Cone_TipType(i_C,1)--裂纹i_C裂尖1的联通裂纹号
                                                              !Cracks_Cone_TipType(i_C,2)--裂纹i_C裂尖2的联通裂纹号
  integer Cracks_Cone_TipJuEle(Max_Num_Cr,2)                  !每条裂缝的两个裂尖直接相连的裂纹交叉点单元号
  real(kind=FT) Cracks_Cone_TipJuCor(Max_Num_Cr,2,2)          !每条裂缝的两个裂尖直接相连的裂纹交叉点坐标,就是裂尖坐标
  !--------------中部联通------------
  integer Cracks_Cone_NumMidCr(Max_Num_Cr)                   !与每条裂缝中部Junction相连的裂纹数目
  integer Cracks_Cone_MidCrNum(Max_Num_Cr,Max_Num_Cone_Cr)   !与每条裂缝中部Junction相连的裂纹号
  integer Cracks_Cone_MidCrTip(Max_Num_Cr,Max_Num_Cone_Cr)   !与每条裂缝中部Junction相连的裂纹对应的裂尖号
  integer Cracks_Cone_MidJuEle(Max_Num_Cr,Max_Num_Cone_Cr)   !每条裂缝中部Junction点所在单元号
  real(kind=FT) Cracks_Cone_MidJuCor(Max_Num_Cr, Max_Num_Cone_Cr,2)             !每条裂缝中部Junction点坐标
  !*****************
  !应力强度因子相关
  !*****************
  real(kind=FT) KI(Max_Num_Cr,2),KII(Max_Num_Cr,2)           !每条裂纹两个裂尖的I型和II型应力强度因子KI和KII

  !******************************************************
  !水力压裂天然裂缝相关(见Check_Crack_Grows_MCSC.f)
  !******************************************************
  integer  Cracks_NF_JiS_Cr_Num(Max_Num_Cr)       !HF和NF相交生成的新计算裂缝对应的天然裂缝号
  integer  Cracks_NF_JiS_Stat(Max_Num_Cr,2)       !HF和NF相交寄生状态(两个裂尖都有)
  integer  Cracks_NF_T_Stat(Max_Num_Cr,2)         !HF和NF交汇问题相关,确定新生成裂缝的T型交叉状态(与裂尖无关)
  integer  Cracks_NF_Cement_Tip_Num(Max_Num_Cr,2) !HF和NF交汇问题相关,裂缝i_C和胶结裂缝相交后,是沿着天然裂缝的1号裂尖扩展还是2号裂尖扩展呢？
  integer  Cracks_fric_NF_num(Max_Num_Cr)         !用于摩擦型天然裂缝,保存各裂缝对应的摩擦天然裂缝号
  integer  Cracks_QinS_Stat(Max_Num_Cr)           !用于摩擦型天然裂缝,用于标记每一个裂缝是否被水压裂缝侵蚀过
  !****************
  !粘聚裂缝相关
  !****************
  integer  Cracks_Coh_Ele_Type(Max_Num_Cr,Max_Num_Cr_CalP-1)    !每条裂缝粘聚单元的类型,=1表示为粘聚单元，=0为一般单元
  integer  Cracks_Tip_Num_of_Coh_Ele(Max_Num_Cr,2)              !每条裂缝两个裂尖对应的粘聚单元数目(仅为数目,而不是编号)

  !裂缝与多边形夹杂相遇相关
  integer Crack_Tip_Ploy_Inc_Info(Max_Num_Cr,2,5)         !裂尖与多边形夹杂相遇后用到:
                                                          !1记录多边形夹杂号,2记录裂尖所在多边形的边号
  !REAL(kind=FT) Cracks_CalP_Coh_FN_FT(Max_Num_Cr,Max_Num_Cr_CalP,2)  !每条裂缝计算点法向和切向粘聚力
  integer Key_Crack_Aperture_Method     !裂缝开度计算方法, =1:根据公式计算; =2:根据偏置点计算(default). 2023-08-12.
  
  real(kind=FT) Crack_Max_Min_Aperture(Max_Num_Cr,3)    !保存每条裂缝的最大最小和平均开度. 2023-08-27.
end module Global_Crack

!-----------------------------------
! 5.2 2D和3D裂缝共有. 2022-09-05.
!-----------------------------------
module Global_Crack_Common
  use Global_Float_Type
  !use Global_Ragged_Array_Real_Classs
  !use Global_Ragged_Array_Int_Classs
  implicit none
  save

  integer num_Crack                                             !当前载荷步的裂纹数
  integer num_Arc_Crack                                         !当前载荷步的弧形裂纹数
  integer num_Hole,num_Circ_Hole,num_Ellip_Hole                 !孔洞数目
  integer Each_Cr_Poi_Num(1000)                                 !每个裂纹对应的裂纹坐标点数
  integer num_Na_Crack                                          !天然裂缝数目
  integer Key_Na_Crack_Type                                     !天然裂缝的类型,1: 摩擦裂缝; 2: 胶结裂缝
  integer Key_NaCr_Friction                                     !天然裂缝之摩擦裂缝是否考虑摩擦效应,若考虑,
  integer n_h_Node,n_t_Node,n_j_Node,n_hl_Node,n_c_Node
  !******************
  !随机生成孔洞相关
  !******************
  integer Key_Random_Hole                    !是否随机生成孔洞
  integer num_Rand_Hole                      !随机生成的孔洞数目
  real(kind=FT) Rand_Hole_R                  !随机生成的孔洞的平均半径
  real(kind=FT) Rand_Hole_R_Delta            !随机生成的孔洞的平均半径的变化范围(+-)
  !******************
  !裂缝萌生相关
  !******************
  integer Key_Hole_Crack_Generate            !是否允许孔洞位置萌生裂缝
  integer Num_Crack_Hole_Generated           !每个孔洞允许生成的裂缝数目
  integer num_Hole_Crack_Generated(1000)     !各个Hole对应的生成的裂缝数目
  integer Hole_Crack_Generated_num(1000,10)  !各个Hole对应的生成的裂缝号

  !------缝内压力-------
  real(kind=FT) Crack_Pressure(1000)         !缝内压力(最多1000条裂缝)
  integer Crack_Pressure_Type                !流体压力类型:=1,固定压力;=2,压力自动调整,使得裂缝正好扩展

  !integer Key_Cr_Pressure                   !是否施加裂缝压强:=1,常压强;=2,线性压强(裂尖为0);=3,二次压强
  !REAL(kind=FT) Cr_Pressure_Value           !动态分析裂缝压强的大小

  !*****************************************
  !裂纹是否参被水驱动（仅在水力压裂时用到）
  !*****************************************
  integer Cracks_HF_State(1000)                        !每条裂缝是否被水驱动:=0: 无水;=1: 有水.
  integer Cracks_HF_Propp(1000)                        !每条裂缝是否含有支撑剂:=0: 无; =1: 有.
  !******************************************************
  !Location of the injection point for the full HF model
  !******************************************************
  real(kind=FT) Inj_Point_Loc(2)                             !Location of the injection point for the full HF model
  integer Current_Inj_Crack                                  !当前注水裂缝号
  integer CalP_num_InjP_Local                                !全模型注水点对应的计算点号(对应裂缝的局部编号)
                                                             !对于对称HF压裂,该编号显然是等于1,所以对称模型不需要该参数
  real(kind=FT) Cracks_HF_ConPressure(1000)                  !常水压裂缝的水压大小
  !******************************************************
  !裂缝是否允许扩展
  !******************************************************
  !integer Cracks_Allow_Propa(Max_Num_Cr)                     !各条裂缝是否允许扩展
  integer,ALLOCATABLE::Cracks_Allow_Propa(:)                  !各条裂缝是否允许扩展. BUGFIX2022082101.
  !integer Cracks_Tips_Allow_Propa(Max_Num_Cr,2)              !各条裂缝各个裂尖是否允许扩展

  integer Each_Na_Cr_Poi_Num(1000)                        !每个天然裂纹对应的裂纹坐标点数
  integer Key_CS_Crack(1000)                              !标记各裂缝是否是压剪裂缝，=1则是

  !***************************************************
  !裂缝计算点相关(所谓的计算点就是水力裂缝单元的节点)
  !对于非水力压裂问题,就是计算裂纹开度的坐标点
  !-----------
  !这一部分是对应当前破裂步的数据
  !***************************************************
  integer num_Tol_CalP_Water                          !模型总的参与流固耦合计算点的计算点数目(即水压驱动裂纹对应的)
  integer num_Tol_CalP_Water_ConP                     !模型总的参与流固耦合计算点的计算点数目(即常水压裂纹对应的)
  integer num_Tol_CalP_All                            !模型总的计算点数目(即所有裂纹对应的)
  real(kind=FT) Total_Conductivity                    !压裂系统的总导流系数
  real(kind=FT) Ave_Conductivity                      !压裂系统的平均导流系数
  !----------用于裂纹面接触迭代
  integer,ALLOCATABLE::Ele_NumCalP(:)                          !各单元内的计算点数,Ele_NumCalP(num_elem)
  integer,ALLOCATABLE::Ele_CalPNum(:,:)                        !各单元内的计算点编号(全局),Ele_CalPNum(num_elem,Max_Num_Ele_CalP)
  integer Conta_Integ_Point                                    !接触积分点数目
  real(kind=FT) Norm2_Contact_R_PSI_0                          !接触迭代第一个迭代步的残差

  !2022-11-14.
  real(kind=FT),ALLOCATABLE:: Crack_Coor_Range(:,:,:)          !裂缝左边范围. 2022-11-14.
  !2023-01-07
  integer Key_NaCr_Active_Scheme_3D   !用于控制3D天然裂缝激活算法.
                                 != 1，初始时刻即激活全部天然裂缝，且所有天然裂缝等于实际天然裂缝尺寸，HF沟通后裂缝充满压裂液. 默认.
                                 != 2，被HF沟通后才激活，HF沟通后裂缝充满压裂液.
                                 != 3，被HF沟通后才激活，HF沟通后仅部分裂缝张开，沿着天然裂缝所在面扩展，天然裂缝穿过的单元断裂韧度低.
  real(kind=FT) Size_Factor_of_Active_NaCr !被沟通的天然裂缝的直径(Size_of_Active_NaCr*交点所在单元的特征尺寸),Key_NaCr_Active_Scheme_3D = 3时用到. 2023-01-11.
  real(kind=FT) KIc_NaCr(50000)      !天然裂缝的断裂韧度. 最多50000个天然裂缝. 2023-01-12.
  real(kind=FT) St_NaCr(50000)       !天然裂缝的抗拉强度. 最多50000个天然裂缝. 2024-02-22. NEWFTU2024022202.
  integer Key_Ele_Max_Related_Cracks !用于指定单元最多可以关联的裂缝数目. NEWFTU2023022501.
  integer Max_Related_Cracks         !单元最大裂缝关联数目. 2023-02-25.
  integer Key_Print_SIFs_to_Screen   !将应力强度因子输出到屏幕. 2023-08-22.
end module Global_Crack_Common

!------------------
! 5.3 3D裂缝相关
!------------------
module Global_Crack_3D
  use Global_Float_Type
  use Global_Ragged_Array_Real_Classs
  use Global_Ragged_Array_Int_Classs
  implicit none
  save
  !3D裂缝相关
  integer Max_Num_Cr_3D
  !integer Max_N_Node_3D
  !integer Max_N_CalP_3D
  integer Max_Num_El_3D
  !integer Max_N_FluEl_3D
  !----原默认-----
  !parameter (Max_Num_Cr_3D        = 5   )                 !最多10条裂纹
  !parameter (Max_N_Node_3D        = 5000   )               !每个裂缝面最多5000个离散节点(之前用过1000)
  !parameter (Max_N_CalP_3D        = 20000 )                 !每条裂纹最多包含20000个计算点
  !-----减小----
  !parameter (Max_Num_Cr_3D        = 5   )                 !最多10条裂纹
  !parameter (Max_N_Node_3D        = 1000   )               !每个裂缝面最多5000个离散节点(之前用过1000)
  !parameter (Max_N_CalP_3D        = 3000 )                 !每条裂纹最多包含20000个计算点
  !-----增大----
  !parameter (Max_Num_Cr_3D        = 50   )                 !最多50条裂纹
  !parameter (Max_N_Node_3D        = 5000   )               !每个裂缝面最多5000个离散节点(之前用过1000)
  !parameter (Max_N_CalP_3D        = 20000 )                 !每条裂纹最多包含20000个计算点
  !-----增大----
  parameter (Max_Num_Cr_3D        = 10000)               !最多1000条裂纹. 2022-11-25增加到10000.
  !
  !integer :: Max_N_Node_3D        = 200 !最初每个裂缝面最多200个离散节点. IMPROV2022110501. 若不足则每个裂缝自动扩充.
  integer :: Max_N_Node_3D(1:Max_Num_Cr_3D)  = 200  !最初每个裂缝面最多200个离散节点，若不足则每个裂缝自动扩充. IMPROV2023081306.
  integer Max_Max_N_Node_3D !2023-08-13.
  !
  !integer :: Max_N_FluEl_3D       = 200 !最初每个裂缝面最多200个流体单元. IMPROV20221105011. 若不足则每个裂缝自动扩充.
  !
  integer :: Max_N_FluEl_3D(1:Max_Num_Cr_3D)  = 200 !最初每个裂缝面最多200个流体单元. 若不足则每个裂缝自动扩充. IMPROV2023081304.
  integer Max_Max_N_FluEl_3D !2023-08-13.
  !
  !integer :: Max_N_CalP_3D        = 200 !最初每个裂缝面最多200个流体节点. IMPROV20221105021. 若不足则每个裂缝自动扩充.
  integer :: Max_N_CalP_3D(1:Max_Num_Cr_3D)    = 200 !最初每个裂缝面最多200个流体节点. 若不足则每个裂缝自动扩充. IMPROV2023081305.
  integer Max_Max_N_CalP_3D !2023-08-13.
  !
  integer :: Max_ele_num_CalP     = 100  !每个流体单元最多100个流体节点. 2023-08-13. IMPROV2023081303.
  !---------

  !---------
  !real(kind=FT) Crack3D_Coor(Max_Num_Cr_3D,4,3)            !裂缝号，裂缝面坐标点号，(x,y,z)
  real(kind=FT),allocatable::Crack3D_Coor(:,:,:)            !裂缝号，裂缝面坐标点号，(x,y,z).  2022-06-16.
  real(kind=FT),allocatable::Na_Crack3D_Coor(:,:,:)         !多边形天然裂缝，裂缝号，裂缝面坐标点号，(x,y,z).  2023-01-07.
  real(kind=FT),allocatable::Na_Crack3D_St(:)               !天然裂缝的抗拉强度. 2023-03-25.
  real(kind=FT),allocatable::Na_Crack3D_KIc(:)              !天然裂缝的断裂韧度. 2023-03-25.
  real(kind=FT),allocatable::Na_Crack3D_Friction(:)         !天然裂缝的摩擦系数. 2023-03-25.
  integer,ALLOCATABLE:: Each_NaCr3D_Poi_Num(:)              !每个天然多边形裂缝的边数. 2023-01-07.
  integer,ALLOCATABLE:: NaCr3D_Status(:,:)                  !保存每个3D天然裂缝得到激活状态等状态变量. 2023-01-09.
                                                            !第1列保存激活状态;
                                                            !第2列保存该天然裂缝所在固体单元数目.
  type(Ragged_Int_Array_1D),allocatable::Na_Crack3D_Ele_List(:)   !使用参差数组, 保存天然裂缝所在单元列表. 2023-01-12. NEWFTU2023011202.
  !type(Ragged_Array_2D)::Crack3D_Coor(Max_Num_Cr_3D)       !每一个裂缝分配一个Ragged_Array_2D类对象(allocate后分配内存). 2022-09-02.

  !real(kind=FT),ALLOCATABLE:: Dis_Node_to_FS(:,:)          !节点距离各裂缝面的符号距离,FS表示Fracture Surface
  type(Ragged_Array_1D),allocatable::Dis_Node_to_FS(:)      !IMPROV2022091802.

  real(kind=FT),ALLOCATABLE::Vector_o_Orient(:,:)            !原点相对于各裂缝面的方位向量(共八种情况,如1,-1,-1等)
  integer,ALLOCATABLE::Sign_o_Orient(:)                     !原点相对于各裂缝面的方位的正负

  !real(kind=FT)Cr_Plane_Line_Center(Max_Num_Cr_3D,4,3)     !裂缝面边线的中点,用于计算应力强度因子
  !real(kind=FT)Cr_Plane_Line_Center(Max_Num_Cr_3D,10,3)     !裂缝面边线的中点,用于计算应力强度因子.  2022-06-16.
  !real(kind=FT)Cr_Plane_Normal_vector(Max_Num_Cr_3D,3)     !裂缝面的外法线向量

  !real(kind=FT) KI_3D(Max_Num_Cr_3D,Max_N_Node_3D)                  !每个裂缝面各个边界点的应力强度因子
  !real(kind=FT) KII_3D(Max_Num_Cr_3D,Max_N_Node_3D)
  !real(kind=FT) KIII_3D(Max_Num_Cr_3D,Max_N_Node_3D)
  !real(kind=FT) KI_eq_3D(Max_Num_Cr_3D,Max_N_Node_3D)
  !NEWFTU2022090201.
  type(Ragged_Array_1D),allocatable::KI_3D(:)        !每一个裂缝分配一个Ragged_Array_1D类对象(allocate后分配内存). 2022-09-02.
  type(Ragged_Array_1D),allocatable::KII_3D(:)       !每一个裂缝分配一个Ragged_Array_1D类对象(allocate后分配内存). 2022-09-02.
  type(Ragged_Array_1D),allocatable::KIII_3D(:)      !每一个裂缝分配一个Ragged_Array_1D类对象(allocate后分配内存). 2022-09-02.
  type(Ragged_Array_1D),allocatable::KI_eq_3D(:)     !每一个裂缝分配一个Ragged_Array_1D类对象(allocate后分配内存). 2022-09-02.

  !-------离散裂缝面相关-----
  !real(kind=FT) Crack3D_Meshed_Node(Max_Num_Cr_3D,Max_N_Node_3D,3)            !离散化之后的3D裂缝节点坐标,每个裂缝最多由1000个点组成
  !integer Crack3D_Meshed_Ele(Max_Num_Cr_3D,Max_N_Node_3D,3)     !离散化之后的3D裂缝单元编号,每个裂缝最多由1000个点组成
  !real(kind=FT) Crack3D_Meshed_Node_Value(Max_Num_Cr_3D,Max_N_Node_3D,3)      !离散化之后的3D裂缝节点变量(第一个变量为开度)
  !integer Cr3D_Meshed_Node_in_Ele_Num(Max_Num_Cr_3D,Max_N_Node_3D)            !离散化之后的3D裂缝节点所在单元号
  integer,allocatable::Crack3D_Meshed_Node_num(:)                !离散化之后的3D裂缝节点数目
  !integer Crack3D_Meshed_Node_num_Old(Max_Num_Cr_3D)            !上一步对应的离散化之后的3D裂缝节点数目, 2022-06-21.
  !real(kind=FT) Cr3D_Meshed_Node_in_Ele_Local(Max_Num_Cr_3D,Max_N_Node_3D,3)  !离散化之后的3D裂缝节点所在单元号的局部坐标
  !real(kind=FT) Crack3D_Meshed_Ele_Attri(Max_Num_Cr_3D,Max_N_Node_3D,5)       !离散化之后的3D裂缝单元特性参数(周长、面积等)
  integer,allocatable::Crack3D_Meshed_Ele_num(:)                 !离散化之后的3D裂缝单元数目
  !real(kind=FT)  Crack3D_Meshed_Ele_Nor_Vector(Max_Num_Cr_3D,Max_N_Node_3D,3)    !离散化之后的3D裂缝单元外法线向量
  !real(kind=FT)  Crack3D_Meshed_Node_Nor_Vector(Max_Num_Cr_3D,Max_N_Node_3D,3)   !离散化之后的3D裂缝节点外法线向量
  !integer Crack3D_Meshed_Outline(Max_Num_Cr_3D,Max_N_Node_3D,4) !离散化之后的3D裂缝外边界相关信息
                                                                !数据1为裂缝前缘边界线第1个点
                                                                !数据2为裂缝前缘边界线第2个点
                                                                !数据3为对应的单元号
                                                                !数据4用于标记该边界线的两个点是否允许扩展,扩展非常小的步长(2021-08-20)
  !integer Crack3D_Meshed_Outline_Grow_From(Max_Num_Cr_3D,Max_N_Node_3D)   !用于标记3D裂缝面边界顶点从哪个点扩展而来. NEWFTU2022071301.
  integer num_Suspended_Point                                   !用于Fracture Front Segmentation,历史的抑制的点的数目(2021-08-20)
  real(kind=FT),allocatable::Suspended_Points(:,:)                       !用于Fracture Front Segmentation,历史的抑制的点的坐标(2021-08-20)
  !REAL(kind=FT) Crack3D_Meshed_Outline_Vertex(Max_Num_Cr_3D,Max_N_Node_3D,3)        !离散化之后的3D裂缝外边界顶点坐标
  !integer Crack3D_Meshed_Outline_Vertex_Ele_num(Max_Num_Cr_3D,Max_N_Node_3D)        !离散化之后的3D裂缝外边界顶点所在固体单元号
  integer,allocatable::Crack3D_Meshed_Outline_num(:)             !离散化之后的3D裂缝外边界线条数
  !------圆形初始3D裂缝------
  real(kind=FT),allocatable::Crack3D_Cir_Coor(:,:)               !圆形初始裂缝,裂缝号,圆形坐标(x,y,z)+外法线向量+半径
  real(kind=FT),allocatable::Na_Crack3D_Cir_Coor(:,:)            !天然圆形初始裂缝,裂缝号,圆形坐标(x,y,z)+外法线向量+半径. 2021-01-07.
  !------椭圆初始3D裂缝------
  real(kind=FT),allocatable::Crack3D_Ellip_Coor(:,:)             !椭圆初始裂缝,裂缝号,圆形坐标(x,y,z)+外法线向量+半径a+半径b
  !real(kind=FT) Crack3D_Meshed_Vertex_x_Vector(Max_Num_Cr_3D,Max_N_Node_3D,3) !离散化之后的3D裂缝边界点的局部x坐标向量
  !real(kind=FT) Crack3D_Meshed_Vertex_y_Vector(Max_Num_Cr_3D,Max_N_Node_3D,3) !离散化之后的3D裂缝边界点的局部y坐标向量
  !real(kind=FT) Crack3D_Meshed_Vertex_z_Vector(Max_Num_Cr_3D,Max_N_Node_3D,3) !离散化之后的3D裂缝边界点的局部坐标向量
  !real(kind=FT) Crack3D_Meshed_Vertex_T_Matrx(Max_Num_Cr_3D,Max_N_Node_3D,3,3)!离散化之后的3D裂缝边界点的旋转矩阵
  !real(kind=FT) Crack3D_Meshed_Vertex_T_Theta(Max_Num_Cr_3D,Max_N_Node_3D,3)!离散化之后的3D裂缝边界点的旋转角
  !real(kind=FT) Crack3D_Vector_S1(Max_Num_Cr_3D,Max_N_Node_3D,3)!离散化之后的3D裂缝节点的最大主应力向量

  !type(Ragged_Array_2D)::Crack3D_Meshed_Node(Max_Num_Cr_3D)                    !使用参差数组, 2022-09-03.
  !type(Ragged_Int_Array_2D)::Crack3D_Meshed_Ele(Max_Num_Cr_3D)   !使用参差数组, 2022-09-03.
  !type(Ragged_Array_2D)::Crack3D_Meshed_Node_Value(Max_Num_Cr_3D)  !使用参差数组, 2022-09-03.
  !type(Ragged_Int_Array_1D)::Cr3D_Meshed_Node_in_Ele_Num(Max_Num_Cr_3D)   !使用参差数组, 2022-09-03.
  !type(Ragged_Array_2D)::Cr3D_Meshed_Node_in_Ele_Local(Max_Num_Cr_3D)  !使用参差数组, 2022-09-03.
  !type(Ragged_Array_2D)::Crack3D_Meshed_Ele_Attri(Max_Num_Cr_3D)  !使用参差数组, 2022-09-03.
  !type(Ragged_Array_2D)::Crack3D_Meshed_Ele_Nor_Vector(Max_Num_Cr_3D)  !使用参差数组, 2022-09-03.
  !type(Ragged_Array_2D)::Crack3D_Meshed_Node_Nor_Vector(Max_Num_Cr_3D)  !使用参差数组, 2022-09-03.
  !type(Ragged_Int_Array_2D)::Crack3D_Meshed_Outline(Max_Num_Cr_3D)  !使用参差数组, 2022-09-03.
  !type(Ragged_Array_2D)::Crack3D_Meshed_Vertex_x_Vector(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_2D)::Crack3D_Meshed_Vertex_y_Vector(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_2D)::Crack3D_Meshed_Vertex_z_Vector(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_3D)::Crack3D_Meshed_Vertex_T_Matrx(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_2D)::Crack3D_Meshed_Vertex_T_Theta(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_2D)::Crack3D_Vector_S1(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !logical Crack3D_Meshed_Arrays_Objects_Created          !逻辑变量，用于标记Crack3D_Meshed相关参差数组是否已经生成对象
  type(Ragged_Array_2D),allocatable::Crack3D_Meshed_Node(:)                    !使用参差数组, 2022-09-03.
  type(Ragged_Int_Array_2D),allocatable::Crack3D_Meshed_Ele(:)   !使用参差数组, 2022-09-03.
  type(Ragged_Array_2D),allocatable::Crack3D_Meshed_Node_Value(:)  !使用参差数组, 2022-09-03.
  type(Ragged_Int_Array_1D),allocatable::Cr3D_Meshed_Node_in_Ele_Num(:)   !使用参差数组, 2022-09-03.
  type(Ragged_Array_2D),allocatable::Cr3D_Meshed_Node_in_Ele_Local(:)  !使用参差数组, 2022-09-03.
  type(Ragged_Array_2D),allocatable::Crack3D_Meshed_Ele_Attri(:)  !使用参差数组, 2022-09-03.
  type(Ragged_Array_2D),allocatable::Crack3D_Meshed_Ele_Nor_Vector(:)  !使用参差数组, 2022-09-03.
  type(Ragged_Array_2D),allocatable::Crack3D_Meshed_Node_Nor_Vector(:)  !使用参差数组, 2022-09-03.
  type(Ragged_Int_Array_2D),allocatable::Crack3D_Meshed_Outline(:)  !使用参差数组, 2022-09-03.
  type(Ragged_Array_2D),allocatable::Crack3D_Meshed_Vertex_x_Vector(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Crack3D_Meshed_Vertex_y_Vector(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Crack3D_Meshed_Vertex_z_Vector(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Crack3D_Meshed_Vertex_T_Matrx(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Crack3D_Meshed_Vertex_T_Theta(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Crack3D_Vector_S1(:)  !使用参差数组, 2022-09-04.

  !------流体单元及流体单元计算点相关------
  integer,allocatable::Cracks_FluidEle_num_3D(:)                 !每条裂缝流体单元的数目
  integer,allocatable::Cracks_CalP_Num_3D(:)                     !每个裂缝计算点数目(有些不参与计算)
  integer,allocatable::Cracks_Real_CalP_Num_3D(:)                !每个裂缝计算点数目(参与计算的)
  real(kind=FT),allocatable::Cracks_Volume(:)                    !每条裂缝的体积
  real(kind=FT),allocatable::Cracks_Volume_Old(:)                !每条裂缝的体积(上一步的)
  integer Key_3D_FluEle_Triang                                  !对3D流体单元进行拆分,确保每个流体单元都是三角形单元
  integer,allocatable::Cracks_FluidEle_CalP_Glo_Info(:,:)         !保存全局流体节点编号对应的局部信息:包括裂缝号、流体单元号、对应流体单元的流体节点号, 2022-06-04.  type(Ragged_Int_Array_2D)::Cracks_FluidEle_CalP_3D(Max_Num_Cr_3D)       !使用参差数组, 2022-09-04.
  real(kind=FT),allocatable::Cracks_FluidEle_CalP_Glo_Insitu(:)           !全局编号流体节点的地应力(垂直于裂缝面方向), 2022-06-04.
  real(kind=FT),allocatable::Crack3D_Centroid(:,:)               !3D裂缝面的形心

  !integer Cracks_FluidEle_num_3D_Old(Max_Num_Cr_3D)             !上一步对应的每条裂缝流体单元的数目, 2022-06-21.
  !integer Cracks_CalP_Num_3D_Old(Max_Num_Cr_3D)                 !每个裂缝计算点数目(有些不参与计算). 上一步的. 2022-06-21.
  !integer Cracks_Real_CalPs_3D(Max_Num_Cr_3D,Max_N_CalP_3D)     !每个裂缝计算点编号(参与计算的),2022-06-04.

  !integer Cracks_FluidEle_CalP_3D(Max_Num_Cr_3D,Max_N_CalP_3D, 7)  !每条裂缝流体单元计算点编号
  !integer Cracks_FluidEle_Glo_CalP_3D(Max_Num_Cr_3D,Max_N_CalP_3D,7)              !每条裂缝流体单元计算点编号(全局编号,用于组集Q矩阵的编号)
  !integer Cracks_FluidEle_num_CalP_3D(Max_Num_Cr_3D,Max_N_CalP_3D)             !每条裂缝流体单元的计算点数目
  !integer Cracks_FluidEle_EleNum_3D(Max_Num_Cr_3D,Max_N_CalP_3D)!流体单元对应的固体单元号
  !real(kind=FT) Cracks_FluidEle_Area_3D(Max_Num_Cr_3D,Max_N_CalP_3D)          !每条裂缝流体单元的面积
  !real(kind=FT) Cracks_FluidEle_Centroid_3D(Max_Num_Cr_3D,Max_N_CalP_3D,3)       !每条裂缝流体单元的形心
  !real(kind=FT) Cracks_FluidEle_LCS_x_3D(Max_Num_Cr_3D,Max_N_CalP_3D,3)!每条裂缝流体单元的质心位置的局部坐标系x轴
  !real(kind=FT) Cracks_FluidEle_LCS_y_3D(Max_Num_Cr_3D,Max_N_CalP_3D,3)       !每条裂缝流体单元的质心位置的局部坐标系y轴
  !real(kind=FT) Cracks_FluidEle_LCS_z_3D(Max_Num_Cr_3D,Max_N_CalP_3D,3)       !每条裂缝流体单元的质心位置的局部坐标系z轴(法向向量)
  !real(kind=FT) Cracks_FluidEle_LCS_T_3D(Max_Num_Cr_3D,Max_N_CalP_3D,3,3)     !每条裂缝流体单元的质心位置的局部坐标系的转换矩阵
  !real(kind=FT) Cracks_FluidEle_Vector_3D(Max_Num_Cr_3D,Max_N_CalP_3D,3)         !每条裂缝流体单元的平均外法线向量
  !real(kind=FT) Cracks_FluidEle_Aper_3D(Max_Num_Cr_3D,Max_N_CalP_3D)           !每条裂缝流体单元形心的开度
  !real(kind=FT) Cracks_CalP_Coors_3D(Max_Num_Cr_3D,Max_N_CalP_3D,3)             !每条裂缝计算点坐标
  !real(kind=FT) Cracks_CalP_Orient_3D(Max_Num_Cr_3D,Max_N_CalP_3D,3)             !每条裂缝计算点对应的裂缝方位(裂缝面的单位外法线向量)
  !integer Cracks_CalP_MeshedEl_3D(Max_Num_Cr_3D,Max_N_CalP_3D)  !每条裂缝计算点对应的裂纹离散裂缝单元号
  !integer Cracks_CalP_Elem_3D(Max_Num_Cr_3D,Max_N_CalP_3D,2)    !每条裂缝计算点所在单元号及棱边号
  !real(kind=FT) Cracks_CalP_Aper_3D(Max_Num_Cr_3D,Max_N_CalP_3D)!每条裂缝计算点开度
  !real(kind=FT) Cracks_CalP_UpDis_3D(Max_Num_Cr_3D,Max_N_CalP_3D,3)           !每条裂缝计算点上偏置点的位移向量
  !real(kind=FT) Cracks_CalP_LowDis_3D(Max_Num_Cr_3D,Max_N_CalP_3D,3)          !每条裂缝计算点下偏置点的位移向量
  !real(kind=FT) Cracks_CalP_Pres_3D(Max_Num_Cr_3D,Max_N_CalP_3D)!每条裂缝计算点水压
  !real(kind=FT) Cracks_CalP_Tractions_3D(Max_Num_Cr_3D,Max_N_CalP_3D,3)       !转换到直角坐标系下的粘聚牵引力(用于粘聚裂缝)
  !real(kind=FT) Cracks_CalP_Pgra_3D(Max_Num_Cr_3D,Max_N_CalP_3D)!每条裂缝计算点压力梯度(有方向)
  !real(kind=FT) Cracks_CalP_Velo_3D(Max_Num_Cr_3D,Max_N_CalP_3D)!每条裂缝计算点流速(有方向)
  !real(kind=FT) Cracks_CalP_Quan_3D(Max_Num_Cr_3D,Max_N_CalP_3D)!每条裂缝计算点流量(有方向)
  !real(kind=FT) Cracks_CalP_Conc_3D(Max_Num_Cr_3D,Max_N_CalP_3D)!每条裂缝计算点浓度
  !real(kind=FT) Cracks_CalP_Remo_Strs_3D(Max_Num_Cr_3D,Max_N_CalP_3D)   !每条裂缝计算点远场应力

  !type(Ragged_Int_Array_2D)::Cracks_FluidEle_CalP_3D(Max_Num_Cr_3D)   !使用参差数组, 2022-09-04.
  !type(Ragged_Int_Array_2D)::Cracks_FluidEle_Glo_CalP_3D(Max_Num_Cr_3D)   !使用参差数组, 2022-09-04.
  !type(Ragged_Int_Array_1D)::Cracks_FluidEle_num_CalP_3D(Max_Num_Cr_3D)   !使用参差数组, 2022-09-04.
  !type(Ragged_Int_Array_1D)::Cracks_FluidEle_EleNum_3D(Max_Num_Cr_3D)     !使用参差数组, 2022-09-04.
  !type(Ragged_Array_1D)::Cracks_FluidEle_Area_3D(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_2D)::Cracks_FluidEle_Centroid_3D(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_2D)::Cracks_FluidEle_LCS_x_3D(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !typeRagged_Array_2D)::Cracks_FluidEle_LCS_y_3D(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !typeRagged_Array_2D)::Cracks_FluidEle_LCS_z_3D(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !typeRagged_Array_3D)::Cracks_FluidEle_LCS_T_3D(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !typeRagged_Array_2D)::Cracks_FluidEle_Vector_3D(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !typeRagged_Array_1D)::Cracks_FluidEle_Aper_3D(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !typeRagged_Array_2D)::Cracks_CalP_Coors_3D(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !typeRagged_Array_2D)::Cracks_CalP_Orient_3D(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !typeRagged_Int_Array_1D)::Cracks_CalP_MeshedEl_3D(Max_Num_Cr_3D)   !使用参差数组, 2022-09-04.
  !typeRagged_Int_Array_2D)::Cracks_CalP_Elem_3D(Max_Num_Cr_3D)   !使用参差数组, 2022-09-04.
  !typeRagged_Array_1D)::Cracks_CalP_Aper_3D(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !typeRagged_Array_2D)::Cracks_CalP_UpDis_3D(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !typeRagged_Array_2D)::Cracks_CalP_LowDis_3D(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !typeRagged_Array_1D)::Cracks_CalP_Pres_3D(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !typeRagged_Array_2D)::Cracks_CalP_Tractions_3D(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !typeRagged_Array_1D)::Cracks_CalP_Pgra_3D(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !typeRagged_Array_1D)::Cracks_CalP_Velo_3D(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !typeRagged_Array_1D)::Cracks_CalP_Quan_3D(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !typeRagged_Array_1D)::Cracks_CalP_Conc_3D(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_1D)::Cracks_CalP_Remo_Strs_3D(Max_Num_Cr_3D)  !使用参差数组, 2022-09-04.

  type(Ragged_Int_Array_2D),allocatable::Cracks_FluidEle_CalP_3D(:)   !使用参差数组, 2022-09-04.
  type(Ragged_Int_Array_2D),allocatable::Cracks_FluidEle_Glo_CalP_3D(:)   !使用参差数组, 2022-09-04.
  type(Ragged_Int_Array_1D),allocatable::Cracks_FluidEle_num_CalP_3D(:)   !使用参差数组, 2022-09-04.
  type(Ragged_Int_Array_1D),allocatable::Cracks_FluidEle_EleNum_3D(:)     !使用参差数组, 2022-09-04.
  type(Ragged_Array_1D),allocatable::Cracks_FluidEle_Area_3D(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Cracks_FluidEle_Centroid_3D(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Cracks_FluidEle_LCS_x_3D(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Cracks_FluidEle_LCS_y_3D(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Cracks_FluidEle_LCS_z_3D(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Cracks_FluidEle_LCS_T_3D(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Cracks_FluidEle_Vector_3D(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_1D),allocatable::Cracks_FluidEle_Aper_3D(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Cracks_CalP_Coors_3D(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Cracks_CalP_Orient_3D(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Int_Array_1D),allocatable::Cracks_CalP_MeshedEl_3D(:)   !使用参差数组, 2022-09-04.
  type(Ragged_Int_Array_2D),allocatable::Cracks_CalP_Elem_3D(:)   !使用参差数组, 2022-09-04.
  type(Ragged_Array_1D),allocatable::Cracks_CalP_Aper_3D(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Cracks_CalP_UpDis_3D(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Cracks_CalP_LowDis_3D(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_1D),allocatable::Cracks_CalP_Pres_3D(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Cracks_CalP_Tractions_3D(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_1D),allocatable::Cracks_CalP_Pgra_3D(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_1D),allocatable::Cracks_CalP_Velo_3D(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_1D),allocatable::Cracks_CalP_Quan_3D(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_1D),allocatable::Cracks_CalP_Conc_3D(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_1D),allocatable::Cracks_CalP_Remo_Strs_3D(:)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_2D),allocatable::Cracks_CalP_k_3D(:)     !使用参差数组, 2022-11-26.

  !----支撑裂缝开度和导流能力计算相关-------
  !REAL(kind=FT) Cracks_CalP_wpnp_3D(Max_Num_Cr_3D,Max_N_CalP_3D)        !每条裂缝计算点支撑裂缝初始开度
  !REAL(kind=FT) Cracks_CalP_wpor_3D(Max_Num_Cr_3D,Max_N_CalP_3D)        !每条裂缝计算点支撑裂缝初始开度
  !REAL(kind=FT) Cracks_CalP_wdeform_3D(Max_Num_Cr_3D,Max_N_CalP_3D)     !支撑裂缝开度的弹性变形量(小于支撑裂缝开度变化总量)
  !REAL(kind=FT) Cracks_CalP_Conductivity_3D(Max_Num_Cr_3D,Max_N_CalP_3D)     !每条裂缝计算点支撑裂缝的导流系数
  !REAL(kind=FT) Cracks_CalP_kf_3D(Max_Num_Cr_3D,Max_N_CalP_3D)               !每条裂缝计算点支撑裂缝的渗透系数
  !----裂尖增强单元和节点相关----
  integer Solid_El_Max_num_Crs                               !每个固体单元最多关联的裂缝数目, 默认为5
  parameter (Solid_El_Max_num_Crs   = 8)                     !原值为5. 基本不影响内存占用.
  !integer(kind=1),ALLOCATABLE:: Solid_El_num_Crs(:)          !每个固体单元相关裂缝数目, 2022-08-15. IMPROV2022091803.
  integer,ALLOCATABLE:: Solid_El_num_Crs(:)

  integer,ALLOCATABLE:: Solid_El_Crs(:,:)                    !每个固体单元相关裂缝号码, 2022-08-15.
  !----
  !integer,ALLOCATABLE:: Solid_El_Vertex_Num(:,:)                  !每个固体单元含有的离散裂缝面边界顶点数目(最多3个顶点)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Vertex_Coor(:,:,:,:)       !每个固体单元含有的离散裂缝面边界顶点坐标(最多3个顶点)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Vertex_Nor_Vec(:,:,:,:)    !每个固体单元含有的离散裂缝面边界顶点外法线向量(最多3个顶点)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Vertex_x_Vec(:,:,:,:)      !每个固体单元含有的离散裂缝面边界顶点局部x坐标轴向量(最多3个顶点)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Vertex_y_Vec(:,:,:,:)      !每个固体单元含有的离散裂缝面边界顶点局部y坐标轴向量(最多3个顶点)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Vertex_z_Vec(:,:,:,:)      !每个固体单元含有的离散裂缝面边界顶点局部z坐标轴向量(最多3个顶点)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Pre_Vertex_Coor(:,:,:,:)   !每个固体单元含有的离散裂缝面边界顶点的上一顶点坐标(最多3个顶点)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Pre_Vertex_Nor_Vec(:,:,:,:)!每个固体单元含有的离散裂缝面边界顶点的上一顶点外法线向量(最多3个顶点)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Pre_Vertex_x_Vec(:,:,:,:)  !每个固体单元含有的离散裂缝面边界顶点的上一顶点局部x坐标轴向量(最多3个顶点)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Pre_Vertex_y_Vec(:,:,:,:)  !每个固体单元含有的离散裂缝面边界顶点的上一顶点局部y坐标轴向量(最多3个顶点)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Pre_Vertex_z_Vec(:,:,:,:)  !每个固体单元含有的离散裂缝面边界顶点的上一顶点局部z坐标轴向量(最多3个顶点)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Nex_Vertex_Coor(:,:,:,:)   !每个固体单元含有的离散裂缝面边界顶点的下一顶点坐标(最多3个顶点)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Nex_Vertex_Nor_Vec(:,:,:,:)!每个固体单元含有的离散裂缝面边界顶点的下一顶点外法线向量(最多3个顶点)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Nex_Vertex_x_Vec(:,:,:,:)  !每个固体单元含有的离散裂缝面边界顶点的下一顶点局部x坐标轴向量(最多3个顶点)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Nex_Vertex_y_Vec(:,:,:,:)  !每个固体单元含有的离散裂缝面边界顶点的下一顶点局部y坐标轴向量(最多3个顶点)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Nex_Vertex_z_Vec(:,:,:,:)  !每个固体单元含有的离散裂缝面边界顶点的下一顶点局部z坐标轴向量(最多3个顶点)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Tip_BaseLine(:,:,:,:)      !每个固体单元裂尖增强基准线
  !real(kind=FT),ALLOCATABLE:: Solid_El_Tip_BaseLine_Nor_Vec(:,:,:)!每个固体单元裂尖增强基准线外法线向量
  !real(kind=FT),ALLOCATABLE:: Solid_El_Tip_BaseLine_x_Vec(:,:,:)  !每个固体单元裂尖增强基准线上的局部坐标系x坐标轴向量
  !real(kind=FT),ALLOCATABLE:: Solid_El_Tip_BaseLine_y_Vec(:,:,:)  !每个固体单元裂尖增强基准线上的局部坐标系y坐标轴向量
  !real(kind=FT),ALLOCATABLE:: Solid_El_Tip_BaseLine_z_Vec(:,:,:)  !每个固体单元裂尖增强基准线上的局部坐标系z坐标轴向量
  !real(kind=FT),ALLOCATABLE:: Solid_El_Tip_BaseLine_T_theta(:,:,:)!每个固体单元裂尖增强基准线上的局部坐标系旋转角
  !real(kind=FT),ALLOCATABLE:: Solid_El_Tip_BaseLine_T_Matrix(:,:,:,:)!每个固体单元裂尖增强基准线上的局部坐标系旋转角
  !IMPROV2022090401.
  !type(Ragged_Int_Array_1D)::Solid_El_Vertex_Num(Max_Num_El_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Vertex_Coor(Max_Num_El_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Vertex_Nor_Vec(Max_Num_El_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Vertex_x_Vec(Max_Num_El_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Vertex_y_Vec(Max_Num_El_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Vertex_z_Vec(Max_Num_El_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Pre_Vertex_Coor(Max_Num_El_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Pre_Vertex_Nor_Vec(Max_Num_El_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Pre_Vertex_x_Vec(Max_Num_El_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Pre_Vertex_y_Vec(Max_Num_El_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Pre_Vertex_z_Vec(Max_Num_El_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Nex_Vertex_Coor(Max_Num_El_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Nex_Vertex_Nor_Vec(Max_Num_El_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Nex_Vertex_x_Vec(Max_Num_El_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Nex_Vertex_y_Vec(Max_Num_El_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Nex_Vertex_z_Vec(Max_Num_El_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Tip_BaseLine(Max_Num_El_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_2D)::Solid_El_Tip_BaseLine_Nor_Vec(Max_Num_El_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_2D)::Solid_El_Tip_BaseLine_x_Vec(Max_Num_El_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_2D)::Solid_El_Tip_BaseLine_y_Vec(Max_Num_El_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_2D)::Solid_El_Tip_BaseLine_z_Vec(Max_Num_El_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_2D)::Solid_El_Tip_BaseLine_T_theta(Max_Num_El_3D)  !使用参差数组, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Tip_BaseLine_T_Matrix(Max_Num_El_3D)  !使用参差数组, 2022-09-04.
  !logical Solid_El_Arrays_Objects_Created          !逻辑变量，用于标记Solid_El相关参差数组是否已经生成对象
  type(Ragged_Int_Array_1D),allocatable::Solid_El_Vertex_Num(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Vertex_Coor(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Vertex_Nor_Vec(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Vertex_x_Vec(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Vertex_y_Vec(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Vertex_z_Vec(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Pre_Vertex_Coor(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Pre_Vertex_Nor_Vec(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Pre_Vertex_x_Vec(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Pre_Vertex_y_Vec(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Pre_Vertex_z_Vec(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Nex_Vertex_Coor(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Nex_Vertex_Nor_Vec(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Nex_Vertex_x_Vec(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Nex_Vertex_y_Vec(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Nex_Vertex_z_Vec(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Tip_BaseLine(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Solid_El_Tip_BaseLine_Nor_Vec(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Solid_El_Tip_BaseLine_x_Vec(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Solid_El_Tip_BaseLine_y_Vec(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Solid_El_Tip_BaseLine_z_Vec(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Solid_El_Tip_BaseLine_T_theta(:)  !使用参差数组, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Tip_BaseLine_T_Matrix(:)  !使用参差数组, 2022-09-04.

  !--------------------
  integer Num_Check_T_Matrix !Tool_ThetaX_ThetaY_ThetaZ_3D_rotation.f遍历法计算局部坐标系转角的划分份数(默认等于180;越小计算越快)
  integer,allocatable::Cracks_Initial_Meshed(:)           !用于标记每个裂缝是否被初始化离散过了.
  integer,allocatable::Cracks_Initial_Adjusted(:)         !用于标记每个裂缝是否被调整过了.
  integer,allocatable::Cracks_Checked_and_Adjusted(:)     !用于标记每个裂缝是否被检查且调整过了. 2022-08-01.
  !--------------------
  integer,allocatable::Crack_Type_Status_3D(:,:)      !用于标记裂缝类型和裂缝状态.
                                                        !   + 第1列(裂缝类型)：                 =1，HF裂缝；=2，天然裂缝; =3,压后水力裂缝
                                                        !                                         注意:对于天然裂缝和压后水力裂缝，有可能还会变成HF裂缝
                                                        !   + 第2列(裂缝状态)：                 =1，HF裂缝压裂未完成；=2，HF裂缝压裂完毕
                                                        !   + 第3列(裂缝能否继续扩展)：         =1，能；=0；不能
                                                        !   + 第4列(裂缝是否已获得流体节点)：   =1，是; =0; 否
                                                        !   + 第5列(上一步裂缝是否发生了扩展)： =1，是; =0; 否
                                                        !   + 第6列(假如是从天然裂缝激活二次，则为对应的天然裂缝号)
  !3D天然裂缝相关.
  integer Key_NaCr_Type_3D                               !初始天然裂缝类型:=1,矩形;=2,圆形;=3,多边形.
  integer Num_Poly_Edges_NaCr                            !多边形初始天然裂缝的边数，默认为6.
  integer Key_NaCr_Cross                                 !是否允许初始裂缝交叉(默认为0,不允许).
  integer Key_NaCr_Growth                                !是否允许初始天然裂缝扩展(被HF沟通前)(默认为0,不允许).
  real(kind=FT) NaCr_3D_n_Vector(3)                      !初始裂缝的法线方向.
  real(kind=FT) NaCr_3D_n_Vector_Delta                   !法线方向的波动幅度(单位为度,默认为0).
  real(kind=FT) NaCr_3D_Size                             !对于矩形裂缝指的是边长,对于圆形裂缝指的是半径.
  real(kind=FT) NaCr_3D_Sz_Delta                         !尺寸的波动幅度(默认为0).
  real(kind=FT) NaCr_3D_Check_R                          !是否重叠的检测半径.
  real(kind=FT) NaCr_3D_Rect_Longside_Vector(3)          !狭长矩形的长边方向向量. 2023-02-28.
  real(kind=FT) NaCr_3D_Rect_L                           !狭长矩形的长边长度. 2023-02-28.
  real(kind=FT) NaCr_3D_Rect_W                           !狭长矩形的短边长度. 2023-02-28.
  real(kind=FT) NaCr_3D_Rect_L_Delta                     !狭长矩形的长边长度波动幅度. 2023-03-01.
  real(kind=FT) NaCr_3D_Rect_W_Delta                     !狭长矩形的短边长度波动幅度. 2023-03-01.
  real(kind=FT) NaCr_3D_Rect_Longside_Vector_Delta       !狭长矩形的长边方向向量波动幅度(单位为度,默认为0). 2023-03-01.
  !3D裂缝交叉状态.
  integer,ALLOCATABLE::Cracks_3D_Inter_Status(:,:)

  !3D增强单元相关. 2022-09-05. IMPROV2022090502.
  !integer(Kind=1),ALLOCATABLE:: Elem_Type_3D(:,:)           !IMPROV2022091803. Kind=1降低内存占用.
  integer,ALLOCATABLE:: Elem_Type_3D(:,:)
  integer,ALLOCATABLE:: c_POS_3D(:,:)
  !integer(Kind=1),ALLOCATABLE:: Enriched_Node_Type_3D(:,:)  !IMPROV2022091803. Kind=1降低内存占用.
  integer,ALLOCATABLE:: Enriched_Node_Type_3D(:,:)

  !real(kind=FT),ALLOCATABLE::Enriched_Node_Crack_n_Vector_3D(:,:,:)!增强节点对应的裂缝面的外法线向量,added on 2022-05-12.
  type(Ragged_Array_2D),allocatable::Enriched_Node_Crack_n_Vector_3D(:)  !使用参差数组. IMPROV2022091801.

  !3D Junction增强单元相关.
  !integer,ALLOCATABLE::Node_Jun_elem_3D(:,:)                      !Junction增强节点对应的Junction单元号,added on 2016-07-10.
  !integer,ALLOCATABLE::Jun_Ele_Negative_Cr_Num_3D(:,:)            !Juntion增强单元对应的被动裂缝号
  !real(kind=FT),ALLOCATABLE::Coors_Junction_3D(:,:,:)
  !使用参差数组:IMPROV2022090701.
  type(Ragged_Int_Array_1D),allocatable::Node_Jun_elem_3D(:)
  type(Ragged_Int_Array_1D),allocatable::Jun_Ele_Negative_Cr_Num_3D(:)
  type(Ragged_Array_2D),allocatable::Coors_Junction_3D(:)

  !integer,ALLOCATABLE::Ele_Num_Tip_Enriched_Node_3D(:,:)    !增强节点对应的增强单元号(参考单元号)
  type(Ragged_Int_Array_1D),allocatable::Ele_Num_Tip_Enriched_Node_3D(:)  !IMPROV2022091804

  integer n_h_Node_3D,n_t_Node_3D,n_j_Node_3D,n_hl_Node_3D,n_c_Node_3D
  !流固耦合矩阵Q: 采用参差数组. 2022-09-16. IMPROV2022091601.
  type(Ragged_Array_1D),allocatable::Coupled_Q_3D(:)
  type(Ragged_Int_Array_1D),allocatable::Coupled_Q_3D_Index(:) !IMPROV2022111101.

  !EBE-单元刚度矩阵相关. 2022-09-19.
  !real(kind=FT),ALLOCATABLE::storK_XFEM(:,:,:)
  type(Ragged_Array_2D),allocatable::storK_XFEM(:)
  !real(kind=FT),ALLOCATABLE::storK_XFEM_Old(:,:,:)
  type(Ragged_Array_2D),allocatable::storK_XFEM_Old(:)
  real(kind=FT),ALLOCATABLE::storK_FEM(:,:,:)
  real(kind=FT),ALLOCATABLE::storK_FEM_Sym(:,:)   !2022-11-10.
  !real(kind=FT),ALLOCATABLE::storK_XFEM_Updated(:,:,:)
  type(Ragged_Array_2D),allocatable::storK_XFEM_Updated(:)
  !其他
  !integer(kind=2),ALLOCATABLE:: Elem_num_Related_Cracks(:)       !每个单元相关联的增强裂缝数目. 2022-07-16. IMPROV2022091803.
  integer,ALLOCATABLE:: Elem_num_Related_Cracks(:)
  integer,ALLOCATABLE:: Elem_Related_Cracks(:,:)  !用于存储每个单元相关联的裂缝号. 最多Key_Ele_Max_Related_Cracks个裂缝. 后续裂缝不再增强.
  !固体单元渗透率. 2022-11-28. NEWFTU2022112801.
  real(kind=FT),ALLOCATABLE::Ele_Permeability_3D(:,:)
  !每天裂缝的边界线面积. 2023-02-22.
  real(kind=FT),allocatable::Cracks_Outline_Area(:)   !NEWFTU202302202.
  !
  !新奥相关.
  !
  integer num_XA_Input_Cracks                              !保存新奥传入的天然裂缝数目. 2023-03-23.
  real(kind=FT) XA_Min_Frac_Radius                         !保存天然裂缝半径阈值. 2023-03-23.
  real(kind=FT),ALLOCATABLE:: XA_Ini_Cracks(:,:,:)         !新奥传入的天然裂缝坐标. 2023-03-23.
  real(kind=FT),ALLOCATABLE:: XA_Ini_Crack_St(:)           !新奥传入的天然裂缝坐标. 2023-03-23.
  real(kind=FT),ALLOCATABLE:: XA_Ini_Crack_FracFriction(:) !新奥传入的天然裂缝坐标. 2023-03-23.
  !固体单元裂缝体积比. 2023-03-26. NEWFTU2023032601.
  real(kind=FT),ALLOCATABLE::Ele_VolumeRatio_3D(:)
  !2023-08-10. 3D DIM SIF related.
  real(kind=FT) SIFs_DIM_3D_Offset_Delta_Factor,SIFs_DIM_3D_r_1_Factor,SIFs_DIM_3D_r_2_Factor,SIFs_DIM_3D_r_k_Factor
end module Global_Crack_3D

!------------------
! 5.2 十字裂缝相关
!------------------
module Global_Cross
  use Global_Float_Type
  implicit none
  save
  integer Max_Num_Cross
  integer n_Cross_Node                                       !十字交叉增强节点数目
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !cccccccccccccccccc         程序规模控制参数        ccccccccccccccccccccccc
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  parameter (Max_Num_Cross  = 100   )                        !总的十字交叉最大数目
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !cccccccccccccccccc         程序规模控制参数        ccccccccccccccccccccccc
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  integer num_Cross                                          !总的十字交叉数目
  integer,ALLOCATABLE:: Elem_Type_Cross(:,:)                 !用于十字裂缝
  integer,ALLOCATABLE:: Enriched_Node_Type_Cross(:,:)        !用于十字裂缝
  integer,ALLOCATABLE:: c_POS_Cross(:,:)                     !用于十字裂缝
  integer,ALLOCATABLE:: Node_Cross_elem(:,:)                 !Cross增强节点对应的Cross单元号(2017-05-02)
  integer,ALLOCATABLE:: Cross_Point_Cr_num(:,:)              !每个十字交叉点对应的主次裂缝号
  integer,ALLOCATABLE:: Cross_Point_Ele_num(:)               !每个十字交叉点对应的单元号
  !real(kind=FT),ALLOCATABLE:: Cross_Point_Cr_Seg_num(:,:)   !用于十字型裂缝
  real(kind=FT),ALLOCATABLE:: Cross_Point_RABCD(:,:,:)       !每个十字交叉点对应的交点和ABCD点坐标
end module Global_Cross

!--------------
! 5.3 夹杂相关
!--------------
module Global_Inclusion
  use Global_Float_Type
  implicit none
  save
  integer Max_Num_Circ_Incl,Max_Num_Tri_Incl
  integer Max_Num_Quad_Incl,Max_Num_Penta_Incl
  integer Max_Num_Ellip_Incl,Max_Num_Incl,Max_Num_Poly_Incl
  integer Max_Num_Edges_Poly
  integer n_Incl_Node            !夹杂增强节点数目
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !cccccccccccccccccc         程序规模控制参数        ccccccccccccccccccccccc
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  parameter (Max_Num_Incl  = 500   )           !总的夹杂最大数目
  parameter (Max_Num_Circ_Incl  = 100   )      !最多100个圆形夹杂
  !parameter (Max_Num_Tri_Incl   = 100   )      !最多100个三角形夹杂
  !parameter (Max_Num_Quad_Incl  = 100   )      !最多100个四边形夹杂
  !parameter (Max_Num_Penta_Incl = 100   )      !最多100个五边形夹杂
  parameter (Max_Num_Poly_Incl = 100   )       !最多100个多边形夹杂
  parameter (Max_Num_Ellip_Incl = 100   )      !最多100个椭圆形夹杂
  parameter (Max_Num_Edges_Poly = 100   )      !每个多边形夹杂的边数最大数目
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !cccccccccccccccccc         程序规模控制参数        ccccccccccccccccccccccc
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  integer num_Inclusion                                         !总的夹杂数目
  integer num_Circ_Incl                                    !圆形夹杂,通过圆形和半径定义
  !integer num_Tri_Incl                                     !三角形夹杂,通过三个点的坐标定义
  !integer num_Quad_Incl                                    !四边形夹杂,通过4个点的坐标定义
  !integer num_Penta_Incl                                   !五边形夹杂,通过5个点的坐标定义
  integer num_Poly_Incl                                    !多边形夹杂
  integer num_Ellip_Incl                                   !椭圆形夹杂
  !圆形夹杂位置大小以及材料号的定义规则
  real(kind=FT) Circ_Inclu_Coor(Max_Num_Circ_Incl,3)       !(x,y,r)
  integer Circ_Inclu_Mat_Num(Max_Num_Circ_Incl)            !圆形夹杂的材料号
  !多边形夹杂位置大小以及材料号的定义规则
  real(kind=FT) Poly_Incl_Coor_x(Max_Num_Poly_Incl,Max_Num_Edges_Poly)       !多边形夹杂的x坐标
  real(kind=FT) Poly_Incl_Coor_y(Max_Num_Poly_Incl,Max_Num_Edges_Poly)       !多边形夹杂的y坐标
  integer Poly_Inclu_Edges_Num(Max_Num_Poly_Incl)          !每个多边形夹杂的边数(边数等于顶点数)
  integer Poly_Inclu_Mat_Num(Max_Num_Poly_Incl)            !多边形夹杂的材料号
  !闭合形式的多边形夹杂
  real(kind=FT) Poly_Incl_Coor_x_Cl(Max_Num_Poly_Incl,Max_Num_Edges_Poly)
  real(kind=FT) Poly_Incl_Coor_y_Cl(Max_Num_Poly_Incl,Max_Num_Edges_Poly)
  !增强单元相关
  integer,ALLOCATABLE:: Elem_Type_Incl(:,:)                !用于夹杂
  integer,ALLOCATABLE:: Enriched_Node_Type_Incl(:,:)       !用于夹杂
  integer,ALLOCATABLE:: c_POS_Incl(:,:)                    !用于夹杂
  !随机生成规则夹杂相关
  integer Key_Rand_Circ_Incl                 !是否随机生成圆形夹杂
  integer num_Rand_Circ_Incl                 !随机生成的圆形夹杂数目
  real(kind=FT) Rand_Circ_Incl_R             !随机生成的多边形夹杂外接圆平均半径
  real(kind=FT) Rand_Circ_Inc_R_Delta        !随机生成的多边形夹杂外接圆半径变化范围(+-)
  integer Key_Rand_Poly_Incl                 !是否随机生成多边形夹杂
  integer num_Rand_Poly_Incl                 !随机生成的多边形夹杂数目
  integer num_Vert_Poly_Incl                 !随机生成的多边形夹杂的边数
  real(kind=FT)Rand_Poly_Incl_R              !随机生成的多边形夹杂外接圆平均半径
  real(kind=FT)Rand_Poly_Inc_R_Delta         !随机生成的多边形夹杂外接圆半径变化范围(+-)
  !随机生成不规则夹杂相关
  integer Key_Rand_Poly_Incl_Irregular              !是否随机生成不规则多边形夹杂
  integer num_Rand_Poly_Incl_for_Each_Type(10)      !各级夹杂的数目,最多可定义10级尺寸
  real(kind=FT) Rand_Poly_Incl_R_Min_and_Max(10,2)  !各级夹杂的半径范围
  real(kind=FT) Rand_Poly_Incl_Irregular_Extension_Factor     !伸长率(1.0-3.0)
  real(kind=FT) Rand_Poly_Incl_Irregular_Inclination          !倾角(用于伸长率>1时)
  real(kind=FT) Rand_Poly_Incl_Irregular_R_Delta_Factor       !多边形生成过程中的半径变化系数:(0.0 to 1.0),delta_R = Factor*R
  real(kind=FT) Rand_Poly_Incl_Irregular_Angle_Factor         !多边形生成过程中的角度变化系数:(0.0 to 1.0)
end module Global_Inclusion

!-----------------------------------------------
! 6.单元边长、面积和体积（常用，所以单独设立）
!-----------------------------------------------
module Global_Elem_Area_Vol
  use Global_Float_Type
  implicit none
  save
  real(kind=FT) :: Max_Elem_Area,Min_Elem_Area,Ave_Elem_Area,Ave_Elem_L,Ave_Elem_Vol,Max_Elem_Vol,Min_Elem_Vol
  real(kind=FT) :: Max_Elem_Area_Enrich,Min_Elem_Area_Enrich!增强单元对应的
  real(kind=FT) :: Ave_Elem_Area_Enrich,Ave_Elem_L_Enrich
  real(kind=FT) :: Ave_Elem_Vol_Enrich
  real(kind=FT) :: Min_Ele_Edge_Length,Max_Ele_Edge_Length
  real(kind=FT) :: Ave_Elem_L_Enrich_Unlocalrefined    !局部加密前的增强单元特征长度(2021-08-22)
end module Global_Elem_Area_Vol

!----------------------------
!    7.材料参数
!----------------------------
module Global_Material
  use Global_Float_Type
  implicit none
  save
  real(kind=FT),ALLOCATABLE:: D(:,:,:)
  real(kind=FT),ALLOCATABLE:: D4(:,:,:)    !4x4的材料矩阵,Ref:Introduction_to_Nonlinear_Finite_Element_Analys一书中文版第5版200页
  real(kind=FT),ALLOCATABLE:: D_Comp(:,:,:) !Composite material
  real(kind=FT),ALLOCATABLE:: S(:,:,:)    !D的逆阵
  real(kind=FT),ALLOCATABLE:: St(:,:)
  real(kind=FT),ALLOCATABLE:: Sc(:,:)
  real(kind=FT),ALLOCATABLE:: T_Alpha(:)
  real(kind=FT),ALLOCATABLE:: KIc(:,:)
  real(kind=FT),ALLOCATABLE:: E(:,:)
  real(kind=FT),ALLOCATABLE:: v(:,:)
  real(kind=FT),ALLOCATABLE:: density(:)
  real(kind=FT),ALLOCATABLE:: thick(:)
  real(kind=FT) Global_K_m
  real(kind=FT),ALLOCATABLE:: Fd_k_MT(:,:,:)
  real(kind=FT),ALLOCATABLE:: Fd_c(:)
  real(kind=FT),ALLOCATABLE:: Lame_lambda(:)       !拉梅常数
  real(kind=FT),ALLOCATABLE:: Lame_mu(:)           !拉梅常数
  real(kind=FT),ALLOCATABLE:: Ele_ComMat_RotMatrix(:,:,:)   !单元复合材料的旋转矩阵
  real(kind=FT),ALLOCATABLE:: MC_dt(:)!Mohr-Coulomb准则黏塑性方法pseudo time step,Ref:MATLAB FEM Code - From Elasticity to Plasticity, Eq.6.10
  real(kind=FT),ALLOCATABLE:: MC_phi_deg(:)   !内摩擦角
  real(kind=FT),ALLOCATABLE:: MC_phi_rad(:)
  real(kind=FT),ALLOCATABLE:: MC_psi_deg(:)   !剪胀角
  real(kind=FT),ALLOCATABLE:: MC_psi_rad(:)
  real(kind=FT),ALLOCATABLE:: MC_c(:)         !粘聚力
  real(kind=FT),ALLOCATABLE:: D_for_cylindrical(:,:,:) !用于柱坐标系
  real(kind=FT) St_KIc_Conversion                      !抗拉强度和断裂韧度换算系数. KIc = St/St_KIc_Conversion. 2023-03-25.
end module Global_Material

!----------------------------
!   8.位移
!----------------------------
module Global_DISP
  use Global_Float_Type
  implicit none
  save
  real(kind=FT),ALLOCATABLE:: DISP(:),DISP_InSitu(:)
  real(kind=FT),ALLOCATABLE:: DISP_Cylinder(:)
  !2D Gauss点位移
  real(kind=FT),ALLOCATABLE:: DISP_x_Gauss(:)
  real(kind=FT),ALLOCATABLE:: DISP_y_Gauss(:)
  !3D Gauss点位移
  real(kind=FT),ALLOCATABLE:: DISP_z_Gauss(:)
end module Global_DISP

!----------------------------
!    9.水力压裂
!----------------------------
module Global_HF
  use Global_Float_Type
  implicit none
  save
  integer Max_Num_Frac              !程序允许的最大破裂步数
  integer Key_HF_num_Contact        !每个破裂步执行的接触迭代数目(默认仅在第一个流固耦合迭代步执行接触迭代,即=1)
  integer Max_Num_Inject_Crack      !程序运行的最大注水裂纹数
  integer Max_MS_Num_Crack
  integer Key_AAP                   !是否开启6,7号迭代器的AAP功能
  integer Key_Proppant              !是否考虑支撑剂
  integer Key_IniPre_PassOn         !各个破裂步初始迭代时是否继承上一破裂步最后的水压、开度等
                                    !                    0:不继承,total_time置零,裂缝开度从0开始迭代
                                    !                    1:继承,思路详见我的笔记,V3_P78
  real(kind=FT) SOR_factor       !NR迭代和割线法迭代逐次超松弛迭代系数，默认值为0.75
  integer Key_HF_Conv_Crite            !迭代收敛准则, 1: 通过裂缝开度判断
                                    !              2: 通过裂缝开度和水压
  integer Key_Cal_deltaTime         !计算时间增量的准则,1:矩形法,delta_V=delta_L*delta_w1,
                                    !                     算出的delta_V偏小,于是delta_Time偏小
                                    !                     Picard迭代必须采用矩形法,否则算出来的水压是线性下降到0的
                                    !                   2:梯形法,delta_L*0.5*(w1+w2)
                                    !                     算出的delta_V偏小,于是delta_Time偏大
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !cccccccccccccccccc         程序规模控制参数        ccccccccccccccccccccccc
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  parameter (Max_Num_Frac  = 200 )          !水力压裂分析最多200个破裂步
  parameter (Max_Num_Inject_Crack = 10)    !水力压裂分析最多10个注水裂纹
  parameter (Max_MS_Num_Crack = 20)        !分段压裂允许的压裂段数
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !cccccccccccccccccc         程序规模控制参数        ccccccccccccccccccccccc
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  integer Num_Frac                  !破裂步数
  integer ifra_Data_iter_num(Max_Num_Frac)  !每一个破裂步的第一迭代步对应的总迭代步数
                                    !仅在此步保存当前破裂步通用数据，如增强节点信息，
                                    !裂缝计算点信息等
  !integer Max_Picard_Iter,Max_NR_Iter,Max_Secant_Iter  !最大Picard迭代步数,最大NR迭代步数,最大割线法迭代步数
  integer Max_Picard_Iter,Max_NR_Iter!最大Picard迭代步数,最大NR迭代步数,最大割线法迭代步数
  real(kind=FT) Viscosity           !Viscosity of Water.
  real(kind=FT) Viscosity_Par_m     !动态粘度指数m.
  integer Key_Visco_Type            !压裂液粘度计算方式, 1: 粘度保持不变;2:粘度随支撑剂的浓度变化;3:时变粘度
  real(kind=FT) Visco_Zoom_Factor!                       动态粘度允许的最大增大倍数(仅用于Key_Visco_Type=1时)
  integer Key_Leakoff               !是否考虑压裂液的泄露(仅在破裂步大于1时激活)
  real(kind=FT) Coeff_Leak          !泄露系数
  real(kind=FT) Dia_of_Proppant     !支撑剂的最大直径
  real(kind=FT) Alpha_Picard        !Parameter of Picard iteration.
  !real(kind=FT) Picard_Tol          !Tolerance of Picard iteration.
  real(kind=FT) NR_Tol              !Tolerance of Newton-Raphson iteration.
  real(kind=FT) Secant_Tol          !割线法迭代的收敛容差
  integer Max_PNR_Iter              !P-NR迭代(先进行Picard迭代,再进行NR迭代)总的迭代次数
  integer Max_NR_AAP_Iter           !NR_AAP迭代最大次数
  integer Max_NR_MS_Iter            !用于分段压裂模拟的NR_MS迭代器各段压裂最大次数
  integer Max_NR_Red_AAP_Iter       !NR_Red_AAP迭代最大次数
  integer Num_Pic_PNR_Iter          !P-NR迭代先进行的Picard迭代次数
  integer Max_MNR_Iter              !MNR迭代总的迭代次数
  integer Max_MNR_Red_Iter          !MNR_Red迭代总的迭代次数
  integer Max_Num_Lnsrch            !每个NR迭代步最多执行的线搜索和回溯次数
  real(kind=FT) MNR_Tol             !Tolerance of MNR iteration.
  real(kind=FT) PNR_Tol             !Tolerance of P-NR iteration.
  real(kind=FT) NR_AAP_Tol          !Tolerance of NR_AAP iteration.
  real(kind=FT) NR_Red_AAP_Tol      !Tolerance of NR_Red_AAP iteration.
  real(kind=FT) NR_MS_Tol           !Tolerance of NR_AAP iteration.
  real(kind=FT) MNR_Red_Tol         !Tolerance of MNR_Red_Tol iteration.
  integer Key_Symm_HF               !是否是对称水力压裂模型
  integer Type_of_HF                ! = 1,断裂韧度支配;=2,粘度支配
  real(kind=FT) Viscosity_td_m,Viscosity_td_n !时变粘度参数
  !-------注水相关-------
  integer Inject_Crack_Num          !包含注水点的裂纹号
  !integer Inject_Cracks(Max_Num_Inject_Crack)!包含注水点的裂纹号
  !real(kind=FT) Inject_Qs(Max_Num_Inject_Crack)    !每个裂纹的注水量
  !real(kind=FT) Inject_Q                           !注水量
  real(kind=FT) Inject_Q_Time(200)                   !注水点时刻(最多20个时间点)
  real(kind=FT) Inject_Q_Val(200)                    !注水点流量值
  integer Key_Propp_Trans                           !是否考虑支撑剂的运移
  real(kind=FT) Max_c                               !允许的最大支撑剂浓度值
  real(kind=FT) Inject_c_Time(200)                   !注水点支撑剂浓度改变时刻
  real(kind=FT) Inject_c_Val(200)                    !注水点支撑剂浓度值
  real(kind=FT) Inject_P_Time(200)                   !注水点注水水压改变时刻
  real(kind=FT) Inject_P_Val(200)                    !注水点注水水压
  logical Propp_Trans_Start                         !逻辑变量,用于标记支撑剂是否开始了运移
  integer Counter_Num_iFrac(Max_Num_Frac)           !各破裂步结束对应的总的迭代次数
  real(kind=FT),ALLOCATABLE:: F_ConP(:)
  !--------分段压裂相关--------
  integer i_MS,MS_Crack_Num                              !分段裂缝段数(裂缝数,最多支持20级压裂)
  integer MS_NaturalCr_Num                               !分段压裂模型中的天然裂缝数目
  integer MS_Crack_Order(Max_MS_Num_Crack)               !各段压裂对应的初始裂缝号
  !integer MS_Each_Cr_Poi_Num(Max_MS_Num_Crack)          !分段压裂初始左边点数目
  !real(kind=FT) MS_Crack_Coor(Max_MS_Num_Crack,200,2)   !各段裂缝坐标
  real(kind=FT) MS_InP_Loc(Max_MS_Num_Crack,1:2)         !各段注水点坐标
  integer MS_Finish_Counter_Iter(Max_MS_Num_Crack)       !各段压裂完成时对应的总的迭代号
  real(kind=FT) HF_Theor_Time                            !压裂到一定长度的理论时间
  real(kind=FT) HF_Theor_Aper                            !压裂到一定长度的理论最大开度
  integer Key_Paper1_Alter
  integer Key_Paper1_finish
  real(kind=FT) Last_Inj_Pres                            !上一步注水点的水压
  !滑溜水分析
  real(kind=FT)  Current_SlipWater_P                     !当前的水压(整个裂缝网络一致)
  real(kind=FT)  Picard_Alpha                            !水力压裂Picard迭代系数(3D水力压裂)
  real(kind=FT)  Picard_Tol                              !水力压裂Picard迭代收敛容差
  !井筒相关, 2022-04-19
  integer Max_WB,Max_Stages,Max_Clust
  parameter (Max_WB       = 20)                              !最大井筒数目
  parameter (Max_Stages   = 20)                              !每个井筒的最大分段数目
  parameter (Max_Clust = 20)                                 !每个分段的最大裂缝数目
  integer num_Wellbore                                       !井筒数目，最多20个井筒，默认0个
  integer num_Points_WB(Max_WB)                              !最多20个井筒
  real(kind=FT) Wellbore_Coors(Max_WB,20,3)                  !井筒的点的坐标,每个井筒最多20个点
  integer num_Stages_Wellbores(Max_WB)                       !每个井筒的分段数
  integer num_Crs_Stages_Wellbores(Max_WB,Max_Stages)             !每个井筒每个分段的分段簇数(裂缝数)
  real(kind=FT) Injection_Q_Stages_Wellbores(Max_WB,Max_Stages)      !每个井筒每个分段的压裂液流量,单位：m^3/s
  real(kind=FT) Injection_T_Stages_Wellbores(Max_WB,Max_Stages)      !每个井筒每个分段的压裂液注入时间,单位：s
  integer Key_Gen_Ini_Crack_Wellbores                        !是否自动生成初始裂缝（注:自动生成的初始裂缝垂直于井筒）
                                                             !=1,生成矩形初始裂缝
                                                             !=2,生成圆形初始裂缝
                                                             !=3,生成多边形初始裂缝
  integer Num_Poly_Edges_NaCr_WB                             !多边形初始裂缝的边数
  real(kind=FT) Size_Ini_Crack_Wellbores                     !自动生成的初始裂缝尺寸(正方形)
  real(kind=FT) Wellbores_Start_Point(Max_WB,3)              !井筒上分段压裂的起点坐标和终点坐标(注:由于裂缝在各段均匀分布，所以初始裂缝不一定在端点位置)
  real(kind=FT) Wellbores_End_Point(Max_WB,3)                !井筒上分段压裂的起点坐标和终点坐标(注:由于裂缝在各段均匀分布，所以初始裂缝不一定在端点位置)
  integer Cracks_Stages_Wellbores(Max_WB,Max_Stages,Max_Clust)  !每个井筒每个分段对应的各个裂缝编号
  
  integer Key_3D_HF_SlipWater_fk_Type                          !2023-08-08. 3D清水压裂裂尖fk函数计算类型： NEWFTU2023080801.
                                                               !   =1,最大值达到KIc(default);=2,平均值达到KIc;=3,最小值达到KIc
                                                               !   注：仅在PhiPsi3D_Static_HF_SlipWater中起作用.          
  integer SlipWater_Max_Time_Steps_3D,SlipWater_Max_Pres_Steps_3D !3D SlipWater NR迭代时间步和压力步最大迭代次数. IMPROV2024022801.
  integer SlipWater_Time_Step_Conv_Check                          !3D SlipWater NR迭代时间步收敛检测(默认为0，即进行检测). IMPROV2024022802.
  integer SlipWater_Pres_Step_Conv_Check                          !3D SlipWater NR迭代时间步收敛检测(默认为0，即进行检测). IMPROV2024022802.
end module Global_HF

!----------------------------
!  10.裂缝面接触及支撑剂相关
!----------------------------
module Global_Contact
  use Global_Float_Type
  implicit none
  save
  integer,ALLOCATABLE:: Elem_Conta_Sta(:,:)   !当前接触迭代步每个单元的接触状态(相对于每条裂纹，(num_elem,Max_Num_Cr))
                                              ! 1: 状态,=0,未接触;=1,接触
  integer,ALLOCATABLE:: Elem_Conta_Sta_Last(:,:)   !上一接触迭代步每个单元的接触状态,(num_elem,2)
                                              ! 1: 状态,=0,未接触;=1,接触
  real(kind=FT),ALLOCATABLE:: Elem_Proppant_Coor(:,:)    !对于支撑剂问题,包含支撑剂单元的支撑剂直径、坐标
end module Global_Contact

!----------------------------
!    11.应力相关
!----------------------------
module Global_Stress
  use Global_Float_Type
  implicit none
  save
  !2D节点应力
  real(kind=FT),ALLOCATABLE:: Stress_xx_Node(:)
  real(kind=FT),ALLOCATABLE:: Stress_yy_Node(:)
  real(kind=FT),ALLOCATABLE:: Stress_xy_Node(:)
  real(kind=FT),ALLOCATABLE:: Stress_vm_Node(:)
  !2D节点应力热应力
  real(kind=FT),ALLOCATABLE:: TStress_xx_Node(:)
  real(kind=FT),ALLOCATABLE:: TStress_yy_Node(:)
  real(kind=FT),ALLOCATABLE:: TStress_xy_Node(:)
  real(kind=FT),ALLOCATABLE:: TStress_vm_Node(:)
  !3D节点应力补充
  real(kind=FT),ALLOCATABLE:: Stress_zz_Node(:)
  real(kind=FT),ALLOCATABLE:: Stress_yz_Node(:)
  real(kind=FT),ALLOCATABLE:: Stress_xz_Node(:)
  !2D Gauss点应力
  real(kind=FT),ALLOCATABLE:: Stress_xx_Gauss(:)
  real(kind=FT),ALLOCATABLE:: Stress_yy_Gauss(:)
  real(kind=FT),ALLOCATABLE:: Stress_xy_Gauss(:)
  real(kind=FT),ALLOCATABLE:: Stress_vm_Gauss(:)
  !2D Gauss点热应力
  real(kind=FT),ALLOCATABLE:: TStress_xx_Gauss(:)
  real(kind=FT),ALLOCATABLE:: TStress_yy_Gauss(:)
  real(kind=FT),ALLOCATABLE:: TStress_xy_Gauss(:)
  real(kind=FT),ALLOCATABLE:: TStress_vm_Gauss(:)
  !3D Gauss点应力补充
  real(kind=FT),ALLOCATABLE:: Stress_zz_Gauss(:)
  real(kind=FT),ALLOCATABLE:: Stress_yz_Gauss(:)
  real(kind=FT),ALLOCATABLE:: Stress_xz_Gauss(:)
  !单元应力状态,是否满足σ1-σ3 > Tol
  integer,ALLOCATABLE:: Ele_State_Stress_1_3(:)
  !2D Gauss点应力_无水压时地应力作用下的高斯应力场
  real(kind=FT),ALLOCATABLE:: Stress_xx_Gas_InSitu(:)
  real(kind=FT),ALLOCATABLE:: Stress_yy_Gas_InSitu(:)
  real(kind=FT),ALLOCATABLE:: Stress_xy_Gas_InSitu(:)
  real(kind=FT),ALLOCATABLE:: Stress_vm_Gas_InSitu(:)

  !2D Gauss点应力_真实水压(非净水压)作用下的高斯应力场
  real(kind=FT),ALLOCATABLE:: Stress_xx_Gas_InSitu2(:)
  real(kind=FT),ALLOCATABLE:: Stress_yy_Gas_InSitu2(:)
  real(kind=FT),ALLOCATABLE:: Stress_xy_Gas_InSitu2(:)
  real(kind=FT),ALLOCATABLE:: Stress_vm_Gas_InSitu2(:)
  !Gauss点处的初始应力场
  real(kind=FT),ALLOCATABLE:: InSitu_Strs_Gaus_xx(:,:)
  real(kind=FT),ALLOCATABLE:: InSitu_Strs_Gaus_yy(:,:)
  real(kind=FT),ALLOCATABLE:: InSitu_Strs_Gaus_xy(:,:)
  !3D补充
  real(kind=FT),ALLOCATABLE:: InSitu_Strs_Gaus_zz(:,:)
  real(kind=FT),ALLOCATABLE:: InSitu_Strs_Gaus_yz(:,:)
  real(kind=FT),ALLOCATABLE:: InSitu_Strs_Gaus_xz(:,:)
  !节点处的初始应力场
  real(kind=FT),ALLOCATABLE::Str_xx_InSitu(:),Str_yy_InSitu(:),Str_xy_InSitu(:),Str_vm_InSitu(:)
  !3D补充
  real(kind=FT),ALLOCATABLE::Str_zz_InSitu(:),Str_yz_InSitu(:),Str_xz_InSitu(:)
  !圆柱坐标系
  real(kind=FT),ALLOCATABLE:: Stress_Crr_Node(:)
  real(kind=FT),ALLOCATABLE:: Stress_Ctt_Node(:)  !tt表示theta_theta
  real(kind=FT),ALLOCATABLE:: Stress_Czz_Node(:)
  real(kind=FT),ALLOCATABLE:: Stress_Crt_Node(:)
  real(kind=FT),ALLOCATABLE:: Stress_Ctz_Node(:)
  real(kind=FT),ALLOCATABLE:: Stress_Crz_Node(:)
  real(kind=FT),ALLOCATABLE:: Stress_Cvm_Node(:)
  !以下变量用于快速定义初始应力场（不分区域）.
  real(kind=FT) InSitu_S1_3D
  real(kind=FT) InSitu_S2_3D
  real(kind=FT) InSitu_S3_3D
  real(kind=FT) InSitu_S1_nv_3D(3)
  real(kind=FT) InSitu_S2_nv_3D(3)
  real(kind=FT) InSitu_S3_nv_3D(3)
  !非均匀初始应力场(x,y,z方向). 2022-07-06. NEWFTU2022070601.
  integer Key_Nonuniform_InSitu_X_with_Z                        !X方向给定不均匀初始应力场.
  real(kind=FT) InSitu_Sx_3D_Seg_Strs_X_with_Z(100)             !分成n段.
  real(kind=FT) InSitu_Sx_3D_Seg_Loca_X_with_Z(100)             !坐标位置n+1.
  integer Key_Nonuniform_InSitu_X_with_Y                        !X方向给定不均匀初始应力场.
  real(kind=FT) InSitu_Sx_3D_Seg_Strs_X_with_Y(100)             !分成n段.
  real(kind=FT) InSitu_Sx_3D_Seg_Loca_X_with_Y(100)             !坐标位置n+1.
  integer Key_Nonuniform_InSitu_Y_with_Z                        !Y方向给定不均匀初始应力场.
  real(kind=FT) InSitu_Sy_3D_Seg_Strs_Y_with_Z(100)             !分成n段.
  real(kind=FT) InSitu_Sy_3D_Seg_Loca_Y_with_Z(100)             !坐标位置n+1.

  integer Key_Nonuniform_InSitu_Y_with_X                        !Y方向给定不均匀初始应力场.
  real(kind=FT) InSitu_Sy_3D_Seg_Strs_Y_with_X(100)             !分成n段.
  real(kind=FT) InSitu_Sy_3D_Seg_Loca_Y_with_X(100)             !坐标位置n+1.
  integer Key_Nonuniform_InSitu_Z_with_X                        !Z方向给定不均匀初始应力场.
  real(kind=FT) InSitu_Sz_3D_Seg_Strs_Z_with_X(100)             !分成n段.
  real(kind=FT) InSitu_Sz_3D_Seg_Loca_Z_with_X(100)             !坐标位置n+1.
  integer Key_Nonuniform_InSitu_Z_with_Y                        !Z方向给定不均匀初始应力场.
  real(kind=FT) InSitu_Sz_3D_Seg_Strs_Z_with_Y(100)             !分成n段.
  real(kind=FT) InSitu_Sz_3D_Seg_Loca_Z_with_Y(100)             !坐标位置n+1.
  !Gauss点处的初始应变场
  real(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_xx(:,:)
  real(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_yy(:,:)
  real(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_xy(:,:)
  !3D补充
  real(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_zz(:,:)
  real(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_yz(:,:)
  real(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_xz(:,:)
  !单元的初始应力. XA. 2023-03-24.
  real(kind=FT),ALLOCATABLE:: XA_Ele_InSitu_S1_Vector(:,:)
  real(kind=FT),ALLOCATABLE:: XA_Ele_InSitu_S2_Vector(:,:)
  real(kind=FT),ALLOCATABLE:: XA_Ele_InSitu_S3_Vector(:,:)
  real(kind=FT),ALLOCATABLE:: XA_Ele_InSitu_S1_S2_S3(:,:)
  !单元应力. XA. 2023-03-26.
  real(kind=FT),ALLOCATABLE:: XA_Ele_Stress(:,:)
end module Global_Stress

!----------------------------
! 11.2 应变相关 (2021-09-10)
!----------------------------
module Global_Strain
  use Global_Float_Type
  implicit none
  save
  !2D节点应力
  real(kind=FT),ALLOCATABLE:: Strain_xx_Node(:)
  real(kind=FT),ALLOCATABLE:: Strain_yy_Node(:)
  real(kind=FT),ALLOCATABLE:: Strain_xy_Node(:)
  real(kind=FT),ALLOCATABLE:: Strain_vm_Node(:)
  !圆柱坐标系
  real(kind=FT),ALLOCATABLE:: Strain_Crr_Node(:)
  real(kind=FT),ALLOCATABLE:: Strain_Ctt_Node(:)  !tt表示theta_theta
  real(kind=FT),ALLOCATABLE:: Strain_Czz_Node(:)
  real(kind=FT),ALLOCATABLE:: Strain_Crt_Node(:)
  real(kind=FT),ALLOCATABLE:: Strain_Ctz_Node(:)
  real(kind=FT),ALLOCATABLE:: Strain_Crz_Node(:)
  real(kind=FT),ALLOCATABLE:: Strain_Cvm_Node(:)

  !2D节点应力热应力
  real(kind=FT),ALLOCATABLE:: TStrain_xx_Node(:)
  real(kind=FT),ALLOCATABLE:: TStrain_yy_Node(:)
  real(kind=FT),ALLOCATABLE:: TStrain_xy_Node(:)
  real(kind=FT),ALLOCATABLE:: TStrain_vm_Node(:)
  !3D节点应力补充
  real(kind=FT),ALLOCATABLE:: Strain_zz_Node(:)
  real(kind=FT),ALLOCATABLE:: Strain_yz_Node(:)
  real(kind=FT),ALLOCATABLE:: Strain_xz_Node(:)
  !2D Gauss点应力
  real(kind=FT),ALLOCATABLE:: Strain_xx_Gauss(:)
  real(kind=FT),ALLOCATABLE:: Strain_yy_Gauss(:)
  real(kind=FT),ALLOCATABLE:: Strain_xy_Gauss(:)
  real(kind=FT),ALLOCATABLE:: Strain_vm_Gauss(:)
  !2D Gauss点热应力
  real(kind=FT),ALLOCATABLE:: TStrain_xx_Gauss(:)
  real(kind=FT),ALLOCATABLE:: TStrain_yy_Gauss(:)
  real(kind=FT),ALLOCATABLE:: TStrain_xy_Gauss(:)
  real(kind=FT),ALLOCATABLE:: TStrain_vm_Gauss(:)
  !3D Gauss点应力补充
  real(kind=FT),ALLOCATABLE:: Strain_zz_Gauss(:)
  real(kind=FT),ALLOCATABLE:: Strain_yz_Gauss(:)
  real(kind=FT),ALLOCATABLE:: Strain_xz_Gauss(:)
  !单元应力状态,是否满足σ1-σ3 > Tol
  integer,ALLOCATABLE:: Ele_State_Strain_1_3(:)
  !2D Gauss点应力_无水压时地应力作用下的高斯应力场
  real(kind=FT),ALLOCATABLE:: Strain_xx_Gas_InSitu(:)
  real(kind=FT),ALLOCATABLE:: Strain_yy_Gas_InSitu(:)
  real(kind=FT),ALLOCATABLE:: Strain_xy_Gas_InSitu(:)
  real(kind=FT),ALLOCATABLE:: Strain_vm_Gas_InSitu(:)

  !2D Gauss点应力_真实水压(非净水压)作用下的高斯应力场
  real(kind=FT),ALLOCATABLE:: Strain_xx_Gas_InSitu2(:)
  real(kind=FT),ALLOCATABLE:: Strain_yy_Gas_InSitu2(:)
  real(kind=FT),ALLOCATABLE:: Strain_xy_Gas_InSitu2(:)
  real(kind=FT),ALLOCATABLE:: Strain_vm_Gas_InSitu2(:)
  !Gauss点处的初始应变场. BUGFIX2022070906. 全局变量重复定义.
  !REAL(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_xx(:,:)
  !REAL(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_yy(:,:)
  !REAL(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_xy(:,:)
  !3D补充
  !REAL(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_zz(:,:)
  !REAL(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_yz(:,:)
  !REAL(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_xz(:,:)
  !节点处的初始应变场
  real(kind=FT),ALLOCATABLE::Strain_xx_InSitu(:),Strain_yy_InSitu(:),Strain_xy_InSitu(:),Strain_vm_InSitu(:)
  !3D补充
  real(kind=FT),ALLOCATABLE::Strain_zz_InSitu(:),Strain_yz_InSitu(:),Strain_xz_InSitu(:)
end module Global_Strain

!----------------------------
!    12.后处理相关
!----------------------------
module Global_POST
  use Global_Float_Type
  implicit none
  save
  integer Key_Post_CS_G_Coor         !是否保存Gauss点坐标Calculate and Save Gauss Coors
  integer Key_Post_CS_G_Disp         !是否计算并保存Gauss点位移(默认值为0)
  integer Key_Post_CS_G_Strs         !是否计算并保存Gauss点应力(默认值为0)
  integer Key_Post_S_Dof_F           !是否保存各自由度的载荷值,以便用于后处理(默认值为0)
  integer Key_Post_Cracked_Ele       !是否计算并保存破裂的单元,根据主应力差确定,σ1-σ3>Tol(默认值为0)
  integer Key_Post_S_TanDisp         !是否计算并保存裂缝的切向相对位移
  real(kind=FT) Tol_Stress_1_3       !是否满足σ1-σ3 > Tol中的Tol的值
  integer Key_Node_Value             !节点应力计算算法(=1,直接平均法(默认); =2,最小二乘匹配法)
  integer Key_Save_vtk               !是否保存vtk文件(默认为1,保存)
  integer Key_Simple_Post            !简洁后处理，仅保存基本数据，如节点位移、裂缝开度等. 2022-06-25.
  integer Key_Save_Nothing           !不保存任何数据. 2022-09-06. NEWFTU2022090601.
  integer Key_Post_Elements_Gauss_Num!是否保存每个单元的Gauss积分点数目. 2022-07-16.
  integer Key_Get_Permeability       !计算并保存裂缝渗透率. 2022-11-26.
end module Global_POST

!----------------------------------------
!  13.随机生成小球的多体动力学模拟相关变量
!----------------------------------------
module Global_Rigid_Balls
  use Global_Float_Type
  implicit none
  save
  real(kind=FT) W_Ball_Model     !模型的宽度
  real(kind=FT) H_Ball_Model     !模型的高度
  real(kind=FT) Ave_R_Ball       !小球的平均半径
  real(kind=FT) Delta_R_Ball     !小球半径的变化范围
  real(kind=FT) Max_R_Ball       !小球的最大半径
  real(kind=FT) Min_R_Ball       !小球的最小半径
  integer num_Balls              !小球的数目
end module Global_Rigid_Balls

!------------------------------------------------
!   14.场问题相关(也包括比奥固结之孔隙水压力)
!------------------------------------------------
module Global_Field_Problem
  use Global_Float_Type
  implicit none
  save
  integer Num_Fd_Bou_fixed                      !场变量值为0的节点数目
  integer,ALLOCATABLE::Fd_Bou_fixed(:)          !场变量的值为0的节点号
  integer Num_Fd_Bou_vl                         !定义的场变量边值数目(包括0和非0)
  real(kind=FT),ALLOCATABLE::Fd_Bou_vl(:,:)     !定义的场变量边值节点号和边值的值(包括0和非零)
  integer Num_Fd_Bou_vl_nz                      !定义的场变量边值数目(非0)
  real(kind=FT),ALLOCATABLE::Fd_Bou_vl_nz(:,:)  !定义的场变量边值节点号和边值的值(非零)
  integer Num_Fd_Ini_vl                         !定义的场变量初值数目(包括0和非0)
  real(kind=FT),ALLOCATABLE::Fd_Ini_vl(:,:)     !定义的场变量初值节点号和初值的值(包括0和非0)

  integer Num_Fd_Bou_qn
  !integer Num_Fd_Bou_qx,Num_Fd_Bou_qy
  real(kind=FT),ALLOCATABLE::Fd_Bou_qn(:,:)     !n方向流量边界条件
  !real(kind=FT),ALLOCATABLE::Fd_Bou_qx(:,:)    !x方向流量边界条件
  !real(kind=FT),ALLOCATABLE::Fd_Bou_qy(:,:)    !y方向流量边界条件
  !real(kind=FT) Fd_kxx                          !系数kxx
  !real(kind=FT) Fd_kyy                          !系数kyy
  !real(kind=FT) Fd_c                            !场问题瞬态分析相关,类似于热传导问题的比热
  !real(kind=FT) Fd_k_MT(2,2)
  real(kind=FT),ALLOCATABLE::Fd_Value(:)        !节点场值
  real(kind=FT),ALLOCATABLE::Fd_ele_k_MT(:,:,:) !每个单元的k矩阵(可能随时间变化,所以需要保存起来)
  real(kind=FT),ALLOCATABLE::Fd_Flux_x(:)       !节点x方向流量值
  real(kind=FT),ALLOCATABLE::Fd_Flux_y(:)       !节点y方向流量值
  integer Key_Fd_Body_Source                    !计算区域内是否有流量源(如混凝土浇筑产生的热源)
  real(kind=FT) Fd_Body_Source                           !均匀流量源的大小
  !以下是Biot固结孔隙水压力相关
  real(kind=FT),ALLOCATABLE::Porous_P(:)        !每个节点的孔隙水压力
  real(kind=FT),ALLOCATABLE::Biot_c_MAT(:,:)    !比奥固结c矩阵,详见Smith_5th_P55或V5_P72
  !时间步及时间积分相关
  integer Fd_IDy_Num_Iteras                     !隐式动态分析步数
  real(kind=FT) Delt_Time_Trapez                !广义梯形时间积分时间增量大小
  !以下是页岩气产量评估相关(Key_Analysis_Type=17)
  integer Key_Gas_Production                    !Key_Analysis_Type = 16时可用
  integer GasP_num_Fractures                    !总的压裂裂缝数目
  real(kind=FT) GasP_Thic_Reservoir             !储层厚度
  integer GasP_Well_Nodes(100)                  !
  real(kind=FT) P_Langmuir                      !Langmuir压力
  real(kind=FT) V_Langmuir                      !Langmuir体积
  real(kind=FT) Density_gst                     !标准状态下页岩气的密度
  real(kind=FT) Width_of_crack
  integer Key_Langmuir_Source                   !是否考虑解析气体
  real(kind=FT) porosity_Shale                  !页岩的孔隙度
  integer Key_Changing_BHP                      !是否是变化的抽采压力
  integer Num_BHP_curve_point
  real(kind=FT),ALLOCATABLE:: BHP_Curve(:,:)    !井底压力曲线的时间压力曲线
  real(kind=FT) Gas_P_stress_x,Gas_P_stress_y   !产量评估分析地应力水平
  integer Key_Changing_Kf                       !是否考虑裂缝渗透率的变化(依据论文拟合数据曲线)
  integer Key_Proppant_Active                   !是否激活支撑剂,若该选项激活,则需要先执行水力压裂分析,并得到了wpnp文件
  integer Key_Proppant_Creep                    !是否考虑支撑剂的蠕变
  integer Key_Proppant_Crush                    !是否考虑支撑剂的破碎
  real(kind=FT) Proppant_Strength               !支撑剂的强度
  real(kind=FT) Proppant_visco_factor           !支撑剂的粘度系数
  real(kind=FT) Rock_visco_factor               !围岩的粘度系数
  !-----------场问题XFEM相关-------------
  integer Fd_n_h_Node,Fd_n_t_Node,Fd_n_j_Node,Fd_n_hl_Node,Fd_n_c_Node,Fd_n_cross_Node,Fd_n_Incl_Node
  integer Fd_Usual_Freedom,Fd_Enrich_Freedom
  integer ,ALLOCATABLE:: Fd_c_POS(:,:)
  integer ,ALLOCATABLE:: Fd_c_POS_Hl(:,:)
  integer ,ALLOCATABLE:: Fd_c_POS_Cross(:,:)
  integer ,ALLOCATABLE:: Fd_c_POS_Incl(:,:)
  integer Key_Fd_TipEnrich  !场问题裂尖增强方案, 0: 无裂尖增强,裂尖自动调整到单元边界上
                            !              1: 强间断,一项,sqrt(r)*sin(theta/2)
                            !              2: 弱间断,一项,sqrt(r)*cos(theta/2)
  integer,ALLOCATABLE:: Fd_Ele_yes_FEM_asemd(:)         !用于标记该单元的FEM单元刚度是否已经组集到总刚中
  integer,ALLOCATABLE:: Fd_EleGaus_yes_FEM_asemd(:,:)   !用于标记该单元该高斯点的FEM单元刚度是否已经组集到总刚中
  real(kind=FT),ALLOCATABLE:: Fd_Gauss_CoorX(:)
  real(kind=FT),ALLOCATABLE:: Fd_Gauss_CoorY(:)
  real(kind=FT),ALLOCATABLE:: Field_Value_Gauss(:)
  logical Fd_Yes_XFEM
end module Global_Field_Problem

!----------------------
!15.分子动力学模拟相关
!----------------------
module Global_MD
  use Global_Float_Type
  implicit none
  save
  integer MD_num_molecule              !分子的数目
  real(kind=FT) MD_mss_molecule        !分子的质量
  integer MD_num_time_step             !时间步数
  real(kind=FT) MD_Delt_Time           !时间步长
  real(kind=FT) MD_Dimension_x         !运动区域x方向尺寸
  real(kind=FT) MD_Dimension_y         !运动区域y方向尺寸
  real(kind=FT) MD_Dimension_z         !运动区域z方向尺寸
  integer MD_step_print_num            !平面显示间隔计算步数
  integer MD_step_save_num             !数据保存间隔计算步数
end module Global_MD

!----------------------
!  16.塑性分析相关
!----------------------
module Global_Plasticity
  use Global_Float_Type
  implicit none
  save
  real(kind=FT),ALLOCATABLE:: dsdeEl(:,:,:,:)   !每个单元每个Gauss点的弹性D矩阵
  real(kind=FT),ALLOCATABLE:: dsdePl(:,:,:,:)   !每个单元每个Gauss点的D矩阵
  real(kind=FT),ALLOCATABLE:: STATEV(:,:,:)     !每个单元每个Gauss点的状态变量
  real(kind=FT),ALLOCATABLE:: Last_U_of_Ele(:,:)!每个单元4个节点的8个位移分量
end module Global_Plasticity

!----------------------
!   17.粘聚裂缝相关
!----------------------
module Global_Cohesive
  use Global_Float_Type
  implicit none
  save
  integer Coh_Constitutive_type             !Constitutive model of the F-w curve
  real(kind=FT) Coh_Width_Critical1         !极限拉伸开度1,此开度对应极限牵引力,Coh_Constitutive_type=1时可用
  real(kind=FT) Coh_Width_Critical2         !极限拉伸开度2,此开度下牵引力为0
  real(kind=FT) Coh_f_Ultimate              !最大牵引力,裂尖的牵引力
  integer Coh_Tangential_Key                !是否考虑切向牵引力,若考虑,应给出对应的参数
  real(kind=FT) Coh_Width_Critical1_T       !极限拉伸开度1,此开度对应极限切向牵引力,Coh_Constitutive_type=1时可用
  real(kind=FT) Coh_Width_Critical2_T       !极限拉伸开度2,此开度下切向牵引力为0
  real(kind=FT) Coh_f_Ultimate_T            !最大牵引力,裂尖的切向牵引力
  integer,ALLOCATABLE:: Elem_Coh_Sta(:,:)   !每个单元的粘聚裂缝状态(相对于每条裂纹，(num_elem,Max_Num_Cr))
                                            !         状态,=0,非粘聚裂缝;=1,粘聚裂缝
end module Global_Cohesive


!----------------------
! 18.近场动力学模拟相关
!----------------------
module Global_PD
  use Global_Float_Type
  implicit none
  save
  integer PD_num_points              !点的数目
  !REAL(kind=FT) MD_mss_molecule        !分子的质量
  !integer MD_num_time_step             !时间步数
  !REAL(kind=FT) MD_Delt_Time           !时间步长
  !REAL(kind=FT) MD_Dimension_x         !运动区域x方向尺寸
  !REAL(kind=FT) MD_Dimension_y         !运动区域y方向尺寸
  !REAL(kind=FT) MD_Dimension_z         !运动区域z方向尺寸
  integer PD_step_print_num            !平面显示间隔计算步数
  integer PD_step_save_num             !数据保存间隔计算步数
  integer PD_num_time_step!
  integer PD_ndivx        !ndivx: Number of divisions in x direction - except boundary region
  integer PD_ndivy        !ndivy: Number of divisions in y direction - except boundary region
  integer PD_nbnd         !nbnd: Number of divisions in the boundary region
  integer PD_maxfam       !Maximum number of material points inside a horizon of a material point
  integer PD_Delt_Time
  real(kind=FT),ALLOCATABLE:: PD_coor(:,:)
  real(kind=FT),ALLOCATABLE:: PD_pforce(:,:)
  real(kind=FT),ALLOCATABLE:: PD_pforceold(:,:)
  real(kind=FT),ALLOCATABLE:: PD_bforce(:,:)
  real(kind=FT),ALLOCATABLE:: PD_stendens(:,:)
  real(kind=FT),ALLOCATABLE:: PD_fncst(:,:)
  real(kind=FT),ALLOCATABLE:: PD_disp(:,:)
  real(kind=FT),ALLOCATABLE:: PD_vel(:,:)
  real(kind=FT),ALLOCATABLE:: PD_velhalfold(:,:)
  real(kind=FT),ALLOCATABLE:: PD_velhalf(:,:)
  real(kind=FT),ALLOCATABLE:: PD_acc(:,:)
  real(kind=FT),ALLOCATABLE:: PD_massvec(:,:)

  real(kind=FT),ALLOCATABLE:: PD_enddisp(:)
  real(kind=FT),ALLOCATABLE:: PD_endtime(:)
  real(kind=FT),ALLOCATABLE:: PD_dmg(:)
  integer,ALLOCATABLE:: PD_numfam(:), PD_pointfam(:)
  integer,ALLOCATABLE:: PD_nodefam(:)
  integer,ALLOCATABLE:: PD_fail(:,:)

end module Global_PD


!----------------------
! 19.非线性(NL)分析相关
!----------------------
module Global_NonLinear
      use Global_Float_Type
      implicit none
      save
      real(kind=FT) NL_TIMS(1000,5)      !每行代表一个载荷子步设置,各列(起始时间,结束时间,时间增量,初始载荷因子,终止载荷因子)
      INTEGER       NL_ITRA             !N-R最大迭代数目
      real(kind=FT) NL_ATOL             !允许的最大残差的norm2模
      INTEGER       NL_NTOL             !允许的最大载荷二分数目
      real(kind=FT) NL_TOL              !N-R迭代收敛容差
      integer       NL_NLOAD            !载荷步数
      real(kind=FT),ALLOCATABLE:: NL_Delta_U(:)  !位移增量
end module Global_NonLinear

!!---------------------------------------
!! 20.HYPLAS非线性(NL)分析相关,2021-07-17
!!---------------------------------------
!module Global_NonLinear_HYPLAS
!  use Global_Float_Type
!  implicit none
!  save
!  real(kind=FT) NL_Time_Steps(1000,5)      !每行代表一个载荷子步设置,各列(起始时间,结束时间,时间增量,初始载荷因子,终止载荷因子)
!end module Global_NonLinear_HYPLAS

!---------------------------------------
! 21.表面载荷相关. 2023-01-21.
!---------------------------------------
module Global_Surface_Load
  use Global_Float_Type
  use Global_Ragged_Array_Real_Classs
  use Global_Ragged_Array_Int_Classs
  implicit none
  save
  character(256) File_Surface_Load(100)  !表面载荷文件名后缀，由ANSYS生成. 2023-01-21.
  integer Num_Surface_Loads
  type(Ragged_Int_Array_2D),allocatable::Surface_Load_Elements_Nodes(:)  !各个表面载荷对应的单元和节点，由Ansys2PhiPsi_3D_Surface_Pre.mac输出.
  type(Ragged_Array_1D),allocatable::Surface_Load_Elements_Area(:)  !各个表面载荷对应的单元面积.
  type(Ragged_Array_2D),allocatable::Surface_Load_Elements_Normal(:)  !各个表面载荷对应的单元外法线向量.
  real(kind=FT) Surface_Pressure(100)        !面载荷大小. 压为正.
end module Global_Surface_Load

!----------------------------------------------
! 22.3D水力压裂实验仿真相关参数. 2023-01-23.
!----------------------------------------------
module Global_3D_HF_Experiment
  use Global_Float_Type
  use Global_Ragged_Array_Real_Classs
  use Global_Ragged_Array_Int_Classs
  implicit none
  save
  integer HFE_Surface_Load_Num                   !压裂面载荷号
  real(kind=FT) HFE_Initial_Injection_Rate       !起裂前的初始注入流量，单位是ml/min.
  integer HFE_Hole_Mat_Number                    !钻孔加压段对应的材料号.
  real(kind=FT) HFE_Initial_Try_Pressure         !给定的初始孔壁压力. 2023-04-18.
  real(kind=FT) HFE_Initial_Pressure_Step_Size   !起裂分析压力加载增量.
end module Global_3D_HF_Experiment

!!-----------------------------------------
!! 23. XinAo相关. 新奥相关. 2023-03-14.
!!-----------------------------------------
!module Global_XA
!  use Global_Float_Type
!  implicit none
!  save
!  integer,ALLOCATABLE::XA_size_local(:)
!  integer,ALLOCATABLE::XA_all_local(:,:)
!end module Global_XA

!-----------------------------------------
! 23. Read_kpp相关. 2023-08-24.
!-----------------------------------------
module Global_Read_kpp
  use Global_Float_Type
  implicit none
  save
  real(kind=FT) Read_kpp_Na_CRACK3D_COOR(100,10,3)
  real(kind=FT) Read_kpp_Na_Crack3D_Cir_Coor(100,7)
  integer Read_kpp_Each_NaCr3D_Poi_Num(100)
end module Global_Read_kpp

!------------------------------------------
!   XX. Element3D类, 2022-05-01， 用于测试
!------------------------------------------
!     module Element3D_class
!         use Global_Float_Type
!         implicit none
!         type,public :: Element3D
!             real(kind=FT) volume
!         end type Element3D
!    end module Element3D_class

