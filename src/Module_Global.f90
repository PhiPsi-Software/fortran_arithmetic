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
  !���������ڼ���ˮ��ѹ������ʱ���ý������׼ȷ
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
  !����
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
  !ע��:��Ҫ�����޸�Tool_Function_GAMMA.f�е�DGAMMA����!!!!!!
  !     ����֤�����Բ��޸�,2019-04-27
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
  !����
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
!   01. �β�������(Real). 2022-09-02.
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
!   02. �β�������(Int). 2022-09-03.
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
!   03.1 ϡ�����ģ��. 1D����. 2022-09-14.
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
!!   03.2 ϡ�����ģ��. 2D����. 2022-09-14.
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
!!   03.3 ϡ�����ģ��. 3D����. 2022-09-14.
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
!!   03.4 ϡ�����ģ��. 4D����. 2022-09-14.
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
!!  03.5 ϡ�����ģ��. 1D Real. 2022-09-14.
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
!!  03.6 ϡ�����ģ��. 2D Real. 2022-09-14.
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
!!  03.7 ϡ�����ģ��. 3D Real. 2022-09-14.
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
!!  03.8 ϡ�����ģ��. 4D Real. 2022-09-14.
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
!     1.ͨ��ȫ�ֱ���
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
  real(kind=FT) Factor_Propagation                  !������չ����ϵ��.
  real(kind=FT) Propagation_Length                  !�ѷ���չ����,�������ò�����>0,��Factor_Propagation��������,default to -99.0
  integer Key_Propa_Type                            !��չ����:=1,�̶�������չ;=2,ʵ�ʲ�����չ(����ƣ�ͷ����Ͷ�̬����,�Զ��߼�����)
  integer Max_Step,Max_Itera
  integer max_nthreads                              !����ܵ��߳���Ŀ
  character(80) :: mac_Address
  character(80) :: CPU_ID,CPU_Type,DISK_ID,MAC_ID
  character(1) :: String_Connector  !"/" for Linux or "\" for windows.
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !cccccccccccccccccc         �����ڲ����Ʋ���        ccccccccccccccccccccccc
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  parameter (Delta_Factor_Edge    = 0.001D0 )       !ƫ�������Ʋ�������Ե���Ƶ�ƫ����,ǧ��֮һ
  parameter (Delta_Factor_Aper    = 0.001D0 )       !ƫ�������Ʋ�����������ƫ��,���ڼ����ѷ쿪��,���֮һ
                                                    !0.001*��Ԫƽ������
  parameter (Delta_Factor_Junc    = 0.001D0 )       !ƫ�������Ʋ�����Junction���ļ������Ҫƫ��һ��,�����������Ŀ��Ȼ����
                                                    !0.001*��Ԫƽ������
  parameter (Max_Step        = 1000  )              !����غɲ���, Ĭ��:1000
  parameter (Max_Itera       = 10000 )              !������������10000
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !cccccccccccccccccc         �����ڲ����Ʋ���        ccccccccccccccccccccccc
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  integer Key_Heaviside_Value
  integer Key_Cond_Number                           !���㲢��ʾ������,������Lapack�����,Ĭ�Ϲر�
  integer Key_Determinant                           !���㲢��ʾ�նȾ���K������ʽ(��֧��ϡ��������������֧��EBE-PCG�����),Ĭ�Ϲر�
  integer Key_Eigenvalue                            !���㲢��ʾ�նȾ���K������ֵ(��֧��ϡ��������������֧��EBE-PCG�����),Ĭ�Ϲر�
  integer Key_BLAS                                  !ʹ��BLAS��(����Intel Fortran���Զ�����),���Խ����Ӱ��. Ĭ��Ϊ0. ʹ��BLAS������ӳ���:
                                                    !EBE_XFEM_PCG_3D_with_K.f90
  integer Key_Hole_Value
  real(kind=FT) Delta_Factor_SIFs
  integer Key_Tip_Pres_Zero                         !ˮ��ѹ�ѷ����Ƿ������Ѽ�ˮѹΪ0
  integer Num_Gauss_Points,Num_Substeps,Key_Integral_Sol
  integer Num_Sub_Quads                             !���ı����ʷ��ı�����Ŀ(Key_Integral_Sol= 3ʱ����),Ĭ��16,����ȡ����
  integer Num_Sub_3D_Cubes                          !3D���������ʷַֿ���
  integer Num_Gau_Points_3D_Cube                    !3D���������ʷַֿ��˹���ֵ���Ŀ
  integer Num_Gau_Points_3D,Num_Gauss_P_FEM_3D
  integer Num_Gau_Points_3D_MC                      !��3�������ѷ��3D��ǿ��Ԫ�ĸ�˹���ֵ���: 1000=10^3(default);3375=15^3;5832=18^3;8000=20^3
  integer Num_Gauss_P_Inc
  integer Num_Gauss_P_FEM
  integer Num_Gau_P_SubInteg_6                      !3D�ֿ����6���嵥Ԫ���ֵ���Ŀ. 2022-07-28.
  integer Num_Gau_P_SubInteg_4                      !3D�ֿ����4���嵥Ԫ���ֵ���Ŀ. 2022-07-28.
  integer Key_Initiation,Key_Propagation,Key_Type_2D,CFCP
  integer Key_CFCP_3_Type                           !CFCP=3׼���ѷ���չ��Ϊ,2022-07-02.
  integer Key_3D_Cr_Update_Scheme                   !3D�ѷ�������㷨, 1: �����Ƿ���չ������չһ�����������ڲ���չ���Ѽ⣬����һ��΢С��.
                                                    !                  2: ����չ�Ѽⲻ��չ����չ��С�������Ѽ��������(default).
  integer Key_Large_Deform                          !���������, ������Key_Analysis_Type = 8ʱ
  integer Key_Ini_Rule                              !���ɳ�ʼ�ѷ���ѭ��׼��:=1,�����Ӧ��׼��(Ĭ��)
  integer Key_Ini_Cr_3D_Type                        !���ɳ�ʼ�ѷ������(����3D����ʱ��Ҫ):=1,Բ��;=2,����
  real(kind=FT) Size_Ini_Crack                      !���ɵĳ�ʼ�ѷ�ĳߴ�(����3DԲ�γ�ʼ�ѷ�,��ʾֱ��;����3D���γ�ʼ�ѷ�,��ʾ�߳�;����2D�ѷ�,��ʾ����)
  integer Key_Schollmann_Twist
  real(kind=FT) Schollm_Max_Theta                   !�����3D Schollmann��s criterion�����Thetaƫת��(��λΪ��,����С��75,Ĭ��55). NEWFTU2022071001.
  integer Key_Adj_Prp_Step_3D                       !3D�ѷ���չ���Ʋ���(default:0); =1�����Ӧ����Ӧ��ǿ�����ӿ����ѷ���չ������ʵ�ʴ�С
  real(kind=FT) Adj_Prp_Step_3D_Max                 !3D�ѷ���չ���Ʋ���(default:3.0),Key_Adj_Prp_Step_3D = 1ʱ������,3D�ѷ���չ��󲽳�
  real(kind=FT) Prp_3D_Factor_m                     !delta_l  = delta_L_Max*(S1/St)^m��ʽ�е�ϵ��m. Ĭ��ֵΪ1/3��0.333333.
  real(kind=FT) Prp_Bisec_Factor_3D                 !3D�ѷ���չ���Ʋ���:�߽��ʷ�ϵ��,��3D�ѷ���ɢ��Ԫ�߽糤��>Prp_Bisec_Factor_3D*Ave_Elem_L_Enrich,�����
  integer Key_Smooth_Front                          !3D�ѷ���չ���Ʋ���:(default:0),>=1����ѷ�ǰԵ���й⻬����
  integer Key_Smooth_Front_Twice                    !=1ʱ��ִ������Smooth������Ȼ�����δ���������ȡƽ��ֵ
  !integer Key_Smooth_Pressure_Curve                 !ѹ�����߹⻬����,2�ι⻬����. 2022-10-14.
  !integer Key_OpenMP                               !�Ƿ���OpenMP
  integer Key_Num_Process                           !���м���ʹ�õĺ�����Ŀ(��ȡΪ99,��ʹ��ȫ������)

  integer Key_Ave_Stress                            !��Ȩƽ��Ӧ���ļ��㷽��
  real(kind=FT) S1_Point_Factor                     !��ȨӦ��������ƶ�ϵ��(ƫ���ѷ����������ƶ�; defaut to 0.2)
  integer Key_Ave_Half_HF                           !����ˮ��ѹ�ѷ���,ȡ������ˮѹ���õ��ұ�һ������Ȩƽ��Ӧ��
  real(kind=FT) a_Ave_Shi                           !������ļ�Ȩƽ�������㷨�еĲ���a
  real(kind=FT) Factor_Ave_R                        !��Ȩƽ������Ӧ����������뾶ϵ��(0.1-1.5)
  integer num_Gauss_S1                              !��Ȩƽ������Ӧ�����㵥Ԫ��˹����Ŀ(default to 6^3=216)
  real(kind=FT) Prop_Angle_Alowed                   !������ѷ���չƫת��(0��180֮�����)
  integer Key_HF_Del_Ne_Pres                        !ˮ��ѹ�ѷ����и���ˮѹת����0
  real(kind=FT) Coff_a_AMPT
  integer,ALLOCATABLE::Material_Type(:)                         !��������
  real(kind=FT),ALLOCATABLE:: Material_Para(:,:)                !���ϲ���
  real(kind=FT),ALLOCATABLE:: Material_Para_Added(:,:)          !���ϲ��ϲ���
  real(kind=FT),ALLOCATABLE:: Mat_Cartesian_Coor_Vector_x(:,:)  !���ϲ���������������ϵx������
  real(kind=FT),ALLOCATABLE:: Mat_Cartesian_Coor_Vector_y(:,:)  !���ϲ���������������ϵy������
  real(kind=FT),ALLOCATABLE:: Mat_cylinder_Coor_Center(:,:)     !���ϲ���Բ����������ϵԭ��
  real(kind=FT),ALLOCATABLE:: Mat_cylinder_Coor_Vector_z(:,:)   !���ϲ���Բ����������ϵz������
  real(kind=FT) Desired_KIc                                     !ˮ��ѹ��Ŀ����K_C,����PhiPsi3D_Static_HF_SlipWater. 2023-02-12.

  real(kind=FT) Material_Interface(2)               !���ӵĽ�����ϲ���:1-St,2-KIc
  integer Key_SIFs_Method,Key_Force_Control,Key_Analysis_Type
  integer Key_SIFs_DIM_Points                       !λ�Ʋ�ֵ������Ӧ��ǿ�����ӵ���ȡ�����Ŀ(1��2��3,Ĭ��Ϊ2)
  integer Key_SIFs_DIM_Method                       !=1,����ƽ��Ӧ����ʽ;=2,����ƽ��Ӧ�乫ʽ. 2023-03-19.
  integer Key_FS_Seque_Coupling                     !Sequential coupling of solid and field problems (only for 2D problem and Key_Analysis_Type = 1)
  integer Key_TS_Seque_Coupling                     !�ȹ���ϼ���(2D��̬+˲̬�¶ȳ�����) (only for 2D problem AND key_analysis_type = 1)
  integer Num_Force_Divs                            !Key_Force_Control=3ʱ��Ҫ
  integer Key_Save_f_d_Curve                        !�Ƿ񵼳��غ�λ����������
  integer f_d_Curve_node_num                        !�����غ�λ���������ݵĽڵ��
  integer Key_Save_f_COD_Curve                      !���������ѷ���غ�-�ѷ��ſ�λ��(COD)����,���浽fccu�ļ���. 2022-10-15.
  integer f_COD_Curve_Crack_Num                     !�غ�-�ѷ��ſ�λ��(COD)�����ѷ��. 2022-10-15.
  integer num_Crack_Log(Max_Itera)                  !ÿһ�������Ƶ���Ŀ
  integer Key_Gravity                               !��������
  real(kind=FT) g_X_Y_Z(3)                          !���������ϵ��������ٶ�
  integer Key_Geo_Stress                            !��Ӧ������,��=1����Ҫ���е�Ӧ��ƽ��
  integer Key_SLOE                                  !���Է�������ⷽ��
  integer Key_EBE_Precondition                      !EBE�����Ԥ����ؼ���, 2022-06-02.
  integer Key_EBE_Condition_Number                  !=1ʱ�����ÿ����ǿ��Ԫ��������������ȫ�ֱ���EBE_Condition_Number(Num_Elem). NEWFTU2022070901.
  integer Key_EBE_Sym_Storage_K                     !�Գƴ洢��Ԫ�նȾ���storK_FEM. 2022-11-10. NEWFTU2022111001.
  real(kind=FT),ALLOCATABLE::EBE_Condition_Number(:)!��ǿ��Ԫ��������.
  integer Lis_Number_Iteration                      !Lis������ĵ�������(Ĭ��5000)
  integer MDOF_2D                                   !�����ڲ�����,Max_number_of_DOFs, 2D���ⵥԪ�նȾ���������ɶ���Ŀ(default to 200;��Ķ�).
  integer MDOF_3D                                   !�����ڲ�����,Max_number_of_DOFs, 3D���ⵥԪ�նȾ���������ɶ���Ŀ(default to 200;��Ķ�).
  integer Old_MDOF_3D                               !��һ��MDOF_3D. 2023-01-11.
  integer Key_Kink_Point                            !�Ƿ���յ㻮�ּ����
  integer Key_Data_Format                           !���ݱ����ʽ��1���и�ʽ�ģ�2�������Ƶ�
  integer Key_XA                                    !�°����. 2022-09-10.
  integer Key_K_Sparse                              !�Ƿ���ϡ��������ʽ����նȾ���
  real(kind=FT) Sparse_Ratio                        !����ϡ�����ʱ�ٶ������ϡ���
  integer Sparse_Store_Method                       !�м����(Location_COO)���ݴ洢��ʽ, 2:�ڴ�;1:Ӳ�̶������ļ�(Ĭ��)
  !integer Key_Asem_Sparse_Scheme                   !ϡ������鼯�㷨,1:����,ռ�ڴ�;2:����,ʡ�ڴ�
  !real(kind=FT) Spa_Work_Ratio                     !�ۼ�֮ǰ�Ĺ��������ϡ���
  integer Key_Unit_System                           !��λ��,       1: ���ʵ�λ��(Ĭ��);2: mm-MPa��λ��
  integer Key_Dimension                             !2D��3D����,   2: ��ά����;3: ��ά����
  integer Key_Contact                               !������������Ļ���Ƕ��(Ĭ�Ͽ���,��=1)
  real(kind=FT) Contact_Aper_Tol                    !�Ӵ�����ݲ�.
  !integer Key_Contact_Reduce                       !�����ĽӴ��㷨(�������ڷ�������(1))
  integer Key_TipEnrich                             !�Ѽ���ǿ����, 1: ��׼�Ѽ���ǿ(4��,Ĭ��ֵ)
                                                    !              2: ��������һ��(���ڶ�̬����,��ΪĬ��)
                                                    !              3: Heaviside�⻬���ȷ���(����ҵĲ�ʿ����)
                                                    !              4: Cohesive
  integer Key_CS_Natural_Crack                      !����ѹ����Ȼ�ѷ췣��������. 2022-10-22.
  real(kind=FT) Penalty_CS_Natural_Crack            !ѹ�����ѷ�ķ�����. Ĭ��1.0D10. 2022-12-23.
  integer Key_Penalty_CS_Method                     !=1(default)�򷣺�������l��m��nλ�ƣ�=2
                                                    !�������������n����λ�ƣ������߷���λ��. NEWFTU2023082701.
  integer Num_F_Functions                           !�Ѽ���ǿF��������Ŀ(�ڲ�����-����Ҫ����)
  integer Key_Junction_Enrich                       !�Ƿ���Junction��ǿ(Ĭ��Ϊ0)��������3D. 2022-08-25.
  integer Key_Tip_Fluid_Element_3D                  !����3D�Ѽ���ǿ��Ԫ�ڼ������嵥Ԫ
  integer Key_Multi_TipEnrNode                      !���Ѽ���ǿ
  integer Key_Pre_Conditioner                       !����Pre_Conditioner,���ڼ�С�նȾ�������������Ӷ����ͷ����鲡̬���ԡ���С������;Ŀǰ������3D
  real(kind=FT) Key_TipEnrich_Radius_Factor         !���Ѽ���ǿ�ڵ���뾶ϵ��
  integer Key_Local_Mesh_Refine                     !����ֲ��Ż�, =0,�رգ�=1������ȫ����ǿ�ڵ㣻=2���������Ѽ���ǿ�ڵ�
  logical Flag_Local_Refined                        !ȫ�ֱ���,���ڱ���Ƿ��Ѿ��ֲ�����
  integer Key_Front_Segmentation                    !Allow 3D fracture front segmentation
  integer Number_front_Segments                     !Number of Segments(default:2).

  integer Key_Adj_Ini_Crack_3D                      !Adjust initail crack. For 3D only.
  integer Key_Check_and_Adjust_Cracks_3D            !�����ѷ���͵�Ԫ���ཻ��������ѷ���λ��. 2022-08-01.
  real(kind=FT) Adjust_Cracks_Delta_Angle           !��λ�����ֱ���. Key_Check_and_Adjust_Cracks_3D =  2��3ʱ�õ�. 90.0D0, 60.0D0, 45.0D0, 30.0D0, 22.5D0
  integer Adjust_Cracks_Resolution                  !��λ�����ֱ���. 2022-08-02.
  real(kind=FT) k_tt                                !��ֹ������Ƕ�뼰ģ��HF֧�ż����߸ն�.
  real(kind=FT) k_nn                                !��ֹ������Ƕ�뼰ģ��HF֧�ż�����ն�.
  integer Key_Min_Aperture                          !�Ƿ�������С����(������ˮ��ѹ�ѷ���).
  real(kind=FT) Min_Aperture                        !�պ��ѷ����С����.
  !�Ӵ��������
  real(kind=FT) fric_mu_Cont                        !�Ӵ�����Ħ��ϵ��
  integer Max_Contact_Iter                          !������Ӵ�(��֧�ż�����)������������
  integer Key_Conta_ConCrit                         !�Ӵ���������׼��(1:ͨ���в�ȷ��;2:ͨ��λ��ȷ��)
  real(kind=FT) Aper_Tol_Factor                     !�ѷ�Ӵ����ȼ��ϵ��,�˲����ԽӴ������������зǳ����Ӱ��(Խ��Խ��������,Ĭ��:1.0D-4*Ave_Elem_L_Enrich)
  real(kind=FT) kn_Cont_Penalty,kt_Cont_Penalty     !�Ӵ��������������Ӵ��ն�
  real(kind=FT) Conve_Tol_Penalty                   !�Ӵ������������������ݲ�
  !ճ���Ѽ��ճ���ѷ����
  integer Max_Cohesive_Iter                         !ճ���ѷ������������ >=50
  integer Coh_Integ_Point                           !ճ���ѷ���ֵ���Ŀ(1��2)
  integer Key_Coh_ConCrit                           !ճ���ѷ��������׼��(1:ͨ���в�ȷ��;2:ͨ��λ��ȷ��)
  integer Key_Play_Sounds                           !�Ƿ񲥷�����
  integer Key_Memo_Moni                             !�Ƿ����ڴ�������
  integer Key_Clear_All                             !ɾ��ȫ������������
  integer Key_Junction_Check                        !�Ƿ��Junction����м��,��֤�����ֻ��Ҫһ��Junction��ǿ��Ԫ
  real(kind=FT) Factor_Check_Dis                    !�ڲ�����:�ѷ콻���ⳤ��(�ܹؼ�,̫���������������bug,��V5-P50)
  real(kind=FT) Factor_L_Jun_NewCr                  !HF��NF�������γɵ����ѷ�ĳ���ϵ��
  !----------------------------------------
  real(kind=FT) HF_Ini_Pressure                     !ˮ��ѹ�ѷ�����ʼѹ��
  !----------------------------------------
  !integer Num_Gauss_P_FEM_3D                       !��ͨ3D��Ԫ�ĸ�˹���ֵ���Ŀ
  integer Key_Post_CS_N_Strs                        !ÿ���غɲ�(�����Ѳ�)����Ӧ��
  integer Key_Post_CS_N_Stra                        !�Ƿ���㲢����Node��Ӧ��(Ĭ��ֵΪ0)
  integer Key_Save_avol_file                        !�����ѷ�������ļ�. 2022-12-18. Ĭ��Ϊ0.
  integer Key_Post_CS_N_Stra_Cylindr                !�Ƿ���㲢����Node��Ӧ��(Ĭ��ֵΪ0)������ϵ
  integer Key_Post_CS_N_Strs_Cylindr                !�Ƿ���㲢����Node��Ӧ��(Ĭ��ֵΪ0)������ϵ
  integer Key_Post_CS_N_Disp_Cylindr                !�Ƿ���㲢����Node��λ��(Ĭ��ֵΪ0)������ϵ
  !������Բ������ϵ��Ӧ�䣨����Ӧ���)����Ҫ�������²���
  real(kind=FT) Post_cylinder_Coor_Center(3)        !Բ����������ϵԭ��
  real(kind=FT) Post_cylinder_Coor_Vector_x(3)      !Բ����������ϵx������
  real(kind=FT) Post_cylinder_Coor_Vector_y(3)      !Բ����������ϵy������
  real(kind=FT) Post_cylinder_Coor_Vector_z(3)      !Բ����������ϵz������
  real(kind=FT),ALLOCATABLE::Theta_Cartesian_to_Cylinder(:,:)!�ѿ������굽������ĽǶ�theta(��Ԫ���ڵ�).
  real(kind=FT),ALLOCATABLE::Theta_Cartesian_to_Cylinder_Node(:)!�ѿ������굽������ĽǶ�theta(�ڵ�).
  real(kind=FT) Water_Pressure                      !��������4�ľ���ˮѹ��С
  integer Type_Water_Pressure                       !ˮѹ����:=1,��ѹǿ;=2,����ѹǿ(�Ѽ�Ϊ0);=3,����ѹǿ
  real(kind=FT) Num_Div_Elment                      !��ˮ��ѹ�ѷ���ʱ����㻮���ܶ�
  integer Key_InSitu_Method                         !���ǵ�Ӧ���ķ���(����HF������5�ŵ�����)
  integer Key_InSitu_Strategy                       !���ǵ�Ӧ���ķ���(����HF������6�ź�8�ŵ�����)
  integer Key_Read_Initial_Node_Stress_File         !�ӳ�ʼӦ���ļ�(*.istn,�ڵ�Ӧ���ļ�)�����ʼӦ��������.
  real(kind=FT) InSitu_x                            !x�����Ӧ��
  real(kind=FT) InSitu_y                            !y�����Ӧ��
  real(kind=FT) InSitu_z                            !z�����Ӧ��
  real(kind=FT) InSitu_xy,InSitu_yz,InSitu_xz
  integer Key_InStress_for_Mat                      !���ض�����ʩ��ԤӦ��
  integer Mat_Number_of_InStress(100)               !ʩ��ԤӦ���Ĳ��Ϻ�
  real(kind=FT) Mat_InStress_x                      !x����ԤӦ��
  real(kind=FT) Mat_InStress_y                      !y����ԤӦ��
  real(kind=FT) Mat_InStress_z                      !z����ԤӦ��

  integer Key_HF_Secant_TS                          !ˮ��ѹ�ѷ������Ƿ�ͨ�����߷�ȷ��ʱ�䲽��
  integer Key_HF_LineSearch                         !HF�����Ƿ���������
  integer Key_HF_Multistage                         !�Ƿ���ˮ��ѹ�ѷֶ�ѹ��
  integer Key_HF_Multistage_3D                      !�Ƿ���ˮ��ѹ�ѷֶ�ѹ��(3D)
  integer Key_HF_MS_Contc_Check                     !�ֶ�ѹ���Ƿ���к�֧�ż��ѷ���ĽӴ�����
  integer Key_HF_Cont_Scheme                        !ˮ��ѹ�ѷ����Ӵ���ⷽ��:=1,���ڵ�һ�ε���ȷ���Ӵ�״̬,֮�󱣳ֲ���(Ĭ��);
                                                    !                         =2,����ǰ�������̵���ִ�нӴ����;
                                                    !                         =3,ÿ��������ϵ�����ִ�нӴ�����.
  character*1  Keywords_Blank
  parameter (Keywords_Blank  = ' ')
  character*4 Space_4
  parameter  (Space_4  = '    ')
  character*8 Space_8
  parameter  (Space_8  = '        ')
  !����ض������Ż����
  integer Key_Paper1
  integer Type_Cal_Propped                          !��ǵ�ǰ����Ϊ֧�ż�֧�Ų�����
  !integer Key_No_Tip_Enrich                        !�Ƿ�����Ѽ���ǿ
  !��Ӧ�����
  integer State_InSitu
  !---------ƣ�ͷ������--------------
  integer Key_Static_Fatigue                        !Fatigue static analysis,�����Ƿ���׼��̬ƣ�ͷ���
  integer Key_Fatigue_Cri                           !ƣ�ͷ���׼��
  integer Num_Fatigue_N                             !ƣ��ѭ������
  real(kind=FT) Fatigue_Paris_C,Fatigue_Paris_m     !Paris׼����ر���
  !---------��ʼ��϶ѹ�����----------
  integer Key_PoreP                                 !�Ƿ��г�ʼ��϶ˮѹ��
  real(kind=FT) Initial_PoreP                       !��ʼ��϶ѹ�Ĵ�С
  real(kind=FT) Initial_Biot_Coeff                  !��ʼ�Ȱ�ϵ��. 2023-03-19.
  !-------��������֧���ѷ쿪�ȼ���,Key_Propped_Width=1------------------
  integer Key_Propped_Width                         !�Ƿ����֧���ѷ�Ŀ���(Newton-Raphson��������),Paper03,��Ҫ����
                                                    !���ѷ������W_P_0(wpnp�ļ�),�����ڷ�������1
  real(kind=FT)Prop_Size                            !֧�ż��ߴ�
  real(kind=FT)Prop_Yita                            !֧�ż��������ܶ�
  real(kind=FT)Prop_Elas                            !֧�ż�����ģ��
  real(kind=FT)Prop_Poss                            !֧�ż��Ĳ��ɱ�
  real(kind=FT)Prop_D                               !��ĺ��
  !real(kind=FT)Prop_C1,Prop_C2                      !������ʽ�е�C1��C2,���Paper03
  logical Yes_XFEM                                  !����Ƿ���XFEM����
  !��Ӧ���������
  integer Key_Thermal_Stress                        !�Ƿ�����Ӧ��
  integer Key_Initial_Temperature                   !��ʼ�¶ȸ�������. =1, ���ݲ��Ϻ�. =2, ���ļ����루��ʱ�����ã�. 2023-03-13.
  integer Key_Scheme_Thermal_Stress                 !�����趨�¶�Ӧ�����㷽��. = 1�����ݾ����¶ȼ�����Ӧ��; = 2����������¶ȣ��²������Ӧ��.
  real(kind=FT) Thermal_Str_Temper(100)             !��Ӧ��������¶�ֵ(��ͬ���Ϻŵ�)
  integer Key_Random                              !������ɷ��� 0:ϵͳ�Դ����Զ����ɺ���random_number,ÿ�����ɵ���ȫ��ͬ��Ψһ
                          !             1:ϵͳ�Դ����Զ����ɺ���random_number,ÿ�����ɵĲ�ͬ
                          !             2:Handbook of Simulation: Principles, Methodology, Advances, Applications
                          !              һ���е������ӷ���,ÿ��������������ӿ���
  real(kind=FT) Inject_Prop_c
  integer Key_Plasticity
  !����
  integer Key_Close_Window                          !����������Ƿ�ر�DOS����
  integer Key_Visit_PhiPsi_top                      !����PhiPsi��վ��Ϣ���ϴ�����
  integer Key_Window_Log                            !���洰�ڼ�¼
  character(200) Window_log_filename
  character(200) PhiPsi_Current_Directory           !PhiPsi����·��
  integer Seed                                      !����������ɱ���������
  integer Key_Damage
  real(kind=FT),ALLOCATABLE:: Ele_Damage(:,:)       !ÿ����Ԫÿ��Gauss�������״̬����
  real(kind=FT),ALLOCATABLE:: Damage_Gauss(:)       !ÿ����Ԫÿ��Gauss�������״̬����(���ں���)
  real(kind=FT) Material_Dam2Frac_Value             !�����γ��ѷ�ļ�������ֵ(���˳�����ֵ�������ѷ�)
  integer Crack_Gen_from_Damage                     !�������ɵ��ѷ���Ŀ
  !real(kind=FT),ALLOCATABLE:: Ele_Damage_D(:,:,:,:)!ÿ����Ԫÿ����˹�㿼������֮���D����
  logical file_Sparse_K_Location_COO_bin            !���ڱ��Sparse_K_Location_COO.bin�������ļ��Ƿ����
  logical file_Sparse_K_Mask_COO_bin                !���ڱ��Sparse_K_Mask_COO.bin�������ļ��Ƿ����

  integer Key_InPlane_Growth                        !����3D XFEM��������ѷ����ԭƽ������չ(2022-04-18).
  integer Key_Stop_Outside_Crack                    !һ���ѷ���չ��ģ���ⲿ�������Ϊ������չ(2022-10-02). NEWFTU2022100201.
  integer Key_3D_HF_Time_Step_Method                !3D��ˮѹ�ѷ���ʱ�䲽��������:=1,NR����(default);=2,���ַ�.
  !******************
  !������Ԫ���
  !******************
  integer Key_EKILL
  real(kind=FT) EKILL_Weaken_Factor              !������Ԫ����ϵ��
  integer Ele_Killed_Each_Load_Step(Max_Step,1000)   !ÿ���غɲ�ɱ���ĵ�Ԫ���,���֧��1000����Ԫ
  !ALLOCATA��DEALLOCATA���������
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
  !3D�ѷ�ǰԵS1��K�⻬����.
  !*****************************
  integer Key_Denoise_Vertex_Value   !3D�ѷ�ǰԵS1��K�����ݽ���ȥ�����.
  integer Key_Smooth_Vertex_Value    !��3D�ѷ�ǰԵS1��K�����ݽ��й⻬����, =1, ����ƽ����,��ǰ��ʱ�̵�һ��2n+1��ֵ��ƽ��.
  integer Smooth_Vertex_n      !Key_Smooth_Vertex_Method= 1ʱ�õ�. ����ƽ������n.
  integer Key_Smooth_Vertex_Value2    !��3D�ѷ�ǰԵS1��K�����ݽ��й⻬�������δ���, =1, ����ƽ����,��ǰ��ʱ�̵�һ��2n+1��ֵ��ƽ��.
  integer Smooth_Vertex_n2      !Key_Smooth_Vertex_Method= 1ʱ�õ������δ���. ����ƽ������n.
  !*****************************************
  !3D�ѷ�ǰԵTheta�⻬����. 2022-07-14.
  !*****************************************
  integer Key_Denoise_Theta_Value !3D�ѷ�ǰԵS1��K�����ݽ���ȥ�����. Ĭ��ֵΪ0.
  integer Key_Smooth_Theta_Value  !��3D�ѷ�ǰԵS1��K�����ݽ��й⻬����. Ĭ��ֵΪ0.!=1, ����ƽ����,��ǰ��ʱ�̵�һ��2n+1��ֵ��ƽ��.
  integer Smooth_Theta_n         !Key_Smooth_Vertex_Method= 1ʱ�õ�. ����ƽ������n. Ĭ��ֵΪ4.
  integer Key_Smooth_Theta_Value2 !��3D�ѷ�ǰԵS1��K�����ݽ��й⻬����(���δ���). Ĭ��ֵΪ0.!=1, ����ƽ����,��ǰ��ʱ�̵�һ��2n+1��ֵ��ƽ��.
  integer Smooth_Theta_n2         !���δ���:Key_Smooth_Vertex_Method= 1ʱ�õ�. ����ƽ������n. Ĭ��ֵΪ3.
  !*********************************************
  !3D�ѷ�ǰԵGrowthFactor�⻬����. 2022-07-14.
  !*********************************************
  integer Key_Denoise_GF_Value !3D�ѷ�ǰԵS1��K�����ݽ���ȥ�����. Ĭ��ֵΪ0.
  integer Key_Smooth_GF_Value  !��3D�ѷ�ǰԵS1��K�����ݽ��й⻬����. Ĭ��ֵΪ0.!=1, ����ƽ����,��ǰ��ʱ�̵�һ��2n+1��ֵ��ƽ��.
  integer Smooth_GF_n          !Key_Smooth_Vertex_Method= 1ʱ�õ�. ����ƽ������n. Ĭ��ֵΪ4.
  integer Key_Smooth_GF_Value2 !��3D�ѷ�ǰԵS1��K�����ݽ��й⻬����(���δ���). Ĭ��ֵΪ0.!=1, ����ƽ����,��ǰ��ʱ�̵�һ��2n+1��ֵ��ƽ��.
  integer Smooth_GF_n2        !���δ���:Key_Smooth_Vertex_Method= 1ʱ�õ�. ����ƽ������n. Ĭ��ֵΪ3.
  !******************
  !����
  !******************
  integer Key_Crack_Inner_Pressure   !�Ƿ���ڷ�������ѹ��
  integer Key_Block_Model            !�Ƿ�Ϊ����ģ��
  integer Flag_HF_3D                 !3Dˮ��ѹ�ѷ������. 2022-06-02.
  integer Key_Cpp_Call_Fortran_Lib   !2023-03-23.
  integer XA_Step_Count              !���ڱ���ܵ����Ѳ�����ȫ�ֱ���. 2023-04-03.
  integer Key_Save_Crack_Radius      !���ڱ����ѷ쿪��. 2023-02-15.
  integer Circle_3D_Eqv_Polygon_Resolution !3DԲ�ε�ЧΪ����εķֱ���(Ĭ��Ϊ21����). 2024-02-26.
end module Global_Common

!----------------------------
!     2.ģ�����ȫ�ֱ���
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
  !cccccccccccccccccc         �����ģ���Ʋ���        ccccccccccccccccccccccc
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  parameter (Max_Materials     = 100)          !���100�ֲ���
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !cccccccccccccccccc         �����ģ���Ʋ���        ccccccccccccccccccccccc
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
  integer,ALLOCATABLE::Elem_Node(:,:)         !ÿ����Ԫ���ĸ��ڵ�
  integer,ALLOCATABLE::Elem_Node_Num(:,:)     !number of nodes of each element, for 2D and 3D.
  integer,ALLOCATABLE::Node_Elements(:,:)     !ÿ���ڵ���Χ�ĵ�Ԫ.
  !integer,ALLOCATABLE::Node_Elements(:,:)     !ÿ���ڵ���Χ�ĵ�Ԫ.
  type(Ragged_Int_Array_1D),allocatable::Node_Elements_3D(:)   !ÿ���ڵ���Χ�ĵ�Ԫ. IMPROV2023061401. �β���ֵ.
  integer,ALLOCATABLE::num_Node_Elements(:)   !ÿ���ڵ���Χ�ĵ�Ԫ����
  integer,ALLOCATABLE::Ele_Elements(:,:)      !ÿ����Ԫ��Χ�ĵ�Ԫ����
  integer,ALLOCATABLE::num_Ele_Eles(:)        !ÿ����Ԫ��Χ�ĵ�Ԫ����
  integer,ALLOCATABLE::Element_Edges(:,:,:)   !ÿ����Ԫ��12����
  integer Max_Diff_Elem_Num                   !��Ԫ�ڵ�����Ų�
  integer Max_Half_Band_Width                 !���չ�ʽ��������������,�ڵ����ɶ���*(��Ԫ�ڵ�����Ų�+1)
  integer,ALLOCATABLE::Elem_Mat(:)
  integer,ALLOCATABLE::Outline(:,:)   !ģ����߽�(����2D��3D)
  integer,ALLOCATABLE::OutArea(:,:)   !ģ�������(����3D)
  integer Num_Surface_Nodes             !ģ�������ڵ���Ŀ(����3D),2021-09-08
  integer,ALLOCATABLE::Surface_Nodes(:)   !ģ�������ڵ��(����3D),2021-09-08
  integer,ALLOCATABLE:: G_NN(:,:)
  real(kind=FT),ALLOCATABLE:: G_X_NODES(:,:),G_Y_NODES(:,:),G_Z_NODES(:,:)
  real(kind=FT),ALLOCATABLE:: Elem_Area(:),Elem_Vol(:),Elem_Max_L(:),Elem_Min_L(:),Elem_Ave_L(:)
  real(kind=FT),ALLOCATABLE:: Node_Max_L(:)   !ÿ���ڵ���Χ��Ԫ�����Ԫ����. 2023-02-22. IMPROV2023022203.
  logical,ALLOCATABLE:: Elem_Break(:)                !���ڱ�Ǹ�����Ԫ�Ƿ��ƻ�
  real(kind=FT),ALLOCATABLE:: Elem_Ave_Gauss_Stress(:,:)  !ÿ����Ԫ��ƽ��GaussӦ��
  real(kind=FT),ALLOCATABLE:: Elem_Ave_Gauss_S1(:)  !ÿ����Ԫ��ƽ��Gauss�����Ӧ��
  real(kind=FT),ALLOCATABLE:: Elem_Centroid(:,:)     !������Ԫ������
  integer,ALLOCATABLE:: Ele_yes_FEM_asemd(:)         !���ڱ�Ǹõ�Ԫ��FEM��Ԫ�ն��Ƿ��Ѿ��鼯���ܸ���
  logical*1,ALLOCATABLE:: EleGaus_yes_FEM_asemd(:,:)   !���ڱ�Ǹõ�Ԫ�ø�˹���FEM��Ԫ�ն��Ƿ��Ѿ��鼯���ܸ���
  integer Total_FD,Usual_Freedom,Enrich_Freedom
  integer Total_FD_P,Total_FD_U
  integer,ALLOCATABLE:: Flag_FreeDOF(:)             !���ڱ�����ɶ��Ƿ������ɵ�. 2022-06-12.
  integer,ALLOCATABLE:: Location_FreeDOF(:)         !���ڱ�����ɶȶ�Ӧ���������ɶ�λ��. 2022-10-25.
  integer,ALLOCATABLE:: FreeDOF_for_Det(:)          !��������ʽ�����ͼ����FreeDOF. 2022-09-25.
  real(kind=FT) k_PenaltyStiffness
  !-------------Gauss�������---------------
  !��������ȫ�ֱ������鼯�նȾ���Ĺ����л��
  integer,ALLOCATABLE::num_GP_Elem(:)           !ÿ����Ԫ��Gauss����Ŀ
  integer,ALLOCATABLE::Ele_GP_Start_Num(:)      !ÿ����Ԫ��Gauss����ʼ���
  !-------------Gauss������---------------
  real(kind=FT),ALLOCATABLE:: Gauss_CoorX(:)
  real(kind=FT),ALLOCATABLE:: Gauss_CoorY(:)
  !-------------������---------
  integer Key_Fracture_Zone              !������ε���������(�������ڸ÷�Χ����չ),��������,����ȫģ������չ
  real(kind=FT) Frac_Zone_MinX           !��������x���귶Χ
  real(kind=FT) Frac_Zone_MaxX           !��������x���귶Χ
  real(kind=FT) Frac_Zone_MinY           !��������y���귶Χ
  real(kind=FT) Frac_Zone_MaxY           !��������y���귶Χ
  real(kind=FT) Frac_Zone_MinZ           !��������z���귶Χ
  real(kind=FT) Frac_Zone_MaxZ           !��������z���귶Χ
  !-------------�����Ȼ�ѷ�---------
  integer Key_Random_NaCr                !�Ƿ����������Ȼ�ѷ�
  integer num_Rand_Na_Crack              !������ɵ���Ȼ�ѷ���Ŀ
  real(kind=FT) NaCr_Orientation         !��Ȼ�ѷ�ƽ����λ(��)
  real(kind=FT) NaCr_Ori_Delta           !��Ȼ�ѷ췽λ�Ĳ�������(��)
  real(kind=FT) NaCr_Length              !��Ȼ�ѷ��ƽ������
  real(kind=FT) NaCr_Len_Delta           !��Ȼ�ѷ��ƽ�����ȵĲ�������
  real(kind=FT) Random_NaCr_Rad_Factor   !��Ȼ�ѷ�����ɵļ��뾶ϵ��(Ĭ��:1.5)
  !-------------ģ�͵ĳ�ʼ��Ӧ��
  !real(kind=FT) Insitu_x,Insitu_y
  !-------------�ڵ�������
  !option1-ͨ���ؼ����ļ�ֱ�Ӷ�����Ͻڵ�(���ַ�ʽÿ������ֻ�ܶ���һ�����)
  integer num_CP_x_nodes,num_CP_y_nodes
  integer CP_x_nodes(1:5000)         !Ҫ��ϵ�x�������ɶȽڵ��б�,���֧��5000���ڵ�
  integer CP_y_nodes(1:5000)         !Ҫ��ϵ�y�������ɶȽڵ��б�,���֧��5000���ڵ�
  !option2-ͨ��dofx,dofy,dofz�ļ�������Ͻڵ�(���ַ�ʽÿ������ֻ�ܶ���������)
  integer num_CP_set_x,num_CP_set_y  !ÿ���������10��
  integer num_nodes_CP_set_x(10),num_nodes_CP_set_y(10)
  integer CP_nodes_x(10,5000),CP_nodes_y(10,5000)

  !-------------ÿ����Ԫx,y,z����ķ�Χ--------
  real(kind=FT),ALLOCATABLE:: x_max_Elements(:)
  real(kind=FT),ALLOCATABLE:: x_min_Elements(:)
  real(kind=FT),ALLOCATABLE:: y_max_Elements(:)
  real(kind=FT),ALLOCATABLE:: y_min_Elements(:)
  real(kind=FT),ALLOCATABLE:: z_max_Elements(:)
  real(kind=FT),ALLOCATABLE:: z_min_Elements(:)
  real(kind=FT) penalty_k_bou_nonzero             !����λ�Ʊ߽��������������ķ�����

  integer D3_nband_FEM  !3D��������(��������ǿ�ڵ�)

  integer Num_Elem_Block_Bou         !����ģ�ͱ߽��ϵĵ�Ԫ��Ŀ
  integer,ALLOCATABLE::Elems_Block_Bou(:)   !����ģ�ͱ߽��ϵĵ�Ԫ,��СΪNum_Elem_Block_Bou
  integer Ele_3D_Edges_Node(12,2)           !����һ�����󣬴洢12����ߵĽڵ���
  !-------------��˹�������--------
  integer,ALLOCATABLE::Elements_Gauss_Num(:)  !2022-07-16. ÿ����Ԫ�ĸ�˹���ֵ���Ŀ.
  integer,ALLOCATABLE::Elems_Integration_Type(:,:)      !��������. 2022-07-27.
  integer,ALLOCATABLE::Elems_Num_SubEles(:,:)           !�ӵ�Ԫ��Ŀ
  integer,ALLOCATABLE::Elems_Type_SubEles(:,:)          !�ӵ�Ԫ����, =1��ʾ6���嵥Ԫ,����=2��ʾ4���嵥Ԫ
  integer,ALLOCATABLE::Elems_SubEles_Index(:,:)         !�ֿ鵥Ԫ��SubEles_Coors�����еı��.
  integer num_SubEles                                   !�ܵķֿ鵥Ԫ��Ŀ.
  integer,ALLOCATABLE::SubEles_Integ_Num(:)             !�ֿ鵥Ԫ�Ļ��ֵ���Ŀ.
  real(kind=FT),ALLOCATABLE::SubEles_Integ_Coors(:,:,:)  !ר�����ڴ�ŷֿ鵥Ԫ�Ļ��ֵ������. 1:�ֿ鵥Ԫ��; 2:���ֵ��;3:���ֵ�ֲ����꼰Ȩ��.
  integer Num_Max_3D_gauss                              !3D��Ԫ���������ֵ���Ŀ.
  !--------------
  !integer Ele_Num_Cache_by_Coors_3D                     !���棬���������ͨ�������õĵ�Ԫ��. 2022-09-24.
  !--------------
  !2022-11-21.
  !��ģ�͸�������ֳ�8������. ����8������ĵ�Ԫ���. ���ڸ���������ҵ�Ԫ���. IMPROV2022112101.
  real(kind=FT) Model_Center_x,Model_Center_y,Model_Center_z
  integer Domain_Elements_Num(8)
  integer,ALLOCATABLE::Ele_Domain_ID(:)
  integer,ALLOCATABLE::Domain_Elements(:,:)
  integer First_XFEM_Step             !�����ɵĳ�ʼ�ѷ켤��ĵ�һ��XFEM��. 2023-01-23.
  !ÿ�ֲ��϶�Ӧ�ĵ�Ԫ�б�. 2023-01-24. NEWFTU2023012401.
  type(Ragged_Int_Array_1D),allocatable::List_Elements_Mat(:)
  integer Elements_Num_Mat(Max_Materials)    !ÿ�ֲ��ϵĵ�Ԫ��Ŀ.
  !��Ԫ�ĳ�ʼ�¶Ⱥ͵�ǰ�¶�. 2023-03-13.
  real(kind=FT),ALLOCATABLE:: Elem_Initial_T(:),Elem_Current_T(:),Elem_T_for_Stress(:)
  !����ȫ�ֱ���. Size_Local_3D, All_Local_3D. 2023-03-15. IMPROV2023031501.
  integer,ALLOCATABLE::Size_Local_3D(:)
  integer,ALLOCATABLE::All_Local_3D(:,:)
  !
  !��Ԫ�ĳ�ʼ��϶ѹ������ǰ��϶ѹ�����Ȱ�ϵ��. 2023-03-19. IMPROV2023031901.
  real(kind=FT),ALLOCATABLE::Elem_Initial_PoreP(:),Elem_Current_PoreP(:),Elem_Biots(:)
  !��Ԫ�ĵ���ģ�������ɱȡ�������ϵ���������Ͷȡ�����ǿ��. 2023-03-19. IMPROV2023031903.
  real(kind=FT),ALLOCATABLE::Elem_E_XA(:),Elem_Mu_XA(:),Elem_TEC_XA(:),Elem_KIc_XA(:),Elem_St_XA(:)
  !��Ԫ��D����. 2023-03-19. IMPROV2023031904.
  real(kind=FT),ALLOCATABLE::Elem_D_XA(:,:,:)
  !�Ƿ������ѷ�λ��ģ���ⲿ���߽��ѷ죩. Ĭ��Ϊ0. NEWFTU2023050701.
  integer Key_Allow_3D_Outside_Crack
  !ÿ����Ԫ�Ķ����ڵ���Ŀ. 2023-06-14. IMPROV2023061402.
  integer,ALLOCATABLE::Elem_Uniqued_Nodes(:)
  logical,ALLOCATABLE::Yes_Degenarated_Elem(:)
  integer Num_Degenarated_Elems
  integer MAT_ALLOW_CRACK_Initiation(Max_Materials)  !�Ƿ�����ĳ�ֲ������ɳ�ʼ�ѷ�. Ĭ��ȫ������.
  !real(kind=FT) f_t_concrete
  integer Key_Max_Num_Initiation_Cracks              !������������ĳ�ʼ�ѷ���Ŀ.
  integer Num_Initiation_Cracks 
end module Global_Model

!--------------------------------
! 2.1 ��ǿ��Ԫ���. 2022-06-24.
!--------------------------------
module Global_XFEM_Elements
  !---------------FEM��Ԫ�б��XFEM��ǿ��Ԫ�б�2022-04-16------------
  integer num_FEM_Elem,num_XFEM_Elem !FEM��Ԫ��Ŀ��XFEM��Ԫ��Ŀ
  integer,ALLOCATABLE::FEM_Elem_List(:)   !FEM��Ԫ�б�
  integer,ALLOCATABLE::XFEM_Elem_List(:)  !XFEM��ǿ��Ԫ�б�
  integer,ALLOCATABLE::Elem_XFEM_Flag(:)  !���ڱ�ǵ�Ԫ�Ƿ�����ǿ��Ԫ
  integer,ALLOCATABLE::Elem_Location(:,:) !���ڷ���洢��Ԫ����List�е�λ�ã���һ��ΪXFEM����2��ΪFEM
  integer,ALLOCATABLE::Elem_New_XFEM_Flag(:)     !���ڱ�ǵ�Ԫ�Ƿ���������ǿ��Ԫ. 2022-06-24.
  integer,ALLOCATABLE::Elem_Update_XFEM_Flag(:)  !���ڱ�ǵ�Ԫ�Ƿ�����Ҫ���µ�Ԫ�նȾ������ǿ��Ԫ. 2022-06-24.
  integer,ALLOCATABLE::Elem_XFEM_Flag_Old(:)  !���ڱ�ǵ�Ԫ�Ƿ�����ǿ��Ԫ
  integer,ALLOCATABLE::Elem_Location_Old(:,:) !���ڷ���洢��Ԫ����List�е�λ�ã���һ��ΪXFEM����2��ΪFEM
  integer,ALLOCATABLE::size_local_Old(:)   !���ڱ�����һ���ĸ�����Ԫ�նȾ���Ĵ�С.

  !BUGFIX2022092602.
  integer Must_Gauss_Number_3D        !�նȾ����鼯��Ҫ���ѷ��������˹����Ŀ,�ѷ������඼������Must_Gauss_Number_3D��Gauss�㣬����ɾ��Heaviside��ǿ�ڵ�.
  parameter (Must_Gauss_Number_3D = 50)!���Ĳ���. Ĭ��ֵ20.
  !2023-02-15.
  integer,ALLOCATABLE::Rollbacked_FEM_Elements(:) !�˻��γɵ�FEM��Ԫ�б�.
  integer Num_Rollbacked_FEM_Elements             !�˻��γɵ�FEM��Ԫ��Ŀ.
end module Global_XFEM_Elements


!----------------------------
!   3.�ļ������ȫ�ֱ���
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
! 4.��̬�������(������ʽ����ʽ)
!---------------------------------
module Global_Dynamic
  use Global_Float_Type
  implicit none
  save
  integer Num_Ivex                              !x�����ʼ�ٶ���Ŀ
  integer Num_Ivey                              !y�����ʼ�ٶ���Ŀ
  integer Num_Ivez                              !z�����ʼ�ٶ���Ŀ
  real(kind=FT),ALLOCATABLE::Ive_x(:,:)         !x�����ʼ�ٶ�
  real(kind=FT),ALLOCATABLE::Ive_y(:,:)         !y�����ʼ�ٶ�
  real(kind=FT),ALLOCATABLE::Ive_z(:,:)         !z�����ʼ�ٶ�
  integer Num_Iacx                              !x�����ʼ���ٶ���Ŀ
  integer Num_Iacy                              !y�����ʼ���ٶ���Ŀ
  integer Num_Iacz                              !z�����ʼ���ٶ���Ŀ
  real(kind=FT),ALLOCATABLE::Iac_x(:,:)         !x�����ʼ���ٶ�
  real(kind=FT),ALLOCATABLE::Iac_y(:,:)         !y�����ʼ���ٶ�
  real(kind=FT),ALLOCATABLE::Iac_z(:,:)         !z�����ʼ���ٶ�
  integer IDy_Num_Iteras                        !Total number of iterations
  integer IDy_Num_force_Itr                     !Number of iterations with force applied
  real(kind=FT) delt_time_NewMark               !Delta time of Newmark
  integer Key_EQ                                !�Ƿ��ǵ������,����,����Ҫ���������ٶ�ֵ
  real(kind=FT)  EQ_Ac_Time_Gap                 !������ٶ�����ʱ����
  real(kind=FT),ALLOCATABLE::EQ_Accel_data(:)   !������ٶ�����
  integer num_EQ_Accel                   !������ٶ����ݸ���
  integer num_EQ_Ac_nodes                       !������ٶ�ʩ�ӵ��Ľڵ���Ŀ
  integer EQ_Ac_nodes(5000)                     !������ٶ�ʩ�ӵ��Ľڵ��б�(���5000���ڵ���)
  !���Ҽ��ٶȼ������
  integer Key_Sin_Accel                         !�Ƿ񼤻����Ҽ��ٶȼ���
  integer  Sin_Accel_Dire                       !���Ҽ��ٶȼ����ķ���:=1,x;=2,y
  real(kind=FT) Sin_Accel_A                     !���Ҽ��ٶȼ��������
  real(kind=FT) Sin_Accel_T                     !���Ҽ��ٶȼ���������
  integer  Sin_Accel_num_Nodes                  !���Ҽ��ٶȼ����Ľڵ���Ŀ
  integer Sin_Accel_Nodes(5000)                 !���Ҽ��ٶȼ����Ľڵ��б�

  real(kind=FT) Factor_Prop_Dy                  !��̬����������ѷ���չ��������
  integer EDy_Num_Iteras                        !Total number of iterations(��ʽ)
  integer EDy_Num_force_Itr                     !Number of iterations with force applieds(��ʽ)
  real(kind=FT) Delt_Time_Explicit              !ʱ��������С
  integer Key_Mass_Lumped                       !�Ƿ���ü�����������(�Խ���������,Ĭ�Ͽ���)
  real(kind=FT) Explicit_time_inc               !��ʽ��̬������Сʱ�䲽��


  real(kind=FT),ALLOCATABLE::EDy_DISP(:),EDy_VELC(:),EDy_ACCL(:)
  real(kind=FT),ALLOCATABLE::IDy_DISP(:),IDy_VELC(:),IDy_ACCL(:)
end module Global_Dynamic

!------------------------------------------------------
!  5.2D�ѷ���أ�������ǿ�ڵ㣬����㣬������ͨ��ϵ
!  ����������������ȫ�ֱ�����;���⻹�����׶���ر���
!------------------------------------------------------
module Global_Crack
  use Global_Float_Type
  implicit none
  save
  integer  Max_Num_Cr,Max_Num_Arc_Cr,Max_Num_Cr_P
  integer  Max_Num_Cr_CalP,Max_Num_Seg_CalP,Max_Num_Cone_Cr
  integer  Max_Num_Ele_QuadHF,Max_Num_Ele_CalP,Max_Num_Hl
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !cccccccccccccccccc         �����ģ���Ʋ���        ccccccccccccccccccccccc
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !��Ҫgfortran֧��150���ѷ�,��Ҫ:-fopenmp -fno-align-commons -fno-range-check -fmax-stack-var-size=100000    !100000���ĸ���֧�ָ����ѷ�(2021-08-19)
  !parameter (Max_Num_Cr        = 150   )      !���150������,100��gfortranû����,Intel Fortran������; 50����û����
  !parameter (Max_Num_Cr        = 100   )      !���150������,100��gfortranû����,Intel Fortran������; 50����û����
  parameter (Max_Num_Cr        = 50   )      !���150������,100��gfortranû����,Intel Fortran������; 50����û����
  parameter (Max_Num_Cr_P      = 200   )      !ÿ���������100�����Ƶ�
  parameter (Max_Num_Cr_CalP   = 300 )        !ÿ������������300�������(ע,���ܹ���,�����ڴ�����,��ԭ����)
  parameter (Max_Num_Seg_CalP  = 200  )       !ÿ������Ƭ��������200�������
  parameter (Max_Num_Cone_Cr   = 10   )       !ÿ�����������10����������������
  !---------
  parameter (Max_Num_Arc_Cr    = 100   )      !���100����������
  !---------
  parameter (Max_Num_Ele_QuadHF= 100  )       !ÿ�������������ĸ߽�(����)HF��Ԫ��Ŀ
  !---------
  parameter (Max_Num_Ele_CalP  = 5   )        !ÿ����Ԫ�����������ڵļ������Ŀ
  parameter (Max_Num_Hl        = 100   )      !���100���׶�
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !cccccccccccccccccc         �����ģ���Ʋ���        ccccccccccccccccccccccc
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !********************************************************
  !��ǿ�ڵ���أ�����������ӳ���Determine_Enriched_Nodes
  !********************************************************
  real(kind=FT) Crack_Coor(Max_Num_Cr,Max_Num_Cr_P,2)           !�ѷ�ţ���������㣬(x,y)
  real(kind=FT) Arc_Crack_Coor(Max_Num_Cr,Max_Num_Cr_P-1,11)    !�����ѷ����һ��Ƭ���ǻ��ε�,��������Ӧ��Բ������(
                                                                !x,y,Direction(1Ϊ��ʱ��,-1Ϊ˳ʱ��),r,Radian_Start,Radian_End,Radian,
                                                                !Point_Start_x,Point_Start_y,Point_End_x,Point_End_y;
                                                                !����ʱ��������Բ������(x,y)�Լ�����
                                                                !4,5,6,7,8,9,10,11�ű�����ͨ��Tool_Cal_Arc_r_and_Radian_Given_Coors�ӳ������õ�
                                                                !ע��:��ʼ����Radian_Start,Radian_End��3����Ϊ0��,0-360����ʱ��
  !integer Arc_Crack_Passed_Ele(Max_Num_Cr,Max_Num_Cr_P-1,1000) !�����ѷ촩���ĵ�Ԫ��


  real(kind=FT) Hole_Coor(Max_Num_Hl,3)                         !Բ�ο׶�����(x,y,r)
  real(kind=FT) Ellip_Hole_Coor(Max_Num_Hl,5)                   !��Բ�ο׶�����(x,y,r)
  real(kind=FT) Na_Crack_Coor(Max_Num_Cr,Max_Num_Cr_P,2)        !��Ȼ�ѷ�ţ����������ţ�(x,y)
  real(kind=FT) Cr_First_Tip(Max_Num_Cr,2),Cr_Second_Tip(Max_Num_Cr,2)                  !�������Ƶ��Ѽ�����
  real(kind=FT) Cr_First_Tip_Ori(Max_Num_Cr),Cr_Second_Tip_Ori(Max_Num_Cr)                !�������Ƶ��Ѽ���������Ƭ�ε����


  !real(kind=FT) Penalty_CS
  integer,ALLOCATABLE:: Elem_Type(:,:)
  integer ,ALLOCATABLE:: c_POS(:,:)
  integer,ALLOCATABLE:: Enriched_Node_Type(:,:)
  real(kind=FT),ALLOCATABLE::Enriched_Node_Crack_n_Vector(:,:,:)!��ǿ�ڵ��Ӧ���ѷ�����ⷨ������,added on 2022-05-12.
  integer,ALLOCATABLE:: Node_Jun_elem(:,:)                      !Junction��ǿ�ڵ��Ӧ��Junction��Ԫ��,added on 2016-07-10.
  integer,ALLOCATABLE:: Jun_Ele_Negative_Cr_Num(:,:)            !Juntion��ǿ��Ԫ��Ӧ�ı����ѷ��
  integer,ALLOCATABLE:: Elem_Type_Hl(:,:)                       !���ڿ׶�
  integer,ALLOCATABLE:: Enriched_Node_Type_Hl(:,:)              !���ڿ׶�
  integer ,ALLOCATABLE:: c_POS_Hl(:,:)                          !���ڿ׶�

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
  integer,ALLOCATABLE:: TipEle_Adjacent_Ele(:,:)      !���ѷ��Ѽ����ڵ�Ԫ���ٽ���Ԫ(�����ѷ촩����)
  integer Crack_Jun_Elem(Max_Num_Cr,2)
  real(kind=FT) Crack_Tip_Coor(Max_Num_Cr,2,2)        !�������Ѽ�����
  real(kind=FT) Edge_Disposed_Crack(Max_Num_Cr,Max_Num_Cr_P,2)!��Ե���ƺ����õ�
  logical Flag_Crack_Tip_Out_Mol(Max_Num_Cr,2)        !���ڱ�����Ƶ������Ѽ��Ƿ񳬳�ģ�ͷ�Χ
  logical Yes_Arc_Crack                               !�Ƿ���������ѷ���ѷ��
  !***************************************************
  !�ѷ��������(��ν�ļ�������ˮ���ѷ쵥Ԫ�Ľڵ�)
  !���ڷ�ˮ��ѹ������,���Ǽ������ƿ��ȵ������
  !-----------
  !��һ�����Ƕ�Ӧ��ǰ���Ѳ�������
  !***************************************************
  !���¸�ΪAllocateble����, 2022-09-02. IMPROV2022090201.
  !integer Cracks_CalP_Num(Max_Num_Cr)                 !ÿ���ѷ��������
  !real(kind=FT) Cracks_CalP_Coors(Max_Num_Cr,Max_Num_Cr_CalP,2)   !ÿ���ѷ���������
  !real(kind=FT) Cracks_CalP_Orient(Max_Num_Cr,Max_Num_Cr_CalP)  !ÿ���ѷ������Ӧ���ѷ췽λ
  !integer  Cracks_CalP_Seg(Max_Num_Cr,Max_Num_Cr_CalP)  !ÿ���ѷ������Ӧ������Ƭ�κ�
  !integer  Cracks_CalP_Elem(Max_Num_Cr,Max_Num_Cr_CalP)  !ÿ���ѷ��������ڵ�Ԫ��
  !real(kind=FT) Cracks_CalP_Aper(Max_Num_Cr,Max_Num_Cr_CalP)  !ÿ���ѷ����㿪��
  !real(kind=FT) Cracks_CalP_Pres(Max_Num_Cr,Max_Num_Cr_CalP)  !ÿ���ѷ�����ˮѹ
  !real(kind=FT) Cracks_CalP_Tractions(Max_Num_Cr,Max_Num_Cr_CalP,2)!ת����ֱ������ϵ�µ�ճ��ǣ����(����ճ���ѷ�)
  !real(kind=FT) Cracks_CalP_Pgra(Max_Num_Cr,Max_Num_Cr_CalP)  !ÿ���ѷ�����ѹ���ݶ�(�з���)
  !real(kind=FT) Cracks_CalP_Velo(Max_Num_Cr,Max_Num_Cr_CalP)  !ÿ���ѷ���������(�з���)
  !real(kind=FT) Cracks_CalP_Quan(Max_Num_Cr,Max_Num_Cr_CalP)  !ÿ���ѷ���������(�з���)
  !real(kind=FT) Cracks_CalP_Conc(Max_Num_Cr,Max_Num_Cr_CalP)  !ÿ���ѷ�����Ũ��
  !real(kind=FT) Cracks_CalP_Remo_Strs(Max_Num_Cr,Max_Num_Cr_CalP)  !ÿ���ѷ�����Զ��Ӧ��
  !real(kind=FT) Cracks_CalP_Contact_Strs(Max_Num_Cr,Max_Num_Cr_CalP,2)!ÿ���ѷ����㷨�������Ӵ�Ӧ��(���ڵ�Ӧ������)
  !real(kind=FT) Cracks_CalP_Contact_Force_x(Max_Num_Cr,Max_Num_Cr_CalP)!ÿ���ѷ����㷨�������Ӵ�Ӧ��(��˹���ƽ��ֵ,�����ĽӴ�Ӧ��,��λΪPa)
  !real(kind=FT) Cracks_CalP_Contact_Force_y(Max_Num_Cr,Max_Num_Cr_CalP)!ÿ���ѷ����㷨�������Ӵ�Ӧ��(��˹���ƽ��ֵ,�����ĽӴ�Ӧ��,��λΪPa)
  !!-----2016-08-25����--֧���ѷ쿪�Ⱥ͵��������������-------
  !real(kind=FT) Cracks_CalP_wpnp(Max_Num_Cr,Max_Num_Cr_CalP)     !ÿ���ѷ�����֧���ѷ��ʼ����
  !real(kind=FT) Cracks_CalP_wpor(Max_Num_Cr,Max_Num_Cr_CalP)     !ÿ���ѷ�����֧���ѷ��ʼ����
  !real(kind=FT) Cracks_CalP_wdeform(Max_Num_Cr,Max_Num_Cr_CalP)     !֧���ѷ쿪�ȵĵ��Ա�����(С��֧���ѷ쿪�ȱ仯����)
  !real(kind=FT) Cracks_CalP_Conductivity(Max_Num_Cr,Max_Num_Cr_CalP)     !ÿ���ѷ�����֧���ѷ�ĵ���ϵ��
  !real(kind=FT) Cracks_CalP_kf(Max_Num_Cr,Max_Num_Cr_CalP)     !ÿ���ѷ�����֧���ѷ����͸ϵ��
  !!real(kind=FT),ALLOCATABLE:: Cracks_CalP_Elem_CalP(:,:)!ÿ����Ԫ(����ڸ��ѷ����)�����ļ������ʼ��(���ѷ�ֲ����)����Ŀ
  !!-----------���±������ֶ�ѹ�����----------
  !integer MS_Cracks_CalP_Num(20,Max_Num_Cr)              !����ѹ�ѽ���ʱÿ���ѷ��������
  !real(kind=FT) MS_Cracks_CalP_Aper(20,Max_Num_Cr,Max_Num_Cr_CalP)        !����ѹ�ѽ���ʱÿ���ѷ����㿪��
  !real(kind=FT) MS_Cracks_CalP_Conc(10,Max_Num_Cr,Max_Num_Cr_CalP)  !����ѹ�ѽ���ʱÿ���ѷ�����Ũ��
  !real(kind=FT) MS_CalP_Propped_Aper(Max_Num_Cr,Max_Num_Cr_CalP)    !���ѷ캬֧�ż��ıպϿ���
  integer,ALLOCATABLE::  Cracks_CalP_Num(:)                 !ÿ���ѷ��������
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Coors(:,:,:)   !ÿ���ѷ���������
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Orient(:,:)  !ÿ���ѷ������Ӧ���ѷ췽λ
  integer,ALLOCATABLE::   Cracks_CalP_Seg(:,:)  !ÿ���ѷ������Ӧ������Ƭ�κ�
  integer,ALLOCATABLE::   Cracks_CalP_Elem(:,:)  !ÿ���ѷ��������ڵ�Ԫ��
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Aper(:,:)  !ÿ���ѷ����㿪��
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Pres(:,:)  !ÿ���ѷ�����ˮѹ
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Tractions(:,:,:)!ת����ֱ������ϵ�µ�ճ��ǣ����(����ճ���ѷ�)
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Pgra(:,:)  !ÿ���ѷ�����ѹ���ݶ�(�з���)
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Velo(:,:)  !ÿ���ѷ���������(�з���)
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Quan(:,:)  !ÿ���ѷ���������(�з���)
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Conc(:,:)  !ÿ���ѷ�����Ũ��
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Remo_Strs(:,:)  !ÿ���ѷ�����Զ��Ӧ��
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Contact_Strs(:,:,:)!ÿ���ѷ����㷨�������Ӵ�Ӧ��(���ڵ�Ӧ������)
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Contact_Force_x(:,:)!ÿ���ѷ����㷨�������Ӵ�Ӧ��(��˹���ƽ��ֵ,�����ĽӴ�Ӧ��,��λΪPa)
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Contact_Force_y(:,:)!ÿ���ѷ����㷨�������Ӵ�Ӧ��(��˹���ƽ��ֵ,�����ĽӴ�Ӧ��,��λΪPa)
  !-----2016-08-25����--֧���ѷ쿪�Ⱥ͵��������������-------
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_wpnp(:,:)     !ÿ���ѷ�����֧���ѷ��ʼ����
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_wpor(:,:)     !ÿ���ѷ�����֧���ѷ��ʼ����
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_wdeform(:,:)     !֧���ѷ쿪�ȵĵ��Ա�����(С��֧���ѷ쿪�ȱ仯����)
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_Conductivity(:,:)     !ÿ���ѷ�����֧���ѷ�ĵ���ϵ��
  real(kind=FT),ALLOCATABLE::  Cracks_CalP_kf(:,:)     !ÿ���ѷ�����֧���ѷ����͸ϵ��
  !real(kind=FT),ALLOCATABLE:: Cracks_CalP_Elem_CalP(:,:)!ÿ����Ԫ(����ڸ��ѷ����)�����ļ������ʼ��(���ѷ�ֲ����)����Ŀ
  !-----------���±������ֶ�ѹ�����----------
  integer,ALLOCATABLE::  MS_Cracks_CalP_Num(:,:)              !����ѹ�ѽ���ʱÿ���ѷ��������
  real(kind=FT),ALLOCATABLE::  MS_Cracks_CalP_Aper(:,:,:)        !����ѹ�ѽ���ʱÿ���ѷ����㿪��
  real(kind=FT),ALLOCATABLE::  MS_Cracks_CalP_Conc(:,:,:)  !����ѹ�ѽ���ʱÿ���ѷ�����Ũ��
  real(kind=FT),ALLOCATABLE::  MS_CalP_Propped_Aper(:,:)    !���ѷ캬֧�ż��ıպϿ���

  !***************************************************
  !���Ƕ�Ӧ����һ���Ѳ����ѷ������������
  !�Ȳ�ָ���ڴ�ռ�,�������Ѳ���>=2,�����ٷ����ڴ�ռ�
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
  real(kind=FT),ALLOCATABLE:: Map_L_Cracks_CalP_Conc(:,:)    !ӳ�䵽���ѷ����ѷ�����Ũ��
  !----------
  integer Cracks_CalP_Type(Max_Num_Cr,Max_Num_Cr_CalP,2) !ÿ���ѷ���������ͼ�����������ƺ�
                                                         !(1)���������,Cracks_CalP_Type(i_C,i_CalP,1)
                                                         !     0 --- ��ͨ
                                                         !     1 --- ����(i_C)�Ѽ�1����һ����(other_C)��Junction���ü����;
                                                         !     2 --- ����(i_C)�Ѽ�2����һ����(other_C)��Junction���ü����;
                                                         !     3 --- ����(i_C)�в�ĳ������һ����(other_C)��Junction���ü����;
                                                         !     4 --- ����(i_C)�в�ĳ������һ����(other_C)��ʮ�ֽ��湲�ü����;
                                                         !     5 --- ����(i_C)�Ѽⲻ��������������ʱ����ͨ�����,���������
                                                         !           Լ��HFѹ���߽�Ϊ0,����ӳ���Boundary_Cond_HF
                                                         !(2)��Ӧ�����ƺ�other_C,Cracks_CalP_Type(i_C,i_CalP,2)
                                                         !     ˵��: Cracks_CalP_Type(i_C,i_CalP,1)=5ʱ����Ҫ
  integer Cracks_TipJunCalpNum(Max_Num_Cr,2)             !ÿ���ѷ��Ѽ�Junction���Ӧ�ļ�����(��ǰ���Ƶļ�����)
  integer Cracks_MidJunCalpNum(Max_Num_Cr,Max_Num_Cr_CalP) !ÿ���ѷ���Ѽ�Junction���Ӧ�ļ�����(��ǰ���Ƶļ�����)
  !----------�����ȫ�ֱ�ż���֮��Ӧ�ľֲ����---------------------
  !�Ȱ�����ˮ����Ҳ����������������ϵĲ���ˮ����
  integer Cracks_GloNumCalP(Max_Num_Cr,Max_Num_Cr_CalP)        !�������Ƹ�������ȫ�ֱ��
  integer Cracks_LocalNumCalP(Max_Num_Cr*Max_Num_Cr_CalP,2)    !ȫ�ּ�����Ӧ�����ƺź;ֲ�������
                                                               !1: ���ƺ�
                                                               !2: �ֲ�������
  !�Ȱ�����ˮ����Ҳ����������������ϵĲ���ˮ����
  integer Cracks_GloNumCalP_W(Max_Num_Cr,Max_Num_Cr_CalP)      !������ˮ���Ƹ�������ȫ�ֱ��
  integer Cracks_LocalNumCalP_W(Max_Num_Cr*Max_Num_Cr_CalP,2)  !ȫ�ּ�����Ӧ�����ƺź;ֲ�������
                                                               !1: ���ƺ�
                                                               !2: �ֲ�������
  !----------
  integer Num_JunPair                                          !Junction�����
  integer Cracks_JunPair(Max_Num_Cr,2)                         !Junction��Զ�Ӧ�ļ�����(ȫ�ֺ���),1��Ӧ������,2��Ӧ������
  !*********************************
  !ˮ��ѹ�Ѹ߽׵�Ԫ(���ε�Ԫ)���е�
  !*********************************
  integer Ele_Nodes_QuadHF(Max_Num_Cr,Max_Num_Ele_QuadHF,3)   !���ѷ�ˮ��ѹ�Ѷ��ε�Ԫ�������ڵ����
  !********************
  !ˮ���ѷ쵥Ԫ���
  !********************
  real(kind=FT) Cracks_HF_Ele_L(Max_Num_Cr,Max_Num_Cr_CalP-1)         !ÿ��ˮ���ѷ쵥Ԫ�ĳ���
  !*******************************************
  !�ѷ���ͨ��ϵ��� ������ˮ��ѹ��ʱ�õ���
  !*******************************************
  integer  Cracks_Cone_Num(Max_Num_Cr)                        !ÿ���ѷ�ֱ��������������Ŀ(�����Ѽ���в�)
  integer  Cracks_Cone_Cr(Max_Num_Cr,Max_Num_Cone_Cr)         !ÿ���ѷ�ֱ�����������ƺ�(�����Ѽ���в�)
  !--------------�Ѽ���ͨ------------
  integer Cracks_Cone_NumTipCr(Max_Num_Cr)                    !ÿ���ѷ�������Ѽ����������Ƶ���ͨ����Ŀ,Ϊ1����2
  integer Cracks_Cone_TipCrNum(Max_Num_Cr,2)                  !ÿ���ѷ�������Ѽ����������Ƶ���ͨ��ϵ:
                                                              !Cracks_Cone_TipType(i_C,1)--����i_C�Ѽ�1����ͨ���ƺ�
                                                              !Cracks_Cone_TipType(i_C,2)--����i_C�Ѽ�2����ͨ���ƺ�
  integer Cracks_Cone_TipJuEle(Max_Num_Cr,2)                  !ÿ���ѷ�������Ѽ�ֱ�����������ƽ���㵥Ԫ��
  real(kind=FT) Cracks_Cone_TipJuCor(Max_Num_Cr,2,2)          !ÿ���ѷ�������Ѽ�ֱ�����������ƽ��������,�����Ѽ�����
  !--------------�в���ͨ------------
  integer Cracks_Cone_NumMidCr(Max_Num_Cr)                   !��ÿ���ѷ��в�Junction������������Ŀ
  integer Cracks_Cone_MidCrNum(Max_Num_Cr,Max_Num_Cone_Cr)   !��ÿ���ѷ��в�Junction���������ƺ�
  integer Cracks_Cone_MidCrTip(Max_Num_Cr,Max_Num_Cone_Cr)   !��ÿ���ѷ��в�Junction���������ƶ�Ӧ���Ѽ��
  integer Cracks_Cone_MidJuEle(Max_Num_Cr,Max_Num_Cone_Cr)   !ÿ���ѷ��в�Junction�����ڵ�Ԫ��
  real(kind=FT) Cracks_Cone_MidJuCor(Max_Num_Cr, Max_Num_Cone_Cr,2)             !ÿ���ѷ��в�Junction������
  !*****************
  !Ӧ��ǿ���������
  !*****************
  real(kind=FT) KI(Max_Num_Cr,2),KII(Max_Num_Cr,2)           !ÿ�����������Ѽ��I�ͺ�II��Ӧ��ǿ������KI��KII

  !******************************************************
  !ˮ��ѹ����Ȼ�ѷ����(��Check_Crack_Grows_MCSC.f)
  !******************************************************
  integer  Cracks_NF_JiS_Cr_Num(Max_Num_Cr)       !HF��NF�ཻ���ɵ��¼����ѷ��Ӧ����Ȼ�ѷ��
  integer  Cracks_NF_JiS_Stat(Max_Num_Cr,2)       !HF��NF�ཻ����״̬(�����Ѽⶼ��)
  integer  Cracks_NF_T_Stat(Max_Num_Cr,2)         !HF��NF�����������,ȷ���������ѷ��T�ͽ���״̬(���Ѽ��޹�)
  integer  Cracks_NF_Cement_Tip_Num(Max_Num_Cr,2) !HF��NF�����������,�ѷ�i_C�ͽ����ѷ��ཻ��,��������Ȼ�ѷ��1���Ѽ���չ����2���Ѽ���չ�أ�
  integer  Cracks_fric_NF_num(Max_Num_Cr)         !����Ħ������Ȼ�ѷ�,������ѷ��Ӧ��Ħ����Ȼ�ѷ��
  integer  Cracks_QinS_Stat(Max_Num_Cr)           !����Ħ������Ȼ�ѷ�,���ڱ��ÿһ���ѷ��Ƿ�ˮѹ�ѷ���ʴ��
  !****************
  !ճ���ѷ����
  !****************
  integer  Cracks_Coh_Ele_Type(Max_Num_Cr,Max_Num_Cr_CalP-1)    !ÿ���ѷ�ճ�۵�Ԫ������,=1��ʾΪճ�۵�Ԫ��=0Ϊһ�㵥Ԫ
  integer  Cracks_Tip_Num_of_Coh_Ele(Max_Num_Cr,2)              !ÿ���ѷ������Ѽ��Ӧ��ճ�۵�Ԫ��Ŀ(��Ϊ��Ŀ,�����Ǳ��)

  !�ѷ������μ����������
  integer Crack_Tip_Ploy_Inc_Info(Max_Num_Cr,2,5)         !�Ѽ������μ����������õ�:
                                                          !1��¼����μ��Ӻ�,2��¼�Ѽ����ڶ���εıߺ�
  !REAL(kind=FT) Cracks_CalP_Coh_FN_FT(Max_Num_Cr,Max_Num_Cr_CalP,2)  !ÿ���ѷ����㷨�������ճ����
  integer Key_Crack_Aperture_Method     !�ѷ쿪�ȼ��㷽��, =1:���ݹ�ʽ����; =2:����ƫ�õ����(default). 2023-08-12.
  
  real(kind=FT) Crack_Max_Min_Aperture(Max_Num_Cr,3)    !����ÿ���ѷ�������С��ƽ������. 2023-08-27.
end module Global_Crack

!-----------------------------------
! 5.2 2D��3D�ѷ칲��. 2022-09-05.
!-----------------------------------
module Global_Crack_Common
  use Global_Float_Type
  !use Global_Ragged_Array_Real_Classs
  !use Global_Ragged_Array_Int_Classs
  implicit none
  save

  integer num_Crack                                             !��ǰ�غɲ���������
  integer num_Arc_Crack                                         !��ǰ�غɲ��Ļ���������
  integer num_Hole,num_Circ_Hole,num_Ellip_Hole                 !�׶���Ŀ
  integer Each_Cr_Poi_Num(1000)                                 !ÿ�����ƶ�Ӧ�������������
  integer num_Na_Crack                                          !��Ȼ�ѷ���Ŀ
  integer Key_Na_Crack_Type                                     !��Ȼ�ѷ������,1: Ħ���ѷ�; 2: �����ѷ�
  integer Key_NaCr_Friction                                     !��Ȼ�ѷ�֮Ħ���ѷ��Ƿ���Ħ��ЧӦ,������,
  integer n_h_Node,n_t_Node,n_j_Node,n_hl_Node,n_c_Node
  !******************
  !������ɿ׶����
  !******************
  integer Key_Random_Hole                    !�Ƿ�������ɿ׶�
  integer num_Rand_Hole                      !������ɵĿ׶���Ŀ
  real(kind=FT) Rand_Hole_R                  !������ɵĿ׶���ƽ���뾶
  real(kind=FT) Rand_Hole_R_Delta            !������ɵĿ׶���ƽ���뾶�ı仯��Χ(+-)
  !******************
  !�ѷ��������
  !******************
  integer Key_Hole_Crack_Generate            !�Ƿ�����׶�λ�������ѷ�
  integer Num_Crack_Hole_Generated           !ÿ���׶��������ɵ��ѷ���Ŀ
  integer num_Hole_Crack_Generated(1000)     !����Hole��Ӧ�����ɵ��ѷ���Ŀ
  integer Hole_Crack_Generated_num(1000,10)  !����Hole��Ӧ�����ɵ��ѷ��

  !------����ѹ��-------
  real(kind=FT) Crack_Pressure(1000)         !����ѹ��(���1000���ѷ�)
  integer Crack_Pressure_Type                !����ѹ������:=1,�̶�ѹ��;=2,ѹ���Զ�����,ʹ���ѷ�������չ

  !integer Key_Cr_Pressure                   !�Ƿ�ʩ���ѷ�ѹǿ:=1,��ѹǿ;=2,����ѹǿ(�Ѽ�Ϊ0);=3,����ѹǿ
  !REAL(kind=FT) Cr_Pressure_Value           !��̬�����ѷ�ѹǿ�Ĵ�С

  !*****************************************
  !�����Ƿ�α�ˮ����������ˮ��ѹ��ʱ�õ���
  !*****************************************
  integer Cracks_HF_State(1000)                        !ÿ���ѷ��Ƿ�ˮ����:=0: ��ˮ;=1: ��ˮ.
  integer Cracks_HF_Propp(1000)                        !ÿ���ѷ��Ƿ���֧�ż�:=0: ��; =1: ��.
  !******************************************************
  !Location of the injection point for the full HF model
  !******************************************************
  real(kind=FT) Inj_Point_Loc(2)                             !Location of the injection point for the full HF model
  integer Current_Inj_Crack                                  !��ǰעˮ�ѷ��
  integer CalP_num_InjP_Local                                !ȫģ��עˮ���Ӧ�ļ�����(��Ӧ�ѷ�ľֲ����)
                                                             !���ڶԳ�HFѹ��,�ñ����Ȼ�ǵ���1,���ԶԳ�ģ�Ͳ���Ҫ�ò���
  real(kind=FT) Cracks_HF_ConPressure(1000)                  !��ˮѹ�ѷ��ˮѹ��С
  !******************************************************
  !�ѷ��Ƿ�������չ
  !******************************************************
  !integer Cracks_Allow_Propa(Max_Num_Cr)                     !�����ѷ��Ƿ�������չ
  integer,ALLOCATABLE::Cracks_Allow_Propa(:)                  !�����ѷ��Ƿ�������չ. BUGFIX2022082101.
  !integer Cracks_Tips_Allow_Propa(Max_Num_Cr,2)              !�����ѷ�����Ѽ��Ƿ�������չ

  integer Each_Na_Cr_Poi_Num(1000)                        !ÿ����Ȼ���ƶ�Ӧ�������������
  integer Key_CS_Crack(1000)                              !��Ǹ��ѷ��Ƿ���ѹ���ѷ죬=1����

  !***************************************************
  !�ѷ��������(��ν�ļ�������ˮ���ѷ쵥Ԫ�Ľڵ�)
  !���ڷ�ˮ��ѹ������,���Ǽ������ƿ��ȵ������
  !-----------
  !��һ�����Ƕ�Ӧ��ǰ���Ѳ�������
  !***************************************************
  integer num_Tol_CalP_Water                          !ģ���ܵĲ���������ϼ����ļ������Ŀ(��ˮѹ�������ƶ�Ӧ��)
  integer num_Tol_CalP_Water_ConP                     !ģ���ܵĲ���������ϼ����ļ������Ŀ(����ˮѹ���ƶ�Ӧ��)
  integer num_Tol_CalP_All                            !ģ���ܵļ������Ŀ(���������ƶ�Ӧ��)
  real(kind=FT) Total_Conductivity                    !ѹ��ϵͳ���ܵ���ϵ��
  real(kind=FT) Ave_Conductivity                      !ѹ��ϵͳ��ƽ������ϵ��
  !----------����������Ӵ�����
  integer,ALLOCATABLE::Ele_NumCalP(:)                          !����Ԫ�ڵļ������,Ele_NumCalP(num_elem)
  integer,ALLOCATABLE::Ele_CalPNum(:,:)                        !����Ԫ�ڵļ������(ȫ��),Ele_CalPNum(num_elem,Max_Num_Ele_CalP)
  integer Conta_Integ_Point                                    !�Ӵ����ֵ���Ŀ
  real(kind=FT) Norm2_Contact_R_PSI_0                          !�Ӵ�������һ���������Ĳв�

  !2022-11-14.
  real(kind=FT),ALLOCATABLE:: Crack_Coor_Range(:,:,:)          !�ѷ���߷�Χ. 2022-11-14.
  !2023-01-07
  integer Key_NaCr_Active_Scheme_3D   !���ڿ���3D��Ȼ�ѷ켤���㷨.
                                 != 1����ʼʱ�̼�����ȫ����Ȼ�ѷ죬��������Ȼ�ѷ����ʵ����Ȼ�ѷ�ߴ磬HF��ͨ���ѷ����ѹ��Һ. Ĭ��.
                                 != 2����HF��ͨ��ż��HF��ͨ���ѷ����ѹ��Һ.
                                 != 3����HF��ͨ��ż��HF��ͨ��������ѷ��ſ���������Ȼ�ѷ���������չ����Ȼ�ѷ촩���ĵ�Ԫ�����Ͷȵ�.
  real(kind=FT) Size_Factor_of_Active_NaCr !����ͨ����Ȼ�ѷ��ֱ��(Size_of_Active_NaCr*�������ڵ�Ԫ�������ߴ�),Key_NaCr_Active_Scheme_3D = 3ʱ�õ�. 2023-01-11.
  real(kind=FT) KIc_NaCr(50000)      !��Ȼ�ѷ�Ķ����Ͷ�. ���50000����Ȼ�ѷ�. 2023-01-12.
  real(kind=FT) St_NaCr(50000)       !��Ȼ�ѷ�Ŀ���ǿ��. ���50000����Ȼ�ѷ�. 2024-02-22. NEWFTU2024022202.
  integer Key_Ele_Max_Related_Cracks !����ָ����Ԫ�����Թ������ѷ���Ŀ. NEWFTU2023022501.
  integer Max_Related_Cracks         !��Ԫ����ѷ������Ŀ. 2023-02-25.
  integer Key_Print_SIFs_to_Screen   !��Ӧ��ǿ�������������Ļ. 2023-08-22.
end module Global_Crack_Common

!------------------
! 5.3 3D�ѷ����
!------------------
module Global_Crack_3D
  use Global_Float_Type
  use Global_Ragged_Array_Real_Classs
  use Global_Ragged_Array_Int_Classs
  implicit none
  save
  !3D�ѷ����
  integer Max_Num_Cr_3D
  !integer Max_N_Node_3D
  !integer Max_N_CalP_3D
  integer Max_Num_El_3D
  !integer Max_N_FluEl_3D
  !----ԭĬ��-----
  !parameter (Max_Num_Cr_3D        = 5   )                 !���10������
  !parameter (Max_N_Node_3D        = 5000   )               !ÿ���ѷ������5000����ɢ�ڵ�(֮ǰ�ù�1000)
  !parameter (Max_N_CalP_3D        = 20000 )                 !ÿ������������20000�������
  !-----��С----
  !parameter (Max_Num_Cr_3D        = 5   )                 !���10������
  !parameter (Max_N_Node_3D        = 1000   )               !ÿ���ѷ������5000����ɢ�ڵ�(֮ǰ�ù�1000)
  !parameter (Max_N_CalP_3D        = 3000 )                 !ÿ������������20000�������
  !-----����----
  !parameter (Max_Num_Cr_3D        = 50   )                 !���50������
  !parameter (Max_N_Node_3D        = 5000   )               !ÿ���ѷ������5000����ɢ�ڵ�(֮ǰ�ù�1000)
  !parameter (Max_N_CalP_3D        = 20000 )                 !ÿ������������20000�������
  !-----����----
  parameter (Max_Num_Cr_3D        = 10000)               !���1000������. 2022-11-25���ӵ�10000.
  !
  !integer :: Max_N_Node_3D        = 200 !���ÿ���ѷ������200����ɢ�ڵ�. IMPROV2022110501. ��������ÿ���ѷ��Զ�����.
  integer :: Max_N_Node_3D(1:Max_Num_Cr_3D)  = 200  !���ÿ���ѷ������200����ɢ�ڵ㣬��������ÿ���ѷ��Զ�����. IMPROV2023081306.
  integer Max_Max_N_Node_3D !2023-08-13.
  !
  !integer :: Max_N_FluEl_3D       = 200 !���ÿ���ѷ������200�����嵥Ԫ. IMPROV20221105011. ��������ÿ���ѷ��Զ�����.
  !
  integer :: Max_N_FluEl_3D(1:Max_Num_Cr_3D)  = 200 !���ÿ���ѷ������200�����嵥Ԫ. ��������ÿ���ѷ��Զ�����. IMPROV2023081304.
  integer Max_Max_N_FluEl_3D !2023-08-13.
  !
  !integer :: Max_N_CalP_3D        = 200 !���ÿ���ѷ������200������ڵ�. IMPROV20221105021. ��������ÿ���ѷ��Զ�����.
  integer :: Max_N_CalP_3D(1:Max_Num_Cr_3D)    = 200 !���ÿ���ѷ������200������ڵ�. ��������ÿ���ѷ��Զ�����. IMPROV2023081305.
  integer Max_Max_N_CalP_3D !2023-08-13.
  !
  integer :: Max_ele_num_CalP     = 100  !ÿ�����嵥Ԫ���100������ڵ�. 2023-08-13. IMPROV2023081303.
  !---------

  !---------
  !real(kind=FT) Crack3D_Coor(Max_Num_Cr_3D,4,3)            !�ѷ�ţ��ѷ��������ţ�(x,y,z)
  real(kind=FT),allocatable::Crack3D_Coor(:,:,:)            !�ѷ�ţ��ѷ��������ţ�(x,y,z).  2022-06-16.
  real(kind=FT),allocatable::Na_Crack3D_Coor(:,:,:)         !�������Ȼ�ѷ죬�ѷ�ţ��ѷ��������ţ�(x,y,z).  2023-01-07.
  real(kind=FT),allocatable::Na_Crack3D_St(:)               !��Ȼ�ѷ�Ŀ���ǿ��. 2023-03-25.
  real(kind=FT),allocatable::Na_Crack3D_KIc(:)              !��Ȼ�ѷ�Ķ����Ͷ�. 2023-03-25.
  real(kind=FT),allocatable::Na_Crack3D_Friction(:)         !��Ȼ�ѷ��Ħ��ϵ��. 2023-03-25.
  integer,ALLOCATABLE:: Each_NaCr3D_Poi_Num(:)              !ÿ����Ȼ������ѷ�ı���. 2023-01-07.
  integer,ALLOCATABLE:: NaCr3D_Status(:,:)                  !����ÿ��3D��Ȼ�ѷ�õ�����״̬��״̬����. 2023-01-09.
                                                            !��1�б��漤��״̬;
                                                            !��2�б������Ȼ�ѷ����ڹ��嵥Ԫ��Ŀ.
  type(Ragged_Int_Array_1D),allocatable::Na_Crack3D_Ele_List(:)   !ʹ�òβ�����, ������Ȼ�ѷ����ڵ�Ԫ�б�. 2023-01-12. NEWFTU2023011202.
  !type(Ragged_Array_2D)::Crack3D_Coor(Max_Num_Cr_3D)       !ÿһ���ѷ����һ��Ragged_Array_2D�����(allocate������ڴ�). 2022-09-02.

  !real(kind=FT),ALLOCATABLE:: Dis_Node_to_FS(:,:)          !�ڵ������ѷ���ķ��ž���,FS��ʾFracture Surface
  type(Ragged_Array_1D),allocatable::Dis_Node_to_FS(:)      !IMPROV2022091802.

  real(kind=FT),ALLOCATABLE::Vector_o_Orient(:,:)            !ԭ������ڸ��ѷ���ķ�λ����(���������,��1,-1,-1��)
  integer,ALLOCATABLE::Sign_o_Orient(:)                     !ԭ������ڸ��ѷ���ķ�λ������

  !real(kind=FT)Cr_Plane_Line_Center(Max_Num_Cr_3D,4,3)     !�ѷ�����ߵ��е�,���ڼ���Ӧ��ǿ������
  !real(kind=FT)Cr_Plane_Line_Center(Max_Num_Cr_3D,10,3)     !�ѷ�����ߵ��е�,���ڼ���Ӧ��ǿ������.  2022-06-16.
  !real(kind=FT)Cr_Plane_Normal_vector(Max_Num_Cr_3D,3)     !�ѷ�����ⷨ������

  !real(kind=FT) KI_3D(Max_Num_Cr_3D,Max_N_Node_3D)                  !ÿ���ѷ�������߽���Ӧ��ǿ������
  !real(kind=FT) KII_3D(Max_Num_Cr_3D,Max_N_Node_3D)
  !real(kind=FT) KIII_3D(Max_Num_Cr_3D,Max_N_Node_3D)
  !real(kind=FT) KI_eq_3D(Max_Num_Cr_3D,Max_N_Node_3D)
  !NEWFTU2022090201.
  type(Ragged_Array_1D),allocatable::KI_3D(:)        !ÿһ���ѷ����һ��Ragged_Array_1D�����(allocate������ڴ�). 2022-09-02.
  type(Ragged_Array_1D),allocatable::KII_3D(:)       !ÿһ���ѷ����һ��Ragged_Array_1D�����(allocate������ڴ�). 2022-09-02.
  type(Ragged_Array_1D),allocatable::KIII_3D(:)      !ÿһ���ѷ����һ��Ragged_Array_1D�����(allocate������ڴ�). 2022-09-02.
  type(Ragged_Array_1D),allocatable::KI_eq_3D(:)     !ÿһ���ѷ����һ��Ragged_Array_1D�����(allocate������ڴ�). 2022-09-02.

  !-------��ɢ�ѷ������-----
  !real(kind=FT) Crack3D_Meshed_Node(Max_Num_Cr_3D,Max_N_Node_3D,3)            !��ɢ��֮���3D�ѷ�ڵ�����,ÿ���ѷ������1000�������
  !integer Crack3D_Meshed_Ele(Max_Num_Cr_3D,Max_N_Node_3D,3)     !��ɢ��֮���3D�ѷ쵥Ԫ���,ÿ���ѷ������1000�������
  !real(kind=FT) Crack3D_Meshed_Node_Value(Max_Num_Cr_3D,Max_N_Node_3D,3)      !��ɢ��֮���3D�ѷ�ڵ����(��һ������Ϊ����)
  !integer Cr3D_Meshed_Node_in_Ele_Num(Max_Num_Cr_3D,Max_N_Node_3D)            !��ɢ��֮���3D�ѷ�ڵ����ڵ�Ԫ��
  integer,allocatable::Crack3D_Meshed_Node_num(:)                !��ɢ��֮���3D�ѷ�ڵ���Ŀ
  !integer Crack3D_Meshed_Node_num_Old(Max_Num_Cr_3D)            !��һ����Ӧ����ɢ��֮���3D�ѷ�ڵ���Ŀ, 2022-06-21.
  !real(kind=FT) Cr3D_Meshed_Node_in_Ele_Local(Max_Num_Cr_3D,Max_N_Node_3D,3)  !��ɢ��֮���3D�ѷ�ڵ����ڵ�Ԫ�ŵľֲ�����
  !real(kind=FT) Crack3D_Meshed_Ele_Attri(Max_Num_Cr_3D,Max_N_Node_3D,5)       !��ɢ��֮���3D�ѷ쵥Ԫ���Բ���(�ܳ��������)
  integer,allocatable::Crack3D_Meshed_Ele_num(:)                 !��ɢ��֮���3D�ѷ쵥Ԫ��Ŀ
  !real(kind=FT)  Crack3D_Meshed_Ele_Nor_Vector(Max_Num_Cr_3D,Max_N_Node_3D,3)    !��ɢ��֮���3D�ѷ쵥Ԫ�ⷨ������
  !real(kind=FT)  Crack3D_Meshed_Node_Nor_Vector(Max_Num_Cr_3D,Max_N_Node_3D,3)   !��ɢ��֮���3D�ѷ�ڵ��ⷨ������
  !integer Crack3D_Meshed_Outline(Max_Num_Cr_3D,Max_N_Node_3D,4) !��ɢ��֮���3D�ѷ���߽������Ϣ
                                                                !����1Ϊ�ѷ�ǰԵ�߽��ߵ�1����
                                                                !����2Ϊ�ѷ�ǰԵ�߽��ߵ�2����
                                                                !����3Ϊ��Ӧ�ĵ�Ԫ��
                                                                !����4���ڱ�Ǹñ߽��ߵ��������Ƿ�������չ,��չ�ǳ�С�Ĳ���(2021-08-20)
  !integer Crack3D_Meshed_Outline_Grow_From(Max_Num_Cr_3D,Max_N_Node_3D)   !���ڱ��3D�ѷ���߽綥����ĸ�����չ����. NEWFTU2022071301.
  integer num_Suspended_Point                                   !����Fracture Front Segmentation,��ʷ�����Ƶĵ����Ŀ(2021-08-20)
  real(kind=FT),allocatable::Suspended_Points(:,:)                       !����Fracture Front Segmentation,��ʷ�����Ƶĵ������(2021-08-20)
  !REAL(kind=FT) Crack3D_Meshed_Outline_Vertex(Max_Num_Cr_3D,Max_N_Node_3D,3)        !��ɢ��֮���3D�ѷ���߽綥������
  !integer Crack3D_Meshed_Outline_Vertex_Ele_num(Max_Num_Cr_3D,Max_N_Node_3D)        !��ɢ��֮���3D�ѷ���߽綥�����ڹ��嵥Ԫ��
  integer,allocatable::Crack3D_Meshed_Outline_num(:)             !��ɢ��֮���3D�ѷ���߽�������
  !------Բ�γ�ʼ3D�ѷ�------
  real(kind=FT),allocatable::Crack3D_Cir_Coor(:,:)               !Բ�γ�ʼ�ѷ�,�ѷ��,Բ������(x,y,z)+�ⷨ������+�뾶
  real(kind=FT),allocatable::Na_Crack3D_Cir_Coor(:,:)            !��ȻԲ�γ�ʼ�ѷ�,�ѷ��,Բ������(x,y,z)+�ⷨ������+�뾶. 2021-01-07.
  !------��Բ��ʼ3D�ѷ�------
  real(kind=FT),allocatable::Crack3D_Ellip_Coor(:,:)             !��Բ��ʼ�ѷ�,�ѷ��,Բ������(x,y,z)+�ⷨ������+�뾶a+�뾶b
  !real(kind=FT) Crack3D_Meshed_Vertex_x_Vector(Max_Num_Cr_3D,Max_N_Node_3D,3) !��ɢ��֮���3D�ѷ�߽��ľֲ�x��������
  !real(kind=FT) Crack3D_Meshed_Vertex_y_Vector(Max_Num_Cr_3D,Max_N_Node_3D,3) !��ɢ��֮���3D�ѷ�߽��ľֲ�y��������
  !real(kind=FT) Crack3D_Meshed_Vertex_z_Vector(Max_Num_Cr_3D,Max_N_Node_3D,3) !��ɢ��֮���3D�ѷ�߽��ľֲ���������
  !real(kind=FT) Crack3D_Meshed_Vertex_T_Matrx(Max_Num_Cr_3D,Max_N_Node_3D,3,3)!��ɢ��֮���3D�ѷ�߽�����ת����
  !real(kind=FT) Crack3D_Meshed_Vertex_T_Theta(Max_Num_Cr_3D,Max_N_Node_3D,3)!��ɢ��֮���3D�ѷ�߽�����ת��
  !real(kind=FT) Crack3D_Vector_S1(Max_Num_Cr_3D,Max_N_Node_3D,3)!��ɢ��֮���3D�ѷ�ڵ�������Ӧ������

  !type(Ragged_Array_2D)::Crack3D_Meshed_Node(Max_Num_Cr_3D)                    !ʹ�òβ�����, 2022-09-03.
  !type(Ragged_Int_Array_2D)::Crack3D_Meshed_Ele(Max_Num_Cr_3D)   !ʹ�òβ�����, 2022-09-03.
  !type(Ragged_Array_2D)::Crack3D_Meshed_Node_Value(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-03.
  !type(Ragged_Int_Array_1D)::Cr3D_Meshed_Node_in_Ele_Num(Max_Num_Cr_3D)   !ʹ�òβ�����, 2022-09-03.
  !type(Ragged_Array_2D)::Cr3D_Meshed_Node_in_Ele_Local(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-03.
  !type(Ragged_Array_2D)::Crack3D_Meshed_Ele_Attri(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-03.
  !type(Ragged_Array_2D)::Crack3D_Meshed_Ele_Nor_Vector(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-03.
  !type(Ragged_Array_2D)::Crack3D_Meshed_Node_Nor_Vector(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-03.
  !type(Ragged_Int_Array_2D)::Crack3D_Meshed_Outline(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-03.
  !type(Ragged_Array_2D)::Crack3D_Meshed_Vertex_x_Vector(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_2D)::Crack3D_Meshed_Vertex_y_Vector(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_2D)::Crack3D_Meshed_Vertex_z_Vector(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_3D)::Crack3D_Meshed_Vertex_T_Matrx(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_2D)::Crack3D_Meshed_Vertex_T_Theta(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_2D)::Crack3D_Vector_S1(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !logical Crack3D_Meshed_Arrays_Objects_Created          !�߼����������ڱ��Crack3D_Meshed��زβ������Ƿ��Ѿ����ɶ���
  type(Ragged_Array_2D),allocatable::Crack3D_Meshed_Node(:)                    !ʹ�òβ�����, 2022-09-03.
  type(Ragged_Int_Array_2D),allocatable::Crack3D_Meshed_Ele(:)   !ʹ�òβ�����, 2022-09-03.
  type(Ragged_Array_2D),allocatable::Crack3D_Meshed_Node_Value(:)  !ʹ�òβ�����, 2022-09-03.
  type(Ragged_Int_Array_1D),allocatable::Cr3D_Meshed_Node_in_Ele_Num(:)   !ʹ�òβ�����, 2022-09-03.
  type(Ragged_Array_2D),allocatable::Cr3D_Meshed_Node_in_Ele_Local(:)  !ʹ�òβ�����, 2022-09-03.
  type(Ragged_Array_2D),allocatable::Crack3D_Meshed_Ele_Attri(:)  !ʹ�òβ�����, 2022-09-03.
  type(Ragged_Array_2D),allocatable::Crack3D_Meshed_Ele_Nor_Vector(:)  !ʹ�òβ�����, 2022-09-03.
  type(Ragged_Array_2D),allocatable::Crack3D_Meshed_Node_Nor_Vector(:)  !ʹ�òβ�����, 2022-09-03.
  type(Ragged_Int_Array_2D),allocatable::Crack3D_Meshed_Outline(:)  !ʹ�òβ�����, 2022-09-03.
  type(Ragged_Array_2D),allocatable::Crack3D_Meshed_Vertex_x_Vector(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Crack3D_Meshed_Vertex_y_Vector(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Crack3D_Meshed_Vertex_z_Vector(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Crack3D_Meshed_Vertex_T_Matrx(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Crack3D_Meshed_Vertex_T_Theta(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Crack3D_Vector_S1(:)  !ʹ�òβ�����, 2022-09-04.

  !------���嵥Ԫ�����嵥Ԫ��������------
  integer,allocatable::Cracks_FluidEle_num_3D(:)                 !ÿ���ѷ����嵥Ԫ����Ŀ
  integer,allocatable::Cracks_CalP_Num_3D(:)                     !ÿ���ѷ�������Ŀ(��Щ���������)
  integer,allocatable::Cracks_Real_CalP_Num_3D(:)                !ÿ���ѷ�������Ŀ(��������)
  real(kind=FT),allocatable::Cracks_Volume(:)                    !ÿ���ѷ�����
  real(kind=FT),allocatable::Cracks_Volume_Old(:)                !ÿ���ѷ�����(��һ����)
  integer Key_3D_FluEle_Triang                                  !��3D���嵥Ԫ���в��,ȷ��ÿ�����嵥Ԫ���������ε�Ԫ
  integer,allocatable::Cracks_FluidEle_CalP_Glo_Info(:,:)         !����ȫ������ڵ��Ŷ�Ӧ�ľֲ���Ϣ:�����ѷ�š����嵥Ԫ�š���Ӧ���嵥Ԫ������ڵ��, 2022-06-04.  type(Ragged_Int_Array_2D)::Cracks_FluidEle_CalP_3D(Max_Num_Cr_3D)       !ʹ�òβ�����, 2022-09-04.
  real(kind=FT),allocatable::Cracks_FluidEle_CalP_Glo_Insitu(:)           !ȫ�ֱ������ڵ�ĵ�Ӧ��(��ֱ���ѷ��淽��), 2022-06-04.
  real(kind=FT),allocatable::Crack3D_Centroid(:,:)               !3D�ѷ��������

  !integer Cracks_FluidEle_num_3D_Old(Max_Num_Cr_3D)             !��һ����Ӧ��ÿ���ѷ����嵥Ԫ����Ŀ, 2022-06-21.
  !integer Cracks_CalP_Num_3D_Old(Max_Num_Cr_3D)                 !ÿ���ѷ�������Ŀ(��Щ���������). ��һ����. 2022-06-21.
  !integer Cracks_Real_CalPs_3D(Max_Num_Cr_3D,Max_N_CalP_3D)     !ÿ���ѷ�������(��������),2022-06-04.

  !integer Cracks_FluidEle_CalP_3D(Max_Num_Cr_3D,Max_N_CalP_3D, 7)  !ÿ���ѷ����嵥Ԫ�������
  !integer Cracks_FluidEle_Glo_CalP_3D(Max_Num_Cr_3D,Max_N_CalP_3D,7)              !ÿ���ѷ����嵥Ԫ�������(ȫ�ֱ��,�����鼯Q����ı��)
  !integer Cracks_FluidEle_num_CalP_3D(Max_Num_Cr_3D,Max_N_CalP_3D)             !ÿ���ѷ����嵥Ԫ�ļ������Ŀ
  !integer Cracks_FluidEle_EleNum_3D(Max_Num_Cr_3D,Max_N_CalP_3D)!���嵥Ԫ��Ӧ�Ĺ��嵥Ԫ��
  !real(kind=FT) Cracks_FluidEle_Area_3D(Max_Num_Cr_3D,Max_N_CalP_3D)          !ÿ���ѷ����嵥Ԫ�����
  !real(kind=FT) Cracks_FluidEle_Centroid_3D(Max_Num_Cr_3D,Max_N_CalP_3D,3)       !ÿ���ѷ����嵥Ԫ������
  !real(kind=FT) Cracks_FluidEle_LCS_x_3D(Max_Num_Cr_3D,Max_N_CalP_3D,3)!ÿ���ѷ����嵥Ԫ������λ�õľֲ�����ϵx��
  !real(kind=FT) Cracks_FluidEle_LCS_y_3D(Max_Num_Cr_3D,Max_N_CalP_3D,3)       !ÿ���ѷ����嵥Ԫ������λ�õľֲ�����ϵy��
  !real(kind=FT) Cracks_FluidEle_LCS_z_3D(Max_Num_Cr_3D,Max_N_CalP_3D,3)       !ÿ���ѷ����嵥Ԫ������λ�õľֲ�����ϵz��(��������)
  !real(kind=FT) Cracks_FluidEle_LCS_T_3D(Max_Num_Cr_3D,Max_N_CalP_3D,3,3)     !ÿ���ѷ����嵥Ԫ������λ�õľֲ�����ϵ��ת������
  !real(kind=FT) Cracks_FluidEle_Vector_3D(Max_Num_Cr_3D,Max_N_CalP_3D,3)         !ÿ���ѷ����嵥Ԫ��ƽ���ⷨ������
  !real(kind=FT) Cracks_FluidEle_Aper_3D(Max_Num_Cr_3D,Max_N_CalP_3D)           !ÿ���ѷ����嵥Ԫ���ĵĿ���
  !real(kind=FT) Cracks_CalP_Coors_3D(Max_Num_Cr_3D,Max_N_CalP_3D,3)             !ÿ���ѷ���������
  !real(kind=FT) Cracks_CalP_Orient_3D(Max_Num_Cr_3D,Max_N_CalP_3D,3)             !ÿ���ѷ������Ӧ���ѷ췽λ(�ѷ���ĵ�λ�ⷨ������)
  !integer Cracks_CalP_MeshedEl_3D(Max_Num_Cr_3D,Max_N_CalP_3D)  !ÿ���ѷ������Ӧ��������ɢ�ѷ쵥Ԫ��
  !integer Cracks_CalP_Elem_3D(Max_Num_Cr_3D,Max_N_CalP_3D,2)    !ÿ���ѷ��������ڵ�Ԫ�ż���ߺ�
  !real(kind=FT) Cracks_CalP_Aper_3D(Max_Num_Cr_3D,Max_N_CalP_3D)!ÿ���ѷ����㿪��
  !real(kind=FT) Cracks_CalP_UpDis_3D(Max_Num_Cr_3D,Max_N_CalP_3D,3)           !ÿ���ѷ�������ƫ�õ��λ������
  !real(kind=FT) Cracks_CalP_LowDis_3D(Max_Num_Cr_3D,Max_N_CalP_3D,3)          !ÿ���ѷ�������ƫ�õ��λ������
  !real(kind=FT) Cracks_CalP_Pres_3D(Max_Num_Cr_3D,Max_N_CalP_3D)!ÿ���ѷ�����ˮѹ
  !real(kind=FT) Cracks_CalP_Tractions_3D(Max_Num_Cr_3D,Max_N_CalP_3D,3)       !ת����ֱ������ϵ�µ�ճ��ǣ����(����ճ���ѷ�)
  !real(kind=FT) Cracks_CalP_Pgra_3D(Max_Num_Cr_3D,Max_N_CalP_3D)!ÿ���ѷ�����ѹ���ݶ�(�з���)
  !real(kind=FT) Cracks_CalP_Velo_3D(Max_Num_Cr_3D,Max_N_CalP_3D)!ÿ���ѷ���������(�з���)
  !real(kind=FT) Cracks_CalP_Quan_3D(Max_Num_Cr_3D,Max_N_CalP_3D)!ÿ���ѷ���������(�з���)
  !real(kind=FT) Cracks_CalP_Conc_3D(Max_Num_Cr_3D,Max_N_CalP_3D)!ÿ���ѷ�����Ũ��
  !real(kind=FT) Cracks_CalP_Remo_Strs_3D(Max_Num_Cr_3D,Max_N_CalP_3D)   !ÿ���ѷ�����Զ��Ӧ��

  !type(Ragged_Int_Array_2D)::Cracks_FluidEle_CalP_3D(Max_Num_Cr_3D)   !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Int_Array_2D)::Cracks_FluidEle_Glo_CalP_3D(Max_Num_Cr_3D)   !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Int_Array_1D)::Cracks_FluidEle_num_CalP_3D(Max_Num_Cr_3D)   !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Int_Array_1D)::Cracks_FluidEle_EleNum_3D(Max_Num_Cr_3D)     !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_1D)::Cracks_FluidEle_Area_3D(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_2D)::Cracks_FluidEle_Centroid_3D(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_2D)::Cracks_FluidEle_LCS_x_3D(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !typeRagged_Array_2D)::Cracks_FluidEle_LCS_y_3D(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !typeRagged_Array_2D)::Cracks_FluidEle_LCS_z_3D(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !typeRagged_Array_3D)::Cracks_FluidEle_LCS_T_3D(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !typeRagged_Array_2D)::Cracks_FluidEle_Vector_3D(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !typeRagged_Array_1D)::Cracks_FluidEle_Aper_3D(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !typeRagged_Array_2D)::Cracks_CalP_Coors_3D(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !typeRagged_Array_2D)::Cracks_CalP_Orient_3D(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !typeRagged_Int_Array_1D)::Cracks_CalP_MeshedEl_3D(Max_Num_Cr_3D)   !ʹ�òβ�����, 2022-09-04.
  !typeRagged_Int_Array_2D)::Cracks_CalP_Elem_3D(Max_Num_Cr_3D)   !ʹ�òβ�����, 2022-09-04.
  !typeRagged_Array_1D)::Cracks_CalP_Aper_3D(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !typeRagged_Array_2D)::Cracks_CalP_UpDis_3D(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !typeRagged_Array_2D)::Cracks_CalP_LowDis_3D(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !typeRagged_Array_1D)::Cracks_CalP_Pres_3D(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !typeRagged_Array_2D)::Cracks_CalP_Tractions_3D(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !typeRagged_Array_1D)::Cracks_CalP_Pgra_3D(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !typeRagged_Array_1D)::Cracks_CalP_Velo_3D(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !typeRagged_Array_1D)::Cracks_CalP_Quan_3D(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !typeRagged_Array_1D)::Cracks_CalP_Conc_3D(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_1D)::Cracks_CalP_Remo_Strs_3D(Max_Num_Cr_3D)  !ʹ�òβ�����, 2022-09-04.

  type(Ragged_Int_Array_2D),allocatable::Cracks_FluidEle_CalP_3D(:)   !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Int_Array_2D),allocatable::Cracks_FluidEle_Glo_CalP_3D(:)   !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Int_Array_1D),allocatable::Cracks_FluidEle_num_CalP_3D(:)   !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Int_Array_1D),allocatable::Cracks_FluidEle_EleNum_3D(:)     !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_1D),allocatable::Cracks_FluidEle_Area_3D(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Cracks_FluidEle_Centroid_3D(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Cracks_FluidEle_LCS_x_3D(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Cracks_FluidEle_LCS_y_3D(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Cracks_FluidEle_LCS_z_3D(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Cracks_FluidEle_LCS_T_3D(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Cracks_FluidEle_Vector_3D(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_1D),allocatable::Cracks_FluidEle_Aper_3D(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Cracks_CalP_Coors_3D(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Cracks_CalP_Orient_3D(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Int_Array_1D),allocatable::Cracks_CalP_MeshedEl_3D(:)   !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Int_Array_2D),allocatable::Cracks_CalP_Elem_3D(:)   !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_1D),allocatable::Cracks_CalP_Aper_3D(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Cracks_CalP_UpDis_3D(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Cracks_CalP_LowDis_3D(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_1D),allocatable::Cracks_CalP_Pres_3D(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Cracks_CalP_Tractions_3D(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_1D),allocatable::Cracks_CalP_Pgra_3D(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_1D),allocatable::Cracks_CalP_Velo_3D(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_1D),allocatable::Cracks_CalP_Quan_3D(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_1D),allocatable::Cracks_CalP_Conc_3D(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_1D),allocatable::Cracks_CalP_Remo_Strs_3D(:)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_2D),allocatable::Cracks_CalP_k_3D(:)     !ʹ�òβ�����, 2022-11-26.

  !----֧���ѷ쿪�Ⱥ͵��������������-------
  !REAL(kind=FT) Cracks_CalP_wpnp_3D(Max_Num_Cr_3D,Max_N_CalP_3D)        !ÿ���ѷ�����֧���ѷ��ʼ����
  !REAL(kind=FT) Cracks_CalP_wpor_3D(Max_Num_Cr_3D,Max_N_CalP_3D)        !ÿ���ѷ�����֧���ѷ��ʼ����
  !REAL(kind=FT) Cracks_CalP_wdeform_3D(Max_Num_Cr_3D,Max_N_CalP_3D)     !֧���ѷ쿪�ȵĵ��Ա�����(С��֧���ѷ쿪�ȱ仯����)
  !REAL(kind=FT) Cracks_CalP_Conductivity_3D(Max_Num_Cr_3D,Max_N_CalP_3D)     !ÿ���ѷ�����֧���ѷ�ĵ���ϵ��
  !REAL(kind=FT) Cracks_CalP_kf_3D(Max_Num_Cr_3D,Max_N_CalP_3D)               !ÿ���ѷ�����֧���ѷ����͸ϵ��
  !----�Ѽ���ǿ��Ԫ�ͽڵ����----
  integer Solid_El_Max_num_Crs                               !ÿ�����嵥Ԫ���������ѷ���Ŀ, Ĭ��Ϊ5
  parameter (Solid_El_Max_num_Crs   = 8)                     !ԭֵΪ5. ������Ӱ���ڴ�ռ��.
  !integer(kind=1),ALLOCATABLE:: Solid_El_num_Crs(:)          !ÿ�����嵥Ԫ����ѷ���Ŀ, 2022-08-15. IMPROV2022091803.
  integer,ALLOCATABLE:: Solid_El_num_Crs(:)

  integer,ALLOCATABLE:: Solid_El_Crs(:,:)                    !ÿ�����嵥Ԫ����ѷ����, 2022-08-15.
  !----
  !integer,ALLOCATABLE:: Solid_El_Vertex_Num(:,:)                  !ÿ�����嵥Ԫ���е���ɢ�ѷ���߽綥����Ŀ(���3������)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Vertex_Coor(:,:,:,:)       !ÿ�����嵥Ԫ���е���ɢ�ѷ���߽綥������(���3������)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Vertex_Nor_Vec(:,:,:,:)    !ÿ�����嵥Ԫ���е���ɢ�ѷ���߽綥���ⷨ������(���3������)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Vertex_x_Vec(:,:,:,:)      !ÿ�����嵥Ԫ���е���ɢ�ѷ���߽綥��ֲ�x����������(���3������)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Vertex_y_Vec(:,:,:,:)      !ÿ�����嵥Ԫ���е���ɢ�ѷ���߽綥��ֲ�y����������(���3������)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Vertex_z_Vec(:,:,:,:)      !ÿ�����嵥Ԫ���е���ɢ�ѷ���߽綥��ֲ�z����������(���3������)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Pre_Vertex_Coor(:,:,:,:)   !ÿ�����嵥Ԫ���е���ɢ�ѷ���߽綥�����һ��������(���3������)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Pre_Vertex_Nor_Vec(:,:,:,:)!ÿ�����嵥Ԫ���е���ɢ�ѷ���߽綥�����һ�����ⷨ������(���3������)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Pre_Vertex_x_Vec(:,:,:,:)  !ÿ�����嵥Ԫ���е���ɢ�ѷ���߽綥�����һ����ֲ�x����������(���3������)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Pre_Vertex_y_Vec(:,:,:,:)  !ÿ�����嵥Ԫ���е���ɢ�ѷ���߽綥�����һ����ֲ�y����������(���3������)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Pre_Vertex_z_Vec(:,:,:,:)  !ÿ�����嵥Ԫ���е���ɢ�ѷ���߽綥�����һ����ֲ�z����������(���3������)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Nex_Vertex_Coor(:,:,:,:)   !ÿ�����嵥Ԫ���е���ɢ�ѷ���߽綥�����һ��������(���3������)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Nex_Vertex_Nor_Vec(:,:,:,:)!ÿ�����嵥Ԫ���е���ɢ�ѷ���߽綥�����һ�����ⷨ������(���3������)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Nex_Vertex_x_Vec(:,:,:,:)  !ÿ�����嵥Ԫ���е���ɢ�ѷ���߽綥�����һ����ֲ�x����������(���3������)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Nex_Vertex_y_Vec(:,:,:,:)  !ÿ�����嵥Ԫ���е���ɢ�ѷ���߽綥�����һ����ֲ�y����������(���3������)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Nex_Vertex_z_Vec(:,:,:,:)  !ÿ�����嵥Ԫ���е���ɢ�ѷ���߽綥�����һ����ֲ�z����������(���3������)
  !real(kind=FT),ALLOCATABLE:: Solid_El_Tip_BaseLine(:,:,:,:)      !ÿ�����嵥Ԫ�Ѽ���ǿ��׼��
  !real(kind=FT),ALLOCATABLE:: Solid_El_Tip_BaseLine_Nor_Vec(:,:,:)!ÿ�����嵥Ԫ�Ѽ���ǿ��׼���ⷨ������
  !real(kind=FT),ALLOCATABLE:: Solid_El_Tip_BaseLine_x_Vec(:,:,:)  !ÿ�����嵥Ԫ�Ѽ���ǿ��׼���ϵľֲ�����ϵx����������
  !real(kind=FT),ALLOCATABLE:: Solid_El_Tip_BaseLine_y_Vec(:,:,:)  !ÿ�����嵥Ԫ�Ѽ���ǿ��׼���ϵľֲ�����ϵy����������
  !real(kind=FT),ALLOCATABLE:: Solid_El_Tip_BaseLine_z_Vec(:,:,:)  !ÿ�����嵥Ԫ�Ѽ���ǿ��׼���ϵľֲ�����ϵz����������
  !real(kind=FT),ALLOCATABLE:: Solid_El_Tip_BaseLine_T_theta(:,:,:)!ÿ�����嵥Ԫ�Ѽ���ǿ��׼���ϵľֲ�����ϵ��ת��
  !real(kind=FT),ALLOCATABLE:: Solid_El_Tip_BaseLine_T_Matrix(:,:,:,:)!ÿ�����嵥Ԫ�Ѽ���ǿ��׼���ϵľֲ�����ϵ��ת��
  !IMPROV2022090401.
  !type(Ragged_Int_Array_1D)::Solid_El_Vertex_Num(Max_Num_El_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Vertex_Coor(Max_Num_El_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Vertex_Nor_Vec(Max_Num_El_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Vertex_x_Vec(Max_Num_El_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Vertex_y_Vec(Max_Num_El_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Vertex_z_Vec(Max_Num_El_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Pre_Vertex_Coor(Max_Num_El_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Pre_Vertex_Nor_Vec(Max_Num_El_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Pre_Vertex_x_Vec(Max_Num_El_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Pre_Vertex_y_Vec(Max_Num_El_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Pre_Vertex_z_Vec(Max_Num_El_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Nex_Vertex_Coor(Max_Num_El_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Nex_Vertex_Nor_Vec(Max_Num_El_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Nex_Vertex_x_Vec(Max_Num_El_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Nex_Vertex_y_Vec(Max_Num_El_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Nex_Vertex_z_Vec(Max_Num_El_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Tip_BaseLine(Max_Num_El_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_2D)::Solid_El_Tip_BaseLine_Nor_Vec(Max_Num_El_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_2D)::Solid_El_Tip_BaseLine_x_Vec(Max_Num_El_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_2D)::Solid_El_Tip_BaseLine_y_Vec(Max_Num_El_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_2D)::Solid_El_Tip_BaseLine_z_Vec(Max_Num_El_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_2D)::Solid_El_Tip_BaseLine_T_theta(Max_Num_El_3D)  !ʹ�òβ�����, 2022-09-04.
  !type(Ragged_Array_3D)::Solid_El_Tip_BaseLine_T_Matrix(Max_Num_El_3D)  !ʹ�òβ�����, 2022-09-04.
  !logical Solid_El_Arrays_Objects_Created          !�߼����������ڱ��Solid_El��زβ������Ƿ��Ѿ����ɶ���
  type(Ragged_Int_Array_1D),allocatable::Solid_El_Vertex_Num(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Vertex_Coor(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Vertex_Nor_Vec(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Vertex_x_Vec(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Vertex_y_Vec(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Vertex_z_Vec(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Pre_Vertex_Coor(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Pre_Vertex_Nor_Vec(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Pre_Vertex_x_Vec(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Pre_Vertex_y_Vec(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Pre_Vertex_z_Vec(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Nex_Vertex_Coor(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Nex_Vertex_Nor_Vec(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Nex_Vertex_x_Vec(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Nex_Vertex_y_Vec(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Nex_Vertex_z_Vec(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Tip_BaseLine(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Solid_El_Tip_BaseLine_Nor_Vec(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Solid_El_Tip_BaseLine_x_Vec(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Solid_El_Tip_BaseLine_y_Vec(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Solid_El_Tip_BaseLine_z_Vec(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_2D),allocatable::Solid_El_Tip_BaseLine_T_theta(:)  !ʹ�òβ�����, 2022-09-04.
  type(Ragged_Array_3D),allocatable::Solid_El_Tip_BaseLine_T_Matrix(:)  !ʹ�òβ�����, 2022-09-04.

  !--------------------
  integer Num_Check_T_Matrix !Tool_ThetaX_ThetaY_ThetaZ_3D_rotation.f����������ֲ�����ϵת�ǵĻ��ַ���(Ĭ�ϵ���180;ԽС����Խ��)
  integer,allocatable::Cracks_Initial_Meshed(:)           !���ڱ��ÿ���ѷ��Ƿ񱻳�ʼ����ɢ����.
  integer,allocatable::Cracks_Initial_Adjusted(:)         !���ڱ��ÿ���ѷ��Ƿ񱻵�������.
  integer,allocatable::Cracks_Checked_and_Adjusted(:)     !���ڱ��ÿ���ѷ��Ƿ񱻼���ҵ�������. 2022-08-01.
  !--------------------
  integer,allocatable::Crack_Type_Status_3D(:,:)      !���ڱ���ѷ����ͺ��ѷ�״̬.
                                                        !   + ��1��(�ѷ�����)��                 =1��HF�ѷ죻=2����Ȼ�ѷ�; =3,ѹ��ˮ���ѷ�
                                                        !                                         ע��:������Ȼ�ѷ��ѹ��ˮ���ѷ죬�п��ܻ�����HF�ѷ�
                                                        !   + ��2��(�ѷ�״̬)��                 =1��HF�ѷ�ѹ��δ��ɣ�=2��HF�ѷ�ѹ�����
                                                        !   + ��3��(�ѷ��ܷ������չ)��         =1���ܣ�=0������
                                                        !   + ��4��(�ѷ��Ƿ��ѻ������ڵ�)��   =1����; =0; ��
                                                        !   + ��5��(��һ���ѷ��Ƿ�������չ)�� =1����; =0; ��
                                                        !   + ��6��(�����Ǵ���Ȼ�ѷ켤����Σ���Ϊ��Ӧ����Ȼ�ѷ��)
  !3D��Ȼ�ѷ����.
  integer Key_NaCr_Type_3D                               !��ʼ��Ȼ�ѷ�����:=1,����;=2,Բ��;=3,�����.
  integer Num_Poly_Edges_NaCr                            !����γ�ʼ��Ȼ�ѷ�ı�����Ĭ��Ϊ6.
  integer Key_NaCr_Cross                                 !�Ƿ������ʼ�ѷ콻��(Ĭ��Ϊ0,������).
  integer Key_NaCr_Growth                                !�Ƿ������ʼ��Ȼ�ѷ���չ(��HF��ͨǰ)(Ĭ��Ϊ0,������).
  real(kind=FT) NaCr_3D_n_Vector(3)                      !��ʼ�ѷ�ķ��߷���.
  real(kind=FT) NaCr_3D_n_Vector_Delta                   !���߷���Ĳ�������(��λΪ��,Ĭ��Ϊ0).
  real(kind=FT) NaCr_3D_Size                             !���ھ����ѷ�ָ���Ǳ߳�,����Բ���ѷ�ָ���ǰ뾶.
  real(kind=FT) NaCr_3D_Sz_Delta                         !�ߴ�Ĳ�������(Ĭ��Ϊ0).
  real(kind=FT) NaCr_3D_Check_R                          !�Ƿ��ص��ļ��뾶.
  real(kind=FT) NaCr_3D_Rect_Longside_Vector(3)          !�������εĳ��߷�������. 2023-02-28.
  real(kind=FT) NaCr_3D_Rect_L                           !�������εĳ��߳���. 2023-02-28.
  real(kind=FT) NaCr_3D_Rect_W                           !�������εĶ̱߳���. 2023-02-28.
  real(kind=FT) NaCr_3D_Rect_L_Delta                     !�������εĳ��߳��Ȳ�������. 2023-03-01.
  real(kind=FT) NaCr_3D_Rect_W_Delta                     !�������εĶ̱߳��Ȳ�������. 2023-03-01.
  real(kind=FT) NaCr_3D_Rect_Longside_Vector_Delta       !�������εĳ��߷���������������(��λΪ��,Ĭ��Ϊ0). 2023-03-01.
  !3D�ѷ콻��״̬.
  integer,ALLOCATABLE::Cracks_3D_Inter_Status(:,:)

  !3D��ǿ��Ԫ���. 2022-09-05. IMPROV2022090502.
  !integer(Kind=1),ALLOCATABLE:: Elem_Type_3D(:,:)           !IMPROV2022091803. Kind=1�����ڴ�ռ��.
  integer,ALLOCATABLE:: Elem_Type_3D(:,:)
  integer,ALLOCATABLE:: c_POS_3D(:,:)
  !integer(Kind=1),ALLOCATABLE:: Enriched_Node_Type_3D(:,:)  !IMPROV2022091803. Kind=1�����ڴ�ռ��.
  integer,ALLOCATABLE:: Enriched_Node_Type_3D(:,:)

  !real(kind=FT),ALLOCATABLE::Enriched_Node_Crack_n_Vector_3D(:,:,:)!��ǿ�ڵ��Ӧ���ѷ�����ⷨ������,added on 2022-05-12.
  type(Ragged_Array_2D),allocatable::Enriched_Node_Crack_n_Vector_3D(:)  !ʹ�òβ�����. IMPROV2022091801.

  !3D Junction��ǿ��Ԫ���.
  !integer,ALLOCATABLE::Node_Jun_elem_3D(:,:)                      !Junction��ǿ�ڵ��Ӧ��Junction��Ԫ��,added on 2016-07-10.
  !integer,ALLOCATABLE::Jun_Ele_Negative_Cr_Num_3D(:,:)            !Juntion��ǿ��Ԫ��Ӧ�ı����ѷ��
  !real(kind=FT),ALLOCATABLE::Coors_Junction_3D(:,:,:)
  !ʹ�òβ�����:IMPROV2022090701.
  type(Ragged_Int_Array_1D),allocatable::Node_Jun_elem_3D(:)
  type(Ragged_Int_Array_1D),allocatable::Jun_Ele_Negative_Cr_Num_3D(:)
  type(Ragged_Array_2D),allocatable::Coors_Junction_3D(:)

  !integer,ALLOCATABLE::Ele_Num_Tip_Enriched_Node_3D(:,:)    !��ǿ�ڵ��Ӧ����ǿ��Ԫ��(�ο���Ԫ��)
  type(Ragged_Int_Array_1D),allocatable::Ele_Num_Tip_Enriched_Node_3D(:)  !IMPROV2022091804

  integer n_h_Node_3D,n_t_Node_3D,n_j_Node_3D,n_hl_Node_3D,n_c_Node_3D
  !������Ͼ���Q: ���òβ�����. 2022-09-16. IMPROV2022091601.
  type(Ragged_Array_1D),allocatable::Coupled_Q_3D(:)
  type(Ragged_Int_Array_1D),allocatable::Coupled_Q_3D_Index(:) !IMPROV2022111101.

  !EBE-��Ԫ�նȾ������. 2022-09-19.
  !real(kind=FT),ALLOCATABLE::storK_XFEM(:,:,:)
  type(Ragged_Array_2D),allocatable::storK_XFEM(:)
  !real(kind=FT),ALLOCATABLE::storK_XFEM_Old(:,:,:)
  type(Ragged_Array_2D),allocatable::storK_XFEM_Old(:)
  real(kind=FT),ALLOCATABLE::storK_FEM(:,:,:)
  real(kind=FT),ALLOCATABLE::storK_FEM_Sym(:,:)   !2022-11-10.
  !real(kind=FT),ALLOCATABLE::storK_XFEM_Updated(:,:,:)
  type(Ragged_Array_2D),allocatable::storK_XFEM_Updated(:)
  !����
  !integer(kind=2),ALLOCATABLE:: Elem_num_Related_Cracks(:)       !ÿ����Ԫ���������ǿ�ѷ���Ŀ. 2022-07-16. IMPROV2022091803.
  integer,ALLOCATABLE:: Elem_num_Related_Cracks(:)
  integer,ALLOCATABLE:: Elem_Related_Cracks(:,:)  !���ڴ洢ÿ����Ԫ��������ѷ��. ���Key_Ele_Max_Related_Cracks���ѷ�. �����ѷ첻����ǿ.
  !���嵥Ԫ��͸��. 2022-11-28. NEWFTU2022112801.
  real(kind=FT),ALLOCATABLE::Ele_Permeability_3D(:,:)
  !ÿ���ѷ�ı߽������. 2023-02-22.
  real(kind=FT),allocatable::Cracks_Outline_Area(:)   !NEWFTU202302202.
  !
  !�°����.
  !
  integer num_XA_Input_Cracks                              !�����°´������Ȼ�ѷ���Ŀ. 2023-03-23.
  real(kind=FT) XA_Min_Frac_Radius                         !������Ȼ�ѷ�뾶��ֵ. 2023-03-23.
  real(kind=FT),ALLOCATABLE:: XA_Ini_Cracks(:,:,:)         !�°´������Ȼ�ѷ�����. 2023-03-23.
  real(kind=FT),ALLOCATABLE:: XA_Ini_Crack_St(:)           !�°´������Ȼ�ѷ�����. 2023-03-23.
  real(kind=FT),ALLOCATABLE:: XA_Ini_Crack_FracFriction(:) !�°´������Ȼ�ѷ�����. 2023-03-23.
  !���嵥Ԫ�ѷ������. 2023-03-26. NEWFTU2023032601.
  real(kind=FT),ALLOCATABLE::Ele_VolumeRatio_3D(:)
  !2023-08-10. 3D DIM SIF related.
  real(kind=FT) SIFs_DIM_3D_Offset_Delta_Factor,SIFs_DIM_3D_r_1_Factor,SIFs_DIM_3D_r_2_Factor,SIFs_DIM_3D_r_k_Factor
end module Global_Crack_3D

!------------------
! 5.2 ʮ���ѷ����
!------------------
module Global_Cross
  use Global_Float_Type
  implicit none
  save
  integer Max_Num_Cross
  integer n_Cross_Node                                       !ʮ�ֽ�����ǿ�ڵ���Ŀ
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !cccccccccccccccccc         �����ģ���Ʋ���        ccccccccccccccccccccccc
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  parameter (Max_Num_Cross  = 100   )                        !�ܵ�ʮ�ֽ��������Ŀ
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !cccccccccccccccccc         �����ģ���Ʋ���        ccccccccccccccccccccccc
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  integer num_Cross                                          !�ܵ�ʮ�ֽ�����Ŀ
  integer,ALLOCATABLE:: Elem_Type_Cross(:,:)                 !����ʮ���ѷ�
  integer,ALLOCATABLE:: Enriched_Node_Type_Cross(:,:)        !����ʮ���ѷ�
  integer,ALLOCATABLE:: c_POS_Cross(:,:)                     !����ʮ���ѷ�
  integer,ALLOCATABLE:: Node_Cross_elem(:,:)                 !Cross��ǿ�ڵ��Ӧ��Cross��Ԫ��(2017-05-02)
  integer,ALLOCATABLE:: Cross_Point_Cr_num(:,:)              !ÿ��ʮ�ֽ�����Ӧ�������ѷ��
  integer,ALLOCATABLE:: Cross_Point_Ele_num(:)               !ÿ��ʮ�ֽ�����Ӧ�ĵ�Ԫ��
  !real(kind=FT),ALLOCATABLE:: Cross_Point_Cr_Seg_num(:,:)   !����ʮ�����ѷ�
  real(kind=FT),ALLOCATABLE:: Cross_Point_RABCD(:,:,:)       !ÿ��ʮ�ֽ�����Ӧ�Ľ����ABCD������
end module Global_Cross

!--------------
! 5.3 �������
!--------------
module Global_Inclusion
  use Global_Float_Type
  implicit none
  save
  integer Max_Num_Circ_Incl,Max_Num_Tri_Incl
  integer Max_Num_Quad_Incl,Max_Num_Penta_Incl
  integer Max_Num_Ellip_Incl,Max_Num_Incl,Max_Num_Poly_Incl
  integer Max_Num_Edges_Poly
  integer n_Incl_Node            !������ǿ�ڵ���Ŀ
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !cccccccccccccccccc         �����ģ���Ʋ���        ccccccccccccccccccccccc
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  parameter (Max_Num_Incl  = 500   )           !�ܵļ��������Ŀ
  parameter (Max_Num_Circ_Incl  = 100   )      !���100��Բ�μ���
  !parameter (Max_Num_Tri_Incl   = 100   )      !���100�������μ���
  !parameter (Max_Num_Quad_Incl  = 100   )      !���100���ı��μ���
  !parameter (Max_Num_Penta_Incl = 100   )      !���100������μ���
  parameter (Max_Num_Poly_Incl = 100   )       !���100������μ���
  parameter (Max_Num_Ellip_Incl = 100   )      !���100����Բ�μ���
  parameter (Max_Num_Edges_Poly = 100   )      !ÿ������μ��ӵı��������Ŀ
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !cccccccccccccccccc         �����ģ���Ʋ���        ccccccccccccccccccccccc
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  integer num_Inclusion                                         !�ܵļ�����Ŀ
  integer num_Circ_Incl                                    !Բ�μ���,ͨ��Բ�κͰ뾶����
  !integer num_Tri_Incl                                     !�����μ���,ͨ������������궨��
  !integer num_Quad_Incl                                    !�ı��μ���,ͨ��4��������궨��
  !integer num_Penta_Incl                                   !����μ���,ͨ��5��������궨��
  integer num_Poly_Incl                                    !����μ���
  integer num_Ellip_Incl                                   !��Բ�μ���
  !Բ�μ���λ�ô�С�Լ����ϺŵĶ������
  real(kind=FT) Circ_Inclu_Coor(Max_Num_Circ_Incl,3)       !(x,y,r)
  integer Circ_Inclu_Mat_Num(Max_Num_Circ_Incl)            !Բ�μ��ӵĲ��Ϻ�
  !����μ���λ�ô�С�Լ����ϺŵĶ������
  real(kind=FT) Poly_Incl_Coor_x(Max_Num_Poly_Incl,Max_Num_Edges_Poly)       !����μ��ӵ�x����
  real(kind=FT) Poly_Incl_Coor_y(Max_Num_Poly_Incl,Max_Num_Edges_Poly)       !����μ��ӵ�y����
  integer Poly_Inclu_Edges_Num(Max_Num_Poly_Incl)          !ÿ������μ��ӵı���(�������ڶ�����)
  integer Poly_Inclu_Mat_Num(Max_Num_Poly_Incl)            !����μ��ӵĲ��Ϻ�
  !�պ���ʽ�Ķ���μ���
  real(kind=FT) Poly_Incl_Coor_x_Cl(Max_Num_Poly_Incl,Max_Num_Edges_Poly)
  real(kind=FT) Poly_Incl_Coor_y_Cl(Max_Num_Poly_Incl,Max_Num_Edges_Poly)
  !��ǿ��Ԫ���
  integer,ALLOCATABLE:: Elem_Type_Incl(:,:)                !���ڼ���
  integer,ALLOCATABLE:: Enriched_Node_Type_Incl(:,:)       !���ڼ���
  integer,ALLOCATABLE:: c_POS_Incl(:,:)                    !���ڼ���
  !������ɹ���������
  integer Key_Rand_Circ_Incl                 !�Ƿ��������Բ�μ���
  integer num_Rand_Circ_Incl                 !������ɵ�Բ�μ�����Ŀ
  real(kind=FT) Rand_Circ_Incl_R             !������ɵĶ���μ������Բƽ���뾶
  real(kind=FT) Rand_Circ_Inc_R_Delta        !������ɵĶ���μ������Բ�뾶�仯��Χ(+-)
  integer Key_Rand_Poly_Incl                 !�Ƿ�������ɶ���μ���
  integer num_Rand_Poly_Incl                 !������ɵĶ���μ�����Ŀ
  integer num_Vert_Poly_Incl                 !������ɵĶ���μ��ӵı���
  real(kind=FT)Rand_Poly_Incl_R              !������ɵĶ���μ������Բƽ���뾶
  real(kind=FT)Rand_Poly_Inc_R_Delta         !������ɵĶ���μ������Բ�뾶�仯��Χ(+-)
  !������ɲ�����������
  integer Key_Rand_Poly_Incl_Irregular              !�Ƿ�������ɲ��������μ���
  integer num_Rand_Poly_Incl_for_Each_Type(10)      !�������ӵ���Ŀ,���ɶ���10���ߴ�
  real(kind=FT) Rand_Poly_Incl_R_Min_and_Max(10,2)  !�������ӵİ뾶��Χ
  real(kind=FT) Rand_Poly_Incl_Irregular_Extension_Factor     !�쳤��(1.0-3.0)
  real(kind=FT) Rand_Poly_Incl_Irregular_Inclination          !���(�����쳤��>1ʱ)
  real(kind=FT) Rand_Poly_Incl_Irregular_R_Delta_Factor       !��������ɹ����еİ뾶�仯ϵ��:(0.0 to 1.0),delta_R = Factor*R
  real(kind=FT) Rand_Poly_Incl_Irregular_Angle_Factor         !��������ɹ����еĽǶȱ仯ϵ��:(0.0 to 1.0)
end module Global_Inclusion

!-----------------------------------------------
! 6.��Ԫ�߳����������������ã����Ե���������
!-----------------------------------------------
module Global_Elem_Area_Vol
  use Global_Float_Type
  implicit none
  save
  real(kind=FT) :: Max_Elem_Area,Min_Elem_Area,Ave_Elem_Area,Ave_Elem_L,Ave_Elem_Vol,Max_Elem_Vol,Min_Elem_Vol
  real(kind=FT) :: Max_Elem_Area_Enrich,Min_Elem_Area_Enrich!��ǿ��Ԫ��Ӧ��
  real(kind=FT) :: Ave_Elem_Area_Enrich,Ave_Elem_L_Enrich
  real(kind=FT) :: Ave_Elem_Vol_Enrich
  real(kind=FT) :: Min_Ele_Edge_Length,Max_Ele_Edge_Length
  real(kind=FT) :: Ave_Elem_L_Enrich_Unlocalrefined    !�ֲ�����ǰ����ǿ��Ԫ��������(2021-08-22)
end module Global_Elem_Area_Vol

!----------------------------
!    7.���ϲ���
!----------------------------
module Global_Material
  use Global_Float_Type
  implicit none
  save
  real(kind=FT),ALLOCATABLE:: D(:,:,:)
  real(kind=FT),ALLOCATABLE:: D4(:,:,:)    !4x4�Ĳ��Ͼ���,Ref:Introduction_to_Nonlinear_Finite_Element_Analysһ�����İ��5��200ҳ
  real(kind=FT),ALLOCATABLE:: D_Comp(:,:,:) !Composite material
  real(kind=FT),ALLOCATABLE:: S(:,:,:)    !D������
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
  real(kind=FT),ALLOCATABLE:: Lame_lambda(:)       !��÷����
  real(kind=FT),ALLOCATABLE:: Lame_mu(:)           !��÷����
  real(kind=FT),ALLOCATABLE:: Ele_ComMat_RotMatrix(:,:,:)   !��Ԫ���ϲ��ϵ���ת����
  real(kind=FT),ALLOCATABLE:: MC_dt(:)!Mohr-Coulomb׼������Է���pseudo time step,Ref:MATLAB FEM Code - From Elasticity to Plasticity, Eq.6.10
  real(kind=FT),ALLOCATABLE:: MC_phi_deg(:)   !��Ħ����
  real(kind=FT),ALLOCATABLE:: MC_phi_rad(:)
  real(kind=FT),ALLOCATABLE:: MC_psi_deg(:)   !���ͽ�
  real(kind=FT),ALLOCATABLE:: MC_psi_rad(:)
  real(kind=FT),ALLOCATABLE:: MC_c(:)         !ճ����
  real(kind=FT),ALLOCATABLE:: D_for_cylindrical(:,:,:) !����������ϵ
  real(kind=FT) St_KIc_Conversion                      !����ǿ�ȺͶ����ͶȻ���ϵ��. KIc = St/St_KIc_Conversion. 2023-03-25.
end module Global_Material

!----------------------------
!   8.λ��
!----------------------------
module Global_DISP
  use Global_Float_Type
  implicit none
  save
  real(kind=FT),ALLOCATABLE:: DISP(:),DISP_InSitu(:)
  real(kind=FT),ALLOCATABLE:: DISP_Cylinder(:)
  !2D Gauss��λ��
  real(kind=FT),ALLOCATABLE:: DISP_x_Gauss(:)
  real(kind=FT),ALLOCATABLE:: DISP_y_Gauss(:)
  !3D Gauss��λ��
  real(kind=FT),ALLOCATABLE:: DISP_z_Gauss(:)
end module Global_DISP

!----------------------------
!    9.ˮ��ѹ��
!----------------------------
module Global_HF
  use Global_Float_Type
  implicit none
  save
  integer Max_Num_Frac              !���������������Ѳ���
  integer Key_HF_num_Contact        !ÿ�����Ѳ�ִ�еĽӴ�������Ŀ(Ĭ�Ͻ��ڵ�һ��������ϵ�����ִ�нӴ�����,��=1)
  integer Max_Num_Inject_Crack      !�������е����עˮ������
  integer Max_MS_Num_Crack
  integer Key_AAP                   !�Ƿ���6,7�ŵ�������AAP����
  integer Key_Proppant              !�Ƿ���֧�ż�
  integer Key_IniPre_PassOn         !�������Ѳ���ʼ����ʱ�Ƿ�̳���һ���Ѳ�����ˮѹ�����ȵ�
                                    !                    0:���̳�,total_time����,�ѷ쿪�ȴ�0��ʼ����
                                    !                    1:�̳�,˼·����ҵıʼ�,V3_P78
  real(kind=FT) SOR_factor       !NR�����͸��߷�������γ��ɳڵ���ϵ����Ĭ��ֵΪ0.75
  integer Key_HF_Conv_Crite            !��������׼��, 1: ͨ���ѷ쿪���ж�
                                    !              2: ͨ���ѷ쿪�Ⱥ�ˮѹ
  integer Key_Cal_deltaTime         !����ʱ��������׼��,1:���η�,delta_V=delta_L*delta_w1,
                                    !                     �����delta_VƫС,����delta_TimeƫС
                                    !                     Picard����������þ��η�,�����������ˮѹ�������½���0��
                                    !                   2:���η�,delta_L*0.5*(w1+w2)
                                    !                     �����delta_VƫС,����delta_Timeƫ��
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !cccccccccccccccccc         �����ģ���Ʋ���        ccccccccccccccccccccccc
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  parameter (Max_Num_Frac  = 200 )          !ˮ��ѹ�ѷ������200�����Ѳ�
  parameter (Max_Num_Inject_Crack = 10)    !ˮ��ѹ�ѷ������10��עˮ����
  parameter (Max_MS_Num_Crack = 20)        !�ֶ�ѹ�������ѹ�Ѷ���
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !cccccccccccccccccc         �����ģ���Ʋ���        ccccccccccccccccccccccc
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  integer Num_Frac                  !���Ѳ���
  integer ifra_Data_iter_num(Max_Num_Frac)  !ÿһ�����Ѳ��ĵ�һ��������Ӧ���ܵ�������
                                    !���ڴ˲����浱ǰ���Ѳ�ͨ�����ݣ�����ǿ�ڵ���Ϣ��
                                    !�ѷ�������Ϣ��
  !integer Max_Picard_Iter,Max_NR_Iter,Max_Secant_Iter  !���Picard��������,���NR��������,�����߷���������
  integer Max_Picard_Iter,Max_NR_Iter!���Picard��������,���NR��������,�����߷���������
  real(kind=FT) Viscosity           !Viscosity of Water.
  real(kind=FT) Viscosity_Par_m     !��̬ճ��ָ��m.
  integer Key_Visco_Type            !ѹ��Һճ�ȼ��㷽ʽ, 1: ճ�ȱ��ֲ���;2:ճ����֧�ż���Ũ�ȱ仯;3:ʱ��ճ��
  real(kind=FT) Visco_Zoom_Factor!                       ��̬ճ����������������(������Key_Visco_Type=1ʱ)
  integer Key_Leakoff               !�Ƿ���ѹ��Һ��й¶(�������Ѳ�����1ʱ����)
  real(kind=FT) Coeff_Leak          !й¶ϵ��
  real(kind=FT) Dia_of_Proppant     !֧�ż������ֱ��
  real(kind=FT) Alpha_Picard        !Parameter of Picard iteration.
  !real(kind=FT) Picard_Tol          !Tolerance of Picard iteration.
  real(kind=FT) NR_Tol              !Tolerance of Newton-Raphson iteration.
  real(kind=FT) Secant_Tol          !���߷������������ݲ�
  integer Max_PNR_Iter              !P-NR����(�Ƚ���Picard����,�ٽ���NR����)�ܵĵ�������
  integer Max_NR_AAP_Iter           !NR_AAP����������
  integer Max_NR_MS_Iter            !���ڷֶ�ѹ��ģ���NR_MS����������ѹ��������
  integer Max_NR_Red_AAP_Iter       !NR_Red_AAP����������
  integer Num_Pic_PNR_Iter          !P-NR�����Ƚ��е�Picard��������
  integer Max_MNR_Iter              !MNR�����ܵĵ�������
  integer Max_MNR_Red_Iter          !MNR_Red�����ܵĵ�������
  integer Max_Num_Lnsrch            !ÿ��NR���������ִ�е��������ͻ��ݴ���
  real(kind=FT) MNR_Tol             !Tolerance of MNR iteration.
  real(kind=FT) PNR_Tol             !Tolerance of P-NR iteration.
  real(kind=FT) NR_AAP_Tol          !Tolerance of NR_AAP iteration.
  real(kind=FT) NR_Red_AAP_Tol      !Tolerance of NR_Red_AAP iteration.
  real(kind=FT) NR_MS_Tol           !Tolerance of NR_AAP iteration.
  real(kind=FT) MNR_Red_Tol         !Tolerance of MNR_Red_Tol iteration.
  integer Key_Symm_HF               !�Ƿ��ǶԳ�ˮ��ѹ��ģ��
  integer Type_of_HF                ! = 1,�����Ͷ�֧��;=2,ճ��֧��
  real(kind=FT) Viscosity_td_m,Viscosity_td_n !ʱ��ճ�Ȳ���
  !-------עˮ���-------
  integer Inject_Crack_Num          !����עˮ������ƺ�
  !integer Inject_Cracks(Max_Num_Inject_Crack)!����עˮ������ƺ�
  !real(kind=FT) Inject_Qs(Max_Num_Inject_Crack)    !ÿ�����Ƶ�עˮ��
  !real(kind=FT) Inject_Q                           !עˮ��
  real(kind=FT) Inject_Q_Time(200)                   !עˮ��ʱ��(���20��ʱ���)
  real(kind=FT) Inject_Q_Val(200)                    !עˮ������ֵ
  integer Key_Propp_Trans                           !�Ƿ���֧�ż�������
  real(kind=FT) Max_c                               !��������֧�ż�Ũ��ֵ
  real(kind=FT) Inject_c_Time(200)                   !עˮ��֧�ż�Ũ�ȸı�ʱ��
  real(kind=FT) Inject_c_Val(200)                    !עˮ��֧�ż�Ũ��ֵ
  real(kind=FT) Inject_P_Time(200)                   !עˮ��עˮˮѹ�ı�ʱ��
  real(kind=FT) Inject_P_Val(200)                    !עˮ��עˮˮѹ
  logical Propp_Trans_Start                         !�߼�����,���ڱ��֧�ż��Ƿ�ʼ������
  integer Counter_Num_iFrac(Max_Num_Frac)           !�����Ѳ�������Ӧ���ܵĵ�������
  real(kind=FT),ALLOCATABLE:: F_ConP(:)
  !--------�ֶ�ѹ�����--------
  integer i_MS,MS_Crack_Num                              !�ֶ��ѷ����(�ѷ���,���֧��20��ѹ��)
  integer MS_NaturalCr_Num                               !�ֶ�ѹ��ģ���е���Ȼ�ѷ���Ŀ
  integer MS_Crack_Order(Max_MS_Num_Crack)               !����ѹ�Ѷ�Ӧ�ĳ�ʼ�ѷ��
  !integer MS_Each_Cr_Poi_Num(Max_MS_Num_Crack)          !�ֶ�ѹ�ѳ�ʼ��ߵ���Ŀ
  !real(kind=FT) MS_Crack_Coor(Max_MS_Num_Crack,200,2)   !�����ѷ�����
  real(kind=FT) MS_InP_Loc(Max_MS_Num_Crack,1:2)         !����עˮ������
  integer MS_Finish_Counter_Iter(Max_MS_Num_Crack)       !����ѹ�����ʱ��Ӧ���ܵĵ�����
  real(kind=FT) HF_Theor_Time                            !ѹ�ѵ�һ�����ȵ�����ʱ��
  real(kind=FT) HF_Theor_Aper                            !ѹ�ѵ�һ�����ȵ�������󿪶�
  integer Key_Paper1_Alter
  integer Key_Paper1_finish
  real(kind=FT) Last_Inj_Pres                            !��һ��עˮ���ˮѹ
  !����ˮ����
  real(kind=FT)  Current_SlipWater_P                     !��ǰ��ˮѹ(�����ѷ�����һ��)
  real(kind=FT)  Picard_Alpha                            !ˮ��ѹ��Picard����ϵ��(3Dˮ��ѹ��)
  real(kind=FT)  Picard_Tol                              !ˮ��ѹ��Picard���������ݲ�
  !��Ͳ���, 2022-04-19
  integer Max_WB,Max_Stages,Max_Clust
  parameter (Max_WB       = 20)                              !���Ͳ��Ŀ
  parameter (Max_Stages   = 20)                              !ÿ����Ͳ�����ֶ���Ŀ
  parameter (Max_Clust = 20)                                 !ÿ���ֶε�����ѷ���Ŀ
  integer num_Wellbore                                       !��Ͳ��Ŀ�����20����Ͳ��Ĭ��0��
  integer num_Points_WB(Max_WB)                              !���20����Ͳ
  real(kind=FT) Wellbore_Coors(Max_WB,20,3)                  !��Ͳ�ĵ������,ÿ����Ͳ���20����
  integer num_Stages_Wellbores(Max_WB)                       !ÿ����Ͳ�ķֶ���
  integer num_Crs_Stages_Wellbores(Max_WB,Max_Stages)             !ÿ����Ͳÿ���ֶεķֶδ���(�ѷ���)
  real(kind=FT) Injection_Q_Stages_Wellbores(Max_WB,Max_Stages)      !ÿ����Ͳÿ���ֶε�ѹ��Һ����,��λ��m^3/s
  real(kind=FT) Injection_T_Stages_Wellbores(Max_WB,Max_Stages)      !ÿ����Ͳÿ���ֶε�ѹ��Һע��ʱ��,��λ��s
  integer Key_Gen_Ini_Crack_Wellbores                        !�Ƿ��Զ����ɳ�ʼ�ѷ죨ע:�Զ����ɵĳ�ʼ�ѷ촹ֱ�ھ�Ͳ��
                                                             !=1,���ɾ��γ�ʼ�ѷ�
                                                             !=2,����Բ�γ�ʼ�ѷ�
                                                             !=3,���ɶ���γ�ʼ�ѷ�
  integer Num_Poly_Edges_NaCr_WB                             !����γ�ʼ�ѷ�ı���
  real(kind=FT) Size_Ini_Crack_Wellbores                     !�Զ����ɵĳ�ʼ�ѷ�ߴ�(������)
  real(kind=FT) Wellbores_Start_Point(Max_WB,3)              !��Ͳ�Ϸֶ�ѹ�ѵ����������յ�����(ע:�����ѷ��ڸ��ξ��ȷֲ������Գ�ʼ�ѷ첻һ���ڶ˵�λ��)
  real(kind=FT) Wellbores_End_Point(Max_WB,3)                !��Ͳ�Ϸֶ�ѹ�ѵ����������յ�����(ע:�����ѷ��ڸ��ξ��ȷֲ������Գ�ʼ�ѷ첻һ���ڶ˵�λ��)
  integer Cracks_Stages_Wellbores(Max_WB,Max_Stages,Max_Clust)  !ÿ����Ͳÿ���ֶζ�Ӧ�ĸ����ѷ���
  
  integer Key_3D_HF_SlipWater_fk_Type                          !2023-08-08. 3D��ˮѹ���Ѽ�fk�����������ͣ� NEWFTU2023080801.
                                                               !   =1,���ֵ�ﵽKIc(default);=2,ƽ��ֵ�ﵽKIc;=3,��Сֵ�ﵽKIc
                                                               !   ע������PhiPsi3D_Static_HF_SlipWater��������.          
  integer SlipWater_Max_Time_Steps_3D,SlipWater_Max_Pres_Steps_3D !3D SlipWater NR����ʱ�䲽��ѹ��������������. IMPROV2024022801.
  integer SlipWater_Time_Step_Conv_Check                          !3D SlipWater NR����ʱ�䲽�������(Ĭ��Ϊ0�������м��). IMPROV2024022802.
  integer SlipWater_Pres_Step_Conv_Check                          !3D SlipWater NR����ʱ�䲽�������(Ĭ��Ϊ0�������м��). IMPROV2024022802.
end module Global_HF

!----------------------------
!  10.�ѷ���Ӵ���֧�ż����
!----------------------------
module Global_Contact
  use Global_Float_Type
  implicit none
  save
  integer,ALLOCATABLE:: Elem_Conta_Sta(:,:)   !��ǰ�Ӵ�������ÿ����Ԫ�ĽӴ�״̬(�����ÿ�����ƣ�(num_elem,Max_Num_Cr))
                                              ! 1: ״̬,=0,δ�Ӵ�;=1,�Ӵ�
  integer,ALLOCATABLE:: Elem_Conta_Sta_Last(:,:)   !��һ�Ӵ�������ÿ����Ԫ�ĽӴ�״̬,(num_elem,2)
                                              ! 1: ״̬,=0,δ�Ӵ�;=1,�Ӵ�
  real(kind=FT),ALLOCATABLE:: Elem_Proppant_Coor(:,:)    !����֧�ż�����,����֧�ż���Ԫ��֧�ż�ֱ��������
end module Global_Contact

!----------------------------
!    11.Ӧ�����
!----------------------------
module Global_Stress
  use Global_Float_Type
  implicit none
  save
  !2D�ڵ�Ӧ��
  real(kind=FT),ALLOCATABLE:: Stress_xx_Node(:)
  real(kind=FT),ALLOCATABLE:: Stress_yy_Node(:)
  real(kind=FT),ALLOCATABLE:: Stress_xy_Node(:)
  real(kind=FT),ALLOCATABLE:: Stress_vm_Node(:)
  !2D�ڵ�Ӧ����Ӧ��
  real(kind=FT),ALLOCATABLE:: TStress_xx_Node(:)
  real(kind=FT),ALLOCATABLE:: TStress_yy_Node(:)
  real(kind=FT),ALLOCATABLE:: TStress_xy_Node(:)
  real(kind=FT),ALLOCATABLE:: TStress_vm_Node(:)
  !3D�ڵ�Ӧ������
  real(kind=FT),ALLOCATABLE:: Stress_zz_Node(:)
  real(kind=FT),ALLOCATABLE:: Stress_yz_Node(:)
  real(kind=FT),ALLOCATABLE:: Stress_xz_Node(:)
  !2D Gauss��Ӧ��
  real(kind=FT),ALLOCATABLE:: Stress_xx_Gauss(:)
  real(kind=FT),ALLOCATABLE:: Stress_yy_Gauss(:)
  real(kind=FT),ALLOCATABLE:: Stress_xy_Gauss(:)
  real(kind=FT),ALLOCATABLE:: Stress_vm_Gauss(:)
  !2D Gauss����Ӧ��
  real(kind=FT),ALLOCATABLE:: TStress_xx_Gauss(:)
  real(kind=FT),ALLOCATABLE:: TStress_yy_Gauss(:)
  real(kind=FT),ALLOCATABLE:: TStress_xy_Gauss(:)
  real(kind=FT),ALLOCATABLE:: TStress_vm_Gauss(:)
  !3D Gauss��Ӧ������
  real(kind=FT),ALLOCATABLE:: Stress_zz_Gauss(:)
  real(kind=FT),ALLOCATABLE:: Stress_yz_Gauss(:)
  real(kind=FT),ALLOCATABLE:: Stress_xz_Gauss(:)
  !��ԪӦ��״̬,�Ƿ������1-��3 > Tol
  integer,ALLOCATABLE:: Ele_State_Stress_1_3(:)
  !2D Gauss��Ӧ��_��ˮѹʱ��Ӧ�������µĸ�˹Ӧ����
  real(kind=FT),ALLOCATABLE:: Stress_xx_Gas_InSitu(:)
  real(kind=FT),ALLOCATABLE:: Stress_yy_Gas_InSitu(:)
  real(kind=FT),ALLOCATABLE:: Stress_xy_Gas_InSitu(:)
  real(kind=FT),ALLOCATABLE:: Stress_vm_Gas_InSitu(:)

  !2D Gauss��Ӧ��_��ʵˮѹ(�Ǿ�ˮѹ)�����µĸ�˹Ӧ����
  real(kind=FT),ALLOCATABLE:: Stress_xx_Gas_InSitu2(:)
  real(kind=FT),ALLOCATABLE:: Stress_yy_Gas_InSitu2(:)
  real(kind=FT),ALLOCATABLE:: Stress_xy_Gas_InSitu2(:)
  real(kind=FT),ALLOCATABLE:: Stress_vm_Gas_InSitu2(:)
  !Gauss�㴦�ĳ�ʼӦ����
  real(kind=FT),ALLOCATABLE:: InSitu_Strs_Gaus_xx(:,:)
  real(kind=FT),ALLOCATABLE:: InSitu_Strs_Gaus_yy(:,:)
  real(kind=FT),ALLOCATABLE:: InSitu_Strs_Gaus_xy(:,:)
  !3D����
  real(kind=FT),ALLOCATABLE:: InSitu_Strs_Gaus_zz(:,:)
  real(kind=FT),ALLOCATABLE:: InSitu_Strs_Gaus_yz(:,:)
  real(kind=FT),ALLOCATABLE:: InSitu_Strs_Gaus_xz(:,:)
  !�ڵ㴦�ĳ�ʼӦ����
  real(kind=FT),ALLOCATABLE::Str_xx_InSitu(:),Str_yy_InSitu(:),Str_xy_InSitu(:),Str_vm_InSitu(:)
  !3D����
  real(kind=FT),ALLOCATABLE::Str_zz_InSitu(:),Str_yz_InSitu(:),Str_xz_InSitu(:)
  !Բ������ϵ
  real(kind=FT),ALLOCATABLE:: Stress_Crr_Node(:)
  real(kind=FT),ALLOCATABLE:: Stress_Ctt_Node(:)  !tt��ʾtheta_theta
  real(kind=FT),ALLOCATABLE:: Stress_Czz_Node(:)
  real(kind=FT),ALLOCATABLE:: Stress_Crt_Node(:)
  real(kind=FT),ALLOCATABLE:: Stress_Ctz_Node(:)
  real(kind=FT),ALLOCATABLE:: Stress_Crz_Node(:)
  real(kind=FT),ALLOCATABLE:: Stress_Cvm_Node(:)
  !���±������ڿ��ٶ����ʼӦ��������������.
  real(kind=FT) InSitu_S1_3D
  real(kind=FT) InSitu_S2_3D
  real(kind=FT) InSitu_S3_3D
  real(kind=FT) InSitu_S1_nv_3D(3)
  real(kind=FT) InSitu_S2_nv_3D(3)
  real(kind=FT) InSitu_S3_nv_3D(3)
  !�Ǿ��ȳ�ʼӦ����(x,y,z����). 2022-07-06. NEWFTU2022070601.
  integer Key_Nonuniform_InSitu_X_with_Z                        !X������������ȳ�ʼӦ����.
  real(kind=FT) InSitu_Sx_3D_Seg_Strs_X_with_Z(100)             !�ֳ�n��.
  real(kind=FT) InSitu_Sx_3D_Seg_Loca_X_with_Z(100)             !����λ��n+1.
  integer Key_Nonuniform_InSitu_X_with_Y                        !X������������ȳ�ʼӦ����.
  real(kind=FT) InSitu_Sx_3D_Seg_Strs_X_with_Y(100)             !�ֳ�n��.
  real(kind=FT) InSitu_Sx_3D_Seg_Loca_X_with_Y(100)             !����λ��n+1.
  integer Key_Nonuniform_InSitu_Y_with_Z                        !Y������������ȳ�ʼӦ����.
  real(kind=FT) InSitu_Sy_3D_Seg_Strs_Y_with_Z(100)             !�ֳ�n��.
  real(kind=FT) InSitu_Sy_3D_Seg_Loca_Y_with_Z(100)             !����λ��n+1.

  integer Key_Nonuniform_InSitu_Y_with_X                        !Y������������ȳ�ʼӦ����.
  real(kind=FT) InSitu_Sy_3D_Seg_Strs_Y_with_X(100)             !�ֳ�n��.
  real(kind=FT) InSitu_Sy_3D_Seg_Loca_Y_with_X(100)             !����λ��n+1.
  integer Key_Nonuniform_InSitu_Z_with_X                        !Z������������ȳ�ʼӦ����.
  real(kind=FT) InSitu_Sz_3D_Seg_Strs_Z_with_X(100)             !�ֳ�n��.
  real(kind=FT) InSitu_Sz_3D_Seg_Loca_Z_with_X(100)             !����λ��n+1.
  integer Key_Nonuniform_InSitu_Z_with_Y                        !Z������������ȳ�ʼӦ����.
  real(kind=FT) InSitu_Sz_3D_Seg_Strs_Z_with_Y(100)             !�ֳ�n��.
  real(kind=FT) InSitu_Sz_3D_Seg_Loca_Z_with_Y(100)             !����λ��n+1.
  !Gauss�㴦�ĳ�ʼӦ�䳡
  real(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_xx(:,:)
  real(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_yy(:,:)
  real(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_xy(:,:)
  !3D����
  real(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_zz(:,:)
  real(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_yz(:,:)
  real(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_xz(:,:)
  !��Ԫ�ĳ�ʼӦ��. XA. 2023-03-24.
  real(kind=FT),ALLOCATABLE:: XA_Ele_InSitu_S1_Vector(:,:)
  real(kind=FT),ALLOCATABLE:: XA_Ele_InSitu_S2_Vector(:,:)
  real(kind=FT),ALLOCATABLE:: XA_Ele_InSitu_S3_Vector(:,:)
  real(kind=FT),ALLOCATABLE:: XA_Ele_InSitu_S1_S2_S3(:,:)
  !��ԪӦ��. XA. 2023-03-26.
  real(kind=FT),ALLOCATABLE:: XA_Ele_Stress(:,:)
end module Global_Stress

!----------------------------
! 11.2 Ӧ����� (2021-09-10)
!----------------------------
module Global_Strain
  use Global_Float_Type
  implicit none
  save
  !2D�ڵ�Ӧ��
  real(kind=FT),ALLOCATABLE:: Strain_xx_Node(:)
  real(kind=FT),ALLOCATABLE:: Strain_yy_Node(:)
  real(kind=FT),ALLOCATABLE:: Strain_xy_Node(:)
  real(kind=FT),ALLOCATABLE:: Strain_vm_Node(:)
  !Բ������ϵ
  real(kind=FT),ALLOCATABLE:: Strain_Crr_Node(:)
  real(kind=FT),ALLOCATABLE:: Strain_Ctt_Node(:)  !tt��ʾtheta_theta
  real(kind=FT),ALLOCATABLE:: Strain_Czz_Node(:)
  real(kind=FT),ALLOCATABLE:: Strain_Crt_Node(:)
  real(kind=FT),ALLOCATABLE:: Strain_Ctz_Node(:)
  real(kind=FT),ALLOCATABLE:: Strain_Crz_Node(:)
  real(kind=FT),ALLOCATABLE:: Strain_Cvm_Node(:)

  !2D�ڵ�Ӧ����Ӧ��
  real(kind=FT),ALLOCATABLE:: TStrain_xx_Node(:)
  real(kind=FT),ALLOCATABLE:: TStrain_yy_Node(:)
  real(kind=FT),ALLOCATABLE:: TStrain_xy_Node(:)
  real(kind=FT),ALLOCATABLE:: TStrain_vm_Node(:)
  !3D�ڵ�Ӧ������
  real(kind=FT),ALLOCATABLE:: Strain_zz_Node(:)
  real(kind=FT),ALLOCATABLE:: Strain_yz_Node(:)
  real(kind=FT),ALLOCATABLE:: Strain_xz_Node(:)
  !2D Gauss��Ӧ��
  real(kind=FT),ALLOCATABLE:: Strain_xx_Gauss(:)
  real(kind=FT),ALLOCATABLE:: Strain_yy_Gauss(:)
  real(kind=FT),ALLOCATABLE:: Strain_xy_Gauss(:)
  real(kind=FT),ALLOCATABLE:: Strain_vm_Gauss(:)
  !2D Gauss����Ӧ��
  real(kind=FT),ALLOCATABLE:: TStrain_xx_Gauss(:)
  real(kind=FT),ALLOCATABLE:: TStrain_yy_Gauss(:)
  real(kind=FT),ALLOCATABLE:: TStrain_xy_Gauss(:)
  real(kind=FT),ALLOCATABLE:: TStrain_vm_Gauss(:)
  !3D Gauss��Ӧ������
  real(kind=FT),ALLOCATABLE:: Strain_zz_Gauss(:)
  real(kind=FT),ALLOCATABLE:: Strain_yz_Gauss(:)
  real(kind=FT),ALLOCATABLE:: Strain_xz_Gauss(:)
  !��ԪӦ��״̬,�Ƿ������1-��3 > Tol
  integer,ALLOCATABLE:: Ele_State_Strain_1_3(:)
  !2D Gauss��Ӧ��_��ˮѹʱ��Ӧ�������µĸ�˹Ӧ����
  real(kind=FT),ALLOCATABLE:: Strain_xx_Gas_InSitu(:)
  real(kind=FT),ALLOCATABLE:: Strain_yy_Gas_InSitu(:)
  real(kind=FT),ALLOCATABLE:: Strain_xy_Gas_InSitu(:)
  real(kind=FT),ALLOCATABLE:: Strain_vm_Gas_InSitu(:)

  !2D Gauss��Ӧ��_��ʵˮѹ(�Ǿ�ˮѹ)�����µĸ�˹Ӧ����
  real(kind=FT),ALLOCATABLE:: Strain_xx_Gas_InSitu2(:)
  real(kind=FT),ALLOCATABLE:: Strain_yy_Gas_InSitu2(:)
  real(kind=FT),ALLOCATABLE:: Strain_xy_Gas_InSitu2(:)
  real(kind=FT),ALLOCATABLE:: Strain_vm_Gas_InSitu2(:)
  !Gauss�㴦�ĳ�ʼӦ�䳡. BUGFIX2022070906. ȫ�ֱ����ظ�����.
  !REAL(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_xx(:,:)
  !REAL(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_yy(:,:)
  !REAL(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_xy(:,:)
  !3D����
  !REAL(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_zz(:,:)
  !REAL(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_yz(:,:)
  !REAL(kind=FT),ALLOCATABLE:: InSitu_Strain_Gaus_xz(:,:)
  !�ڵ㴦�ĳ�ʼӦ�䳡
  real(kind=FT),ALLOCATABLE::Strain_xx_InSitu(:),Strain_yy_InSitu(:),Strain_xy_InSitu(:),Strain_vm_InSitu(:)
  !3D����
  real(kind=FT),ALLOCATABLE::Strain_zz_InSitu(:),Strain_yz_InSitu(:),Strain_xz_InSitu(:)
end module Global_Strain

!----------------------------
!    12.�������
!----------------------------
module Global_POST
  use Global_Float_Type
  implicit none
  save
  integer Key_Post_CS_G_Coor         !�Ƿ񱣴�Gauss������Calculate and Save Gauss Coors
  integer Key_Post_CS_G_Disp         !�Ƿ���㲢����Gauss��λ��(Ĭ��ֵΪ0)
  integer Key_Post_CS_G_Strs         !�Ƿ���㲢����Gauss��Ӧ��(Ĭ��ֵΪ0)
  integer Key_Post_S_Dof_F           !�Ƿ񱣴�����ɶȵ��غ�ֵ,�Ա����ں���(Ĭ��ֵΪ0)
  integer Key_Post_Cracked_Ele       !�Ƿ���㲢�������ѵĵ�Ԫ,������Ӧ����ȷ��,��1-��3>Tol(Ĭ��ֵΪ0)
  integer Key_Post_S_TanDisp         !�Ƿ���㲢�����ѷ���������λ��
  real(kind=FT) Tol_Stress_1_3       !�Ƿ������1-��3 > Tol�е�Tol��ֵ
  integer Key_Node_Value             !�ڵ�Ӧ�������㷨(=1,ֱ��ƽ����(Ĭ��); =2,��С����ƥ�䷨)
  integer Key_Save_vtk               !�Ƿ񱣴�vtk�ļ�(Ĭ��Ϊ1,����)
  integer Key_Simple_Post            !������������������ݣ���ڵ�λ�ơ��ѷ쿪�ȵ�. 2022-06-25.
  integer Key_Save_Nothing           !�������κ�����. 2022-09-06. NEWFTU2022090601.
  integer Key_Post_Elements_Gauss_Num!�Ƿ񱣴�ÿ����Ԫ��Gauss���ֵ���Ŀ. 2022-07-16.
  integer Key_Get_Permeability       !���㲢�����ѷ���͸��. 2022-11-26.
end module Global_POST

!----------------------------------------
!  13.�������С��Ķ��嶯��ѧģ����ر���
!----------------------------------------
module Global_Rigid_Balls
  use Global_Float_Type
  implicit none
  save
  real(kind=FT) W_Ball_Model     !ģ�͵Ŀ��
  real(kind=FT) H_Ball_Model     !ģ�͵ĸ߶�
  real(kind=FT) Ave_R_Ball       !С���ƽ���뾶
  real(kind=FT) Delta_R_Ball     !С��뾶�ı仯��Χ
  real(kind=FT) Max_R_Ball       !С������뾶
  real(kind=FT) Min_R_Ball       !С�����С�뾶
  integer num_Balls              !С�����Ŀ
end module Global_Rigid_Balls

!------------------------------------------------
!   14.���������(Ҳ�����Ȱ¹̽�֮��϶ˮѹ��)
!------------------------------------------------
module Global_Field_Problem
  use Global_Float_Type
  implicit none
  save
  integer Num_Fd_Bou_fixed                      !������ֵΪ0�Ľڵ���Ŀ
  integer,ALLOCATABLE::Fd_Bou_fixed(:)          !��������ֵΪ0�Ľڵ��
  integer Num_Fd_Bou_vl                         !����ĳ�������ֵ��Ŀ(����0�ͷ�0)
  real(kind=FT),ALLOCATABLE::Fd_Bou_vl(:,:)     !����ĳ�������ֵ�ڵ�źͱ�ֵ��ֵ(����0�ͷ���)
  integer Num_Fd_Bou_vl_nz                      !����ĳ�������ֵ��Ŀ(��0)
  real(kind=FT),ALLOCATABLE::Fd_Bou_vl_nz(:,:)  !����ĳ�������ֵ�ڵ�źͱ�ֵ��ֵ(����)
  integer Num_Fd_Ini_vl                         !����ĳ�������ֵ��Ŀ(����0�ͷ�0)
  real(kind=FT),ALLOCATABLE::Fd_Ini_vl(:,:)     !����ĳ�������ֵ�ڵ�źͳ�ֵ��ֵ(����0�ͷ�0)

  integer Num_Fd_Bou_qn
  !integer Num_Fd_Bou_qx,Num_Fd_Bou_qy
  real(kind=FT),ALLOCATABLE::Fd_Bou_qn(:,:)     !n���������߽�����
  !real(kind=FT),ALLOCATABLE::Fd_Bou_qx(:,:)    !x���������߽�����
  !real(kind=FT),ALLOCATABLE::Fd_Bou_qy(:,:)    !y���������߽�����
  !real(kind=FT) Fd_kxx                          !ϵ��kxx
  !real(kind=FT) Fd_kyy                          !ϵ��kyy
  !real(kind=FT) Fd_c                            !������˲̬�������,�������ȴ�������ı���
  !real(kind=FT) Fd_k_MT(2,2)
  real(kind=FT),ALLOCATABLE::Fd_Value(:)        !�ڵ㳡ֵ
  real(kind=FT),ALLOCATABLE::Fd_ele_k_MT(:,:,:) !ÿ����Ԫ��k����(������ʱ��仯,������Ҫ��������)
  real(kind=FT),ALLOCATABLE::Fd_Flux_x(:)       !�ڵ�x��������ֵ
  real(kind=FT),ALLOCATABLE::Fd_Flux_y(:)       !�ڵ�y��������ֵ
  integer Key_Fd_Body_Source                    !�����������Ƿ�������Դ(�������������������Դ)
  real(kind=FT) Fd_Body_Source                           !��������Դ�Ĵ�С
  !������Biot�̽��϶ˮѹ�����
  real(kind=FT),ALLOCATABLE::Porous_P(:)        !ÿ���ڵ�Ŀ�϶ˮѹ��
  real(kind=FT),ALLOCATABLE::Biot_c_MAT(:,:)    !�Ȱ¹̽�c����,���Smith_5th_P55��V5_P72
  !ʱ�䲽��ʱ��������
  integer Fd_IDy_Num_Iteras                     !��ʽ��̬��������
  real(kind=FT) Delt_Time_Trapez                !��������ʱ�����ʱ��������С
  !������ҳ���������������(Key_Analysis_Type=17)
  integer Key_Gas_Production                    !Key_Analysis_Type = 16ʱ����
  integer GasP_num_Fractures                    !�ܵ�ѹ���ѷ���Ŀ
  real(kind=FT) GasP_Thic_Reservoir             !������
  integer GasP_Well_Nodes(100)                  !
  real(kind=FT) P_Langmuir                      !Langmuirѹ��
  real(kind=FT) V_Langmuir                      !Langmuir���
  real(kind=FT) Density_gst                     !��׼״̬��ҳ�������ܶ�
  real(kind=FT) Width_of_crack
  integer Key_Langmuir_Source                   !�Ƿ��ǽ�������
  real(kind=FT) porosity_Shale                  !ҳ�ҵĿ�϶��
  integer Key_Changing_BHP                      !�Ƿ��Ǳ仯�ĳ��ѹ��
  integer Num_BHP_curve_point
  real(kind=FT),ALLOCATABLE:: BHP_Curve(:,:)    !����ѹ�����ߵ�ʱ��ѹ������
  real(kind=FT) Gas_P_stress_x,Gas_P_stress_y   !��������������Ӧ��ˮƽ
  integer Key_Changing_Kf                       !�Ƿ����ѷ���͸�ʵı仯(�������������������)
  integer Key_Proppant_Active                   !�Ƿ񼤻�֧�ż�,����ѡ���,����Ҫ��ִ��ˮ��ѹ�ѷ���,���õ���wpnp�ļ�
  integer Key_Proppant_Creep                    !�Ƿ���֧�ż������
  integer Key_Proppant_Crush                    !�Ƿ���֧�ż�������
  real(kind=FT) Proppant_Strength               !֧�ż���ǿ��
  real(kind=FT) Proppant_visco_factor           !֧�ż���ճ��ϵ��
  real(kind=FT) Rock_visco_factor               !Χ�ҵ�ճ��ϵ��
  !-----------������XFEM���-------------
  integer Fd_n_h_Node,Fd_n_t_Node,Fd_n_j_Node,Fd_n_hl_Node,Fd_n_c_Node,Fd_n_cross_Node,Fd_n_Incl_Node
  integer Fd_Usual_Freedom,Fd_Enrich_Freedom
  integer ,ALLOCATABLE:: Fd_c_POS(:,:)
  integer ,ALLOCATABLE:: Fd_c_POS_Hl(:,:)
  integer ,ALLOCATABLE:: Fd_c_POS_Cross(:,:)
  integer ,ALLOCATABLE:: Fd_c_POS_Incl(:,:)
  integer Key_Fd_TipEnrich  !�������Ѽ���ǿ����, 0: ���Ѽ���ǿ,�Ѽ��Զ���������Ԫ�߽���
                            !              1: ǿ���,һ��,sqrt(r)*sin(theta/2)
                            !              2: �����,һ��,sqrt(r)*cos(theta/2)
  integer,ALLOCATABLE:: Fd_Ele_yes_FEM_asemd(:)         !���ڱ�Ǹõ�Ԫ��FEM��Ԫ�ն��Ƿ��Ѿ��鼯���ܸ���
  integer,ALLOCATABLE:: Fd_EleGaus_yes_FEM_asemd(:,:)   !���ڱ�Ǹõ�Ԫ�ø�˹���FEM��Ԫ�ն��Ƿ��Ѿ��鼯���ܸ���
  real(kind=FT),ALLOCATABLE:: Fd_Gauss_CoorX(:)
  real(kind=FT),ALLOCATABLE:: Fd_Gauss_CoorY(:)
  real(kind=FT),ALLOCATABLE:: Field_Value_Gauss(:)
  logical Fd_Yes_XFEM
end module Global_Field_Problem

!----------------------
!15.���Ӷ���ѧģ�����
!----------------------
module Global_MD
  use Global_Float_Type
  implicit none
  save
  integer MD_num_molecule              !���ӵ���Ŀ
  real(kind=FT) MD_mss_molecule        !���ӵ�����
  integer MD_num_time_step             !ʱ�䲽��
  real(kind=FT) MD_Delt_Time           !ʱ�䲽��
  real(kind=FT) MD_Dimension_x         !�˶�����x����ߴ�
  real(kind=FT) MD_Dimension_y         !�˶�����y����ߴ�
  real(kind=FT) MD_Dimension_z         !�˶�����z����ߴ�
  integer MD_step_print_num            !ƽ����ʾ������㲽��
  integer MD_step_save_num             !���ݱ��������㲽��
end module Global_MD

!----------------------
!  16.���Է������
!----------------------
module Global_Plasticity
  use Global_Float_Type
  implicit none
  save
  real(kind=FT),ALLOCATABLE:: dsdeEl(:,:,:,:)   !ÿ����Ԫÿ��Gauss��ĵ���D����
  real(kind=FT),ALLOCATABLE:: dsdePl(:,:,:,:)   !ÿ����Ԫÿ��Gauss���D����
  real(kind=FT),ALLOCATABLE:: STATEV(:,:,:)     !ÿ����Ԫÿ��Gauss���״̬����
  real(kind=FT),ALLOCATABLE:: Last_U_of_Ele(:,:)!ÿ����Ԫ4���ڵ��8��λ�Ʒ���
end module Global_Plasticity

!----------------------
!   17.ճ���ѷ����
!----------------------
module Global_Cohesive
  use Global_Float_Type
  implicit none
  save
  integer Coh_Constitutive_type             !Constitutive model of the F-w curve
  real(kind=FT) Coh_Width_Critical1         !�������쿪��1,�˿��ȶ�Ӧ����ǣ����,Coh_Constitutive_type=1ʱ����
  real(kind=FT) Coh_Width_Critical2         !�������쿪��2,�˿�����ǣ����Ϊ0
  real(kind=FT) Coh_f_Ultimate              !���ǣ����,�Ѽ��ǣ����
  integer Coh_Tangential_Key                !�Ƿ�������ǣ����,������,Ӧ������Ӧ�Ĳ���
  real(kind=FT) Coh_Width_Critical1_T       !�������쿪��1,�˿��ȶ�Ӧ��������ǣ����,Coh_Constitutive_type=1ʱ����
  real(kind=FT) Coh_Width_Critical2_T       !�������쿪��2,�˿���������ǣ����Ϊ0
  real(kind=FT) Coh_f_Ultimate_T            !���ǣ����,�Ѽ������ǣ����
  integer,ALLOCATABLE:: Elem_Coh_Sta(:,:)   !ÿ����Ԫ��ճ���ѷ�״̬(�����ÿ�����ƣ�(num_elem,Max_Num_Cr))
                                            !         ״̬,=0,��ճ���ѷ�;=1,ճ���ѷ�
end module Global_Cohesive


!----------------------
! 18.��������ѧģ�����
!----------------------
module Global_PD
  use Global_Float_Type
  implicit none
  save
  integer PD_num_points              !�����Ŀ
  !REAL(kind=FT) MD_mss_molecule        !���ӵ�����
  !integer MD_num_time_step             !ʱ�䲽��
  !REAL(kind=FT) MD_Delt_Time           !ʱ�䲽��
  !REAL(kind=FT) MD_Dimension_x         !�˶�����x����ߴ�
  !REAL(kind=FT) MD_Dimension_y         !�˶�����y����ߴ�
  !REAL(kind=FT) MD_Dimension_z         !�˶�����z����ߴ�
  integer PD_step_print_num            !ƽ����ʾ������㲽��
  integer PD_step_save_num             !���ݱ��������㲽��
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
! 19.������(NL)�������
!----------------------
module Global_NonLinear
      use Global_Float_Type
      implicit none
      save
      real(kind=FT) NL_TIMS(1000,5)      !ÿ�д���һ���غ��Ӳ�����,����(��ʼʱ��,����ʱ��,ʱ������,��ʼ�غ�����,��ֹ�غ�����)
      INTEGER       NL_ITRA             !N-R��������Ŀ
      real(kind=FT) NL_ATOL             !��������в��norm2ģ
      INTEGER       NL_NTOL             !���������غɶ�����Ŀ
      real(kind=FT) NL_TOL              !N-R���������ݲ�
      integer       NL_NLOAD            !�غɲ���
      real(kind=FT),ALLOCATABLE:: NL_Delta_U(:)  !λ������
end module Global_NonLinear

!!---------------------------------------
!! 20.HYPLAS������(NL)�������,2021-07-17
!!---------------------------------------
!module Global_NonLinear_HYPLAS
!  use Global_Float_Type
!  implicit none
!  save
!  real(kind=FT) NL_Time_Steps(1000,5)      !ÿ�д���һ���غ��Ӳ�����,����(��ʼʱ��,����ʱ��,ʱ������,��ʼ�غ�����,��ֹ�غ�����)
!end module Global_NonLinear_HYPLAS

!---------------------------------------
! 21.�����غ����. 2023-01-21.
!---------------------------------------
module Global_Surface_Load
  use Global_Float_Type
  use Global_Ragged_Array_Real_Classs
  use Global_Ragged_Array_Int_Classs
  implicit none
  save
  character(256) File_Surface_Load(100)  !�����غ��ļ�����׺����ANSYS����. 2023-01-21.
  integer Num_Surface_Loads
  type(Ragged_Int_Array_2D),allocatable::Surface_Load_Elements_Nodes(:)  !���������غɶ�Ӧ�ĵ�Ԫ�ͽڵ㣬��Ansys2PhiPsi_3D_Surface_Pre.mac���.
  type(Ragged_Array_1D),allocatable::Surface_Load_Elements_Area(:)  !���������غɶ�Ӧ�ĵ�Ԫ���.
  type(Ragged_Array_2D),allocatable::Surface_Load_Elements_Normal(:)  !���������غɶ�Ӧ�ĵ�Ԫ�ⷨ������.
  real(kind=FT) Surface_Pressure(100)        !���غɴ�С. ѹΪ��.
end module Global_Surface_Load

!----------------------------------------------
! 22.3Dˮ��ѹ��ʵ�������ز���. 2023-01-23.
!----------------------------------------------
module Global_3D_HF_Experiment
  use Global_Float_Type
  use Global_Ragged_Array_Real_Classs
  use Global_Ragged_Array_Int_Classs
  implicit none
  save
  integer HFE_Surface_Load_Num                   !ѹ�����غɺ�
  real(kind=FT) HFE_Initial_Injection_Rate       !����ǰ�ĳ�ʼע����������λ��ml/min.
  integer HFE_Hole_Mat_Number                    !��׼�ѹ�ζ�Ӧ�Ĳ��Ϻ�.
  real(kind=FT) HFE_Initial_Try_Pressure         !�����ĳ�ʼ�ױ�ѹ��. 2023-04-18.
  real(kind=FT) HFE_Initial_Pressure_Step_Size   !���ѷ���ѹ����������.
end module Global_3D_HF_Experiment

!!-----------------------------------------
!! 23. XinAo���. �°����. 2023-03-14.
!!-----------------------------------------
!module Global_XA
!  use Global_Float_Type
!  implicit none
!  save
!  integer,ALLOCATABLE::XA_size_local(:)
!  integer,ALLOCATABLE::XA_all_local(:,:)
!end module Global_XA

!-----------------------------------------
! 23. Read_kpp���. 2023-08-24.
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
!   XX. Element3D��, 2022-05-01�� ���ڲ���
!------------------------------------------
!     module Element3D_class
!         use Global_Float_Type
!         implicit none
!         type,public :: Element3D
!             real(kind=FT) volume
!         end type Element3D
!    end module Element3D_class

