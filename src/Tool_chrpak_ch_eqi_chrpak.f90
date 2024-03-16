!!-------------------------------------------------------------------
!!
!!chrpak, a FORTRAN90 code which handles characters and strings.
!!
!!Many of the routine names begin with the name of the data type they operate on:
!!
!!B4 - a 4 byte word;
!!CH - a character;
!!CHVEC - a vector of characters;
!!DEC - a decimal fraction;
!!DIGIT - a character representing a numeric digit;
!!I4 - an integer ( kind = 4 );
!!R4 - a real ( kind = 4 );
!!R8 - a real ( kind = 8 );
!!RAT - a ratio I/J;
!!S - a string;
!!SVEC - a vector of strings;
!!SVECI - a vector of strings, implicitly capitalized;
!!
!!
!!
!!https://people.sc.fsu.edu/~jburkardt/f_src/chrpak/chrpak.html
!!



function Tool_chrpak_ch_eqi_chrpak ( c1, c2 )

!*****************************************************************************80
!
!! ch_eqi_chrpak() is a case insensitive comparison of two characters for equality.
!  Ch_eqi_chrpak()是对两个字符进行不区分大小写的相等比较
!  Discussion:
!
!    CH_EQI ( 'A', 'a' ) is TRUE.
!
!  Licensing:
!
!    This code is distributed under the MIT license.
!
!  Modified:
!
!    04 September 2021
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    character C1, C2, the characters to compare.
!
!  Output:
!
!    logical CH_EQI, the result of the comparison.
!
  implicit none

  character c1
  character c1_cap
  character c2
  character c2_cap
  logical Tool_chrpak_ch_eqi_chrpak

  c1_cap = c1
  c2_cap = c2

  call Tool_chrpak_ch_cap_chrpak ( c1_cap )
  call Tool_chrpak_ch_cap_chrpak ( c2_cap )

  if ( c1_cap == c2_cap ) then
    Tool_chrpak_ch_eqi_chrpak = .true.
  else
    Tool_chrpak_ch_eqi_chrpak = .false.
  end if

  return
end function Tool_chrpak_ch_eqi_chrpak

