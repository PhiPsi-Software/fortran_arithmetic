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



function Tool_chrpak_ch_is_digit ( ch )


!*****************************************************************************80
!
!! ch_is_digit() is TRUE if a character is a decimal digit.
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
!    character CH, the character to be analyzed.
!
!  Output:
!
!    logical CH_IS_DIGIT, is TRUE if the character is a digit.
!
  implicit none

  character ch
  logical Tool_chrpak_ch_is_digit

  if ( lle ( '0', ch ) .and. lle ( ch, '9' ) ) then
    Tool_chrpak_ch_is_digit = .true.
  else
    Tool_chrpak_ch_is_digit = .false.
  end if

  return
end function Tool_chrpak_ch_is_digit