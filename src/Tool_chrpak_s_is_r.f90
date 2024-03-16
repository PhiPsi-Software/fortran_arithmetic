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



subroutine Tool_chrpak_s_is_r ( s, r, lval )

!*****************************************************************************80
!
!! S_IS_R is TRUE if a string represents a real number.
!
!  Licensing:
!
!    This code is distributed under the MIT license.
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    character ( len = * ) S, the string to be checked.
!
!  Output:
!
!    real ( kind = rk ) R.  If the string represents a real number,
!    then R is the real number represented.  Otherwise R is 0.
!
!    logical LVAL, is TRUE if the string represents 
!    a real number.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0E+00 )

  integer ierror
  integer length
  logical lval
  real ( kind = rk ) r
  character ( len = * ) s
  integer s_length

  s_length = len_trim ( s )

  call Tool_chrpak_s_to_r4 ( s, r, ierror, length )

  if ( ierror == 0 .and. s_length <= length ) then
    lval = .true.
  else
    lval = .false.
    r = 0.0E+00
  end if

  return
end subroutine Tool_chrpak_s_is_r