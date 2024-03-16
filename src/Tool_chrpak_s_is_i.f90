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



function Tool_chrpak_s_is_i ( s, i )

!*****************************************************************************80
!
!! S_IS_I is TRUE if a string represents an integer.
!
!  Licensing:
!
!    This code is distributed under the MIT license.
!
!  Modified:
!
!    14 August 1999
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
!    integer I.  If the string represents an integer,
!    I is the integer represented.  Otherwise I is 0.
!
!    logical S_IS_I, is TRUE if the string represents an
!    integer.
!
  implicit none

  integer i
  integer ierror
  integer length
  character ( len = * ) s
  logical Tool_chrpak_s_is_i
  integer s_length

  s_length = len_trim ( s )

  call Tool_chrpak_s_to_i4 ( s, i, ierror, length )

  if ( ierror == 0 .and. s_length <= length ) then
    Tool_chrpak_s_is_i = .true.
  else
    Tool_chrpak_s_is_i = .false.
    i = 0
  end if

  return
end function Tool_chrpak_s_is_i