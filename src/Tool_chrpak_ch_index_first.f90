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



function Tool_chrpak_ch_index_first ( s, ch )

!*****************************************************************************80
!
!! ch_index_first_chrpak() returns the first occurrence of a character in a string.
!  返回字符串中字符的第一次出现位置.
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
!    character ( len = * ) S, the string to be searched.
!
!    character CH, the character to be searched for.
!
!  Output:
!
!    integer CH_INDEX_FIRST, the location of the first
!    occurrence of the character in the string, or -1 if it does not occur.
!
  implicit none

  character ch
  integer Tool_chrpak_ch_index_first
  integer i
  character ( len = * ) s
  integer s_length

  Tool_chrpak_ch_index_first = - 1
  s_length = len_trim ( s )

  do i = 1, s_length

    if ( s(i:i) == ch ) then
      Tool_chrpak_ch_index_first = i
      return
    end if

  end do

  return
end function Tool_chrpak_ch_index_first