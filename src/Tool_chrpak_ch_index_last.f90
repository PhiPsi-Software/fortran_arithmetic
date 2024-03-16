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



function Tool_chrpak_ch_index_last( s, ch )

!*****************************************************************************80
!
!! ch_index_last() is the last occurrence of a character in a string.
! 字符在字符串中的最后一次出现位置
!  Licensing:
!
!    This code is distributed under the MIT license.
!
!  Modified:
!
!    03 April 2006
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
!    integer CH_INDEX_LAST, the location of the last
!    occurrence of the character in the string, or -1 if it does not occur.
!
  implicit none

  character ch
  integer Tool_chrpak_ch_index_last
  integer i
  character ( len = * ) s
  integer s_length

  Tool_chrpak_ch_index_last = -1
  s_length = len_trim ( s )

  do i = s_length, 1, -1

    if ( s(i:i) == ch ) then
      Tool_chrpak_ch_index_last = i
      return
    end if

  end do

  return
end function Tool_chrpak_ch_index_last