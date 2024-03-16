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



function Tool_chrpak_s_digits_count ( s )


!*****************************************************************************80
!
!! S_DIGITS_COUNT counts the digits in a string.
!
!  Discussion:
!
!    The string may include spaces, letters, and dashes, but only the
!    digits 0 through 9 will be counted.
!
!  Example:
!
!    S  => 34E94-70.6
!    N <=  7
!
!  Licensing:
!
!    This code is distributed under the MIT license. 
!
!  Modified:
!
!    08 September 2015
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    character ( len = * ) S, the string.
!
!  Output:
!
!    integer S_DIGITS_COUNT, the number of digits.
!
  implicit none

  character c
  logical Tool_chrpak_ch_is_digit
  integer n
  character ( len = * ) s
  integer Tool_chrpak_s_digits_count
  integer s_len
  integer s_pos

  s_len = len_trim ( s )

  s_pos = 0
  n = 0

  do while ( s_pos < s_len )

    s_pos = s_pos + 1

    c = s(s_pos:s_pos)

    if ( Tool_chrpak_ch_is_digit ( c ) ) then
      n = n + 1
    end if

  end do

  Tool_chrpak_s_digits_count = n

  return

end function Tool_chrpak_s_digits_count