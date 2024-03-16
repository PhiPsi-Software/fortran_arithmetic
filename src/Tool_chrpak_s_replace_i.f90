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



subroutine Tool_chrpak_s_replace_i ( s, sub1, sub2 )

!*****************************************************************************80
!
!! S_REPLACE_I replaces all occurrences of SUB1 by SUB2 in a string.
!
!  Discussion:
!
!    Matches are made without regard to case.
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
!    character ( len = * ) S: the string in which occurrences are to be replaced.
!
!    character ( len = * ) SUB1, the string which is to be replaced.
!
!    character ( len = * ) SUB2, the replacement string.
!
!  Output:
!
!    character ( len = * ) S: the transformed string.
!
!    integer IREP, the number of replacements made.
!    If IREP is negative, then its absolute value is the
!    number of replacements made, and SUB2 is longer than
!    SUB1, and at least one substring SUB1 could not be
!    replaced by SUB2 because there was no more space.
!    (If S = 'aab' and SUB1 = 'a' and SUB2 = 'cc'
!    then the result would be S = 'cca'.  The first 'a'
!    was replaced, the 'b' fell off the end, the second 'a'
!    was not replaced because the replacement 'cc' would have
!    fallen off the end)
!
  implicit none

  integer ilo
  integer len1
  integer len2
  integer lens
  integer Tool_chrpak_s_indexi
  character ( len = * ) s
  character ( len = * ) sub1
  character ( len = * ) sub2

  lens = len ( s )

  if ( lens <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'S_REPLACE_I - Serious error!'
    write ( *, '(a)' ) '  Null string not allowed!'
    return
  end if

  len1 = len ( sub1 )

  if ( len1 <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'S_REPLACE_I - Serious error!'
    write ( *, '(a)' ) '  Null SUB1 not allowed!'
    return
  end if

  len2 = len ( sub2 )

  ilo = Tool_chrpak_s_indexi ( s, sub1 )
!
!  If the match string has been found, then insert the replacement.
!
  if ( ilo /= 0 ) then
    s(ilo+len2:lens+len2-len1) = s(ilo+len1:lens)
    s(ilo:ilo+len2-1) = sub2(1:len2)
  end if

  return
end subroutine Tool_chrpak_s_replace_i


