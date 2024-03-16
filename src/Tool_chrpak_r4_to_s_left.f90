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



subroutine Tool_chrpak_r4_to_s_left ( r4, s )

!*****************************************************************************80
!
!! R4_TO_S_LEFT writes an R4 into a left justified character string.
!
!  Licensing:
!
!    This code is distributed under the MIT license.
!
!  Modified:
!
!    10 December 2002
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = rk ) R4, the real number to be written into the string.
!
!  Output:
!
!    character ( len = * ) S, the string into which
!    the real number is to be written.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0E+00 )

  real ( kind = rk ) r4
  character ( len = * ) s
  character ( len = 14 ) s2
  integer Sign_Location
  integer Tool_chrpak_ch_index_first

  if ( real ( int ( r4 ), kind = rk ) == r4 ) then
    write ( s2, '(i14)' ) int ( r4 )
  else if ( abs ( r4 ) < 999999.5E+00 ) then
    write ( s2, '(f14.6)' ) r4
  else
    write ( s2, '(g14.6)' ) r4
  end if

  s = adjustl ( s2 )
  
  !2024-02-24. 删除E后面的加号. 例如8.0E+9，修改为8.0E9.
  Sign_Location = Tool_chrpak_ch_index_first(s,'E')
  if(Sign_Location>=2) then
      if(s(Sign_Location+1:Sign_Location+1)=="+") then
          s(Sign_Location+1:Sign_Location+1)=""
          call Tool_chrpak_s_blank_delete(s)  !Use chrpak to delete all spaces.
      endif
  endif
  
  Sign_Location = Tool_chrpak_ch_index_first(s,'e')
  if(Sign_Location>=2) then
      if(s(Sign_Location+1:Sign_Location+1)=="+") then
          s(Sign_Location+1:Sign_Location+1)=""
          call Tool_chrpak_s_blank_delete(s)  !Use chrpak to delete all spaces.
      endif
  endif  
  
  return
end subroutine Tool_chrpak_r4_to_s_left

