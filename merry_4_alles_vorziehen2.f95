program Merry_Christmas_1994

!+-------------------------------------------------------------------------+
!|                                                                         | 
!|               T A S K :         Merry_Christmas_1994                    |
!|               =======                                                   |
!|                                                                         |
!|  Problem:        There are 2 formulas, where to find valid digits       |
!|                  for each letter                                        |
!|                                                                         |
!|-> 1st formula:   merry x mas + a = happy x new - year + 1994            |
!|                                                                         |
!|-> 2nd formula:   merry x mas + a + happy = new + year * 1994            |
!|                                                                         |
!|                  Each letter means a different digit                    |
!|                  x   means multiply                                     |
!|                                                                         |
!|                  write a program finding all possible combinations      |
!|                  of digits. If one found, don't stop, try to find out,  |
!|                  if there are more combinations possible. Both formulas |
!|                  should be handled in one common set of nested loops.   |
!|                                                                         |
!+-------------------------------------------------------------------------+

!----------------------
!---> declarations <---
!----------------------

   implicit none

   integer, parameter      :: ip  = selected_int_kind(8)

   integer(kind=ip)        :: i0, i1, i2, i3, i4, i5, i6, i7, i8!, i9  ! loop-indices
   integer(kind=ip)        :: merry
   integer(kind=ip)        :: mas
   !integer(kind=ip)        :: a
   integer(kind=ip)        :: happy
   integer(kind=ip)        :: new
   integer(kind=ip)        :: year
   integer(kind=ip)        :: left_side1, right_side1,left_side2, right_side2
   integer(kind=ip)        :: loopcount = 0

   real             :: time_start, time_end, time_used

! My Changes:
  integer, parameter :: iterations = 300;


  integer, parameter :: ziffer = selected_int_kind(1)
  integer(kind=ip), dimension(10) :: zahlen
  integer(kind=ip) ::m,e,r,a,s,h,p,y,n,w
  integer :: I, REPEAT

  integer(kind=ip) :: m100, m10000
  integer(kind=ip) :: e10,e100,e1000
  integer(kind=ip) :: y1000
  integer(kind=ip) :: a10,a1000
  integer(kind=ip) :: h10000
  integer(kind=ip) :: r110
  integer(kind=ip) :: merryxmas, merryxmasPLUSa, yearX1994, MINUSyearPLUS1994, merryxmasPLUSaPLUShappy
  integer(kind=ip) :: me_rry, merr_y
  integer(kind=ip) :: x_e_r, ye_r
  integer(kind=ip) :: ma_
  integer(kind=ip) :: x_a__y, xha__y
  equivalence(right_side2,new)
!------------------------------------------------------------------------


!--------------------
!---> time start <---
!--------------------


  call cpu_time(time_start)

  DO REPEAT=1,iterations


  write(*,'(a)' )'------------------------------------------------------'
  write(*,'(a)' )'formula   m e r y a s h p n w -   result  -  loopcount'
  loopcount = 0 
  !array neu populieren
  do I=1,10
      zahlen(I)=I-1
  end do
  loop0: do, i0=1,10    ! loop for "m"
    m=zahlen(i0)
    zahlen(i0)=zahlen(1)
    zahlen(1)=m
    
    m100=m*100
    m10000=m*10000
    
    loop1: do, i1=2,10    ! loop for "e" !bekannt: m,e
      e=zahlen(i1)
      zahlen(i1)=zahlen(2)
      zahlen(2)=e
      
      e10=e*10
      e100=e*100
      e1000=e*1000

	  !zwischenergebnisse:
        me_rry=m10000+e1000
      !--

      loop2: do, i2=3,10    ! loop for "r" !bekannt: m,e,r
        r=zahlen(i2)
        zahlen(i2)=zahlen(3)
        zahlen(3)=r
        
        r110=r*110
        
        !zwischenergebnisse:
          merr_y=me_rry+r110
          x_e_r=e100+r
        !--
        
        loop3: do, i3=4,10    ! loop for "y" !bekannt: m,e,r,y
          y=zahlen(i3)
          zahlen(i3)=zahlen(4)
          zahlen(4)=y
          
          y1000=y*1000
          
          !zwischenergebnisse:
            ye_r=y1000+x_e_r
          !--
          
          !wortergebnisse:
            merry = merr_y + y
          !--
          
          loop4: do, i4=5,10    ! loop for "a" !bekannt: m,e,r,y,a
            a=zahlen(i4)
            zahlen(i4)=zahlen(5)
            zahlen(5)=a
            
            a10=a*10
            a1000=a*1000
            
            !zwischenergebnisse:
              ma_=m100+a10
              x_a__y=a1000+y
            !--
            
            !wortergebnisse:
            !a=a
              year  = ye_r  + a10
            !--
            
            !wortresultate:
              yearX1994=year*1994
              MINUSyearPLUS1994=-year+1994
            !--
            
            loop5: do, i5=6,10    ! loop for "s" !bekannt: m,e,r,y,a,s
              s=zahlen(i5)
              zahlen(i5)=zahlen(6)
              zahlen(6)=s
              
              !wortergebnisse:
                mas   = ma_   + s  
              !--
              
              !wortresultate:
                merryxmas=merry*mas
                merryxmasPLUSa=merryxmas+a
              !--
              
              !gleichungs-ergebnisse:
                left_side1  = merryxmasPLUSa - MINUSyearPLUS1994
              !--
              
              loop6: do, i6=7,10    ! loop for "h" !bekannt: m,e,r,y,a,s,h
                h=zahlen(i6)
                zahlen(i6)=zahlen(7)
                zahlen(7)=h
                
                h10000=h*10000
                
                !zwischenergebnisse:
                  xha__y=h10000+x_a__y
                !--
                
                loop7: do, i7=8,10    ! loop for "p" !bekannt: m,e,r,y,a,s,h,p
                  p=zahlen(i7)
                  zahlen(i7)=zahlen(8)
                  zahlen(8)=p
                  
                  !wortergebnisse:
                    happy = xha__y + p*110
                  !--

                  !wortresultate:
                    merryxmasPLUSaPLUShappy=merryxmasPLUSa+happy
                  !--

				  !Gleichungs-ergebnisse:
                    left_side2  = merryxmasPLUSaPLUShappy-yearX1994
                  !--
                  loop8: do, i8=9,10    ! loop for "n,w" !bekannt: m,e,r,y,a,s,h,n,w
                    n=zahlen(i8)
                    zahlen(i8)=zahlen(9)
                    zahlen(9)=n
                    w=zahlen(10)

!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----

                      loopcount = loopcount + 1
                      
					  !wortergebnisse:
                        new   = n*100   + e10   + w ! =right_side2
                      !--

!--->
!---> 1st formula: merry x mas + a = happy * new - year + 1994
!--->
                      
                      right_side1 = happy * new 
                      !merry x mas + a + year - 1994= happy * new 
                      if ( left_side1 == right_side1 ) then
                         write(*, '(a,10i2,i12,i12)' ) &
                          '    1:   ',m,e,r,y,a,s,h,p,n,w, &
                                                left_side1, loopcount

!--->
!---> 2nd formula: merry x mas + a + happy = new + year * 1994
!--->
                      else
                         !right_side2 == new wegen equivalence
                         !merry x mas + a + happy -year * 1994 = new
                         if ( left_side2 == right_side2 ) then
                            write(*, '(a,10i2,i12,i12)' ) &
                             '    2:   ',m,e,r,y,a,s,h,p,n,w, &
                                                   left_side2, loopcount
                         end if

                      end if
!----<>--------<>--------<>--------<>--------<>--------<>--------<>--------<>----        
                      
                      
                    ! n=zahlen(9)
                    ! swapping i8 and 9
                    zahlen(9)=zahlen(i8)
                    zahlen(i8)=n

                  end do loop8 !n
                  zahlen(8)=zahlen(i7)
                  zahlen(i7)=p

                end do loop7 !p
                zahlen(7)=zahlen(i6)
                zahlen(i6)=h

              end do loop6 !h
              zahlen(6)=zahlen(i5)
              zahlen(i5)=s
            end do loop5 !s
            zahlen(5)=zahlen(i4)
            zahlen(i4)=a
          end do loop4 !a
          zahlen(4)=zahlen(i3)
          zahlen(i3)=y
        end do loop3 !y
        zahlen(3)=zahlen(i2)
        zahlen(i2)=r
      end do loop2 !r 
      zahlen(2)=zahlen(i1)
      zahlen(i1)=e
    end do loop1 !e 
    zahlen(1)=zahlen(i0)
    zahlen(i0)=m
  end do loop0 !m


  write(*, '(a,i21)' )'          after all loopcount   ',loopcount

!-------------------
!---> time used <---
!-------------------
  end do
  call cpu_time(time_end)

  time_used = time_end - time_start
  write(0,*) 'Durchlaeufe: ', iterations
  write(0,fmt="(a, f15.10)")'===> Zeitverbrauch:',time_used
  write(0,fmt="(a, f15.10)") 'Durchschnitt:', time_used/iterations

end program Merry_Christmas_1994
