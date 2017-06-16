      program random3
      implicit none

      integer :: ISEED, i, j, N, S, M, L 
      real :: Z
      real :: PR
      real :: R(100) 



      write(*,*) "Enter N"
      read(*,*) N  
c     Schrittzahl
      write(*,*) "Enter PR"
      read(*,*) PR
c     Wahrscheinlichkeit für Schritt nach rechts
      write(*,*) "Enter L"
      read(*,*) L
c     Teilchenzahl


      do i = 1, N
         R(i) = 0
      enddo

      
      do j = 1, L
         M = 0
         do i = 1, N
            
            call random_number(Z)
            if (Z <= PR) then
               S = 1
            else
               S = -1
            endif
            M = M + S      

         enddo

c         M = abs(M)

         R(M) = R(M) +1

c         write(*,*) R(M)

c         write(*,*) M
      enddo

      do i = 1, N
         R(i) = R(i)/L
      enddo

      do i = 1, N
         write(*,*) i, R(i)
      enddo
      

      
      
      stop
      end program

      
