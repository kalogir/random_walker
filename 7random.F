      program random5
      implicit none

      integer :: i, j, k, N, S, M, L
      integer, allocatable :: x(:)
      real :: Z
      real :: Mq, vM
      real :: PR
      real, allocatable :: R(:) 

      open(unit=100, file='write.dat', status = 'unknown')
      open(unit=200, file='input3.dat', status = 'old')
      open(unit=101, file='writeM.dat', status = 'unknown')
      
      do k = 1, 15
      
      read(200,*) N, PR, L

      allocate(R(2*N+1))
      allocate(x(2*N+1))

      do i = 1, 2*N+1
         x(i) = i-N-1
      enddo
c      write(*,*) "Enter PR"
c      read(*,*) PR
c     Wahrscheinlichkeit für Schritt nach rechts
c      write(*,*) "Enter L"
c      read(*,*) L
c     Teilchenzahl


      do i = 1, 2*N+1
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



         R(M+N+1) = R(M+N+1) +1




      enddo

      do i = 1, 2*N+1
         R(i) = R(i)/L
      enddo

      write(100,*)'N=', N, 'PR=', PR, 'L=', L
      
      do i = 1, 2*N+1
         write(100,*) x(i), R(i)
      enddo

      Mq = 0
      do i = 1, 2*N+1  
         Mq = Mq+x(i)*R(i)   
      enddo

      vM = 0
      do i = 1, 2*N+1
         vM = vM + (x(i)-Mq)*(x(i)-Mq)*R(i)
      enddo

      

      write(100,*) 'Mittelwert = ', Mq
      write(100,*) 'Varianz = ', vM
      write(101,*) N, vM
      
      deallocate(R)
      deallocate(x)
      

      enddo

      

      close(unit=100)
      close(unit=200)
      close(unit=101)

      
      
      stop
      end program

      
