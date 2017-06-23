      program random8
      implicit none

      integer :: i, j, k, N, S, M, L, a, b
      integer, allocatable :: x(:)
      real :: Z
      real :: Mq, vM
      real :: PR0, PRa, PRb
      real, allocatable :: R(:) 


c     N = number of steps
c     L = number of particles
c     M and x = endposition
c     S = stepwidth
c     a = stepwidth a
c     b = stepwidth b
c     PR0 = probability for stepwidth = 0
c     PRa = probability for stepwidth = a
c     PRb = probability for stepwidth = b
c     Mq = mean
c     vM = variance
      
      
      open(unit=100, file='output_hist.dat', status = 'unknown')
      open(unit=200, file='inputPR.dat', status = 'old')
      open(unit=101, file='writevM.dat', status = 'unknown')
      

      
      do k = 1, 16
      
      read(200,*) L, N, PR0, PRa, PRb, a, b

      allocate(R(2*N+1))
      allocate(x(2*N+1))

      do i = 1, 2*N+1
         x(i) = i-N-1
      enddo


      do i = 1, 2*N+1
         R(i) = 0
      enddo

      
      do j = 1, L
         M = 0
         do i = 1, N
            
            call random_number(Z)
            if (Z <= PR0) then
               S = 0
            else if ((Z > PR0) .and. (Z <= PR0+PRa)) then
               S = a
            else if ((Z > PR0+PRa) .and. (Z <= PR0+2*PRa) .or.
     +          (PRa < 0.5) .and. (Z > PR0+PRa) .and. (PRb == 0)) then
               S = -a
            else if ((Z > PR0+2*PRa) .and. (Z <= PR0+2*PRa+PRb)) then 
               S = b
            else  
               S = -b
            
               
            endif
            M = M + S      

         enddo



         R(M+N+1) = R(M+N+1) +1




      enddo

      do i = 1, 2*N+1
         R(i) = R(i)/L
      enddo

      write(100,*)'#L=',L,'N=',N,'PR0=',PR0,'PRa=',PRa,'a=',a,'PRb=',b 
      
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

      

      write(100,*) '#Mittelwert = ', Mq
      write(100,*) '#Varianz = ', vM
      write(101,*) N, vM
      
      deallocate(R)
      deallocate(x)
      

      enddo

      

      close(unit=100)
      close(unit=200)
      close(unit=101)

      
      
      stop
      end program

      
