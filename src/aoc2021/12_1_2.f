      program AOC1_2

      integer :: ios, u = 42, i = 0, k, n = 0

      integer, dimension(4) :: window

      open(unit=u, file='day_1.txt', iostat=ios)

      if ( ios /= 0 ) stop "ABNORMAL TERMINATE"

      do
         read(u, '(I4)', iostat=ios) k
         if (ios /= 0) exit
         window(mod(n, 4)) = k

         if (n > 2 .and. window(mod(n - 3, 4)) < window(mod(n, 4))) then
            i = i + 1
         end if

         n = n + 1

      end do

      print*, i

      close(u)

      end program AOC1_2
