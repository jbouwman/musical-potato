      program AOC

      integer :: ios, u = 42, i, j, k = 0, l, x, y, x1, y1, x2, y2, xi, yi
      integer, dimension(1000,1000) :: b

      open(unit=u, file='day_5_2.txt', iostat=ios)
      if ( ios /= 0 ) stop "ERR"

      do i = 1, 500
         read(u, *, iostat=ios) x1, y1, x2, y2

         if (x1 == x2) then
            xi = 0
         else if (x1 < x2) then
            xi = 1
            l = x2 - x1
         else
            xi = -1
            l = x1 - x2
         end if

         if (y1 == y2) then
            yi = 0
         else if (y1 < y2) then
            yi = 1
            l = y2 - y1
         else
            yi = -1
            l = y1 - y2
         end if

         do j = 0, l
            x = x1 + (j * xi)
            y = y1 + (j * yi)
            b(x, y) = b(x, y) + 1
            if (b(x,y) == 2) k = k + 1
         end do
      end do

      close(u)
      print*, k
      end program AOC
