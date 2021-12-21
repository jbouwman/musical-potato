      program AOC

      integer :: ios, u = 42, i = 0, lc = 0, gamma = 0, epsilon = 0

      character(12) :: report

      integer, dimension(12) :: sums

      do i = 1, 12
         sums(i) = 0
      end do

      open(unit=u, file='day_3.txt', iostat=ios)

      if ( ios /= 0 ) stop "ABNORMAL TERMINATE"

      do
         read(u, *, iostat=ios) report
         if (ios /= 0) exit

         do i = 1, 12
            if (report(i:i) == "1") then
               sums(i) = sums(i) + 1
            end if
         end do

         lc = lc + 1

      end do

      do i = 1, 12
         if (sums(i) > (lc / 2)) then
            gamma = gamma + ISHFT(1, 12 - i)
         else
            epsilon = epsilon + ISHFT(1, 12 - i)
         end if
      end do

      print*, gamma * epsilon

      close(u)

      end program AOC
