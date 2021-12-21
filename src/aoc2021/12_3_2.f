      program AOC

      integer :: ios, u = 42, kept, ones, result_o2 = 0, result_co2 = 0
      integer :: row_o2, row_co2

      character(12), dimension(1000) :: report
      character(1) :: keeping

      integer, dimension(1000) :: keep

      open(unit=u, file='day_3.txt', iostat=ios)

      if ( ios /= 0 ) stop "ABNORMAL TERMINATE"

      do i = 1, 1000
         read(u, *, iostat=ios) report(i)
         if (ios /= 0) exit
         keep(i) = 1
      end do

C     Part 1: O2 Rating

      do i = 1, 12

         kept = 0
         ones = 0

         do j = 1, 1000
            if (keep(j) == 1) then
               kept = kept + 1
               if (report(j)(i:i) == "1") ones = ones + 1
            end if
         end do

         if (ones >= kept / 2) then
            keeping = "1"
         else
            keeping = "0"
         end if

         kept = 0

         do j = 1, 1000
            if (report(j)(i:i) /= keeping) keep(j) = 0
            if (keep(j) == 1) then
               kept = kept + 1
               row_o2 = j
            end if
         end do

         if (kept == 1) exit

      end do

      do i = 1, 12
         if (report(row_o2)(i:i) == "1") then
            result_o2 = result_o2 + ISHFT(1, 12 - i)
         end if
      end do

C     Part 2: CO2 Rating

      do i = 1, 1000
         keep(i) = 1
      end do

      do i = 1, 12

         kept = 0
         zeroes = 0

         do j = 1, 1000
            if (keep(j) == 1) then
               kept = kept + 1
               if (report(j)(i:i) == "0") then
                  zeroes = zeroes + 1
               end if
            end if
         end do

         if (zeroes <= kept / 2) then
            keeping = "0"
         else
            keeping = "1"
         end if

         kept = 0

         do j = 1, 1000
            if (report(j)(i:i) /= keeping) keep(j) = 0
            if (keep(j) == 1) then
               kept = kept + 1
               row_co2 = j
            end if
         end do

         if (kept == 1) exit

      end do

      do i = 1, 12
         if (report(row_co2)(i:i) == "1") then
            result_co2 = result_co2 + ISHFT(1, 12 - i)
         end if
      end do

      print*, "Overall rating", result_o2 * result_co2

      close(u)

      end program AOC
