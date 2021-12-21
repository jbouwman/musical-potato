      program AOC2_1

      integer :: ios, u = 42, k, h = 0, d = 0, a = 0

      character(20) :: command

      open(unit=u, file='day_2.txt', iostat=ios)

      if ( ios /= 0 ) stop "ABNORMAL TERMINATE"

      do
         read(u, *, iostat=ios) command, k
         if (ios /= 0) exit

         if ( command == 'down' ) then
            a  = a + k
         else if ( command == 'up' ) then
            a = a - k
         else if ( command == 'forward' ) then
            h = h + k
            d = d + ( a * k )
         end if

      end do

      print*, h * d

      close(u)

      end program AOC2_1
