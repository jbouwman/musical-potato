function expand_region(map, i, j) result(c)

  integer :: i, j, x2, y2, region, found, c
  integer, dimension(100,100) :: map

  region = map(i,j)
  found = 1
  c = 1

  ! print*, "Expand region = ", region, "at =", i, j, c

  do while (found == 1)
     found = 0
     do y2 = 1, 100
        do x2 = 1, 100
           if (map(x2,y2) == region) then
              ! above?
              if (y2 > 1 .and. map(x2, y2 - 1) == 0) then
                 map(x2, y2 - 1) = region
                 found = 1
                 c = c + 1
              end if
                 ! below?
              if (y2 < 100 .and. map(x2, y2 + 1) == 0) then
                 map(x2, y2 + 1) = region
                 found = 1
                 c = c + 1
              end if
                 ! left?
              if (x2 > 1 .and. map(x2 - 1, y2) == 0) then
                 map(x2 - 1, y2) = region
                 found = 1
                 c = c + 1
              end if
                 ! right?
              if (x2 < 100 .and. map(x2 + 1, y2) == 0) then
                 map(x2 + 1, y2) = region
                 c = c + 1
                 found = 1
              end if
           end if
        end do
     end do
  end do

end function expand_region

subroutine pmap(map)
  integer, dimension(100,100) :: map
  character(len = 100) :: o
  integer :: x,y,z
  do y = 1, 100
     do x = 1, 100
        z = map(x, y)
        if (z == -1) then
           o(x:x) = "."
        else if (z == 0) then
           o(x:x) = " "
        else
           o(x:x) = "#"
        end if
     end do
     print*, o
  end do
end subroutine pmap

program AOC

  integer :: ios, u = 42, result, x, y, i, j, k, m, region, size
  integer, dimension(100,100) :: map
  integer :: expand_region
  character(len = 100) :: line
  integer, dimension(3) :: bigs

  do y = 1, 3
     bigs(y) = 0
  end do

  open(unit=u, file='day_9.txt', iostat=ios)

  do y = 1, 100
     read(u, *, iostat=ios) line
     do x = 1, 100
        map(x, y) = ichar(line(x:x)) - 48
     end do
  end do

  do y = 1, 100
     do x = 1, 100
        m = map(x, y)
        if (m == 9) then
           map(x,y) = -1
        else
           map(x,y) = 0
        end if
     end do
  end do

  call pmap(map)

  region = 1

  do y = 1, 100
     do x = 1, 100
        m = map(x, y)
        if (m == 0) then
           map(x, y) = region
           size = expand_region(map, x, y)
           print*, "Size", size
           ! call pmap(map)
           do i = 1, 3
              if (size > bigs(i)) then
                 bigs(i) = size
                 exit
              end if
           end do

           region = region + 1
        end if

        ! if (region == 8) stop
     end do
  end do

  print*, "Answer = ", bigs(1) * bigs(2) * bigs(3)

  close(u)
end program AOC
