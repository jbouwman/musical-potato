program AOC

  integer :: ios, u = 42, result, i, j, k, m, m_t, m_b, m_l, m_r
  integer, dimension(100,100) :: map
  character(len = 100) :: line

  open(unit=u, file='day_9.txt', iostat=ios)

  do i = 1, 100
     read(u, *, iostat=ios) line
     do j = 1, 100
        map(j, i) = ichar(line(j:j)) - 48
     end do
  end do

  do i = 1, 100
     do j = 1, 100
        m = map(j, i)
        if (j == 1 .or. m < map(j - 1, i)) then
           if (j == 100 .or. m < map(j + 1, i)) then
              if (i == 1 .or. m < map(j, i - 1)) then
                 if (i == 100 .or. m < map(j, i + 1)) then
                    result = result + 1 + m
                 end if
              end if
           end if
        end if
     end do
  end do

  print*, "Answer = ", result

  close(u)
end program AOC
