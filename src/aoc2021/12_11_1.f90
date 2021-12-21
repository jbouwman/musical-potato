program AOC

  integer :: ios, u = 42
  integer :: i, j, flashed, cycle, fc

  integer, dimension(10,10) :: state, flash
  character(10) :: line

  open(unit=u, file='day_11.txt', iostat=ios)

  do j = 1, 10
     read(u, *, iostat=ios) line
     do i = 1, 10
        state(i, j) = ichar(line(i:i)) - 48
        flash(i, j) = 0
     end do
  end do


  do cycle = 1, 4

     flashed = 1
     fc = 0

     do while (flashed == 1)

        do j = 1, 10
           do i = 1, 10
              if (flash(i, j) == 1) then
                 do l = j - 1, j + 1
                    do k = i - 1, i + 1
                       if (k /= i .and. l /= j .and. k >= 1 .and. k <= 10 .and. l >= 1 .and. l <= 10) then
                          state(k, l) = state(k, l) + 1
                       end if
                    end do
                 end do
              end if
           end do
        end do

        flashed = 0

        do j = 1, 10
           do i = 1, 10
              state(i, j) = state(i, j) + 1
              if (state(i, j) == 10) then
                 flash(i, j) = 1
                 flashed = 1
              end if
           end do
        end do
     end do

     do j = 1, 10
        do i = 1, 10
           if (flash(i, j) == 1) then
              fc = fc + 1
              state(i, j) = 0
           end if
        end do
     end do

     print*, "Flash Count", fc
  end do

  close(u)
end program AOC
