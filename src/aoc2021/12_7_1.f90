program AOC

  integer :: ios, u = 42
  integer(kind=16) :: i, j, k, l, m, n
  integer, dimension(1000) :: d

  open(unit=u, file='day_7.txt', iostat=ios)
  read(u, *, iostat=ios) d

  n = -1

  do i = 1, 1000

     k = 0

     do j = 1, 1000
        if (d(j) > i) then
           l = d(j) - i
        else
           l = i - d(j)
        end if

        do m = 1, l
           k = k + m
        end do

     end do

     ! print*, "K", k, m

     if (n == -1 .or. k < n) then
        n = k
        print*, "A", n
     end if
  end do

  close(u)
end program AOC
