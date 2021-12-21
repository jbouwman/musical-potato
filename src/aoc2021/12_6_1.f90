program AOC

  integer :: ios, u = 42, i
  integer(kind=16) :: j, k
  integer, dimension(300) :: fi
  integer(kind=16), dimension(0:8) :: fs

  open(unit=u, file='day_6.txt', iostat=ios)
  read(u, *, iostat=ios) fi

  do i = 0, 8
     fs(i) = 0
  end do

  do i = 1, 300
     fs(fi(i)) = fs(fi(i)) + 1
  end do

  do i = 1, 256
     k = fs(0)
     do j = 0, 7
        fs(j) = fs(j + 1)
     end do
     fs(8) = k
     fs(6) = fs(6) + k
  end do

  j = 0
  do i = 0, 8
     j = j + fs(i)
  end do

  print*, j

  close(u)
end program AOC
