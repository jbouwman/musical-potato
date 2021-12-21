function ci(s) result(x)
  character(7) :: s
  character(len = 1) :: c
  integer :: x

  x = 0

  do i = 1, 7
     c= s(i:i)
     if (c == "a") then
        x = x + 1
     else if (c == "b") then
        x = x + 2
     else if (c == "c") then
        x = x + 4
     else if (c == "d") then
        x = x + 8
     else if (c == "e") then
        x = x + 16
     else if (c == "f") then
        x = x + 32
     else if (c == "g") then
        x = x + 64
     end if
  end do

end function ci

function bcx(n) result(x)
  integer :: n, j, x

  x = 0

  do j = 0, 7
     if (and(n, ishft(1, j)) /= 0) x = x + 1
  end do

end function bcx


program AOC

  integer :: ios, u = 42, ci2, bcount
  integer :: ci, bcx
  integer :: s_a, s_b, s_c, s_d, s_e, s_f, s_g
  integer :: i, j, k, l, m, n, x, s_bars, lw, mw, tw
  character(7), dimension(15) :: d
  character(7) :: s
  character(len = 1) :: c
  integer, dimension(0:9) :: nums, nmap, sums
  integer, dimension(0:127) :: output


  open(unit=u, file='day_8.txt', iostat=ios)

  do j = 0, 9
        nmap(j) = -1
        sums(j) = 0
     end do



  do i = 1, 200

     do j = 0, 9
        nmap(j) = -1
     end do

     read(u, *, iostat=ios) d

     do j = 0, 9
        nums(j) = 0
        s = d(j + 1)
        print*, "READ: ", s, ci(s)
        ci2 = ci(s)
        bcount = bcx(ci2)
        nums(j) = ci(s)
        if (bcount == 2) then
           nmap(1) = ci2
        else if (bcount == 3) then
           nmap(7) = ci2
        else if (bcount == 4) then
           nmap(4) = ci2
        else if (bcount == 7) then
           nmap(8) = ci2
        end if
     end do

     ! Identify 3 by:
     ! and(7, ?) = 7 .and. bc(? - 7) == 2

     do j = 0, 9
        x = nums(j)
        if (and(x, nmap(7)) == nmap(7)) then
           if (bcx(xor(x, nmap(7))) == 2) then
              print*, "3 is ", x
              nmap(3) = x
           end if
        end if
     end do

     lw = xor(nmap(8), nmap(3))
     mw = xor(nmap(3), nmap(1))

     print*, lw + tw + nmap(1)

     do j = 0, 9
        x = nums(j)
        k = lw + tw + nmap(1)
        if (and(k, x) == k .and. bcx(x) == 6) then
           nmap(0) = x
        else if (bcx(xor(nmap(3), x)) == 1) then
           nmap(9) = x
        else if (bcx(xor(mw + lw, x)) == 1) then
           nmap(6) = x
        end if
     end do

     s_d = xor(nmap(8), nmap(6))
     s_e = xor(nmap(8), nmap(9))

     nmap(5) = xor(nmap(8), s_d + s_e)
     nmap(2) = mw + s_d + s_e

     do j = 12, 15
     end do

     print*, nmap

     do j = 0, 9
        output(nmap(j)) = j
     end do

     do j = 12, 15
        sums(output(ci(d(j)))) = sums(output(ci(d(j)))) + 1
     end do




  end do

  print*, sums(1) + sums(4) + sums(7) + sums(8)

  close(u)
end program AOC
!  _   _   _   _   _   _   _   _   _   _
! |_| |_| |_| |_| |_| |_| |_| |_| |_| |_|
! |_| |_| |_| |_| |_| |_| |_| |_| |_| |_|
!
!  a
! bcd
! efg
!  _       _   _       _   _   _   _   _
! | |   |  _|  _| |_| |_  |_    | |_| |_|
! |_|   | |_   _|   |  _| |_|   | |_|  _|
!
! 8-7 7-1 8-3 3-1
!      _       _
! |_      |    _
! |_      |    _
