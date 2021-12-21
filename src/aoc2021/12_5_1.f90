! --- Day 5: Hydrothermal Venture ---

! You come across a field of hydrothermal vents on the ocean floor! These vents
! constantly produce large, opaque clouds, so it would be best to avoid them if
! possible.

! They tend to form in lines; the submarine helpfully produces a list of nearby
! lines of vents (your puzzle input) for you to review. For example:

! 0,9 -> 5,9
! 8,0 -> 0,8
! 9,4 -> 3,4
! 2,2 -> 2,1
! 7,0 -> 7,4
! 6,4 -> 2,0
! 0,9 -> 2,9
! 3,4 -> 1,4
! 0,0 -> 8,8
! 5,5 -> 8,2

      subroutine intersect(a, b, i)
        integer, dimension(2), intent(in) :: a, b
        integer, intent(out) :: i

        i = 1
      end subroutine intersect

      program AOC

      integer :: ios, u = 42, i = 0, x, y, zz, ys, ye, xs, xe

      integer, dimension(500,4) :: l

      integer, dimension(1000,1000) :: board

      external intersect

      open(unit=u, file='day_5_2.txt', iostat=ios)
      if ( ios /= 0 ) stop "ERR"

      do x = 1, 1000
         do y = 1, 1000
            board(x,y) = 0
         end do
      end do

      do i = 1, 500
         read(u,"(i4, i4, i4, i4)",iostat=ios) l(i,1), l(i,2), l(i,3), l(i,4)
         if (l(i,1) == l(i,3)) then

            if (l(i,2) < l(i,4)) then
               ys = l(i,2)
               ye = l(i,4)
            else
               ys = l(i,4)
               ye = l(i,2)
            end if

            print*, "Horizontal", l(i,1),l(i,2),l(i,3),l(i,4), ye - ys

            do y = ys, ye
               board(l(i,1), y) = board(l(i,1), y) + 1
            end do

         else if (l(i,2) == l(i,4)) then

            if (l(i,1) < l(i,3)) then
               xs = l(i,1)
               xe = l(i,3)
            else
               xs = l(i,3)
               xe = l(i,1)
            end if

            print*, "Vertical", l(i,1),l(i,2),l(i,3),l(i,4), xe - xs

            do x = xs, xe
               board(x, l(i,2)) = board(x, l(i,2)) + 1
            end do
         end if
      end do

      close(u)

      i = 0
      zz = 0

      do x = 1, 1000
         do y = 1, 1000
            if (board(x,y) >= 2) then
               i = i + 1
            end if
            zz = zz + 1
         end do
      end do

      print*, i, zz

      end program AOC




! Each line of vents is given as a line segment in the format x1,y1 -> x2,y2
! where x1,y1 are the coordinates of one end the line segment and x2,y2 are the
! coordinates of the other end. These line segments include the points at both
! ends. In other words:

!     An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
!     An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.

! For now, only consider horizontal and vertical lines: lines where either x1 =
! x2 or y1 = y2.

! So, the horizontal and vertical lines from the above list would produce the
! following diagram:

! .......1..
! ..1....1..
! ..1....1..
! .......1..
! .112111211
! ..........
! ..........
! ..........
! ..........
! 222111....

! In this diagram, the top left corner is 0,0 and the bottom right corner is
! 9,9. Each position is shown as the number of lines which cover that point or .
! if no line covers that point. The top-left pair of 1s, for example, comes from
! 2,2 -> 2,1; the very bottom row is formed by the overlapping lines 0,9 -> 5,9
! and 0,9 -> 2,9.

! To avoid the most dangerous areas, you need to determine the number of points
! where at least two lines overlap. In the above example, this is anywhere in
! the diagram with a 2 or larger - a total of 5 points.

! Consider only horizontal and vertical lines. At how many points do at least
! two lines overlap?
