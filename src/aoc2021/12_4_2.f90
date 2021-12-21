      subroutine score_board(board, index, numbers, i, score)
        integer, dimension(100), intent(in) :: numbers
        integer, dimension(100,5,5), intent(in) :: board
        integer, intent(in) :: index
        integer, intent(out) :: i, score
        integer, dimension(5,5) :: called
        integer :: n

        score = 0

        do j = 1, 5
           do k = 1, 5
              called(j,k) = 0
              score = score + board(index,j,k)
           end do
        end do

        do i = 1, 100
           n = numbers(i)
           do j = 1, 5
              do k = 1, 5
                 if (board(index,j,k) == n) then
                    called(j,k) = 1
                    score = score - board(index,j,k)
                    if (called(j,1) + called(j,2) + called(j,3) + called(j,4) + called(j,5) == 5) then
                       print*, "row"
                       return
                    else if (called(1,k) + called(2,k) + called(3,k) + called(4,k) + called(5,k) == 5) then
                       print*, "col"
                       return
                    end if
                 end if
              end do
           end do
        end do
      end subroutine score_board

      program AOC1_2

      integer :: ios, u = 42, i = 0, n = 0, score, best = 1, bs

      integer, dimension(100) :: numbers

      integer, dimension(100,5,5) :: b

      external score_board

      open(unit=u, file='day_4.txt', iostat=ios)
      if ( ios /= 0 ) stop "ERR"

      read(u, *, iostat=ios) numbers

      do i = 1, 100
         do j = 1, 5
            read(u,*,iostat=ios)b(i,j,1), b(i,j,2), b(i,j,3), b(i,j,4), b(i,j,5)
         end do
      end do

      close(u)

      do i = 1, 100
         call score_board(b, i, numbers, n, score)
         print*, n, score
         if (n > best) then
            best = n
            print*, "best now", best
            bs = score
         end if
      end do

      print*, numbers(best) * bs

      end program AOC1_2

