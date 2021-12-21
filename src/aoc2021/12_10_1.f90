program AOC

  integer :: ios, u = 42, result, i, j, k, m, region, size, scount
  integer(kind = 16) :: score, x
  character(len = 110) :: line, state
  character(len = 1) :: c
  integer(kind = 16), dimension(90) :: scores

  open(unit=u, file='day_10.txt', iostat=ios)



!    ): 3 points.
 !   ]: 57 points.
  !  }: 1197 points.
   ! >: 25137 points.

  result = 0
     scount = 0


  do i = 1, 90
     j = 0
     good = 1
     read(u, *, iostat=ios) line
     do k = 1, 110
        c = line(k:k)
        if (c == '<' .or. c == '{' .or. c == '[' .or. c == '(') then
           j = j + 1
           state(j:j) = c
        else if (c == '>') then
           if (state(j:j) == '<') then
              j = j - 1
           else
              print*, "line", i, "Unmatched > at", k
              result = result + 25137
              good = 0
              exit
           end if
        else if (c == '}') then
           if (state(j:j) == '{') then
              j = j - 1
           else
              print*, "line", i,  "Unmatched } at", k
              result = result + 1197
              good = 0
              exit
           end if
        else if (c == ']') then
           if (state(j:j) == '[') then
              j = j - 1
           else
              print*, "line", i,  "Unmatched ] at", k
              result = result + 57
              good = 0
              exit
           end if
        else if (c == ')') then
           if (state(j:j) == '(') then
              j = j - 1
           else
              print*, "line", i,  "Unmatched ) at", k
              result = result + 3
              good = 0
              exit
           end if
        end if
     end do

     if (good == 1) then

        score = 0
        scount = scount + 1

        do k = 0, j - 1
           c = state(j-k:j-k)
           print*, "close", c
           if (c == '<') then
              score = (score * 5) + 4
           else if (c == '{') then
              score = (score * 5) + 3
           else if (c == '[') then
              score = (score * 5) + 2
           else if (c == '(') then
              score = (score * 5) + 1
           end if
        end do

        print*, "score", score
        scores(scount) = score

     end if
  end do

  print*, scores

  do i = 1, scount
     do j = i, scount
        if (scores(j) < scores(i)) then
           x = scores(j)
           scores(j) = scores(i)
           scores(i) = x
        end if
     end do
  end do

  print*, scores(scount / 2)
  print*, scores(23)



  
  print*, scount

  close(u)
end program AOC
