      subroutine elseif (lab)
      integer lab
      call outgo (lab+1)
      call indent (-1)
      call outcon (lab)
      call indent (1)
      return
      end
