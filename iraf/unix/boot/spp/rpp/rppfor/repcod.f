      subroutine repcod (lab)
      integer lab
      integer labgen
      call outcon (0)
      lab = labgen (3)
      call outcon (lab)
      lab = lab + 1
      call indent (1)
      return
      end
