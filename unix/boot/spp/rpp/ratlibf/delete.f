      subroutine delete (symbol, st)
      integer symbol (100)
      integer st
      integer mem( 60000)
      common/cdsmem/mem
      integer stlu
      integer node, pred
      if (.not.(stlu (symbol, node, pred, st) .eq. 1))goto 23000
      mem (pred + 0) = mem (node + 0)
      call dsfree (node)
23000 continue
      return
      end
