      subroutine entdef (name, defn, table)
      integer name (100), defn (100)
      integer table
      integer lookup
      integer text
      integer sdupl
      if (.not.(lookup (name, text, table) .eq. 1))goto 23000
      call dsfree (text)
23000 continue
      call enter (name, sdupl (defn), table)
      return
      end
