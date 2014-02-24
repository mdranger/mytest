
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


      subroutine prcgrw(imnth)

      integer imnth

c...Compute growing season precipiation

      include 'const.inc'
      include 'parcp.inc'
      include 'wth.inc'

c...Local variables
      integer m


      grwprc = 0.

      do 10 m = imnth, MONTHS
        grwprc = grwprc + prcurr(m)
10    continue

      do 20 m = 1, imnth - 1
        grwprc = grwprc + prcnxt(m)
20    continue

      return
      end
