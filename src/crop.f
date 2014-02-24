
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


      subroutine crop(time, wfunc)

      real     time, wfunc

c...Driver for calling all of crop code.

      include 'dovars.inc'
      include 'isovar.inc'
      include 'ligvar.inc'
      include 'parcp.inc'
      include 'plot2.inc'
      include 'const.inc'
      include 'param.inc'
      include 'seq.inc'
      include 'plot1.inc'


c...Organic matter addition 
      if (doomad) then
        call partit(astgc,astrec,1,csrsnk,esrsnk,astlig,astlbl)
      endif

c...If microcosm selected, skip the rest of the crop code
      if (micosm .eq. 1) then
        return
      endif

c...Fall of standing dead
      call falstd(pltlig)

c...Death of roots
      call droot(pltlig)

c...Death of shoots
      call dshoot(wfunc)

c...Cultivation
      if (docult) then
        call cultiv(pltlig)
      endif

c...Update flows so direct absorption will be accounted for
c     before plant uptake.
      call flowup(time)
      call sumcar

c...Grow (growth checks crpgrw and exactly what should be done)
      call growth(cisofr)

      return
      end
