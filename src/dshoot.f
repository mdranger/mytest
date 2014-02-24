
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


      subroutine dshoot(wfunc)

      real wfunc

c...Simulate death of shoots for the month.
c
      include 'const.inc'
      include 'dovars.inc'
      include 'param.inc'
      include 'parcp.inc'
      include 'parfx.inc'
      include 'plot1.inc'
      include 'plot2.inc'
      include 'zztim.inc'

c...Local variables
      integer   iel, index
      real      accum(ISOS), dthppt, fdeth, sdethe, tostore

      accum(LABELD) = 0.0
      accum(UNLABL) = 0.0


c...Death of shoots.  Most shoots die during the month of senescence.
c     During other months, only a small fraction of the shoots die.
      if (aglivc .gt. 0) then
        index = 1
        dthppt = 1. - wfunc
        if (dosene) then
          index = 2
          dthppt = 1.0
        endif
        fdeth = fsdeth(index) * dthppt

c...Increase the death rate of shoots to account for effect of shading.
c     This is not done during senescence (when the death rate is greater
c     than or equal to .4)
        if (fsdeth(index) .lt. 0.4 .and. aglivc .gt. fsdeth(4)) then
      	   fdeth = fdeth + fsdeth(3)
        endif

c...Constrain the fraction
      	if (fdeth .gt. 0.95) then
          fdeth = 0.95
        endif

c...Calculate the amounts and flow
        sdethc = aglivc * fdeth
	call csched(sdethc,aglcis(LABELD),aglivc,
     $              aglcis(UNLABL),stdcis(UNLABL),
     $              aglcis(LABELD),stdcis(LABELD),
     $              1.0,accum)
        do 10 iel = 1, nelem
          sdethe = fdeth * aglive(iel)
          if (iel .eq. N) then
             vlosse = vlossp * sdethe
             call flow(aglive(iel),esrsnk(iel),time,vlosse)
             volpla = volpla + vlosse
             sdethe = sdethe - vlosse
          endif
          tostore = sdethe * crprtf(iel)
          call flow(aglive(iel),crpstg(iel),time,tostore)
          sdethe = sdethe * (1 - crprtf(iel))
          call flow(aglive(iel),stdede(iel),time,sdethe)
10      continue
      endif

      return
      end
