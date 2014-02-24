
c               Copyright 1993 Colorado State University
c                       All Rights Reserved

      subroutine trees (cisotf, wfunc)

      real      cisotf, wfunc

c...Simulate forest production for the month.

      include 'const.inc'
      include 'dovars.inc'
      include 'fertil.inc'
      include 'monprd.inc'
      include 'param.inc'
      include 'parcp.inc'
      include 'parfs.inc'
      include 'parfx.inc'
      include 'plot1.inc'
      include 'plot2.inc'
      include 'plot3.inc'
      include 'potent.inc'
      include 'seq.inc'
      include 'zztim.inc'
      include 'prof.inc'

c...Function declarations
      real      fsfunc, rtimp
      integer	grochk
      external  fsfunc, rtimp, grochk

c...Local variables
      integer   iel, ipart, iptr, lyr, mmm
      real      amt, calcup, cfrac(FPARTS),
     $          euf(FPARTS), fsol, rimpct,
     $          remc, totcup,
     $          agwdfr, agwdgr, bgwdfr,bgwdgr,wddeath,
     $          netgrowth,yyy,yyy2,
     $          lw,lfr,br,fr,cr,roots,agwoodfr,
     $          ratio,netc,
     $          brgr, brdth,woodfr,nonwoodfr,
     $          uptake(4,MAXIEL), avefrc(MAXIEL)

c...Initialize monthly production variables
      do 10 ipart=1, FPARTS
	mfprd(ipart) = 0.0
10    continue
      if (rleavc .lt. 0.0) then 
         rleavc = 0.01
      endif
      if (fbrchc .lt. 0.0) then 
         fbrchc = 0.01
      endif
      if (rlwodc .lt. 0.0) then 
         rlwodc = 0.01
      endif
      if (frootc .lt. 0.0) then 
         frootc = 0.01
      endif
      if (crootc .lt. 0.0) then 
         crootc = 0.01
      endif

c...Death of tree parts
        call wdeath(tave, wfunc)

c...Option to add organic matter; added here so that it won't be
c     done twice in savanna
      if (doomad) then
        call partit(astgc,astrec,1,csrsnk,esrsnk,astlig,astlbl)
      endif

c...Update flows so direct absorption will be accounted for
c     before plant uptake

      call flowup(time)
      call sumcar

c...Calculate impact of root biomass on available nutrients
      rimpct = rtimp(riint, rictrl, frootc)

c...Determine old or new forest
c     ptr points to new forest carbon allocation fractions (ptr = 1) or
c     mature forest carbon allocation fractions (ptr = 2) for each of the
c     tree parts; leaves, fine roots, fine bres, large wood, and
c     coarse roots.  switch from new forest allocation fractions to old
c     forest allocation fractions at time = swold
 
      if (time .le. swold) then
        iptr = 1
      else
        iptr = 2
      endif

c...Calculate carbon fraction in each part
      do 30 ipart = 1, FPARTS
	cfrac(ipart) = fcfrac(ipart, iptr)
30    continue

c...retain the NPP of previous year for comparing wood death and growth
        lyr = aint(time)
        yyy = float(lyr) * 12.
        yyy2 = (time+0.01) * 12.0
        mmm = yyy2 - yyy + 1
          
       if (mmm .eq. 12) then
          livwodc = curlivwodc
          prevfbrchc = curfbrchc
          prevrlwodc = currlwodc
          prevcrootc = curcrootc
c          curlivwodc = rlwodc
          curlivwodc = fbrchc + rlwodc + crootc
          curfbrchc = fbrchc
          currlwodc = rlwodc
          curcrootc = crootc
          prevnpp = prefstgr 
          prefstgr = 0.0
c       print *, time,forage, mmm, steadyage, steady
c          print *, 'stock: ', livwodc, curlivwodc
        endif
      
c      print *, time,prevnpp,cproda
c...Calculate carbon fraction in each part, Liu sept 2001
      if (forage > 20 .and. steady < 1) then
        netgrowth = curlivwodc - livwodc
        if (netgrowth < 0.01) then
           steady = 1
           steadyage = float(forage) 
c..        print *, 'steady age: ', steadyage
        endif
       endif
      
c      if (forage > 20 .and. death2gr .eq. 0) then

      if (prevnpp > 10.0 .and. 
     $        .not. dotrem .and. .not. dofire(FORSYS)) then
      if (mmm .eq. 12) then
           if (curfbrchc < prevfbrchc ) then
            death2gr = 1
            netc=curfbrchc - prevfbrchc
c       print *, netc,wooddr(FBRCH),fbrchc
c            wooddr(FBRCH)=netc*2.0/(12.0*fbrchc+0.01)+wooddr(FBRCH)
c       print *, netc,wooddr(FBRCH),fbrchc
           endif
           if (currlwodc < prevrlwodc ) then
                 death2gr = 1
            netc=currlwodc - prevrlwodc
c            wooddr(LWOOD)=netc*2.0/(12.0*rlwodc+0.01)+wooddr(LWOOD)
           endif
           if (curcrootc < prevcrootc ) then
                 death2gr = 1
            netc=curcrootc-prevcrootc
c           wooddr(CROOT)=netc*2.0/(12.0*crootc+0.01)+wooddr(CROOT)
           endif
       endif
       endif
c      endif

        if (steady .eq. 1) then
            forage = aint(steadyage)
        endif


c....define the temporal dynamics of allocation coefficients

c       if (death2gr .eq. 0) then
c  modified by zp, change younger forest to higher above ground fraction, the older forest to lower
c  fraction
c      if ( forage > 40 ) then
	lfr = fcfrac(LEAF,2)
	fr = fcfrac(FROOT,2)
	cr = fcfrac(CROOT,2)
	br = fcfrac(FBRCH,2)
	lw = fcfrac(LWOOD,2)
c	else
c	lfr = fcfrac(LEAF,1)
c	fr = fcfrac(FROOT,1)
c	cr = fcfrac(CROOT,1)
c	br = fcfrac(FBRCH,1)
c	lw = fcfrac(LWOOD,1)
c      endif	
c       else
c	lfr = fcfrac(LEAF,2)
c	fr = fcfrac(FROOT,2)
c	cr = crootc*wooddr(CROOT)
c	br = fbrchc*wooddr(FBRCH)
c        lw = rlwodc*wooddr(LWOOD)
c        ratio = (1.0-lfr-fr)/(cr+br+lw)
c        cr = ratio*cr
c        br = ratio*br
c        lw = ratio*lw
c       endif

c....calculate wood fraction first then calculate nowood;
c....equation derived from Maryland FIA data
         woodfr = 0.35*0.0532*forage*exp(-0.0368*forage)+0.177

        if (woodfr .lt. 0.17) then 
            woodfr = 0.17
        endif

         woodfr = (br+lw+cr)/(br+lw+0.01)*woodfr
         nonwoodfr = 1.0 - woodfr

c....Liu, 08Oct04. Modify cfrac to make cproda and fcacc equal in forest ecosystem, the 0.001 issue
        if(fr+lfr .gt. 0) then
          cfrac(LEAF) = nonwoodfr*(lfr/(fr+lfr))
          cfrac(FROOT) = nonwoodfr*(fr/(fr+lfr))
        else
          cfrac(LEAF) = 0
          cfrac(FROOT) = 0
        endif

        if(br+lw+cr .gt. 0) then
          cfrac(CROOT) = woodfr*(cr/(br+lw+cr))
          cfrac(FBRCH) = woodfr*(br/(br+lw+cr))
          cfrac(LWOOD) = woodfr*(lw/(br+lw+cr))
        else
          cfrac(CROOT) = 0
          cfrac(FBRCH) = 0
          cfrac(LWOOD) = 0
        endif
                                                                
c       cfrac(LEAF) = nonwoodfr * lfr / (fr+lfr+0.001)
c	cfrac(FROOT) = nonwoodfr*(fr/(fr+lfr+0.001))
c	cfrac(CROOT) = woodfr*(cr/(br+lw+cr+0.001))
c	cfrac(FBRCH) = woodfr*(br/(br+lw+cr+0.001))
c	cfrac(LWOOD) = woodfr*(lw/(br+lw+cr+0.001))
         
       

      if (mmm .eq. 12) then
c      print 52, time,cfrac(LEAF),cfrac(FROOT),cfrac(CROOT),
c     $     cfrac(FBRCH),cfrac(LWOOD),woodfr,forage
      endif
52    format(f5.1,6f8.4,i4,5f8.2)


c...For seasonal deciduous/conifer forest system, reapportion growth
c     if its the first month of the growing season to simulate leaf
c     bloom (80% of growth goes to leaves)

      if (decid .eq. 1) then
c...Check to see if it is the first month of the growing season
	if (grochk(tave) .eq. 1) then

c...Carbon reapportionment . . .
c       Changed to balance the portions  -rm 5/91
c       REMC - the remaining carbon to be partitioned
c       TOTCUP - total of carbon uptakes for eveything but leaves

          cfrac(LEAF) = 0.8
          remc = 1.0 - cfrac(LEAF)
          totcup = 0
          do 40 ipart = FROOT, CROOT
            totcup = cfrac(ipart) + totcup
40        continue

          do 50 ipart = FROOT, CROOT
            cfrac(ipart) = remc * (fcfrac(ipart,iptr) / (0.01+totcup))
50        continue
        endif
      endif

c...Previous flowup should have updated mineral pools
        do 60 iel = 1, nelem
	  avefrc(iel) = 0.0
          do 55 lyr = 1, nlayer
	    avefrc(iel) = avefrc(iel) + minerl(lyr,iel)
55        continue
60      continue

c     endif


!...Determine actual production values, restricting the C/E ratios

      if (forgrw .eq. 1 .and. pforc.gt.0.) then
	  call restrp(nelem,FPARTS,avefrc,ccefor,cfrac,pforc,rimpct,
     $                forstg,snfxmx(FORSYS),cprodf,eprodf,eup,
     $                uptake,elimit,nfix,snfxac(FORSYS),ptagc)
      else
	  cprodf = 0.
      endif

!      if ((time-int(time)) .lt. 0.5 .and. (time-int(time))
!     $   .gt. 0.4 ) then
!        print *, "after restrp", time, pforc, cprodf
!      endif

c...If growth occurs...
      if (cprodf .gt. 0.) then

c....Update accumulators for N, P, and S uptake
	do 80 iel = 1, nelem
	  eupacc(iel) = eupacc(iel) + eprodf(iel)
	  do 70 ipart = 1, FPARTS
   	    eupprt(ipart,iel) = eupprt(ipart,iel) + eup(ipart,iel)
70        continue
80	continue

!...C/N ratio for production
	tcnpro = cprodf/(0.01+eprodf(1))

c...Calculate production for each tree part
c...New variable MFPRD added for gridded output - 6/96 rm
	do 85 ipart = 1, FPARTS
	  mfprd(ipart) = cfrac(ipart) * cprodf
85	continue


c...extend the root calculation to multiple soil layers but keep the total
c....root production unchanged (Liu 1/24/2001)
c....root production in deep layers is calculated in decom_p.f
        mfprd(FROOT) = mfprd(FROOT) * lyrrtfr(1)
        mfprd(CROOT) = mfprd(CROOT) * lyrrtfr(1)
                                                                                                  


c...Growth of leaves; split into labeled & unlabeled parts
        call csched(mfprd(LEAF),cisotf,1.0,
     $              csrsnk(UNLABL),rlvcis(UNLABL),
     $              csrsnk(LABELD),rlvcis(LABELD),
     $              1.0,alvcis)

c...Growth of fine roots; split into labeled & unlabeled parts
        call csched(mfprd(FROOT),cisotf,1.0,
     $              csrsnk(UNLABL),frtcis(UNLABL),
     $              csrsnk(LABELD),frtcis(LABELD),
     $              1.0,afrcis)

c...Growth of fine branches; split into labeled & unlabeled parts
        call csched(mfprd(FBRCH),cisotf,1.0,
     $              csrsnk(UNLABL),fbrcis(UNLABL),
     $              csrsnk(LABELD),fbrcis(LABELD),
     $              1.0,afbcis)

c...Growth of large wood; split into labeled & unlabeled parts
        call csched(mfprd(LWOOD),cisotf,1.0,
     $              csrsnk(UNLABL),rlwcis(UNLABL),
     $              csrsnk(LABELD),rlwcis(LABELD),
     $              1.0,alwcis)

c...Growth of coarse roots; split into labeled & unlabeled parts
        call csched(mfprd(CROOT),cisotf,1.0,
     $              csrsnk(UNLABL),crtcis(UNLABL),
     $              csrsnk(LABELD),crtcis(LABELD),
     $              1.0,acrcis)

c...Actual Uptake
	do 90 iel = 1, nelem
          euf(LEAF) = eup(LEAF,iel) / (0.01+eprodf(iel))
	  euf(FROOT) = eup(FROOT,iel) / (0.01+eprodf(iel))
	  euf(FBRCH) = eup(FBRCH,iel) / (0.01+eprodf(iel))
	  euf(LWOOD) = eup(LWOOD,iel) / (0.01+eprodf(iel))
	  euf(CROOT) = eup(CROOT,iel) /(0.01+ eprodf(iel))

c...Take up nutrients from internal storage pool
	  amt = uptake(ESTOR,iel) * euf(LEAF)
          call flow(forstg(iel),rleave(iel),time,amt)
	  amt = uptake(ESTOR,iel) * euf(FROOT)
          call flow(forstg(iel),froote(iel),time,amt)
	  amt = uptake(ESTOR,iel) * euf(FBRCH)
          call flow(forstg(iel),fbrche(iel),time,amt)
	  amt = uptake(ESTOR,iel) * euf(LWOOD)
          call flow(forstg(iel),rlwode(iel),time,amt)
	  amt = uptake(ESTOR,iel) * euf(CROOT)
          call flow(forstg(iel),croote(iel),time,amt)


c...Take up nutrients from soil
          do 100 lyr = 1, nlayer
            if (minerl(lyr,iel) .gt. 0.) then
              fsol = 1.0
              if (iel .eq. P) then
                fsol = fsfunc(minerl(SRFC,P), pslsrb, sorpmx)
              endif
      calcup = uptake(ESOIL,iel)*minerl(lyr,iel)*fsol/(0.01+avefrc(iel))
	      amt = calcup * euf(LEAF)
              mfnupt(LEAF) = mfnupt(LEAF) + amt
              call flow(minerl(lyr,iel),rleave(iel),time,amt)
	      amt = calcup * euf(FROOT)
              mfnupt(FROOT) = mfnupt(FROOT) + amt
              call flow(minerl(lyr,iel),froote(iel),time,amt)
	      amt = calcup * euf(FBRCH)
              mfnupt(FBRCH) = mfnupt(FBRCH) + amt
              call flow(minerl(lyr,iel),fbrche(iel),time,amt)
	      amt = calcup * euf(LWOOD)
              mfnupt(LWOOD) = mfnupt(LWOOD) + amt
              call flow(minerl(lyr,iel),rlwode(iel),time,amt)
	      amt = calcup * euf(CROOT)
              mfnupt(CROOT) = mfnupt(CROOT) + amt
              call flow(minerl(lyr,iel),croote(iel),time,amt)
             endif
100        continue


c...Take up nutrients from nitrogen fixation
          if (iel .eq. 1 .and. nfix .gt. 0) then
            amt = uptake(ENFIX,iel) * euf(LEAF)
            call flow(esrsnk(iel),rleave(iel),time,amt)
            amt = uptake(ENFIX,iel) * euf(FROOT)
            call flow(esrsnk(iel),froote(iel),time,amt)
            amt = uptake(ENFIX,iel) * euf(FBRCH)
            call flow(esrsnk(iel),fbrche(iel),time,amt)
            amt = uptake(ENFIX,iel) * euf(LWOOD)
            call flow(esrsnk(iel),rlwode(iel),time,amt)
            amt = uptake(ENFIX,iel) * euf(CROOT)
            call flow(esrsnk(iel),croote(iel),time,amt)
          endif
 
c...Take up nutrients from fertilizer
          if (aufert .ne. 0) then
            if (uptake(EFERT,iel) .gt. 0.) then

c...Automatic fertilizer added to plant pools
              amt = uptake(EFERT,iel) * euf(LEAF)
              call flow(esrsnk(iel),rleave(iel),time,amt)
              amt = uptake(EFERT,iel) * euf(FROOT)
              call flow(esrsnk(iel),froote(iel),time,amt)
              amt = uptake(EFERT,iel) * euf(FBRCH)
              call flow(esrsnk(iel),fbrche(iel),time,amt)
              amt = uptake(EFERT,iel) * euf(LWOOD)
              call flow(esrsnk(iel),rlwode(iel),time,amt)
              amt = uptake(EFERT,iel) * euf(CROOT)
              call flow(esrsnk(iel),croote(iel),time,amt)

c...Automatic fertilizer added to mineral pool
              amt = uptake(EFERT,iel) * (1./(0.01+favail(iel)) - 1.)
              fertot(iel) = fertot(iel) + uptake(EFERT,iel) + amt
              call flow(esrsnk(iel),minerl(SRFC,iel),time,amt)
            endif
	  endif
90      continue
 
c...Else there is no production this month
      else  
	cprodf = 0.
c        mfprd(LWOOD) = 0.
c        mfprd(FBRCH) = 0.
	do 140 iel = 1, 3
	   eprodf(iel) = 0.
	   do 130 ipart = 1, FPARTS
             eup(ipart,iel) = 0.
130	   continue
140	continue
      endif

       
        prefstgr = prefstgr + cprodf

c...Forest removal option
c...Moved to SIMSOM for consistency when running savanna model (rm 7/94)

c     if (dotrem .or. dofire(FORSYS)) then
c       call frem()
c     endif

c      if ((time-int(time)) .lt. 0.5 .and. (time-int(time))
c     $   .gt. 0.4 ) then
c      endif

      return
      end
