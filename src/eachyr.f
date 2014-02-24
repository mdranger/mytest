
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


      subroutine eachyr

c...Perform tasks that only need to be done once a year.

      include 'const.inc'
      include 'chrvar.inc'
      include 'isovar.inc'
      include 'ligvar.inc'
      include 'param.inc'
      include 'parcp.inc'
      include 'parfs.inc'
      include 'plot1.inc'
      include 'plot2.inc'
      include 'plot3.inc'
      include 'seq.inc'
      include 'wth.inc'
      include 'zztim.inc'
      include 'prof.inc'

c...Function declarations
      real     fracis, pevap 
      external fracis, pevap 

c...Local variables
      integer  iel, ipart, mon
      real     lfncmax, lfncmin, lfncon


c...Added for savanna model (plot3.inc) BO

c...Correct for the rounding problem with time. The value for time
c     drifts downward during long runs since dt=1/12 cannot be represented
c     precisely.  At this point in the run, time should be a whole number.

c...Changed increment value from .001 to .5 to correct error in time calculation
c     occuring after year 8192. (mse 3/95).  New code if from Kendrick Killian.

      time = sign(int(abs(time)+.5),int(time))

c...Reset annual accumulators to zero
      call annacc
c...Weather data
      call weathr(precip,prcstd,prcskw,mintmp,maxtmp)

c...Wet-dry fixation of N

c...add forest age to account for the decrease of production over time. 
c   Liu Sept 2001
      if (curtre .ne. pretre .and. time > 1.0) then
        forage = 1
        steadyage = 0
        steady = 0
      else
         forage = forage + 1
      endif
c      print *, time, forage, '  ', pretre, '  ', curtre
      pretre = curtre
     

c...Determine annual precipitation and annual PET
      prcann = 0.0
      petann = 0.0
      nfixac = 0.0
      wdfxas = 0.0
      wdfxaa = 0.0
      do 10 mon = 1, MONTHS
        prcann = prcann + prcurr(mon)
        petann = petann + pevap(mon)
10    continue

c...N fixation in atmosphere
      wdfxa = epnfa(INTCPT)+epnfa(SLOPE)*MIN(prcann,80.0)
      if (wdfxa .lt. 0.) then
        wdfxa = 0.0
      endif
      wdfxs = epnfs(INTCPT)+epnfs(SLOPE)*MIN(prcann,100.0)
      if (wdfxs .lt. 0.)  then
        wdfxs = 0.0
      endif
      wdfx = wdfxa+wdfxs

c...Atmospheric S deposition
      satmt = max(0.0, satmos(1) + satmos(2)*prcann)

c...Determine what fraction of the carbon in new plant tissue is labeled
      if (labtyp .eq. 0) then
        cisofr = 0.0
        cisotf = 0.0
      elseif (labtyp .eq. 1) then
        cisofr = fracis(time,labyr)
        cisotf = cisofr
c     elseif (labtyp .eq. 2) then
c       cropin has set cisofr
c       treein has set cisotf
      endif

c...Initialize co2 effects
      call co2eff(time)

c...Added effect of co2 for forest; done here because not calcualted
c     dynamically based on biomass like grassland/crop
c...Direct CO2 effects only C/E ratio of leaves.
      do 30 iel = 1, nelem
        ccefor(IMIN,LEAF,iel) = cerfor(IMIN,LEAF,iel) *
     $                            co2cce(FORSYS,IMIN,iel)
        ccefor(IMAX,LEAF,iel) = cerfor(IMAX,LEAF,iel) *
     $                            co2cce(FORSYS,IMAX,iel)
 30   continue

      do 50 ipart = 2, FPARTS
        do 40 iel = 1, nelem 
          ccefor(IMIN,ipart,iel) = cerfor(IMIN,ipart,iel)
          ccefor(IMAX,ipart,iel) = cerfor(IMAX,ipart,iel)
 40     continue 
 50   continue


c...Calculate leaf death rate multiplier for continuous forests 11/20/92
c...Initialize LDRMLT to 1.0
      ldrmlt = 1.0

c...Change leaf death rate multiplier if you have floating C/E ratios.
      if (ccefor(IMIN,LEAF,N) .ne. ccefor(IMAX,LEAF,N)) then
        if (rleavc .gt. 0) then
          lfncon = rleave(N) / (0.0001+rleavc)
          lfncmin = 1 / ccefor(IMIN,LEAF,N)
          lfncmax = 1 / ccefor(IMAX,LEAF,N)
          ldrmlt = 1 + (maxldr - 1) *
     $             (lfncon - lfncmin) / (lfncmax - lfncmin)
        endif
      endif

      if (cursys .ne. FORSYS) then

!...Determine what fraction of plant residue added this year
!     will be lignin.
        call cmplig(cursys,fligni,wdlig,pltlig)
!        print *, time, pltlig(2)
      endif

      return
      end
