
c               Copyright 1993 Colorado State University
c                       All Rights Reserved
!... created by Shuguang Liu for costa rica research

      subroutine h2olos(month, aliv, alit, adead, co2val)

      include 'const.inc'
      include 'param.inc'
      include 'parfx.inc'
      include 'plot1.inc'
      include 't0par.inc'
      include 'prof.inc'
      include 'zztim.inc'
      include 'site.inc'

      integer month, cycles
      real aliv, alit, adead, co2val


c...Water Submodel for Century - written by Bill Parton
c     Updated from Fortran 4 - rm 2/92
c     Rewritten by Bill Pulliam - 9/94

c....function declaration
      real  randu
      external randu

c...Local Variables
      integer   j, i, k, m, dummy, jj, fall(31)
      integer   fallen, drydays(31)
      real      abs, add, afl, aint, amelt, asimx, avhsm,
     $	          avw, awwt(MAXLYR), base, evl,
     $            evlos, evmt, evsnow, fwlos, inputs, 
     $            petrem, pevp, rwc1, sd, snow1, strm,
     $            tot, tot2, trap, trl(MAXLYR), winputs, 
     $            nevents, addc(31), trapc, awi, empty,
     $            temp, avia(MAXLYR), amov1(31, MAXLYR),
     $            avlb, tottran,trav,rdd,mm,
     $            delta, x, xx, xxx, ci(31),dry(32),
     $            wfps(MAXLYR), porosity(MAXLYR), dw(2,MAXLYR),dwfps

c...Description of variables
c
c....randu      a randum function generating number from 0 to 1
c   adead       the average monthly standing dead biomass(gm/m..2)
c   adep        depth of the ith soil layer(cm)
c   afiel       the field capacity of the ith soil layer(fraction)
c   alit        the average monthly litter biomass(gm/m..2)
c   aliv        the average monthly live plant biomass(gm/m..2)
c   amov        the index for water movement(0-no flow,1-satruated flow)
c   asmos       the soil water content of the ith soil layer(cm h2o)
c   asnow       the snow pack water contint(cm-h2o)
c   avh2o (1)   water avlbable to plants for growth
c   avh2o (2)   water avlbable to plants for survival
c               (avlbable water in the whole soil profile)
c   avw		avlbable water in current soil layer
c   awilt       the wilting point of the  ith soil layer(fraction)
c   awtl        the weight factor for transpiration water loss from the ith
c               soil layer(nod)
c   evl         evaporation losses,
c   evap        the water evaporated from the  soil and vegetation(cm/mon)
c   evsnow	snow evaporated
c   inputs      rain + irrigation
c   winputs     inputs which are water (not converted to snow)
c   irract	ammount of irrigation (cm)
c   nlayer	number of soil layers with water avlbable for plant survival
c   nlaypg	number of soil layers with water avlbable for plant growth
c   petrem	remaining pet, updated after each incremental h2o loss
c   pevp        the potential evaporation rate from the top  soil layer (cm/day)
c   rain	the total monthly rainfall (cm/month)
c   rwcf        the relative water content of the ith soil layer(0-1)
c   snlq        the liquid water in the snow pack
c   tav		average monthly air temperature (2m-c)
c   tran        transpriation water loss(cm/mon)
c   trl		transpiration water loss
c   tottran     total cumulative tranpisration
c   addc(i)     total rainfall added to the soil during each storm (cm)
c   trav	available tranpiration based on energy that has not been filled

c...Initialize Variables
      if ( debug_flag .eq. 2) then
      open(unit=79, file='swater.out', status='UNKNOWN')
      endif
      
      dummy = 1
      add = 0.0
      amelt = 0.0
      asimx = 0.0
      avh2o(1) = 0.0
      avh2o(2) = 0.0
      avh2o(3) = 0.0
      evap = 0.0
      pevp = 0.0
      pttr = 0.0
      rwc1 = 0.0
      tran = 0.0
      trap = 0.01
      abs = 0.0
      evsnow = 0.0


c...Calculate total inputs 
      inputs = rain + irract

c...Create new local variable for water inputs.
c   Set to zero later if it snows.  -mdh 4/95
      winputs = inputs

c...Throughout, uses petrem as remaining energy for pet after
c     each melting and evaporation step.  Initially calculated
c     pet is not modified.  Pulliam 9/94
      petrem = pet

c...Determine the snow pack, melt snow, and evaporate from the snow pack

c...When mean monthly air temperature is below freezing, 
c     precipitation is in the form of snow.
      if (tave .le. 0.0) then
         snow = snow + inputs
         winputs = 0.0
      endif

c...Melt snow if air temperature is above minimum (tmelt(1))
      if (tave .ge. tmelt(1)) then
c...Calculate the amount of snow to melt:
        amelt = tmelt(2) * (tave - tmelt(1))
        if (amelt .gt. snow) amelt = snow
        snow = snow - amelt

c...Melted snow goes to snow pack and drains excess
c...  add rain-on-snow  and melted snow to snowpack liquid (snlq):
        if (tave .gt. 0 .and. snow .gt. 0) snlq = snlq + inputs
        snlq = snlq + amelt
c...Drain snowpack to 5% liquid content (weight/weight), excess to soil:
        if (snlq .gt. (0.05 * snow)) then
          add = snlq - 0.05 * snow
          snlq = snlq - add
        endif
      endif 

c...Evaporate water from the snow pack (rewritten Pulliam 9/94 to
c     evaporate from both snow aqnd snlq in proportion)  
c...Coefficient 0.87 relates to heat of fusion for ice vs. liquid water
c     wasn't modified as snow pack is at least 95% ice.
      if (snow .gt. 0) then

c...Calculate cm of snow that remaining pet energy can evaporate:
        evsnow = petrem * 0.87

c...Calculate total snowpack water, ice + liquid:
        snow1 = snow + snlq

c...Don't evaporate more snow than actually exists:
        if (evsnow .gt. snow1) evsnow = snow1

c...Take evsnow from snow and snlq in proportion:
        snow = snow - evsnow * (snow/snow1)
        snlq = snlq - evsnow * (snlq/snow1)

c...Add evaporated snow to evaporation accumulator (evap):
        evap = evap + evsnow

c...Decrement remaining pet by energy used to evaporate snow:
        petrem = petrem - evsnow / 0.87
        if (petrem .lt. 0.0)  petrem = 0.0
      endif

c...Calculate bare soil water loss and interception
c     when air temperature is above freezing and no snow cover.
c...Mofified 9/94 to allow interception when t < 0 but no snow
c     cover, Pulliam
      if (snow .eq. 0.0) then

c...Calculate total canopy cover and litter, put cap on effects:

         sd = aliv + adead
         if (sd .gt. 800.0) sd = 800.0
         if (alit .gt. 400.0) alit = 400.0

c...canopy interception, fraction of  precip (aint):
         aint = (0.0003 * alit + 0.0006 * sd) * fwloss(1)

c...Bare soil evaporation, fraction of precip (abs):
         abs = 0.5 * exp((-0.002 * alit) - (0.004 * sd)) * fwloss(2)

c...Calculate total surface evaporation losses, maximum 
c     allowable is 0.4 * pet. -rm 6/94
         evl = MIN(((abs + aint) * inputs), (0.4 * petrem))
         evap = evap + evl

c...Calculate remaining water to add to soil and potential
c     transpiration as remaining pet:
c        add = add + inputs - evl
         add = add + winputs - evl
         trap = petrem - evl
      endif

c...Assume number of rainfall events according to the size of water 
c      input to the soil. Liu, 2/97
c.....La Selva, Costa Rica
c      nevents = 6.022 * log(inputs) + 0.42
c....Batesville, Mississippi
	nevents = 2.7958 * log(inputs) + 1.92
	if (nevents .lt. 1.0) then
	  nevents = 1.0
	endif
	cycles = int(nevents)

c...Calculate the volume of water each rainfall added into the soil
c     during a wet-dry cycle. liu, 2/97
c      addc = add / nevents
c    each addc(i) has a probability of occurence of 0.2, the mean rainfall  
c    depth for each such rainfall events is calculated as below:
c....La Selva, Costa Rica
c       addc(1) = 2.20*add/nevents
c       addc(2) = 0.96*add/nevents
c       addc(3) = 0.60*add/nevents
c       addc(4) = 0.42*add/nevents
c       addc(5) = 0.15*add/nevents

	delta = 1.0/real(cycles)
	   tempci = 0.0
	do 109 i = 1,cycles 
	  x = delta*i
	  xx = x*x
	  xxx = xx*x
	  ci(i)=x*(ralloc(1)+ralloc(2)*x+ralloc(3)*xx+ralloc(4)*xxx)
	  if (i .eq. 1) then
	  addc(i) = ci(i)*add
	  else
	  addc(i) = (ci(i)-ci(i-1))*add
	  endif
c	print *, cycles, inputs, addc(i)
c	  addc(i) = 8.6326*exp(-4.7335*(0.2*i - 0.1))*inputs/real(cycles)
c   assume interception is 1.5 mm during each rainy day
	  addc(i) = addc(i)
c	  addc(i) = addc(i)-0.15
	  if (addc(i) .lt. 0.0) addc(i) = 0.0
109	continue

c...Determine potential transpiration water loss (trap, cm/mon) as a
c     function of precipitation and live biomass.
c...If temperature is less than 2C turn off transpiration. -rm 6/94
c      print *, 'tave = ', tave, 'trap = ', trap
      if (tave .lt. 2.0) then
         pttr = 0.0
      else
         pttr = petrem * 0.65 * (1.0 - exp(-0.020 * aliv)) * co2val
      endif
      if (pttr .le. trap) trap = pttr
      if (trap .le. 0.0) trap = 0.01

c...Calculate potential transpiration during one wet-dry cycle (trapc, cm/cycle)
      trapc = trap / real(cycles)
	tottran = 0.0
c	print *, trapc
      
c...Maintain pttr on a monthly basis for harvest
      hpttr(month) = pttr

c...Calculate the potential evaporation rate from the top soil layer
c     (pevp-cm/day).  This is not actually taken out until after
c     transpiration losses
      pevp = petrem - trap - evl
      if (pevp .lt. 0.0) pevp = 0.0


c...Transpire water from added water first, before passing water
c     on to soil.  This is necessary for a monthly time step to
c     give plants in wet climates adequate access to water for
c     transpiration. -rm 6/94, Pulliam 9/94
c...The following three statements are commented by liu, 2/97
c. ...     tran = MIN((trap - .01), add)
c...      trap = trap - tran
c...      add = add - tran

c...Add water to the soil
c...Changed to add base flow and storm flow.  -rm 2/92

      strm = 0.0
      base = 0.0
      stream(1) = 0.0

c...Calculate transpiration water loss from each layer
c...This section was completely rewritten by Pulliam, though it
c     should still do the same thing.  9/94
      rwc1 = 0.0
      tot = 0.0
      tot2 = 0.0
      tran = 0.0

c......Calculate total avlbable water, Liu, 2/97
	awhc = 0.0

      do 20 j = 1, rlayers
c      do 20 j = 1, nlayer

              awhc = awhc + (afiel(j) - awilt(j)) * adep(j)

c...Calculate avlbable water in layer, asmos minus wilting point:
         avw = asmos(j) - awilt(j) * adep(j)
         if (avw. lt. 0.0) avw = 0.0
c...Calculate avlbable water weighted by transpiration depth
c      distribution factors:
         awwt(j) = avw * awtl(j)
c...Sum up avlbable water:
         tot = tot + avw
c...Sum up weighted avlbable water:
         tot2 = tot2 + awwt(j)
20    continue

c...Calculate the actual transpiration water loss(tran-cm/mon)
c.............the unit of tran should be cm/cycle after the modification.
c             liu, 2/97
c...Also rewritten by Pulliam 9/94, should do the same thing
c...Update potential transpiration to be no greater than avlbable water:
c      trapc = MIN(tot2,trapc)

c................calculate transpiration for a wet-dry cycle based on daily
c                dynamics of soil moisture. Days in the cycle is 30/n. Liu, 2/97

c	do 101 j=1, nlayer
c	  do 97 i=1,cycles
c            mois(i,j) = mosend(j)
c97	continue
c101	continue

c...The following calculations are just dealing with one wet-dry
c       cycle. Because the averages of soil moisture and transpiration 
c       during a wet-dry cycle are
c       representatives of the averages of the month. Liu, 2/97

	depth = 0.0
      do 10 j=1,actlayers
c      do 10 j=1,nlayer

c...Add water to layer j:..........................................................
c... The following is commented by liu, 2/97
c...         asmos(j) = asmos(j) + add 
	depth = depth + adep(j)
         empty = adep(j) * afiel(j) - asmos(j)


c...Calculate field capacity of soil, drain soil, pass excess
c...     on to amov:
         afl = adep(j) * afiel(j)
         awi = adep(j) * awilt(j)

	avlb = 0.0
	addf = 0.0
          amov(j) = 0.0
	    avia(j) = 0.0
	
          asmos(j) = 0.0

c..... do run_num random simulations of rainfall events

	do 5 i=1,run_num 

c....determine the dry days between two consecutive events
        temp=0.0
	do 155 jj=1, cycles
	  dry(jj) = randu(dummy)
	  temp = temp + dry(jj)
155	continue

c.....assume 30 days/per month
	do 55 jj=1, cycles
	  drydays(jj) = int(dry(jj)/temp*30.0)
	  if (drydays(jj) .eq. 0) drydays(jj) = 1
55	continue

c....all rain storms have not occurred.
	  do 1 k=1, cycles
            fall(k) = 0
1	continue

          fallen = 0 
	do 3 k=1,cycles 
c.....random pick a rainfall event
	   rdd = randu(dummy)

	   temp = 1.0/real(cycles-fallen)

	   mm = 0.0
	   do 24 jj =1, cycles
             if (fall(jj) .eq. 0) then
                  mm = mm + temp
             endif
	     if (mm .ge. rdd) then
		fall(jj) = 1
                m = jj
		goto 74
	     endif
24	continue

74	   fallen = fallen + 1


c	print *,j,asmos(j), '   in h2o'
c	if (j .le. nlayer - 2) then
c	do 66 jj=0, 2
c              porosity(j+jj) = 1.0-lyrden(j+jj)/2.65
c              wfps(j+jj) = (mois(i,j+jj)-awi)/
c     $         (porosity(j+jj))
c66	continue
c	  do 68 jj=1,2
c	     dw(jj,j) = 0.0
c             dwfps = 0.25*max(dw(jj,j),wfps(j+jj)-wfps(j))
c     $             *porosity(j+jj)
c                   mois(i,j) = mois(i,j) + dwfps
c                   mois(i,j+jj) = mois(i,j+jj) - dwfps
c                   asmos(j) = asmos(j) + dwfps
c                   asmos(j+jj) = asmos(j+jj) - dwfps
c	     if (time .lt. 1985.1 .and. time .ge. 1985) then
c	       write(*,67) j,jj,time, mois(i,j), mois(i,j+jj),asmos(j), dwfps
c	     endif
c67	format(2i5,10f8.2)
c68	continue
c	endif

!...	print *,j,k,asmos(j), '   in h2o'

c	print *, j, i, k , m, drydays(k),cycles, addc(m)
c....water routing
	   if (j .eq. 1) then
           mois(i, j) = mois(i,j) + addc(m)
	 else 
	    mois(i,j) = mois(i,j) + amov1(i,j-1)
	  endif
c	print *, mois(i,j), afl

         if (mois(i,j) .gt. afl) then
            amov1(i,j) = mois(i, j) - afl
            mois(i, j) = afl

c...If you are at the bottom layer, compute storm flow.
            if (j .eq. actlayers) strm = amov1(i,j) * stormf
c            if (j .eq. nlayer) strm = amov1(i,j) * stormf
         else
            amov1(i,j) = 0.0
         endif


c...Calculate avlbable water in layer j. avia under rainfall class i, in cm:
            avia(j) = mois(i,j) - awilt(j) * adep(j)
            if (avia(j) .lt. 0.0) avia(j) = 0.0

c...trapc is the potential transpiration

	    if (trapc .gt. tottran) then
            trl(j) = trapc * lyrrtfr(j)*fwloss(3)*
     $             real(drydays(k))/30.0*real(cycles)
	    else
	    trl(j) = 0.0
	    endif

            if (trl(j) .gt. avia(j)) trl(j) = avia(j)

	   trav = trapc - tottran
	   if (trav .lt. 0.0) trav = 0.0
	   trl(j) = min(trl(j),trav)

	   if (i .eq. cycles) then
	      tottran = tottran + trl(j)
	   endif

c   calculate monthly mean soil moisture content
c.....The difference of water budget of a single wet-dry cycle is: input - output
c.....The total difference during the month is: nevents * (input - output)
c.....The average monthly soil moisture change would be corresponding to half of the budget change (trl devided by 2). 
c.... why output is higher than input

              temp = mois(i,j)
              mois(i,j) = mois(i,j)-trl(j)
            if (mois(i, j) .lt. awi) mois(i, j) = awi
            if (mois(i,j) .gt. afl) mois(i,j) = afl

c.....compute the unsaturated soil water upward movement caused by
c.....hydraulic pressure difference.  The latter can be approximated
c.....by wfps (water-filled-pore space) according to Hillel, 1980.
c      fi = a*wfps**(-b)
c     therefore water movement can be considered under the control of
c     wfps difference between two consecutive layers
!...          if (j.eq.1) then
!...            print *,"asmos", j, asmos(j), mois(i,j), run_num, cycles
!...          endif
            asmos(j)=asmos(j)+(temp+mois(i,j))/
     $          (real(run_num)*real(cycles)*2.0)
!...soil moisture shouldn't less than 0, if calculated loss is too high
!...Set to a small number
            if (asmos(j) .lt. 0) then
                asmos(j) = 0.001
            endif
2	format (2i5, 9f9.2)

              avia(j) = (temp+mois(i,j))/2.0 - awi

c	print *, time, j, wfps(j), porosity(j), dwfps

c	if (j .le. MAXLYR - 2) then
c	  do 68 jj=1,2
c	     dw(jj,j) = 0.0
c             dwfps = 0.5*max(dw(jj,j),wfps(j+jj)-wfps(j))*porosity(j+jj)
c                   mois(i,j) = mois(i,j) + dwfps
c                   mois(i,j+jj) = mois(i,j+jj) - dwfps
c                   asmos(j) = asmos(j) + dwfps
c                   asmos(j+jj) = asmos(j+jj) - dwfps
c68	continue
c	endif

c            avlb= avlb+ avia(j)/(real(run_num)*real(cycles))
c......Total actual transpiration during a wet-dry cycle: tran, cm/cycle. Liu 2/97
3	 continue
            tran = tran + trl(j)
            amov(j) = amov(j) + amov1(i,j)
            addf = addf + addc(m)
c	   mosend(j) = mois(i,j)
c	print *,mosend(j)
5         continue

c	   write (*, 2) j, j,j, time, afl, awi, asmos(j)

c	if (tran .gt. trapc) stop
             rwcf(j)=(asmos(j)/adep(j)-awilt(j))/(afiel(j)-awilt(j))

	if (j .eq. nlayer .and. time .gt. real(tend)-20.0) then
      if ( debug_flag .eq. 2) then
c	write(*, 7) time, inputs, asmos(1)/adep(1), asmos(2)/adep(2),asmos(3)/adep(3)

	write(79, 7) time, inputs, asmos(1)/adep(1), asmos(2)/adep(2),
     $asmos(3)/adep(3), asmos(4)/adep(4), asmos(5)/adep(5),
     $asmos(6)/adep(6), asmos(7)/adep(7), asmos(8)/adep(8),
     $asmos(9)/adep(9)
      endif
	endif
7	format (15f7.2)

c...Sum up water avlbable to plants for growth:
      avlb= asmos(j) - awilt(j) * adep(j)
c            if (j .le. 2) avh2o(1) = avh2o(1) + avlb
            if (j .lt. rlayers) then
		       avh2o(1) = avh2o(1) + avlb
            else if (j .eq. rlayers) then
	         avh2o(1) = avh2o(1) + avlb
c....modified at Feb 2005
c	        avh2o(1) = avh2o(1) + avlb*
c     $                (adep(j)-depth+rootdep)/adep(j)
	    endif
c            if (j .le. nlaypg) avh2o(1) = avh2o(1) + avlb

c...Sum up water avlbable to plants for survival:
c	if (j .le. nlayer) avh2o(2) = avh2o(2) + avlb
c	if (j .le. slayers)  avh2o(2) = avh2o(2) + avlb
            if (j .lt. slayers) then
		       avh2o(2) = avh2o(2) + avlb
            else if (j .eq. slayers) then
	         avh2o(2) = avh2o(2) + avlb
c....modified at Feb 2005
c	        avh2o(1) = avh2o(1) + avlb*
c     $                (adep(j)-depth+rootdep)/adep(j)
	    endif
c...Calculate parameter of H2O accumulation in top 2 soil layers:
            if (j .le. 2) avh2o(3) = avh2o(3) + avlb
c...Add water to layer j+1:..........................................................
c...Copy amov to add, continue with next layer:
c....         addc = amov(j)
10    continue


c...Compute base flow and stream flow for H2O.
c...Put water draining out bottom that doesn't go to stormflow
c     into nlayer+1 holding tank:

      watertank = watertank + addf - strm
c      asmos(nlayer+1) = asmos(nlayer+1) + addf - strm

c...Drain baseflow fraction from holding tank:
      base = watertank * basef * nevents
      watertank = watertank - base/nevents
c      base = asmos(nlayer+1) * basef * nevents
c      asmos(nlayer+1) = asmos(nlayer+1) - base/nevents

c...Streamflow = stormflow + baseflow:
      stream(1) = strm + base

c...Save asmos(1) before transpiration for future use:
      asimx=asmos(1)



c...Set htran for use in harvst.f
         htran(month) = tran
    

c...Evaporate water from the top layer
c...Rewritten by Pulliam, should still do the same thing 9/94

c...Minimum relative water content for top layer to evaporate:
      fwlos = 0.25

c...Fraction of water content between fwlos and field capacity:
      evmt = (rwcf(1)-fwlos)/(1.-fwlos)
      if (evmt .le. 0.01) evmt = 0.01

c...Evaporation loss from layer 1:
      
      evlos = evmt * pevp * abs * 0.10
!...      print *, 'evmt ', evmt, 'pevp ', pevp, abs
      avlb= asmos(1) - awilt(1) * adep(1)
      if (avlb.lt. 0.0) avlb= 0.0
      if (evlos .gt. avlb) evlos = avlb      

!...  This could get some error when evlos is larger
!      than asmos(1)
!...      print *, month, "asmos1", asmos(1), evlos
      asmos(1) = asmos(1) - evlos
      evap = evap + evlos

      
c...Recalculate rwcf(1) to estimate mid-month water content
      avhsm = (asmos(1) + rwc1 * asimx)/(1. + rwc1)
c      print *, 'asmos1 ', asmos(1), 'rwc1 ', rwc1, evlos
c      print *, 'adep ', adep(1), 'avhsm ', avhsm
      rwcf(1)=(avhsm/adep(1)-awilt(1))/(afiel(1)-awilt(1))
c...Update avlbable water pools minus evaporation from top layer
      avh2o(1) = avh2o(1) - evlos
      avh2o(2) = avh2o(2) - evlos
      avh2o(3) = avh2o(3) - evlos

      return
      end
