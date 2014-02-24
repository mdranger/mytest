
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


      subroutine cycle(month, cancvr, wfunc) 

      integer month
      real cancvr, wfunc

!...Determine relative water content, available water, and
!     decomposition factor related to temperature and water.
!     Initialize mineralization accumulators.  Compute potential
!     net production.
!  Add calculation of GPP and RESP for crop 
!  when using ECLUE model
!...Changed 8-31-90:  added ELITST as a multiplier for SFCLIT.
!...Added savanna model (BO)
!...01/25/2009 Add calcualtion from EC-LUE odel

      include 'comput.inc'
      include 'const.inc'
      include 'dovars.inc'
      include 'param.inc'
      include 'parcp.inc'
      include 'parfs.inc'
      include 'parfx.inc'
      include 'plot1.inc'
      include 'plot2.inc'
      include 'plot3.inc'
      include 'potent.inc'
      include 'seq.inc'
      include 'wth.inc'
      include 'prof.inc'
      include 'zztim.inc'
      include 'wetland.inc'


!...Function declarations
      real anerob, irrigt,  pevap, tcalc
      external anerob, irrigt, pevap, tcalc

!...Local variables
      integer i, iel, lyr
      real aglivb, bio, co2val, pp, rprpet, sfclit, stdead, 
     -     tfunc, tmns, tmxs
      character*80 string

!...Added new variable CANCVR for savanna model.  It will be passed
!     to POTGRS.  -rm 6/91

!...Add cursys .eq. 3 checks (BO)

!...Call schedl to determine scheduling options for this month
      call schedl()

!...Initialize production accumulators

!...For crops, annual accumulation starts at the month of planting.
!...For grass, annual accumulation starts at the beginning of
!     the growing season.
!...For forest, annual accumulation starts at the beginning
!     of the growing season.
      if ((dofrst .or. doplnt .or. dofone) .or.
     +		(month .eq. 1)) then
        call inprac
      endif

!...Average air temperature at 2 meters
      tave = (maxtmp(month) + mintmp(month)) / 2.0

!...Calculate RAIN for an output variable
      rain = prcurr(month)

!...If irrigating, determine actual amount of irrigation, irract
      if (doirri) then
        irract = irrigt(month)
      else
        irract = 0
      endif

!...Add by zp, the calculation of CH4 production from soil
!      if (doflood) then
!         ch4soil = ch4prod(tave, somsc)
!         print *, 'flooding', ch4soil
!      else
!          ch4soil = 0
!          ch4vege = 0
!      endif

!...If planting or growing month, calculate growing season precipitation
      if (doplnt .or. dofrst) then
        call prcgrw(month)
      endif

!...Initialize the mineralization accumulators for each element.
      do 30 iel = 1, MAXIEL
        do 25 lyr = 1, 2
          strmnr(lyr,iel) = 0.0
          metmnr(lyr,iel) = 0.0
          s1mnr(lyr,iel) = 0.0
25      continue
        s2mnr(iel) = 0.0
        s3mnr(iel) = 0.0
        gromin(iel) = 0.0
	w1mnr(iel) = 0.0
	w2mnr(iel) = 0.0
	w3mnr(iel) = 0.0
30    continue

!...Compute the ratio of precipitation to PET.
!   Added avh2o(3) to rprpet calculation  -rm 6/91
      pet = pevap(month)
      
      rprpet = (avh2o(3) + prcurr(month) + irract) / (0.001+pet)
      
!...If the system is a microcosm, skip the rest of the routine
      if (micosm .eq. 1) then

        pet = 15.
        
        anerb = anerob(aneref, drain0, rprpet, pet, tave, micosm, somtc,
     $ cropdrain)

        if (docult) then
          do 32 i = 1, 4
            cltfac(i) = clteff(i)
32        continue
        else
          do 33 i = 1, 4
            cltfac(i) = 1.0
33        continue
        endif
        return
      endif
      
!***********************************************************************
!...Calculate temperature...
      if (cursys .eq. FORSYS) then
!...Live biomass
        aglivb = rleavc * 2.5
!...Surface litter biomass
!...Second mod to remove effect of woodc -rm 1/91
        sfclit = (strucc(SRFC) + metabc(SRFC)) * 2.0

!...Standing dead biomass
        stdead = 0.0

      elseif (cursys .eq. SAVSYS) then
!...Live biomass
        aglivb = (rleavc + aglivc) * 2.5


!...Surface litter biomass
        sfclit = (strucc(SRFC) + metabc(SRFC)) * 2.0

!...Standing dead biomass
        stdead = stdedc * 2.5

      else
!...Not forest
!...Live biomass
!...Removed addition of .25*pp to aglivb (AKM)
        aglivb = aglivc * 2.5

!...Surface litter biomass
        sfclit = (strucc(SRFC) + metabc(SRFC)) * 2.5

!...Standing dead biomass
        stdead = stdedc * 2.5
      endif
      
      bio = aglivb + stdead + elitst * sfclit
      bio = min(bio,pmxbio)

!...Maximum temperature
      tmxs=maxtmp(month)+(25.4/(1.+18.*exp(-.20*maxtmp(month))))
     1             *(exp(pmxtmp*bio)-.13)

!...Minimum temperature
      tmns=mintmp(month)+pmntmp*bio-1.78

!...Average surface temperature
!...Note: soil temperature used to calculate potential production does not
!         take into account the effect of snow (AKM)
      stemp=(tmxs+tmns)/2.


!***********************************************************************
!...Shifted calculation of potential production so that it uses the current
!     month's soil temperature (AKM)

!...Determine potential production if it's during the growth season.
!...The variable, pp, is used below to recompute aglivb for use in h2olos.
      pp = 0.0

      
!...For a Crop System...
!...Check we should use EC-LUE model for GPP
      if (npp_input .eq. 5 ) then
!...For crop system, get the GPP and respiration
!...this may cause some problem for savanna
!...
         if (cursys .eq. CRPSYS) then
!...Calculate the GPP using ECLUE model and cal the auto resp         
           call potcrp(month,cancvr)
           pp = pcropc
!           cgpp = ecluegpp(int(time), month)
!           cresp = cropResp(cgpp)
!...Make sure the NPP is bigger than 0
!           if (cgpp .gt. cresp) then
!             pp = cgpp - cresp
!           else
!             pp = 0.0
!           endif
!           pcropc = pp
         else
!...handle the forest system
           fgpp = ecluegpp(int(time), month)
           fresp = autoResp(fgpp)
           pp = fgpp - fresp
!...make sure we save the pforc, otherwise the soil cannot be used
           pforc = pp
!           print *, time, "cal gpp in cycle ", fgpp
         endif
!.............................................................
      else
!..............................................................
        if (crpgrw .eq. 1) then
!...get the potential production with temp and water stress      
          call potcrp(month,cancvr)
          pp = pcropc
        endif
!...For a Forest System...
        if (forgrw .eq. 1) then
          call potfor(month)
          pp = pforc
        endif

!..........end the GPP calculation
      endif
!...Moisture
      if (cursys .eq. CRPSYS) then
        aglivb = aglivb + .25 * pp * 2.5
      endif
	
!...Here is a problem, for savana, the pp may be override by the
!...forest production
!...12/01/2009, calculate the vegetation derived CH4
!...using Huang, 1998 from paddy rice
!...
!...for Cattail, the factor is adjusted with observation only in
!...several sites with NP
!           if (anerb .lt.0.2) then
!...add by zp, for crop ch4 emission
            if (doflood) then
               ch4vege = 0.27 * pp * exp(stemp*0.01)
               ch4soil = ch4prod(stemp, som1c(2))
!               print *, 'flooding ch4', ch4soil, ch4vege
           else
               ch4vege = 0.0
               ch4soil = 0.0
           endif
!...removed this process to simsom function, move this back 
           
!***********************************************************************


!...Determine co2 effect on transpiration, pass to h2olos
      if (cursys .eq. SAVSYS) then
        if (aglivc + rleavc .eq. 0.0) then
	  co2val = 1.0
        else
	  co2val = (co2ctr(CRPSYS)*aglivc + co2ctr(FORSYS)*rleavc) /
     +             (aglivc + rleavc)
        endif
      else
	co2val = co2ctr(cursys)
      endif

!...      print *,"In cycle asmos", asmos(1)
      call h2olos(month,aglivb,sfclit,stdead,co2val)

!...Average surface temperature
!     Added check for snow  -rm 8/91
      if (snow .gt. 0.0) then
        stemp = 0.0
      endif

!...Effect of temperature on decomposition
      tfunc = tcalc(stemp, teff)

!...Effect of moisture on decomposition
!...Option selection for wfunc depending on idef
!        idef = 1 for linear option
!        idef = 2 for ratio option
!...Changed coefficients in both wfunc equations  -rm 6/91
!...Relative water content -- rwcf

      if (idef .eq. 1) then
        wfunc = 1./(1. + 4.0 * exp(-6.0*rwcf(1)))
      else if (idef .eq. 2) then
        wfunc = 1./(1. + 30.0 * exp(-8.5 * rprpet))
      else
        call message(' ')
        string = '   IDEF may be either 1 or 2, but is currently: ' //
     +           char(ichar(char(idef)) + ichar('0'))
        call message(string)
        call message('   You must reset it and restart the run.')
        call message(' ')
        STOP
      endif

      if (wfunc .ge. 1.0) then
        wfunc = 1.0
      endif

!...Calculate the effect impact of anerobic conditions on decomposition
      anerb = anerob(aneref, drain0, rprpet, pet, tave, micosm, somtc,
     $ cropdrain)

!...Combined effects of temperature and moisture
      defac=tfunc*wfunc
!...Bound defac to >= 0.0 12/21/92
       if (defac .lt. 0.0) defac = 0.0
       defacm(month)=defac

!...Effect of cultivation on decomposition (used in decomp routine)
!...Determine effect of cultivation on decomposition. vk 03-13-91
!...cltfac is this month's effect of cultivation on decomposition
!     of som1, som2, som3, and structural.  It is set to clteff
!     in months when cultivation occurs; otherwise it equals 1.
!...clteff is the effect of cultivation on decomposition read from
!    the cult.100 file

      if (docult) then
	do 34 i = 1, 4
          cltfac(i) = clteff(i)
34      continue
      else
	do 35 i = 1, 4
          cltfac(i) = 1.0
35      continue
      endif
      
      return
      end
