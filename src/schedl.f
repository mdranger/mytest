
c              Copyright 1993 Colorado State University
c                       All Rights Reserved

C...SCHEDL.F

      subroutine schedl()

c...Determine the next set of scheduling options from the 
c     schedule file
c...add a flag to detect the land cover change between
c...species
      include 'chrvar.inc'
      include 'const.inc'
      include 'dovars.inc'
      include 'fertil.inc'
      include 'forrem.inc'
      include 'param.inc'
      include 'parcp.inc'
      include 'parfs.inc'
      include 'plot1.inc'
      include 'plot2.inc'
      include 'schvar.inc'
      include 'seq.inc'
      include 'timvar.inc'
      include 'zztim.inc'
 
c...Local variables
      integer   crtyr, plntd,trevtptr,tremov
      real      savedfert
      character string*80
      character*5 curcult, curfert, curfire, curgraz, curharv, 
     $            curirri, curomad, curtrm, curflood
      save      plntd
      save      curcult, curfert, curfire, curgraz, curharv, 
     $          curirri, curomad, curtrm, savedfert, curflood

      data plntd / 0 /
      data curcult / ' ' /
      data curfert / ' ' /
      data curfire / ' ' /
      data curgraz / ' ' /
      data curharv / ' ' /
      data curirri / ' ' /
      data curomad / ' ' /
      data curtrm / ' ' /
!...added by zli for wetland flooding
      data curflood / ' ' /
      data savedfert / 0.0 /

c...Check crop dolast to reset crpgrw; done here so that crop grows
c     through the last month of growth
c      crpgrw = 1
      if (dofrst) then
          crpgrw = 1
      endif

      if (dolast) then
        crpgrw = 0
        msplt = 0
        dolast = .false.
      endif

c...Check forest doflst to reset forgrw; done here so that forest grows
c     through the last month of growth
      if (doflst) then
        forgrw = 0
        doflst = .false.
      endif

c...Check if months since planting (msplt) needs to be updated
      if (plntd .eq. 1. and .stemp .ge. rtdtmp) then
        msplt = msplt + 1
      endif

c...Reset do variables to false
      docult = .false.
      doerod = .false.
      dofert = .false.
      doflst = .false.
      dofone = .false.
      dofrst = .false.
      dograz = .false.
      dohrvt = .false.
      doirri = .false.
      dolast = .false.
      doomad = .false.
      doplnt = .false.
      dosene = .false.
      dotrem = .false.
!...Added by zp for wetlands, 03/10/2010
      doflood = .false.
      dofire(CRPSYS) = .false.
      dofire(FORSYS) = .false.
      dofire(SAVSYS) = .false.
      aufert = 0.0
      harmth = 0
c     evntyp = 3

c...Convert time to integer year
      crtyr = aint(time + .001)
      crtyr = mod((crtyr - strtyr + 1), rptyrs)

c...Working with real numbers - inexact so check on small number -rm
      if (crtyr .lt. 0.1) then
        crtyr = rptyrs
      endif

c...Determine if evtptr needs to go back to 1
10     continue

      if (ttlind .ne. 1 .and. evtptr .gt. ttlind) then
        evtptr = 1
      endif


c...Look for events in timary that match the current time
c     If found, handle the event
      if ((timary(evtptr,1) .eq. crtyr) .and.
     $     timary(evtptr,2) .eq. month) then

        if (cmdary(evtptr) .eq. 'CROP') then
          if ((typary(evtptr) .ne. 'F') .and. 
     $        (curcrp .ne. typary(evtptr))) then
                 
                 call cropin(typary(evtptr))
                 call co2eff(time)
          endif

        elseif (cmdary(evtptr) .eq. 'PLTM') then
          doplnt = .true.
          seedl = 1
          plntd = 1
          msplt = 0
          crpgrw = 0
!          print *, "Change to 0", month
!...change crpgrw from =1 to = 0
          falprc = 0
          prcfal = 0

        elseif (cmdary(evtptr) .eq. 'HARV') then
          dohrvt = .true.
          plntd = 0
          falprc = 1
          prcfal = 0
          harmth = 1
          if (curharv .ne. typary(evtptr)) then
            call harvin(typary(evtptr),curharv)
          endif 

        elseif (cmdary(evtptr) .eq. 'FRST') then
          dofrst = .true.
          crpgrw = 1

        elseif (cmdary(evtptr) .eq. 'LAST') then
          dolast = .true.

        elseif (cmdary(evtptr) .eq. 'SENM') then
          dosene = .true.

        elseif (cmdary(evtptr) .eq. 'FERT') then
          dofert = .true.
          aufert = savedfert
          if (curfert .ne. typary(evtptr)) then
            call fertin(typary(evtptr),curfert,savedfert)
          endif

        elseif (cmdary(evtptr) .eq. 'CULT') then
          docult = .true.
          if (curcult .ne. typary(evtptr)) then
            call cultin(typary(evtptr),curcult)
          endif

        elseif (cmdary(evtptr) .eq. 'OMAD') then
          doomad = .true.
          if (curomad .ne. typary(evtptr)) then
            call omadin(typary(evtptr),curomad)
          endif

        elseif (cmdary(evtptr).eq. 'IRRI') then
          doirri = .true.
          if (curirri .ne. typary(evtptr)) then
            call irrgin(typary(evtptr),curirri)
          endif

          elseif (cmdary(evtptr).eq. 'FLOD') then
          doflood = .true.
          if (curflood .ne. typary(evtptr)) then
            call floodin(typary(evtptr),curirri)
          endif

        elseif (cmdary(evtptr) .eq. 'GRAZ') then
          dograz = .true.
          if (curgraz .ne. typary(evtptr)) then
            call grazin(typary(evtptr),curgraz)
          endif

        elseif (cmdary(evtptr).eq. 'EROD') then
          doerod = .true.
          psloss = fltary(evtptr, 1)

        elseif (cmdary(evtptr) .eq. 'FIRE') then

          dofire(cursys) = .true.
          if (curfire .ne. typary(evtptr)) then
            call firein(typary(evtptr),curfire)
          endif

        elseif (cmdary(evtptr) .eq. 'TREE' .and. 
     $          curtre .ne. typary(evtptr)) then
            call treein(typary(evtptr))
            forage = 0
            steadyage = 0
            steady = 0
            trevtptr = evtptr
c            if (tremov .eq. 1) then
c               typary(evtptr) = 'TRFCL'
c               tremov = 0
c            endif
           call co2eff(time)

        elseif (cmdary(evtptr) .eq. 'TREM') then
          dotrem = .true.
              if (remf(1) > 0.95) then 
                 forage = 0
                   steadyage = 0
                   steady = 0
             endif
            tremov = 1
c            typary(trevtptr) = 'TRFCL'
          if (curtrm .ne. typary(evtptr)) then
c            print *, "clear cutting"
            call tremin(typary(evtptr),curtrm)
              if (remf(1) > 0.95) then 
                 prdx(3) = 0.01
                curtre = 'TRFCL'
              endif
          endif

        elseif (cmdary(evtptr) .eq. 'TFST') then
          dofone = .true.
          forgrw = 1

        elseif (cmdary(evtptr) .eq. 'TLST') then
          doflst = .true.

        endif

c...Check the next array 'record'
        evtptr = evtptr + 1
        goto 10
      else
        return
      endif

1000  string = '   Type not found: ' // typary(evtptr)
      call message(string)
      STOP
 
      end
