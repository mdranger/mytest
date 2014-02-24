
c               Copyright 1993 Colorado State University
c                       All Rights Reserved

C...TREEIN.F

      subroutine treein(tomatch)

      character tomatch*5

c...Read in the new forest type
 
      include 'chrvar.inc'
      include 'const.inc'
      include 'isovar.inc'
      include 'param.inc'
      include 'parcp.inc'
      include 'parfs.inc'
      include 'site.inc'
      include 'prof.inc'

c...Local variables
      integer   i, j, k
      real      del13c, temp, chkfrc
      character fromdat*5, name*20, string*80


      character*100 fixfile

      fixfile = ' '

      fixfile(1:pathlen) = libpath
      fixfile(pathlen+1:pathlen+9) = '\tree.100'

c      write(*,*) fixfile

      open(unit=11,file=fixfile,status='OLD')

c      open(unit=11, file='tree.100',status='OLD')
      rewind(11)
10    read(11, 20, end=170) fromdat
c      write(*,*) fromdat
c      write(*,*) tomatch
20    format(a5)

c set the crop drain variable
      cropdrain = 0
      read(11, *) temp, name
      decid = int(temp)
      call ckdata('treein','decid',name)
c     read in the new data for forest age
      read(11, *) agefact_k, name
c      print *, "agefact_k", agefact_k, name      
      read(11, *) agefact_a, name      
      read(11, *) agefact_b, name
      read(11, *) agefact_c, name
      read(11, *) agefact_d, name
                                  
      read(11, *) rtprof, name
        temp = log(rtprof)
                                                                                                                        
        rootdep = log(0.05)/temp
        survdep = log(0.01)/temp
                                                                                                                        
c        print *, decid, rtprof, fromdat
 

      do 30 i = 2, 3
        read(11, *) prdx(i), name
        call ckdata('treein','prdx',name)
30    continue

c......assume standard deviation is 20% of the prdx(3)
c.......and a normal distribution      
c      prdx(3) = max(0.5*prdx(3),anorm(prdx(3),0.2*prdx(3)))

      do 40 i = 1, 4
        read(11, *) ppdf(i,2), name
c...... print *, 'ppdf', i, ppdf(i,2)
        call ckdata('treein','ppdf',name)
40    continue
c        ppdf(1,2) = opt_temp



      do 70 i = IMIN, IVAL
        do 60 j = 1, FPARTS
          do 50 k = 1, MAXIEL
            read(11, *) cerfor(i, j, k), name
            call ckdata('treein','cerfor',name)
50        continue
60      continue
70    continue

      read(11, *) decw1, name
      call ckdata('treein','decw1',name)
      read(11, *) decw2, name
      call ckdata('treein','decw2',name)
      read(11, *) decw3, name
      call ckdata('treein','decw3',name)

      do 90 j = NEWFOR, OLDFOR
c...Added check to make sure fractions add to 1
	chkfrc = 0.0
        do 80 i = 1, FPARTS
          read(11, *) fcfrac(i, j), name
	  chkfrc = chkfrc + fcfrac(i, j)
          call ckdata('treein','fcfrac',name)
80	continue

        if (abs(1.0 - chkfrc) .gt. 0.000001) then
	  call message('Total of all FCFRAC != 1')
          STOP
        endif
90    continue

      do 100 i = 1, MONTHS
        read(11, *) leafdr(i), name
        call ckdata('treein','leafdr',name)
100   continue

      read(11,*) btolai, name
      call ckdata('treein','btolai',name)
      read(11, *) klai, name
      call ckdata('treein','klai',name)
      read(11, *) laitop, name
      call ckdata('treein','laitop',name)
      read(11, *) maxlai, name
      call ckdata('treein','maxlai',name)
      read(11, *) maxldr, name
      call ckdata('treein','maxldr',name)

      do 120 i = 1, MAXIEL
        read(11, *) forrtf(i), name
        call ckdata('treein','forrtf',name)
120   continue

      read(11, *) sapk, name
      call ckdata('treein','sapk',name)
      read(11, *) swold, name
      call ckdata('treein','swold',name)

      do 130 i = 1, FPARTS
        read(11, *) wdlig(i), name
        call ckdata('treein','wdlig',name)
130   continue

      do 140 i = 1, FPARTS
        read(11, *) wooddr(i), name
        call ckdata('treein','wooddr',name)
140   continue

      read(11, *) snfxmx(FORSYS), name
      call ckdata('treein','snfxmx',name)
      read(11, *) del13c, name
      call ckdata('treein','del13c',name)

      read(11, *) co2ipr(FORSYS), name
      call ckdata('cropin','co2ipr',name)
      read(11, *) co2itr(FORSYS), name
      call ckdata('cropin','co2itr',name)

      do 160 i = IMIN, IMAX
        do 150 j = 1, MAXIEL
          read(11, *) co2ice(FORSYS,i,j), name
          call ckdata('cropin','co2ice',name)
150     continue
160   continue

      read(11, *) co2irs(FORSYS), name
      call ckdata('cropin','co2irs',name)

      read(11, *) basfc2, name
      call ckdata('treein','basfc2',name)
      read(11, *) basfct, name
      call ckdata('treein','basfct',name)
      read(11, *) sitpot, name
      call ckdata('treein','sitpot',name)

      if (tomatch .ne. fromdat) then
        goto 10
      endif
 
c...Close the file
      close(11)

c...Hold on to the previous tree for age counting
      pretre = curtre

c...Hold on to the current tree just read in
      curtre = tomatch

c...Calculate cisotf as 13C if 13C labeling
      if (labtyp .eq. 2) then
        cisotf = del13c * PEEDEE * 1.0e-03 + PEEDEE
        cisotf = 1 / (1/(0.0001+cisotf) + 1)
      endif

      return

170   call message('   Error reading in values from the tree.100 file.')
      string = '   Looking for tree type: ' // tomatch
      call message(string)
      STOP

      end
