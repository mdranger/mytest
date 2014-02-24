c               Copyright 1993 Colorado State University
c                       All Rights Reserved


      subroutine cropin(tomatch)

      character*5 tomatch

c...Read in the new crop type

      include 'chrvar.inc'
      include 'comput.inc'
      include 'const.inc'
      include 'isovar.inc'
      include 'ligvar.inc'
      include 'param.inc'
      include 'parcp.inc'
      include 'parfs.inc'
      include 'parfx.inc'
      include 'plot1.inc'
      include 'seq.inc'
	include 'zztim.inc'
      include 'prof.inc'

c...Local variables
      integer   i, j, layer
      real      del13c, temp
      character fromdat*5, name*6, string*80
      character*100 fixfile
                                                                                
      fixfile = ' '
                                                                                
      fixfile(1:pathlen) = libpath
	fixfile(pathlen+1:pathlen+1) = '\'
      fixfile(pathlen+2:pathlen+9) = 'crop.100'
                                                                                
c*      write(*,*) fixfile
                                                                                
      open(unit=11,file=fixfile,status='OLD')

c..      open(unit=11, file='crop.100',status='OLD')
      rewind(11)
 10   read(11, 20, end=220) fromdat
 20   format(a5)

c    add drain factor in the crop.100 file to cal the 
c      read(11, *)cropdrain, name
c      print *, "read crop", cropdrain, name
      read(11, *) scale, name
      read(11, *) grcurv, name
c      print *, fromdat, scale
!      FOR ROOT prof
      read(11, *) rtprof, name

	temp = -0.00
	rtprof = rtprof+temp

	temp = log(rtprof)

	rootdep = log(0.05)/temp
	survdep = log(0.02)/temp

      read(11, *) prdx(1), name
      call ckdata('cropin','prdx',name)
	temp = 1.0
	prdx(1) = prdx(1)*temp

      do 30 i = 1, 4
        read(11, *) ppdf(i,1), name
        call ckdata('cropin','ppdf',name)
30    continue

      read(11, *) temp, name
      bioflg = int(temp)
      call ckdata('cropin','bioflg',name)
      read(11, *) biok5, name
      call ckdata('cropin','biok5',name)
      read(11, *) pltmrf, name
      call ckdata('cropin','pltmrf',name)
      read(11, *) fulcan, name
      call ckdata('cropin','fulcan',name)

      do 40 i = 1, 3
        read(11, *) frtc(i), name
        call ckdata('cropin','frtc',name)
40    continue
      if (frtc(3) .eq. 0) frtc(3) = 1

      read(11, *) biomax, name
      call ckdata('cropin','biomax',name)

      do 60 i = 1, 2
        do 50 j = 1, MAXIEL
          read(11, *) pramn(j,i), name
          call ckdata('cropin','pramn',name)
50      continue
60    continue

      do 80 i = 1, 2
        do 70 j = 1, MAXIEL
          read(11, *) pramx(j,i), name
          call ckdata('cropin','pramx',name)
70      continue
80    continue

      do 100 i = 1, 2
        do 90 j = 1, MAXIEL
          read(11, *) prbmn(j,i), name
          call ckdata('cropin','prbmn',name)
90      continue
100   continue

      do 120 i = 1, 2
        do 110 j = 1, MAXIEL
          read(11, *) prbmx(j,i), name
          call ckdata('cropin','prbmx',name)
110     continue
120   continue

      do 140 i = ABOVE, BELOW
        do 130 j = INTCPT, SLOPE
          read(11, *) fligni(j,i), name
          call ckdata('cropin','fligni',name)
130     continue
140   continue

      read(11, *) himax, name
      call ckdata('cropin','himax',name)
      read(11, *) hiwsf, name
      call ckdata('cropin','hiwsf',name)
      read(11, *) temp, name
      himon(1) = int(temp)
      call ckdata('cropin','himon',name)
      read(11, *)temp, name
      himon(2) = int(temp)
      call ckdata('cropin','himon',name)

      do 150 i = 1, MAXIEL
        read(11, *) efrgrn(i), name
        call ckdata('cropin','efrgrn',name)
150   continue

      read(11, *) vlossp, name
      call ckdata('cropin','vlossp',name)

      do 160 i = 1, 4
        read(11, *) fsdeth(i), name
        call ckdata('cropin','fsdeth',name)
160   continue

      read(11, *) fallrt, name
      call ckdata('cropin','fallrt',name)
      read(11, *) rdr, name
      call ckdata('cropin','rdr',name)
      read(11, *) rtdtmp, name
      call ckdata('cropin','rtdtmp',name)

      do 170 i = 1, MAXIEL
        read(11, *) crprtf(i), name
        call ckdata('cropin','crprtf',name)
170   continue

      read(11, *) snfxmx(CRPSYS), name
      call ckdata('cropin','snfxmx',name)
      read(11, *) del13c, name
      call ckdata('cropin','del13c',name)
      read(11, *) co2ipr(CRPSYS), name
      call ckdata('cropin','co2ipr',name)
      read(11, *) co2itr(CRPSYS), name
      call ckdata('cropin','co2itr',name)

      do 190 i = IMIN, IMAX
        do 180 j = 1, MAXIEL
          read(11, *) co2ice(CRPSYS,i,j), name
          call ckdata('cropin','co2ice',name)
180     continue
190   continue

      read(11, *) co2irs(CRPSYS), name
      call ckdata('cropin','co2irs',name)

      if (tomatch .ne. fromdat) then
        goto 10
      endif

c...Close the file
      close(11)

c...Hold on to the current crop just read in
      curcrp = tomatch

c...Determine the 'numerical value' of the curcrp, 
c     for use as an output variable
      crpval = 0
      do 200 i = 1, 5
        if (curcrp(i:i) .ne. ' ') then
	  if (curcrp(i:i) .ge. '0' .and. curcrp(i:i) .le. '9') then
            crpval = crpval +
     -               ((ichar(curcrp(i:i)) - ichar('0')) / 10.0)
          else
            crpval = crpval + (ichar(curcrp(i:i)) - ichar('A')) + 1
          endif
        endif
200   continue

c...Calculate available water holding capacity 11/91 lh
      awhc = 0.0
      do 210 layer = 1, nlaypg
        awhc = awhc + (afiel(layer) - awilt(layer)) * adep(layer)
210   continue

c...Recalculate lignin
      call cmplig(cursys,fligni,wdlig,pltlig)

c...Calculate cisofr as 13C if 13C labeling
      if (labtyp .eq. 2) then
        cisofr = del13c * PEEDEE * 1.0e-03 + PEEDEE
        cisofr = 1 / (1/cisofr + 1)
      endif

      return

220   call message('   Error reading in values from the crop.100 file.')
      string = '   Looking for crop type: ' // tomatch
      call message(string)
      STOP

      end
