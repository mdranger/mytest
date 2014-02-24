
!		Copyright 1993 Colorado State University
!			All Rights Reserved
!


      subroutine detiv(inFlag, outFlag, jfd_case, ensem_case,
     $           outputflag)
      logical inFlag, outFlag
      integer jfd_case, ensem_case, outputflag

c...Determine name of schedule file, which contains the
c     name of the site file, values of timing variables,
c     and order of events
 
      include 'chrvar.inc'
      include 'param.inc'
      include 'seq.inc'
      include 't0par.inc'
      include 'timvar.inc'
      include 'parfs.inc'

c.....      common /libpath/filepath
      character*100 filepath
      character*50 sitnam

c...Local variables
      integer clen, getlen, i, iargc, nargs
      
      character*100 extflag, newbin, oldbin, schnam
      character*100 status_in, status_out
      logical ext, goahead
      character*100 iname
      character*10  age, out_f
      character*10  temp_str


c...Initialize weather labels
      wlabel(1) = 'prec'
      wlabel(2) = 'tmin'
      wlabel(3) = 'tmax'

c...Initialize potential command line arguments
      ext = .false.
      schnam = ' '
      newbin = ' '
      oldbin = ' '
      filepath = ' '
c debug_flag set to display some information as default      
!      debug_flag = 2
      npp_input = 0
c...VAX NOTE: Need to get information interactively from user

c...Get command line arguments
      nargs = iargc()
      if (nargs .eq. 0) then
        call message(' ')
        call message('          EDCM SOIL ORGANIC MATTER MODEL')
        call message('             DEVELOPED FROM CENTURY 4.0')
        call message('                Add check point function')
        call message('                      10/01/05')
        call message(' ')
        call message('   Invalid command line arguments were supplied.')
        call message('   To run EDCM, please supply these arguments')
        call message('   as needed:')
        call message(' -l   directory to search for library files')
        call message(' -n   name of new binary output file (no .bin)')
        call message(' -s   name of schedule file (no .sch)')
        call message('Following commands are optional for execution:')
        call message(' -e   name of old binary file to extend')
        call message('      from, if extending (no .bin)')
c...to specify the age of the forest added by Liu 2001
        call message(' -a   the age of the forest in years')
c...to change GPP dynamically according to ecoregions
        call message(' -g   max net forest prod: prdx(3)'// 
     1               ' in the tree.100')
c...to save and load status file with out side files
        call message(' -endfile   status_end.bin'// 
     1               ' -infile status_start.bin')
        call message('      -d debug flag ') 
        call message(' 0 - least information and middle files, fastest')
        call message(' 1 - show some information with status files')
        call message(' 2 - show all the information and status files')
        call message(' -p   different way to calculate NPP') 
        call message('   0 - use prdx in tree.100')
        call message('   1 - use commandline input -g and do inversion')
        call message('   2 - use the prdxin.100 ')
        call message('   3 - use commandline -g but not do inversion ')
        call message('   4 - use the previous value read in '// 
     1               ' from the status_start.bin')
        call message('   5 - use ECLUE to calculate the GPP ')
        call message('   Example:')
        call message('   edcm -l /lib -e oldata -s schnam -a 100'//
     1               '-g 430 -t 35 -n newoutput -d 0 -p 0')
        call message(' ')
        STOP 'Execution error.'
      endif

c...Process command line arguments
      i = 1
c....sstatus files
             inFlag = .FALSE.      
10    if (i .lt. nargs) then
        call getarg(i, extflag)
        i = i + 1
        call getarg(i, iname)
        i = i + 1
        clen = getlen(iname)


        if (extflag .eq. '-a') then
          age = iname(1:clen)
          read (age, '(I10)'),forage
!         print *, 'forage:  ', forage
          if (forage < 0) then
            call message(' ')
            call message('   Forest age is negative.')
            call message(' ')
            STOP 'Execution error with negative forest age.'
          endif
        else if (extflag .eq. '-endfile') then
          status_out = iname(1:clen)
          if (status_out .ne. "NONE") then
             outFlag = .TRUE.
          else
             outFlag = .FALSE.
          endif
c

        else if (extflag .eq. '-infile') then
          status_in = iname(1:clen)
          print *, status_in
          inquire(file=status_in,exist=goahead)
          if (.not. goahead .and. status_in .ne. "NONE") then
            call message(' ')
            call message('   The old status file could not be read.')
            call message(' ')
            STOP 'Execution error.'
          endif
          if (status_in .ne. "NONE") then
             inFlag = .TRUE.
          else
             inFlag = .FALSE.
          endif
        else if (extflag .eq. '-jfd') then
          temp_str = iname(1:clen)
          read (temp_str, '(I10)'),jfd_case
c         print *, 'JFD_case:  ', jfd_case

        else if (extflag .eq. '-ensem') then
          temp_str = iname(1:clen)
          read (temp_str, '(I10)'),ensem_case

        else if (extflag .eq. '-g') then
          temp_str = iname(1:clen)
          read (temp_str, '(I10)'),gpp_eco
!          print *, 'GPP:  ', gpp_eco

c          prdx_inv = gpp_eco
c          prdx_crop_inv = gpp_eco
c...New prdx of tree and crop read in from command line
c...for inversion
           prdxtree = gpp_eco
           prdxcrop = gpp_eco
           fgrowthlen = 12
           growthlen = 12
           rtsh = 0

          
          if (gpp_eco < 0) then
            call message(' ')
            call message('   Forest GPP is negative.')
            call message(' ')
            STOP 'Execution error.'
          endif

        else if (extflag .eq. '-t') then
          temp_str = iname(1:clen)
          read (temp_str, '(I10)'),opt_temp
        else if (extflag .eq. '-s') then
          schnam = iname(1:clen)
          if (index(schnam,'.sch').eq.0) schnam(clen+1:clen+4) = '.sch'
          inquire(file=schnam,exist=goahead)
          if (.not. goahead) then
            call message(' ')
            call message('   The schedule file could not be read.')
            call message(' ')
            STOP 'Execution error.'
          endif
c
        else if (extflag .eq. '-n') then
            outputflag = 1
          newbin = iname(1:clen)
          if (index(newbin,'.bin').eq.0) newbin(clen+1:clen+4) = '.bin'
          inquire(file=newbin,exist=goahead)
          if (goahead) then
            call message(' ')
            call message('   The new binary file already exists.')
            call message(' ')
            STOP 'Execution error.'
          endif
c
        else if (extflag .eq. '-e') then
!...decide the output format
!1 - EDCM will output a new binary file according to the name of input (default)
!2 - EDCM will append the output to an existing file using jfdcase and ensem_case
!    with the original inputs
! All the other values, use default 1 instead
            outputflag = 2
          ext = .true.
          oldbin = iname
          if (index(oldbin,'.bin').eq.0) oldbin(clen+1:clen+4) = '.bin'
          inquire(file=oldbin,exist=goahead)
          if (.not. goahead) then
            call message(' ')
            call message('   The old binary file could not be read.')
            call message(' ')
            STOP 'Execution error.'
          endif
c
        else if (extflag .eq. '-l') then
          libpath = iname(1:clen)
          pathlen = getlen(libpath)
c          write (*,*) libpath
c          write (*,*) pathlen
        else if (extflag .eq. '-d') then
c         set the debug flag value  
c         debug_flag is defined in param.inc
          temp_str = iname(1:clen)
          read (temp_str, '(I10)'),debug_flag
        else if (extflag .eq. '-p') then
!..         set the flag for different source of prdx  
!..       0 - use prdx in tree.100 no inversion
!..       1 - use commandline input -g and do inversion
!..       2 - use the prdxin.100 
!..       3 - use commandline -g but not do inversion
!..       4 - use the previous value read in from status_start.bin
!         5 - use ECLUE to calculate the GPP, this is done in cycle function
          temp_str = iname(1:clen)
!...only using MODIS NPP then inversion is going on          
         read (temp_str, '(I10)'),npp_input
          if ( npp_input .eq. 1 ) then
              flaginv = 1         
          else
              flaginv = 0
          endif
        else
          call message('   Unknown argument skipped.')
        endif
        goto 10
      endif
      
c...Check that minimal information was entered
      if (schnam .eq. ' ') then
        call message(' ')
        call message('   No schedule file name was given.')
        call message(' ')
        STOP 'Execution error with zero inputs.'
      endif

      if (newbin .eq. ' ') then
        if (ext) then
          newbin = oldbin
        else
          call message(' ')
          call message('   No binary output file name was given.')
          call message(' ')
          STOP 'Execution error.'
        endif
      endif

c...Open binary file to write to
      if (ext .and. newbin .eq. oldbin) then
        open(unit=1,file=newbin,form='UNFORMATTED',status='OLD')
      else
        open(unit=1,file=newbin,form='UNFORMATTED',status='NEW')
      endif
      
c...Open the status file for reading the end status of a previous run
      if (inFlag) then
      open(unit=19,file=status_in,form='UNFORMATTED',status='OLD')
      endif

c...Open the status file for recording the end status

        inquire(file=status_out,exist=goahead)
        if (goahead) then
          out_f = "OLD"
        else
          out_f = "NEW"
        endif

      if (outFlag) then
      if (jfd_case .eq. 1 .and. ensem_case .eq. 1) then
      open(unit=51,file=status_out,form='UNFORMATTED',status=out_f)
      else
      open(unit=51,file=status_out,access='APPEND',
     $  form='UNFORMATTED',status=out_f)
      endif
      endif
      


c...Open the schedule file and read the header lines
      open(unit=15,file=schnam,status='OLD')

      read(15,*) strtyr

      read(15,*) tend
      tend = tend + 1

      read(15,*) sitnam

      read(15,*) labtyp
      read(15,*) labyr

      read(15,*) mctemp
      micosm = 0
      if (mctemp .ge. 0) then
        micosm = 1
      endif

      read(15,*) co2sys
      if (co2sys .gt. 0) then
        read(15,*) co2tm(1), co2tm(2)
      endif

      read(15,*) decsys
      if (decsys .eq. 3) then
        decsys = 2
      endif
      read(15,40) initcp
40    format(a5)
      if (initcp .eq. 'Initi') then
        initcp = ' '
      endif
      read(15,40) initre
      if (initre .eq. 'Initi') then
        initre = ' '
      endif
      curtre = initre
      pretre = initre

      read(15,*)
      read(15,*)

c...Read starting values from fixed parameter file
      call fixin

c...Add this part of code to change the <site>.100
c...file to the 
      filepath = ' '
      filepath(1:pathlen) = libpath
      filepath(pathlen+1:pathlen+1) = '\\'
      filepath(pathlen+2:pathlen+len(sitnam)) = sitnam
  
      print *, filepath

c...Read starting values from site-specific file
c     open(unit=7,file=sitnam,status='OLD',err=1000)
      open(unit=7,file=filepath,status='OLD',err=1000)
      call sitein(ext)

c...Moved the read calls for the initial tree and crop to inside the extend
c     if statement.  This is done to prevent a rather subtle bug that occurs
c     when the initial crop/tree do not match the final values in the 
c     original schedule.  In that case, the derived output values, (crpval ...)
c     do not match the current crop values.
c   The crop/tree reads must occur before the calciv call on a normal run.
c    7/20/95  K. Killian

c...Determine initial values
      if (ext) then
        if (oldbin .ne. newbin) then
          open(unit=3,file=oldbin,form='UNFORMATTED',status='OLD')
          call extend(3,.TRUE.)
          close(unit=3)
        else
          call extend(1,.FALSE.)
        endif
c...Obtain the crop or forest system values (execute a CROP/TREE event)
        if (initcp .ne. ' ') call cropin(initcp)
        if (initre .ne. ' ') call treein(initre)
      else
c...Obtain the initial values for the crop or forest system
        if (initcp .ne. ' ') call cropin(initcp)
        if (initre .ne. ' ') call treein(initre)
        call calciv
      endif

c.
c...Read soil profile data from prof.100 file
      print *, "Call prof"
      call profin
      
c...Sum up isotopes
      call sumcar
      
c...Do preliminary initializations and calculations
      call prelim
      
c...Read the first block of events
      call readblk
      
c.      call message(' ')
      call message('   EDCM Model is running...')

      return
1000    call message(' Fatal error: unknown site file :'//sitnam)
        stop ' Abnormal Termination'
      end

      integer function getlen(name)
      character*(*) name
      integer j
C
C -----------------------------------------------------------------------------
C     this subroutine left justifies the file name and determines the length
C
C Variables
C      Input
C   name    character (*)  the input and processed file name
C
C  Modified by K. Killian 8/11/94
C              included the left justification on a subroutine coded by Laura
C
C -----------------------------------------------------------------------------
 
15    getlen = index(name,' ')-1

      if (getlen .eq. -1) then
        getlen = len(name)
      else if (getlen .eq. 0) then
        do 20 j= 1,len(name)
          if (name(j:j) .ne. ' ') then
            name = name(j:)
            goto 15
          endif
20      continue
        getlen = 0
      endif
 
      return
      end

