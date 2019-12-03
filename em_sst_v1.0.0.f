c em_sst_v0_117.f bo 112.f bo 103.f bo 091.f bo 088.f 
c
c make an EM product
c V0.4.0:
c
c Source products: (See ../ORG/AVAILABILITY)
c   1: MGDSST (NEAR-GOOS)	
c   2: OSTIA-GHRSST          
c   3: JAXA_AMSR-E
c   4: RSS_AMSR-E		
c   5: RSS_MW		
c   6: NOAA_OISST		
c   7: NOAA_OISST +AMSR		
c   8: JAXA WSAT                
c   9: RSS WSAT                 
c  10: RSS TMI (V7.1)		
c  11: Global Tohoku Univ. SST	
c  12: AMSR2 (JAXA)		
c  13: AMSR2 (RSS)		
c  14: GMI (RSS)                
c  15: GMI (RSS)                
c  16: OSTIA-RA (UKMO)          
c  17: CMC   (CMC)              
c  18: GMSSA (BOM)	        
c
c USAGE:
c  em_sst_v0.0.4  ISY IEY
c
c     ISY: start year (e.g. 1988)
c     IEY: end   year (e.g. 2013)
c
c----------------------------------------------------------------------- 
c MEMO: VARIABLES 
c  NP: dimension size for source products
c  IDEM(NP): source product ID defined by the EM PROJECT
c  EM: ensemble median
c  NVD: number of valid data
c  EMS: source products (bit_flag)
c  EMN: source products selected as median (bit_flag)
c  AMIN: minum
c  AMAX: maximum
c
c MEMO: Internal Variables
c  ISW(NP)   : switch of source products
c  IASW(NP)  : swithc of source products (actual,each grid)
c-----------------------------------------------------------------------  
      implicit none
      integer iargc,nargc
      external iargc
      character*4 argv
      integer iy,iyear,isy,iey,im,id
      integer i,j,ix,jy
      integer icx,jcy
      integer n,NP
      parameter(NP=18)
      integer ISW(NP),IASW(NP)
      real spv
      parameter(ix=1440,jy=720,spv=-9999.0)
      parameter(icx=720,jcy=360)
     
      real data(ix,jy,NP)
      real em(ix,jy)
      integer nvd(ix,jy)
      real ave(ix,jy),std(ix,jy)
      real amin(ix,jy),amax(ix,jy)
      real dmin(ix,jy),dmax(ix,jy)
      integer emn(ix,jy),ems(ix,jy)

      integer maxday(12)
      integer IDEM(0:NP)

      character year*4,path*2
      character cemn*4
      character*100 fno1,fno2,fno3,fno4,fno5,fno6,fno7,fno8
      data maxday/31,28,31,30,31,30,31,31,30,31,30,31/              

      path=".."

c get argv
      nargc=iargc()
      do n=0,nargc
       call getarg(n,argv)
       if (n.eq.1) read(argv,'(i4)') isy
       if (n.eq.2) read(argv,'(i4)') iey
      enddo

c open data availability 
      OPEN(10,file="../ORG/AVAILABILITY/data_availability_V0.4.0.csv")
      read(10,*)

      do 10 iy=isy,iey
        write(6,*) iy
        write(year,'(i4)')iy
c----------
c OUTPUT
c----------
c EM
        fno1=path//'/DATA/DAY/V0.4.0/BIN/J-OFURO_EM_SST_V0.4.0_DAILY_'
     &           //year//'.bin'
        open(81,file=fno1,form="unformatted")

c NUM
        fno2=path//'/DATA/DAY/V0.4.0/BIN/J-OFURO_EM_NUM_V0.4.0_DAILY_'
     &           //year//'.bin'
        open(82,file=fno2,form="unformatted")

c AVE
        fno4=path//'/DATA/DAY/V0.4.0/BIN/J-OFURO_EM_AVE_V0.4.0_DAILY_'
     &           //year//'.bin'
        open(84,file=fno4,form="unformatted")
c STD
        fno5=path//'/DATA/DAY/V0.4.0/BIN/J-OFURO_EM_STD_V0.4.0_DAILY_'
     &           //year//'.bin'
        open(85,file=fno5,form="unformatted")
c MIN
        fno6=path//'/DATA/DAY/V0.4.0/BIN/J-OFURO_EM_MIN_V0.4.0_DAILY_'
     &           //year//'.bin'
        open(86,file=fno6,form="unformatted")

c MAX
        fno7=path//'/DATA/DAY/V0.4.0/BIN/J-OFURO_EM_MAX_V0.4.0_DAILY_'
     &           //year//'.bin'
        open(87,file=fno7,form="unformatted")

c EMS: List of IDs of source products
        fno8=path//'/DATA/DAY/V0.4.0/BIN/J-OFURO_EM_EMS_V0.4.0_DAILY_'
     &           //year//'.bin'
        open(88,file=fno8,form="unformatted")

c EMN: List of IDs of source products selected as median
        fno3=path//'/DATA/DAY/V0.4.0/BIN/J-OFURO_EM_EMN_V0.4.0_DAILY_'
     &           //year//'.bin'
        open(83,file=fno3,form="unformatted")

c----------------------------------------------------------------------- 
c Input Switch: (0:NOT USE,  1:USE)
        ISW(1)=1     ! MGDSST JMA
        ISW(2)=0     ! OSTIA-NRT UKMO
        ISW(3)=0     ! AMSR-E JAXA
        ISW(4)=0     ! AMSR-E RSS
        ISW(5)=0     ! MW RSS
        ISW(6)=1     ! OISST NOAA
        ISW(7)=0     ! OISST+AMSR NOAA
        ISW(8)=0     ! WSAT JAXA
        ISW(9)=0     ! WSAT RSS
        ISW(10)=0    ! TMI RSS
        ISW(11)=0    ! GLOBAL_TOHOKU TOHOKU_UNIV
        ISW(12)=0    ! AMSR2 JAXA
        ISW(13)=0    ! AMSR2 RSS
        ISW(14)=0    ! GMI JAXA
        ISW(15)=0    ! GMI RSS
        ISW(16)=1    ! OSTIA-RA UKMO
        ISW(17)=0    ! CMC CMC
        ISW(18)=0    ! GMSSA BOM

c Input Switch from CSV
 111    read(10,*) iyear,(ISW(n),n=1,NP)
        if (iyear.lt.iy) then
         goto 111
        else if (iyear.gt.iy) then
         write(6,*) "ERROR:"
         goto 888
        endif

c Open Input files
        DO n=1,NP
         if (ISW(n).eq.1) then
          call open_file(n,year)
         endif
        ENDDO

        if (mod(iy,4).eq.0) then
         maxday(2)=29
        else
         maxday(2)=28
        endif

        do 20 im=1,12
         do 30 id=1,maxday(im)
           write(6,*) iy,im,id

           call check_end(ISW,iy,im,id)
           call read_data(ISW,data)           
           call EM_CALC(data,em,nvd,ave,std,amin,amax,emn,ems)

c-------------------------------------------------- 
c OUTPUT
           do j=1,jy
            write(81) (em(i,j),i=1,ix)
            write(82) (real(nvd(i,j)),i=1,ix)
            write(84) (ave(i,j),i=1,ix)
            write(85) (std(i,j),i=1,ix)
            write(86) (amin(i,j),i=1,ix)
            write(87) (amax(i,j),i=1,ix)
            write(83) (emn(i,j),i=1,ix)
            write(88) (ems(i,j),i=1,ix)
           enddo

c CHK
           write(6,*) "-----------------------"
           write(6,*) "EM=",em(icx,jcy)
           write(6,*) "AVE and STD", ave(icx,jcy), std(icx,jcy)
           write(6,*) "-----------------------"

c-------------------------------------------------- 

 30      continue
 20     continue
 10   continue
 888  write(6,*)"EOP"

      stop
      end

c-------------------------------------------------- 
c Calculate Ensemble Median
      subroutine EM_CALC(data,em,nvd,ave,std,amin,amax,emn,ems)
      implicit none
      integer i,j,ix,jy,icx,jcy
      integer n,NP,NPM
      integer NN,n_min,n_max,n1,n2
      integer ib_flag
      parameter(NP=18,NPM=18)
      integer IASW(NP)
      real spv,dmin,dmax
      parameter(ix=1440,jy=720,spv=-9999.0)
      real data(ix,jy,NP)
      real em(ix,jy)
      integer nvd(ix,jy)
      real sorted(ix,jy,NP),org(ix,jy,NP)
      integer nsorted(ix,jy,NP)
      real sum(ix,jy),ave(ix,jy),std(ix,jy)
      integer num(ix,jy)
      real amin(ix,jy),amax(ix,jy)
      integer emn(ix,jy),ems(ix,jy)
      

      do i=1,ix
       do j=1,jy
        do n=1,NPM
         sorted(i,j,n)=-888.
         org(i,j,n)=data(i,j,n)
        enddo
        sum(i,j)=0.0
        num(i,j)=0
        nvd(i,j)=0
       enddo
      enddo


      icx=720
      jcy=360

      write(6,*)"ORG"
      do n=1,NPM
       write(6,*) n,org(icx,jcy,n)
      enddo


      do i=1,ix
       do j=1,jy

c MIN and MAX
          dmin=888.
          n_min=0
          do n=1,NPM
            if(org(i,j,n).ne.spv.and.org(i,j,n).lt.dmin) then
             dmin=org(i,j,n)
             n_min=n
            endif
          enddo
c          pause

          dmax=-888
          do n=1,NPM
            if(org(i,j,n).ne.spv.and.org(i,j,n).gt.dmax) then
             dmax=org(i,j,n)
             n_max=n
            endif
          enddo

          if (dmax.ne.-888) then
           amax(i,j)=dmax
          else
           amax(i,j)=spv
          endif

          if (dmin.ne.888) then
           amin(i,j)=dmin
          else
           amin(i,j)=spv
          endif

          if (i.eq.icx.and.j.eq.jcy)then
           write(6,*)"AMIN=",amin(i,j)
           write(6,*)"AMAX=",amax(i,j)
          endif

c Sorting data
         DO NN=1,NPM
          dmax=-888
          n_max=0

          do n=1,NPM
           if (org(i,j,n).ne.spv.and.org(i,j,n).ge.dmax) then
            sorted(i,j,NN)=org(i,j,n)
            dmax=org(i,j,n)
            n_max=n
            nsorted(i,j,NN)=n_max
           endif
          enddo

          if (i.eq.icx.and.j.eq.jcy)then
           write(6,*)NN,nsorted(i,j,NN),sorted(i,j,NN)
          endif
          if (n_max.ne.0) org(i,j,n_max)=spv
         ENDDO

c Count number of valid data (NVD)
         do n=1,NPM
          if (sorted(i,j,n).ne.-888..and.sorted(i,j,n).ne.spv) then   !NOTE patched on 2017.09.04
           nvd(i,j)=nvd(i,j) + 1
          endif
         enddo

c Init for bit_flag for EMN
         do n=1,NPM
          IASW(n)=0
         enddo
 
c Median
         IF(nvd(i,j).ne.0) then
          if (mod(nvd(i,j),2).eq.0)then
           n1=nvd(i,j)/2
           n2=n1+1
           em(i,j)=(sorted(i,j,n1) + sorted(i,j,n2) ) /2.0
           IASW(nsorted(i,j,n1))=1
           IASW(nsorted(i,j,n2))=1
          else
           n1=nint(nvd(i,j)/2.)
           n2=0
           em(i,j)=sorted(i,j,n1)
           IASW(nsorted(i,j,n1))=1
          endif
         ELSE
          em(i,j)=spv
         ENDIF

c Make bit_flag for EMN
         call make_bit_flag(IASW,ib_flag)
         emn(i,j)=ib_flag

c Init for bit_flag for EMS
         do n=1,NPM
          IASW(n)=0
         enddo

c Average and STD
         do n=1,NPM
          if (data(i,j,n).ne.spv) then 
           sum(i,j)=sum(i,j) + data(i,j,n)
           std(i,j)=std(i,j) + data(i,j,n)**2
           num(i,j)=num(i,j) + 1
           IASW(n)=1
          endif
         enddo

         if (num(i,j).gt.0)then
          ave(i,j) = sum(i,j) / real(num(i,j))
          std(i,j) = std(i,j)/real(num(i,j)) - ave(i,j)**2
          std(i,j) = sqrt(std(i,j))
          call make_bit_flag(IASW,ib_flag)
          ems(i,j)=ib_flag
         else
          std(i,j) = spv
          ave(i,j) = spv
         endif

       enddo
      enddo

      write(6,*) "NVD=",NVD(icx,jcy)
      write(6,*) "EM=",em(icx,jcy)
      write(6,*) "EMN(32bit)=",emn(icx,jcy)
      write(6,'(4x,"(",b32.32,")")') emn(icx,jcy)
      write(6,*) "EMS(32bit)=",ems(icx,jcy)
      write(6,'(4x,"(",b32.32,")")') ems(icx,jcy)
          
      RETURN
      END

c-------------------------------------------------- 
c open_file
      SUBROUTINE open_file(n,year)
      implicit none
      integer n
      character year*4,path*2
      character*150 fni

        IF(n.eq.1)THEN
c MGDSST (NEAR-GOOS)
        fni="../ORG/ORG4/MGDSST_NEAR-GOOS/DATA/BIN"
     &         //"/MGDSST_NEAR-GOOS_SST_"
     &         //year//".bin"
        ELSE IF(n.eq.2)THEN
c OSTIA-GHRSST
        fni="../ORG/ORG5/OSTIA_GHRSST/V1.1/BIN"
     &         //"/OSTIA_GHRSST_SST_025D_V1.1_"
     &         //year//".bin"
        ELSE IF(n.eq.3)THEN
c RSS AMSR-E (JAXA)
        fni="../ORG/ORG7/JAXA_AMSR-E/DATA/SST/BIN"
     &         //"/AMSR-E_JAXA_GHRSST_SST_"
     &         //year//".bin"
        ELSE IF(n.eq.4)THEN
c RSS AMSR-E (RSS)
        fni="../ORG/ORG2/AMSR-E/DATA/SST/BIN/AMSR-E_SST_"
     &         //year//"_RSSV7.bin"
        ELSE IF(n.eq.5)THEN
c RSS MW
        fni="../ORG/ORG2/MW/DATA/V1.2/SST/BIN/MW_SST_"
     &         //"V1.2_RSS_V05.0_"//year//".bin"
        ELSE IF(n.eq.6)THEN
c NOAA OISST AVHRR
        fni="../ORG/ORG3/NOAA_OISST/DATA/V1.1/AVHRR/BIN"
     &         //"/NOAA_OISST_AVHRR_SST_V1.1_"
     &         //year//".bin"
        ELSE IF(n.eq.7)THEN
c NOAA OISST AVHRR+AMSR
        fni="../ORG/ORG3/NOAA_OISST/DATA/V1.1/AVHRR_AMSR/BIN"
     &         //"/NOAA_OISST_AVHRR_AMSR_SST_V1.1_"
     &         //year//".bin"
        ELSE IF(n.eq.8)THEN
c JAXA WINDSAT
        fni="../ORG/ORG8/JAXA_WSAT/DATA/V8/SST/BIN"
     &         //"/WINDSAT_JAXA_GHRSST_SST_"
     &         //year//".bin"
        ELSE IF(n.eq.9)THEN
cc RSS WINDSAT
        fni="../ORG/ORG2/WINDSAT/DATA/SST/BIN"
     &         //"/WINDSAT_SST_"
     &         //year//"_RSSV7.bin"
        ELSE IF(n.eq.10)THEN
c RSS TMI
        fni="../ORG/ORG2/TMI/DATA/V7.1/SST/BIN"
     &         //"/TMI_SST_"
     &         //year//"_RSSV7.1.bin"
        ELSE IF(n.eq.11)THEN
c TOHOKU UNIV. GLOBAL SST
        fni="../ORG/ORG6/TOHOKU_UNIV/DATA/V7.0.3/BIN"
     &         //"/TOHOKU_UNIV_GLOBAL_OISST_SST_V7.0.1_"
     &         //year//".bin"
        ELSE IF(n.eq.12)THEN
c JAXA AMSR2
        fni="../ORG/ORG7/JAXA_AMSR2/DATA/V3.0/SST/BIN"
     &           //"/AMSR2_JAXA_GHRSST_SST_"
     &           //year//".bin"
        ELSE IF(n.eq.13)THEN
c RSS AMSR2
        fni="../ORG/ORG2/AMSR2/DATA/V08/SST/BIN"
     &           //"/AMSR2_SST_RSS_V08_"
     &           //year//".bin"
        ELSE IF(n.eq.14)THEN
c JAXA GMI
        fni="../ORG/ORG8/JAXA_GMI/DATA/V3/SST/BIN"
     &           //"/GMI_JAXA_GHRSST_SST_"
     &           //year//".bin"
        ELSE IF(n.eq.15)THEN
c RSS GMI
        fni="../ORG/ORG2/GMI/DATA/V8.2/SST/BIN"
     &           //"/GMI_SST_RSS_V8.2_"
     &           //year//".bin"
        ELSE IF(n.eq.16)THEN
c OSTIA-RA
        fni="../ORG/ORG5/OSTIA_RA/V1.1/BIN"
     &           //"/OSTIA_RA_SST_025D_V1.1_"
     &           //year//".bin"
        ELSE IF(n.eq.17)THEN
c CMC
        fni="../ORG/ORG9/CMC/DATA/025D/V1.1/BIN"
     &           //"/CMC_GHRSST_SST_025D_V1.1_"
     &           //year//".bin"
        ELSE IF(n.eq.18)THEN
c GMSSA
        fni="../ORG/ORG10/GMSSA/DATA/V1.1/BIN"
     &           //"/GMSSA_GHRSST_SST_V1.1_DAILY_"
     &           //year//".bin"
        ENDIF

        open(50+n,file=fni,form="unformatted")
        write(6,*)fni

        RETURN
        END

c-------------------------------------------------- 
c check_end
      SUBROUTINE check_end(ISW,iy,im,id)
      implicit none
      integer n,NP
      parameter(NP=18)
      integer ISW(NP)
      integer iy,im,id

c AMSR-E 
      if (iy.eq.2011.and.im.eq.10.and.id.ge.4)then
       ISW(3)=0
       ISW(4)=0
       ISW(7)=0 
      else if(iy.eq.2011.and.im.ge.11)then
       ISW(3)=0
       ISW(4)=0
       ISW(7)=0 
      endif


      return
      end

c-------------------------------------------------- 
c read_data
      SUBROUTINE read_data(ISW,data)
      implicit none
      integer i,j,ix,jy
      real spv
      parameter(ix=1440,jy=720,spv=-9999.0)
      integer n,NP
      parameter(NP=18)
      integer ISW(NP)
      real data(ix,jy,NP)

      do n=1,NP
          IF(ISW(n).eq.1) then
           do j=1,jy
            read(50+n) (data(i,j,n),i=1,ix)
           enddo
           write(6,*)"READ data: ID:",n
          ELSE
           do i=1,ix
            do j=1,jy
             data(i,j,n)=spv
            enddo
           enddo
          ENDIF          
      enddo

      RETURN
      END

c-------------------------------------------------- 
c make bit_flag (32bit integer flag) 
c  
       SUBROUTINE make_bit_flag(IASW, ib_flag)
       implicit none
       integer NP
       parameter(NP=18)
       integer IASW(NP)
       integer n,bit(32)
       integer ib_flag
c----------------------------------------------------------------------- 
      data bit/1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,
     &         32768,65536,131072,262144,524288,1048576,2097152,4194304,
     &         8388608,16777216,33554432,67108864,134217728,268435456,
     &         536870912,1073741824,2147483648/

c check
c       do n=1,32
c        write(6,'(b32.32)') bit(n)
c       enddo

c make bit_flag
       ib_flag=0
       do n=1,NP
        if(IASW(n).eq.1) then
         ib_flag = ib_flag + bit(n)
        endif
       enddo

       RETURN
       END


     
