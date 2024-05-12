      PROGRAM RHFSTK
      
      IMPLICIT NONE
      
      character*14 infile
      infile = 'Kr83.rhi'
      
      CALL RHFSL(infile)
      
      END
      
      
      SUBROUTINE RHFSL(infile)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
C     pPOgPAMMA H-F-S PElqTiBiCTCKOgO
C     (CM. RHFS)
C
C     BXOd:(KAPTy)
C   1)Z pO (D10.3)
C   2)AN-~iClO HuKlOHOB (RN=1.2*AN**(1/3)FM)
C     pO (D10.3)
C   3)ILAT pO (I1) =1 TO pOpPABKA LATTER'A
C     =0 HET pOpPABKi
C   4)C pO (D10.3) KO|fficiEHT pPi ObMEHHOM
C     ~lEHE ALFA B X-ALFA (OT 1 DO 213)
C   5)AK pO (D10.3) pOKAzATElx CAMOCOglACOB.
C   6)EPS-pO (D10.3)-TO~HOCTx CAMOCOglACOBAHiq
C
C   7)N-pO (I2)-~iClO BBOdiMyX ~iCEl zApOlHEHiq
C   8)N-KAPT :NSQ pO (I1,A2,D10.3)
C     N-gl. KBAHTOBOE ~iClO (KulOH),
C     S-CiMBOl iz:S+,P+,P-,D-,D+,F+,F-,G+,G-,
C     H+,H-,I+,I-,J+,J-,K+,K-
C     gdE buKBA ObOzHA~AET OPbiTAlxHyj
C     MOMEHT, A+ ili - pOlHyj MOMEHT
C     J=L+1/2 ECli + i L-1/2 ECli -
C     Q-~iClO zApOlHEHiq dAHHOj pOdObOlO~Ki
C****
C     BBOdiTx HAdO TOlxKO izMEHEHiq / dOpOlHEHiq
C     K CTAHdAPTHyM zApOlHEHHyM ObOlO~KAM
C     iHEPTHyX ATOMOB C ZI.LE.Z
C****
C     ByXOd:
C     U(1020)-MACCiB pOTEHciAlA F-H-S
C     U(X)=-X*V(X) gdE V(X)-pOTEHciAl
C     U(I,J)=U(XIJ), XIJ=HB((2**(J-1)-1)*NT+(NT-1)*2**(J-1))
C     I=1,60 J=1,17
C     E(100)-MACCiB |HEPgij CBqzi (A.U.)
C     EpOlHAq=MC**2-E
C     zApOlHEHiE CM.RHFS
C     Q(100)-zApOlHEHiE pOdObOlO~EK
C     HB-pAPAMETP TAblic U
C     NITER-~iClO iTEPAcij
C****
C
      DIMENSION U(1020),W(1020),DIM(5100),
     *E(100),Q(100),IST(17),LT(17),AJT(17),JT(17),NR(40),
     *ISR(40),QR(40)
	real*4 ft(2)
C
C
C      INTEGER*2 IST,ISR,LJ
	character*2 IST,ISR,LJ,NMEL
	character*14 infile,oufile
	character*24 fldate
	character*3 month(12)
      DATA IST/'S+','P-','P+','D-','D+','F-','F+','G-','G+',
     *		'H-','H+','I-','I+','J-','J+','K-','K+'/
      DATA LT/0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8/
      DATA JT/1,1,3,3,5,5,7,7,9,9,11,11,13,13,15,15,17/
	DATA month /'Jan','Feb','Mar','Apr','May','Jun',
     *            'Jul','Aug','Sep','Oct','Nov','Dec'/
C      DATA NR/1,2,2,37*0/
C      DATA ISR/S+,S+,P-,37*0/
C      DATA QR/2.D0,2.D0,2.D0,37*0.D0/
C      DATA Z,AN,C,AK,EPS/6.D0,1.2D1,0.8D0,0.8D0,1.D-5/
C      DATA ILAT/1/

c	call dtime(ft)
c	call fdate(fldate)

C	CALL GETDAT (iyear,imon,iday)
	WRITE (fldate,1090) ihr0,imin0,isec0
 1090	format(5X,I2,':',I2,':',I2)

      DO 1 I=1,17
    1 AJT(I)=JT(I)*0.5D0
      DO 4 I=1,100
    4 Q(I)=0.D0
C      
      OPEN(1, file=infile)
	read(1,1000) oufile
	read(1,*) c,ak,eps,ilat
	read(1,1010) NMEL
 1010	format(a2)
	read(1,*) z,an
C
c
c Tkalya 19.01.99: C at lines NN 93-122
c
c      IF(Z.LT.2) GO TO 5
c      Q(1)=2.D0
c      IF(Z.LT.10) GO TO 5
c      Q(2)=2.D0
c      Q(3)=2.D0
c      Q(4)=4.D0
c      IF(Z.LT.18) GO TO 5
c      Q(5)=2.D0
c      Q(6)=2.D0
c      Q(7)=4.D0
c      IF(Z.LT.36) GO TO 5
c      Q(10)=2.D0
c      Q(8)=4.D0
c      Q(9)=6.D0
c      Q(11)=2.D0
c      Q(12)=4.D0
c      IF(Z.LT.54) GO TO 5
c      Q(17)=2.D0
c      Q(18)=2.D0
c      Q(19)=4.D0
c      Q(13)=4.D0
c      Q(14)=6.D0
c      IF(Z.LT.86) GO TO 5
c      Q(26)=2.D0
c      Q(27)=2.D0
c      Q(28)=4.D0
c      Q(15)=6.D0
c      Q(16)=8.D0
c      Q(20)=4.D0
c      Q(21)=6.D0
C
    5 CONTINUE
c
	read(1,*) NB
	if (nb.lt.0) then
	do 676 i=1,100
 676  q(i)=0
	nb=-nb
	end if
	do 998 i=1,nb
998	read(1,1020,end=999) nr(i),isr(i),qr(i)
1000	format(A)
1020	FORMAT(I1,A2,D10.3)
 999    K=0
      CLOSE(1)
C
      DO 10 N=1,10
      DO 11 IL=1,N
      L=IL-1
      DO 12 IJ=1,2
      AJ=L-1.5D0+IJ
      IF(AJ)12,12,13
   13 K=K+1
C
      DO 14 I=1,NB
      IF(N.NE.NR(I)) GO TO 14
      DO 15 J=1,17
      IF(ISR(I).EQ.IST(J)) GO TO 16
   15 CONTINUE
      PRINT 17,ISR(I)
   17 FORMAT(2X,'RHFSL: ',A2)
      STOP
   16 IF(L.NE.LT(J)) GO TO 14
      IF(DABS(AJ-AJT(J)).GT.1.D-10) GO TO 14
      Q(K)=QR(I)
   14 CONTINUE
   12 CONTINUE
   11 CONTINUE
   10 CONTINUE
    7 CONTINUE
C*
      NZR=1
      ZZ=0.
      DO 500 K=1,100
      ZZ=ZZ+Q(K)
 500  CONTINUE
      PRINT 21,nmel,fldate
      IF(ILAT) 26,26,27
   26 PRINT 28
   28 FORMAT(10X,'without Latter correction',/10X,40(1H*))
      GO TO 29
   27 PRINT 30
   30 FORMAT(10X,'with Latter correction',/10X,40(1H*))
   29 CONTINUE
   21 FORMAT(1H1/10X,'Program  RHFS for ',a2,2x,'at ',a24)
      PRINT 22,Z,AN,ZZ,C,AK,EPS
   22 FORMAT(10X,'Nucleus charge',46(1H.),F5.1/
     *10X,'Nuclons number (RN=1.2A**1/3fM)',28(1H.),
     *F6.2/10X,'Number of electrons',41(1H.),F5.1,/
     * 10X,'Exchange coefficient',
     *1X,29(1H.),D15.8/10X,
     *'Selfconcistancy coefficient'
     *,' AK',9(1H.),
     *11(1H.),D15.8/10X,'Accuracy',
     *42(1H.),D15.8//)
C
      IF(NB) 23,23,24
   24 PRINT 25,(NR(I),ISR(I),QR(I),I=1,NB)
   25 FORMAT(10X,'Input data:'I1,A2,
     *2X,D15.8)
   23 CONTINUE
      NT=60
      NSMAX=17
      CALL RHFS(Z,AN,Q,NT,NSMAX,ILAT,C,AK,HB,U,W,E,
     *DIM,EPS)
      PRINT 31
   31 FORMAT(1X/1X,30X,'N L J I ',4X,1HQ,4X,
     *1HI,7X,5HE(EV)/30X,40(1H-)/)
      K=0
      DO 32 N=1,10
      DO 32 IL=1,N
      L=IL-1
      DO 32 IJ=1,2
      AJ=L+IJ-1.5D0
      IF(AJ) 32,32,33
   33 K=K+1
      IF(Q(K)) 32,32,34
   34 J=AJ+AJ+0.1D0
      J9=AJ+0.6D0
      J8=J
      IF(L.EQ.J9) J8=J+1
      LJ=IST(J8)
C
      EW=E(K)*27.2107
      PRINT 35,N,LJ,J,Q(K),EW
   35 FORMAT(30X, I1,A1,I1,2H/2,1X,1HI,
     *1X,F7.3,1X,1HI,1X,D20.12)
   32 CONTINUE
      NS=NSMAX
      NL=1
	OPEN(UNIT=NL,FILE=OUFILE,FORM='UNFORMATTED',ERR=1400)
      WRITE (NL,ERR=1500) Z,AN,ZZ,NT,NS,HB,E,Q,U
	print 1420,oufile
c	call etime(ft)

	ft(1)=(ihr1-ihr0)*3600.+(imin1-imin0)*60+(isec1-isec0)

	print 1430,ft(1)
	stop
 1400	PRINT 1410,OUFILE
	STOP
 1500	PRINT 1510,OUFILE
 1410	FORMAT(1X,'Unable to open output file ',a14)
 1420	format(10x,'The result is placed in file ',a14)
 1430	format(10x,'Ellapsed time:',f7.2,' s')
 1510	FORMAT(1X,'Unable to write data in file ',a14)
      RETURN
      END
C
C==================
C
      SUBROUTINE RHFS(Z,AZ,Q,NT,NSMAX,
     *ILAT,C,AK,HB,U,W,E,DIM,EPS)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
C     pPOgPAMMA PElqTiBiCTCKOgO
C     XAPTPi-OOK-Cl|TEP'A
C
C**** BXOd
C     Z-zAPqd qdPA
C     AZ-~iClO HuKlOHOB B qdPE
C     RADNUC=1.2*AZ**(1/3)FM
C     zAPqd PABHOMEPHO PACpPEdElEH pO qdPu
C
C     Q(100)-MACCiB ~iCEl zApOlHEHiq
C     zApOlHEHiE |TOgO MACCiBA:
C     I=0
C     DO 1 N=1,10
C     DO 1 L=0,N-1
C     DO 1 AJ=L-1/2,L+1/2
C     IF(AJ)1,1,2
C   2 I=I+1
C     Q(I)=Q(N,L,AJ)
C   1 CONTINUE
C
C     NT,NSMAX-pAPAMETPy TAblic fuHKcij
C     X(I,J)=HB(NT(2**(J-1)-1)+(I-1)2**(J-1))
C     I =1,NT   J=1,NSMAX
C
C     ILAT=1 TOgdA iCpOlxzuETCq pOpPABKA LATTER'A
C     C-KO|fficiEHT pEPEd ObMEHHyM ~lEHOM
C     C=1:2/3
C
C     AK-KO|fficiEHT CAMOCOglACOBAHiq(0.5,0.9)
C**   AK.GT.0.AND.LT.1-iTEPAcii pO CXEME
C     i.M.bAHd: DAMP+pPATT (CM. RAINE)
C**   AK.LT.0-ALF=-AK CXEMA DAMP
C**   AK.GT.1-ALF=AK-1 CXEMA pPATT
C
C     EPS-OTHOCiTElxHAq TO~HOCTx CAMOCOglACOBAHiq
C     EPS.GE./U(I)-U(I-1)//U(I) B KAvdOj TO~KE
C     U=-X*V(X), gdE V-pOTEHciAl
C
C**** ByXOd
C     HB-{Ag TAblic
C     U-TAblic U-CAMOCOglACOBAHHOgO(NT*NSMAX)
C     W-TAblic PAdiAlxHOj plOTHOCTi(NT*NSMAX)
C     HOPMiPOBKA:INT(DR*W(R))=N-~iClO |lEKTPOHOB
C     E-TAblicA |HEPgij CBqzi(=-EpOlHAq+MC**2)
C     zApOlHEHiE CM.Q
C
C     DIM(NT*NSMAX*5)-PAbO~ij MACCiB
C
C
C
C
C
      DIMENSION Q(100),E(100),U(NT),W(NT),DIM(NT)
C
C
      COMMON /VLATEA/Z1,A78
C
      ISMS=1
      ALF=AK
      IF(AK) 501,502,502
  502 IF(AK.LT.1.D0) GO TO 503
C**** AK.GE.1-PAbOTAET CXEMA pPATTA
      ISMS=2
      ALF=AK-1.D0
      GO TO 503
  501 CONTINUE
C**** AK.LT.0-PAbOTAET bEz pPATTA
      ALF=-AK
      ISMS=3
  503 CONTINUE
C**** AK.LT.1.AND.GT.0-PAbOTAET CXEMA i.M.bAHd
      BTA=1.D0-ALF
      IPRINT=1
      NMXITR=30
      Z1=Z
      A78=AZ
      D3=1.D0/3.D0
      RN=1.2D-13/0.529177D-8*AZ**D3
C**
      BT=C*1.5D0*(3.D0/3.1415 92653 58979 32D0)**D3
      BT= BT/(4.D0*3.1415 92653 58979 32D0)**D3
      EPSD=EPS*0.01D0
C
      NTS=NT*NSMAX
      NTG=1
      NTF=NTG+NTS
      NTE=NTF+NTS
      NTZ=NTE+NTS
      NTR=NTZ+NTS
C
      ZI=0.D0
      DO 1 I=1,100
      E(I)=0.D0
    1 ZI=ZI+Q(I)
      ZI=Z-ZI+ILAT
C***
      RP=75.D0
      HB=2.D0**NSMAX*NT
      HB=RP/HB
C
      X=0.D0
      H=HB
      K=1
      DO 2 J=1,NSMAX
      DO 3 I=1,NT
      U(K)=DMAX1(VLATA(X),ZI)
      X=X+H
    3 K=K+1
    2 H=H+H
      EMIN=10.D10
      K=0
      DO 4 N=1,10
      DO 5 IL=1,N
      L=IL-1
      DO 6 IJ=1,2
      AJ=L-1.5D0+IJ
      IF(AJ)6,6,7
    7 K=K+1
      IF(Q(K))6,6,8
    8 CONTINUE
      CALL ENERBG(N,L,E(K),U,NT,NSMAX,HB,0.05D0)
      EMIN=DMIN1(EMIN,E(K))
    6 CONTINUE
    5 CONTINUE
    4 CONTINUE
C
      AEXPM=40.D0
      P=DSQRT(EMIN*2.D0)
      HB=2.D0**NSMAX*NT
      HB=AEXPM/P/HB
C***
      X=0.D0
      H=HB
      K=1
      L=NTZ
      DO 9 J=1,NSMAX
      DO 10 I=1,NT
      U(K)=VLATA(X)
      IF(X.GT.RN) U(K)=DMAX1(U(K),ZI)
      Y=X/RN
      VL=Z
      IF(Y.LE.1.D0) VL=Z*Y*0.5D0*(3.D0-Y*Y)
      DIM(L)=VL
      L=L+1
      K=K+1
   10 X=X+H
    9 H=H+H
C
C
C
      NITER=0
C
C
C
C
C
  100 NITER=NITER+1
      IF(NITER.LE.NMXITR) GO TO 11
      PRINT 12,NITER
   12 FORMAT(2X,'RHFS: NITER=',I3)
      STOP
   11 CONTINUE
      DO 13 I=1,NTS
   13 W(I)=0.D0
C
C
      KQE=0
C
      DO 14 N=1,10
      DO 15 IL=1,N
      L=IL-1
      DO 16 IJ=1,2
      AJ=L-1.5D0+IJ
      IF(AJ)16,16,17
   17 KQE=KQE+1
      IF(Q(KQE))16,16,18
   18 CONTINUE
      RP=AEXPM/1.6D0/DSQRT(2.D0*E(KQE))/HB
      AW=1.D0
      NST=1
   20 RPT=(AW-1.D0)*NT+(NT-1)*AW
      IF(RPT.GE.RP) GO TO 19
      AW=AW+AW
      NST=NST+1
      GO TO 20
   19 NST=MIN0(NST,NSMAX)
      HN=E(KQE)*(1.D0-(N/(N+1.D0))**2)*0.2D0
     */2.D0
      HS=4.D0*HN
C
C
C*
C*
      CALL ERDREL(E(KQE),N,L,AJ,HN,HS,HB,NT,NST,DIM(NTG),
     *DIM(NTF),U,EP,EPSD)
C
      E(KQE)=EP
      JMX=NT*NST
      DO 21 I=1,JMX
      W(I)=W(I)+Q(KQE)*(DIM(NTG+I-1)**2+DIM(NTF+I-1)**2)
   21 CONTINUE
C
C
   16 CONTINUE
   15 CONTINUE
   14 CONTINUE
C
C     pOlu~il plOTHOCTx
C
      GO TO (301,302),IPRINT
  301 CONTINUE
      K=NTS
      DO 30 J=1,NSMAX
      DO 31 I=1,NT
      IF(W(K).GE.EPSD) GO TO 32
   31 K=K-1
   30 CONTINUE
   32 NSW=NSMAX-J+1
      CALL TBVRSZ(W,NT,NSW,HB,DIM(NTF))
      ZQ=ZI-ILAT-Z
      IGQ=1
      M1=NTF
      M=NTZ
      X=0.D0
      H=HB
      K=1
      DO 34 J=1,NSMAX
      DO 35 I=1,NT
      VL=BT*(W(K)*X)**D3
      VC=-DIM(M1)*X
      IF(J.GT.NSW) VC=ZQ
      VL=VL+VC+DIM(M)
      GO TO (404,405),IGQ
  404 IF(J.LE.2) GO TO 406
      IF(VL.GT.ZI) GO TO 406
      IGQ=2
  405 VL=ZI
  406 CONTINUE
      DIM(M1)=VL
      M1=M1+1
      M=M+1
      K=K+1
      X=X+H
   35 CONTINUE
   34 H=H+H
C
      IF(NITER.GT.1) GO TO 200
      GO TO (200,504,200),ISMS
  504 CONTINUE
      K=1
      DO 201 J=1,NSMAX
      DO 202 I=1,NT
      DIM(NTE+K-1)=U(K)
      DIM(NTR+K-1)=U(K)
      K=K+1
  202 CONTINUE
  201 CONTINUE
  200 CONTINUE
C
C
      MD=MOD(NITER,2)
C
C
      K=1
      L=NTR
      M=NTE
      DMX=0.D0
      M1=NTF
C
      DO 40 J=1,NSMAX
      DO 41 I=1,NT
      VLI=U(K)
      VLF=DIM(M1)
      VL1=VLI
      GO TO (510,513,512),ISMS
  510 IF(MD) 513,513,514
  514 VL=ALF*VLI+BTA*VLF
      DIM(M)=VLF
      DIM(L)=VLI
      GO TO 515
  513 VLMF=DIM(M)
      VLMI=DIM(L)
      DZ=VLF-VLMF-VLI+VLMI
      IF(DZ.EQ.0.D0) GO TO 516
      VTT=(VLMI*VLF-VLI*VLMF)/DZ
      DZ=VLF-VLI
      IF(DZ.EQ.0.D0) GO TO 517
      VT=ALF*VLI+BTA*VLF
      A=(VLF-VTT)/(VLF-VLI)
      IF(A.LT.0.D0) GO TO 517
      IF(A.GT.ALF) GO TO 519
      VL=VTT
      GO TO (515,518),ISMS
  516 VL=ALF*VLI+BTA*VLF
      GO TO (515,518),ISMS
  517 VL=VLF
      GO TO (515,518),ISMS
  519 VL=VT
      GO TO (515,518),ISMS
  518 CONTINUE
      DIM(M)=VLF
      DIM(L)=VLI
      GO TO 515
  512 CONTINUE
      VL=ALF*VLI+BTA*VLF
  515 CONTINUE
C
      D=DABS(VL-VL1)/(DABS(VL)+1.D-15)
      U(K)=VL
      DMX=DMAX1(D,DMX)
      M1=M1+1
      L=L+1
      M=M+1
      K=K+1
   41 CONTINUE
   40 CONTINUE
      D=DMX
      IF(DMX.GT.EPS) GO TO 100
C
      IPRINT=2
      GO TO 100
  302 CONTINUE
      PRINT 791,NITER
  791 FORMAT(10X,10(1H-),I4,'-Iteration',10(1H-))
      RETURN
      END
C
C==================
C
      SUBROUTINE TBVRSZ(G,NT,NS,HB,U)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     By~MClEHiE KulOHOBCKOgO pOTEHciAlA
C     U(R)=INT(DX/(XbOlx{E)*G(X))
C     iHTEgPAl DO X(NT,NS)
C     G(NT,NS)-TAblicA plOTHOCTi
C     HB-{Ag TAblic G(NT,NS)
C     U(NT,NS)-TAblicA pOTEHciAlA
C     B TO~KAX X(I,J) (U(1,1)=U(0))
C
C     iHTEgPiPOBAHiE pO CplAjHAM
C     HA OTPEzKE NT-TO~EK
C
C**** NT.LE.60 ****
C
      DIMENSION G(NT),U(NT),
     *VSP(244)
C
      IF(NT.LE.60) GO TO 1
      PRINT 2,NT
    2 FORMAT(2X,'TABVRS: NT=',I3)
      STOP
    1 N=NT+1
      S1=0.D0
      S2=0.D0
      C6=1.D0/6.D0
      H=HB*0.5D0
      R2=HB
      DR2=1.D0/R2
      R1=HB
      DR1=1.D0/R1
      R2=0.D0
      K=1
      Y2=0.D0
      YD2=0.D0
      U(1)=0.D0
      DO 3 J=1,NS
      IF(J.EQ.1) GO TO 4
      R1=R2
      DR1=DR2
      R2=R2+H
      DR2=1.D0/R2
      Y1=Y2
      YD1=YD2
      K=K+1
      Y2=G(K)
      YD2=VSP(N)
      S1=S1+H*(Y1+Y2+H*(YD1-YD2)*C6)*0.5D0
      S2=S2+H*(Y1*DR1+Y2*DR2+H*(YD1*DR1-
     *YD2*DR2-Y1*DR1**2+Y2*DR2**2)*C6)*0.5D0
      U(K)=S1/R2-S2
    4 H=H+H
      K1=K-1
      IF(J.EQ.NS) N=NT
      CALL SPLEQ(G(K),VSP,N,H,VSP(N+1))
      YD2=VSP(1)
      DO 6 I=2,NT
      K=K+1
      Y1=Y2
      Y2=G(K)
      YD1=YD2
      YD2=VSP(I)
      R1=R2
      DR1=DR2
      R2=R2+H
      DR2=1.D0/R2
      S1=S1+H*(Y1+Y2+H*C6*(YD1-YD2))*0.5D0
      S2=S2+H*(Y1*DR1+Y2*DR2+H*C6*(YD1*DR1-
     *YD2*DR2-Y1*DR1**2+Y2*DR2**2))*0.5D0
      U(K)=S1/R2-S2
    6 CONTINUE
    3 CONTINUE
      DO 8 I=1,K
    8 U(I)=U(I)+S2
      RETURN
      END
C
C==================
C
      SUBROUTINE CVZCL1(E,U,NT,NS,HB,L,S)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      DIMENSION U(NT)
C
C     BCpOMOgATElxHAq dlq ENERBG
C**** U(1,1)=V(0.)
      MX=NT*NS
      M=1
      S=0.D0
      H=HB
      H3=H/3.D0
      AL=L*(L+1.D0)
      X=0.D0
      Y=0.D0
      IK=NT-2
      DX=1.D0
      DO 1 J=1,NS
      SI=Y
      DO 2 I=2,IK,2
      M=M+1
      X=X+H
      DX=1.D0/X
      F=2.D0*(U(M)*DX-E)-AL*DX*DX
      F=DMAX1(F,0.D0)
      F=DSQRT(F)
      SI=SI+4.D0*F
      X=X+H
      DX=1.D0/X
      M=M+1
      F=2.D0*(U(M)*DX-E)-AL*DX*DX
      F=DMAX1(F,0.D0)
      F=DSQRT(F)
      SI=SI+2.D0*F
    2 CONTINUE
      M=M+1
      X=X+H
      DX=1.D0/X
      F=2.D0*(U(M)*DX-E)-AL*DX*DX
      F=DMAX1(0.D0,F)
      SI=SI+4.D0*DSQRT(F)
      X=X+H
      DX=1.D0/X
      M=M+1
      Y=0.D0
      IF(M.GT.MX) GO TO 50
      F=2.D0*(U(M)*DX-E)-AL*DX*DX
      F=DMAX1(0.D0,F)
      Y=DSQRT(F)
   50 S=S+(SI+Y)*H3
      H3=H3+H3
    1 H=H+H
      RETURN
  500 S=S+SI*H3
      RETURN
      END
C
C==================
C
      SUBROUTINE ENERBG(N,L,E,U,NT,NS,HB,EPS)
        IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION U(NT,NS)
      HE=0.1D0
      S=3.141592654D0*(N-DSQRT(L*(L+1.D0)))
      R=1.5D-4
      I=R/HB+1
      I=MIN0(I,NT)
      Z=U(I,1)
      Z=DMAX1(Z,1.D0)
      EMAX=Z*Z/1.5D0/N/N
C     pPEdpOlOvEHiE
    3 CALL CVZCL1(HE,U,NT,NS,HB,L,A)
      A=A-S
      IF(A)1,1,2
    1 HE=HE*0.5D0
      IF(HE.GT.1.D-10) GO TO 3
      PRINT 30
   30 FORMAT(10X,'ENERB:HET YPOBHq')
      STOP
    2 FA=A
      A=HE
    5 CONTINUE
      B=EMAX
      CALL CVZCL1(B,U,NT,NS,HB,L,FB)
      FB=FB-S
      IF(FB)4,32,33
   32 ER=B
      GO TO 20
   33 FA=FB
      A=B
      EMAX=EMAX*2.D0
      GO TO 5
    4 CONTINUE
      IN=1
      ER1=0.D0
   48 CONTINUE
      ER=A-FA*(A-B)/(FA-FB)
      GO TO (43,44),IN
   43 IN=2
      GO TO 45
   44 IF(DABS(ER-ER1)/ER.LE.EPS) GO TO 20
   45 ER1=ER
      CALL CVZCL1(ER,U,NT,NS,HB,L,FR)
      FR=FR-S
      IF(FR*FA.GT.0.D0) GO TO 10
      B=ER
      FB=FR
      GO TO 41
   10 CONTINUE
      A=ER
      FA=FR
   41 CONTINUE
      EX=(A+B)*0.5D0
      CALL CVZCL1(EX,U,NT,NS,HB,L,FX)
      FX=FX-S
      IF(FX*FA.GT.0.D0) GO TO 42
      B=EX
      FB=FX
      GO TO 48
   42 A=EX
      FA=FX
      GO TO 48
   20 E=ER
C***  pPOgPAMMA pOiCKA |HEPgii pO
C***  KBAziKlACCiKE (CM.viPHOB)
      RETURN
      END
C
C==================
C
      SUBROUTINE ERDREL(EN,N,L,AJ,HN,HS,HB,NT,NS,R1,
     *R2,U,E,EPS)
C
          IMPLICIT REAL*8(A-H,O-Z)
C     CMOTPi pPEpPiHT
C     U(I,J)=V(X(I,J))-zAdAH i BCE
C
      COMMON /DIRMOD/ UR(5),KWW,ST,NQ,JQ(11),HW
      DIMENSION R1(NT,NS), R2(NT,NS), U(NT,NS)
      REAL*8 KWW
      IN=1
 1    KWW=L
      KWW=(AJ+.5D0)*(KWW-AJ)*2.D0
      HW=HB
      UR(1)=U(1,1)
      DO 5 J=1,4
      UR(J+1)=0.125D0*(3.D0*U(J,1)+6.D0*U(J+1,1)-U(J+2,1))
 5    CONTINUE
      ST=DSQRT(KWW*KWW-(UR(1)/137.0388D0)**2)
      IF (IN.GE.2) GO TO 16
      EW=EN
      PT=0.D0
      RP=1.D0
      IF (NS.GE.2) RP=2.D0**(NS-1)
      RP=HW*(NT*(RP-1.D0)+(NT-1.D0)*RP)
 6    E=EN
      H=HN
      Q=FREMQD(E,R1,R2,U,NT,NS)
      N1=NQ
      W=FREMQD(E+H,R1,R2,U,NT,NS)
      N2=NQ
      ED=E+H
 7    IF (Q*W.LE.0.D0) GO TO 9
      IF((E-EW)*PT)17,17,18
   17 IF(PT)19,18,14
   18 CONTINUE
      IF (Q/W*1.D+4**(N1-N2).GT.1.D0) GO TO 8
      E=E-H
      W=Q
      N2=N1
      IF (E.LT.0.D0) E=0.D0
      Q=FREMQD(E,R1,R2,U,NT,NS)
      ED=E
      N1=NQ
      GO TO 7
 8    E=E+H
      Q=W
      W=FREMQD(E+H,R1,R2,U,NT,NS)
      N1=N2
      ED=E+H
      N2=NQ
      GO TO 7
 9    IF (Q.EQ.0.D0) GO TO 13
      IF (W.EQ.0.D0) GO TO 12
      A=E
      B=E+H
 10   E=A-Q*(A-B)/(Q-W*1.D-4**(N1-N2)*DEXP(RP*(DSQRT(2.D0*A)-DSQRT(2.D0*
     *B)))
     *)
      QR=FREMQD(E,R1,R2,U,NT,NS)
      NR=NQ
      ED=E
      IF (QR/Q.GT.0.D0) GO TO 11
      IF (QR.EQ.0.D0) GO TO 13
      IF ((B-E)/E.LT.EPS) GO TO 13
      B=E
      W=QR
      N2=NR
      GO TO 10
 11   IF ((E-A)/E.LT.EPS) GO TO 13
      A=E
      Q=QR
      N1=NR
      GO TO 10
 12   E=E+H
 13   CONTINUE
      IF(ED.NE.E) ABCD=FREMQD(E,R1,R2,U,NT,NS)
      K=FREMND(E,R1,R2,U,NT,NS)+0.1D0
      K=K+L-1
      IF (K.EQ.N) GO TO 15
      IF (K.GT.N) GO TO 14
   19 PT=-1.D0
      EW=EN
      EN=EN-HS
      GO TO 6
   14 EW=EN
      PT=1.D0
      EN=EN+HS
      GO TO 6
   15 AB=FREMAD(E,R1,R2,U,NT,NS)
      RETURN
      ENTRY ERDRLE(EN,N,L,AJ,HN,HS,HB,NT,NS,R1,R2,U,E,
     *EPS)
      IN=2
      GO TO 1
   16 AB=FREMQD(EN,R1,R2,U,NT,NS)
      AB=FREMND(EN,R1,R2,U,NT,NS)
      AB=FREMAD(EN,R1,R2,U,NT,NS)
      E=EN
      RETURN
      END
C
C==================
C
      FUNCTION FREMQD(E,R1,R2,U,NT,NS)
       IMPLICIT REAL*8(A-H,O-Z)
      COMMON /DIRMOD/ UR(5),K,ST,NQ,JQ(11),HW
      DIMENSION R1(NT,NS),R2(NT,NS),U(NT)
      DIMENSION F1(4), F2(4)
      REAL*8 K
      DATA  CS/137.0388D0/, CS2/274.0776D0/
 1    R1(1,1)=0.D0
      R2(1,1)=0.D0
      NQ=0
      H=HW
      EC=E/CS
      H2=H*.5D0
      UC=UR(2)/CS
      EH=EC*H2
      CH=CS*H
      A=-K-ST
      B=K-ST
      D=EH-UC
      C=CH-D
      A1=1.D0
      B1=1.D0
      IF(UR(1).EQ.0.D0) GO TO 2
      AK1=H
      AM1=-CH*A/UR(1)
      A=1.D0+A
      B=1.D0+B
      GO TO 4
    2 AK2=(6.D0*UR(2)-3.D0*U(2)+2.D0/3.D0*UR(3))/CS
     */H
      IF(K.GE.0.D0) GO TO 3
      AK1=1.D0
      AM1=(AK2-EC)/(B-1.D0)
      A1=H
      B1=1.D0/H
      C=C*H2
      D=D/H2
      A=1.D0+A
      GO TO 4
    3 AM1=H
      AK1=(AK2-EC+CS2)/(1.D0-A)*H
      A1=1.D0/H
      B1=H
      C=C/H2
      D=D*H2
      B=1.D0+B
    4 AK2=A*AK1+C*AM1
      AM2=B*AM1+D*AK1
      AK3=A*AK2+C*AM2
      AM3=B*AM2+D*AK2
      UC=U(2)/CS
      D=EH+EH-UC
      C=(CH+CH-D)*A1
      D=D*B1
      AK4=A*AK3+C*AM3
      AM4=B*AM3+D*AK3
      AK1=AK1+AK2+AK2+AK3+AK3+AK4
      AM1=(AM1+AM2+AM2+AM3+AM3+AM4)*A1
      HST=H**ST
      IF (HST.LE.1.D-15) GO TO 5
      HST=HST/H
      R1(2,1)=AK1*HST
      R2(2,1)=AM1*HST
      GO TO 6
 5    R1(2,1)=1.D-15
      R2(2,1)=(AM1/AK1)*1.D-15
 6    X=H
      H6=H/6.D0
      A=K/X
      B=U(2)/X/CS
      DC=1.D0/CS
      C=CS2+B-EC
      D=EC-B
      F1(1)=C*R2(2,1)-A*R1(2,1)
      F2(1)=D*R1(2,1)+A*R2(2,1)
      DO 7 I=3,5
      X=X+H
      X1=X-H2
      I1=I-1
      I2=I-2
      A=K/X1
      B=UR(I)/X1/CS
      C=CS2+B-EC
      D=EC-B
      A1=R1(I1,1)+H2*F1(I2)
      B1=R2(I1,1)+H2*F2(I2)
      AK2=C*B1-A*A1
      AM2=D*A1+A*B1
      A1=R1(I1,1)+H2*AK2
      B1=R2(I1,1)+H2*AM2
      AK3=C*B1-A*A1
      AM3=D*A1+A*B1
      DX=1.D0/X
      A=K*DX
      B=U(I)*DC*DX
      C=CS2+B-EC
      D=EC-B
      A1=R1(I1,1)+AK3*H
      B1=R2(I1,1)+AM3*H
      AM4=D*A1+A*B1
      AK4=C*B1-A*A1
      R1(I,1)=R1(I1,1)+H6*(F1(I2)+AK2+AK2+AK3+AK3+AK4)
      R2(I,1)=R2(I1,1)+H6*(F2(I2)+AM2+AM2+AM3+AM3+AM4)
      F1(I1)=C*R2(I,1)-A*R1(I,1)
 7    F2(I1)=D*R1(I,1)+A*R2(I,1)
      C11=0.D0
      P11=0.D0
      C21=0.D0
      P21=0.D0
      A2=F1(1)
      A3=F1(2)
      A4=F1(3)
      A5=F1(4)
      B2=F2(1)
      B3=F2(2)
      B4=F2(3)
      B5=F2(4)
      Y2=R1(4,1)
      Y3=R1(5,1)
      Z2=R2(4,1)
      Z3=R2(5,1)
      H3=H6+H6
      H9=H/96.D0
      C1=928.D0/1037.D0
      C2=109.D0/1037.D0
      NTJ=NT
      IF (NS.EQ.1) NTJ=NT-1
      DO 9 I=5,NTJ
      X=X+H
      A1=A2
      A2=A3
      A3=A4
      A4=A5
      B1=B2
      B2=B3
      B3=B4
      B4=B5
      Y1=Y2
      Y2=Y3
      R1(I,1)=Y3
      Z1=Z2
      Z2=Z3
      R2(I,1)=Z3
      P12=Y1+H3*(8.D0*A4-5.D0*A3+4.D0*A2-A1)
      P22=Z1+H3*(8.D0*B4-5.D0*B3+4.D0*B2-B1)
      AM1=P12-C1*(P11-C11)
      AM2=P22-C1*(P21-C21)
      DX=1.D0/X
      A=K*DX
      B=U(I+1)*DX*DC
      C=CS2+B-EC
      D=EC-B
      FZ1=C*AM2-A*AM1
      FZ2=D*AM1+A*AM2
      C12=1.75D0*Y2-0.75D0*Y1+H9*(39.D0*FZ1+37.D0*A4-59.D0*A3+7.D0*A2)
      C22=1.75D0*Z2-0.75D0*Z1+H9*(39.D0*FZ2+37.D0*B4-59.D0*B3+7.D0*B2)
      C11=C12
      P11=P12
      C21=C22
      P21=P22
      Y3=C11+C2*(P12-C12)
      Z3=C22+C2*(P22-C22)
      A5=C*Z3-A*Y3
      B5=D*Y3+A*Z3
    9 CONTINUE
      IF(DABS(Y3).LT.1.D+30) GO TO 109
      NQ=NQ+7
      JQ(NQ)=1
      DO 8 M=1,NTJ
      R1(M,1)=R1(M,1)*1.D-28
    8 R2(M,1)=R2(M,1)*1.D-28
      A1=A1*1.D-28
      A2=A2*1.D-28
      A3=A3*1.D-28
      A4=A4*1.D-28
      A5=A5*1.D-28
      B1=B1*1.D-28
      B2=B2*1.D-28
      B3=B3*1.D-28
      B4=B4*1.D-28
      B5=B5*1.D-28
      Z1=Z1*1.D-28
      Z2=Z2*1.D-28
      Z3=Z3*1.D-28
      Y1=Y1*1.D-28
      Y2=Y2*1.D-28
      Y3=Y3*1.D-28
      C11=C11*1.D-28
      P11=P11*1.D-28
      C21=C21*1.D-28
      P21=P21*1.D-28
  109 CONTINUE
      IF (NS.EQ.1) GO TO 13
      N5=NT-5
      DO 12 J=2,NS
      H3=H3+H3
      H9=H9+H9
      J1=J-1
      XY=X-6.D0*H
      A=K/XY
      IJ=NT*(J1-1)+NT-5
      B=U(IJ)/XY*DC
      IJ=NT*J1+2
      C=CS2+B-EC
      D=EC-B
      A2=C*R2(N5,J1)-A*R1(N5,J1)
      B2=D*R1(N5,J1)+A*R2(N5,J1)
      A4=A3
      B4=B3
      A3=A1
      B3=B1
      Y2=Y1
      Z2=Z1
      H=H+H
      JK=NT
      IF (J.GE.NS) JK=NT-1
      DO 11 I=1,JK
      X=X+H
      A1=A2
      A2=A3
      A3=A4
      A4=A5
      B1=B2
      B2=B3
      B3=B4
      B4=B5
      Y1=Y2
      Y2=Y3
      R1(I,J)=Y3
      Z1=Z2
      Z2=Z3
      R2(I,J)=Z3
      P12=Y1+H3*(8.D0*A4-5.D0*A3+4.D0*A2-A1)
      P22=Z1+H3*(8.D0*B4-5.D0*B3+4.D0*B2-B1)
      AM1=P12-C1*(P11-C11)
      AM2=P22-C1*(P21-C21)
      A=K/X
      B=U(IJ)/X*DC
      IJ=IJ+1
      C=CS2+B-EC
      D=EC-B
      FZ1=C*AM2-A*AM1
      FZ2=D*AM1+A*AM2
      C12=1.75D0*Y2-0.75D0*Y1+H9*(39.D0*FZ1+37.D0*A4-59.D0*A3+7.D0*A2)
      C22=1.75D0*Z2-0.75D0*Z1+H9*(39.D0*FZ2+37.D0*B4-59.D0*B3+7.D0*B2)
      Y3=C12+C2*(P12-C12)
      Z3=C22+C2*(P22-C22)
      P11=P12
      P21=P22
      C11=C12
      C21=C22
      A5=C*Z3-A*Y3
      B5=D*Y3+A*Z3
   11 CONTINUE
      IF(DABS(Y3).LT.1.D+30) GO TO 111
      NQ=NQ+7
      JQ(NQ)=J
      DO 10 M=1,JK
      R1(M,J)=R1(M,J)*1.D-28
   10 R2(M,J)=R2(M,J)*1.D-28
      A1=A1*1.D-28
      A2=A2*1.D-28
      A3=A3*1.D-28
      A4=A4*1.D-28
      A5=A5*1.D-28
      B1=B1*1.D-28
      B2=B2*1.D-28
      B3=B3*1.D-28
      B4=B4*1.D-28
      B5=B5*1.D-28
      Z1=Z1*1.D-28
      Z2=Z2*1.D-28
      Z3=Z3*1.D-28
      Y1=Y1*1.D-28
      Y2=Y2*1.D-28
      Y3=Y3*1.D-28
      C11=C11*1.D-28
      P11=P11*1.D-28
      C21=C21*1.D-28
      P21=P21*1.D-28
C****
  111 CONTINUE
 12   CONTINUE
 13   R1(NT,NS)=Y3
      R2(NT,NS)=Z3
      FREMQD=Y3
      RETURN
      ENTRY FREMAD(E,R1,R2,U,NT,NS)
      IN=JQ(1)
      JN=JQ(2)
      IQ=0
      A=DABS(R1(IN,JN))
      IN=IN+1
      IF (NT.GE.IN) GO TO 14
      IN=1
      JN=JN+1
 14   DO 17 J=JN,NS
      IM=1
      IF (JN.GE.J) IM=IN
      DO 17 I=IM,NT
      IF (IQ.GE.1) GO TO 16
      B=DABS(R1(I,J))
      IF (0.D0.LT.B-A) GO TO 15
      A=B
      GO TO 17
 15   IQ=1
 16   R1(I,J)=0.D0
      R2(I,J)=0.D0
 17   CONTINUE
      S=0.D0
      SI=0.D0
      H=HW
      DO 20 J=1,NS
      DO 18 I=2,NT,2
 18   S=S+4.D0*(R1(I,J)**2+R2(I,J)**2)
      NK=NT-1
      DO 19 I=3,NK,2
 19   S=S+2.D0*(R1(I,J)**2+R2(I,J)**2)
      YW=0.D0
      IF (J.NE.NS) YW=R1(1,J+1)**2+R2(1,J+1)**2
      SI=SI+H/3.D0*(S+R1(1,J)**2+R2(1,J)**2+YW)
      S=0.D0
 20   H=H+H
      S=1.D0/DSQRT(SI)
      DO 21 J=1,NS
      DO 21 I=1,NT
      R1(I,J)=R1(I,J)*S
 21   R2(I,J)=R2(I,J)*S
      FREMAD=S
      RETURN
      ENTRY FREMND(E,R1,R2,U,NT,NS)
      IF (NQ.EQ.0) GO TO 24
      IF (JQ(NQ).EQ.1) GO TO 24
      JN=1
      NQ7=NQ/7
      DO 23 N1=1,NQ7
      J1=JQ(N1)
      IF(J1.EQ.JN) GO TO 23
      JK=J1-1
      JKL=(N1-1)*7
      A=1.D-4**(NQ-JKL)
      DO 22 J=JN,JK
      DO 22 I=1,NT
      R1(I,J)=R1(I,J)*A
 22   R2(I,J)=R2(I,J)*A
      JN=JK+1
 23   CONTINUE
 24   M=1
      A=0.D0
      PQ=1.D0
      S=0.D0
      D=-1.D0
      DO 251 J=1,NS
      DO 252 I=1,NT
      B=R1(I,J)
      PW=B-A
      IF(PW*PQ) 253,252,25
 253  C=DABS(B)
      IF(C.LT.S*.1D0) GO TO 26
      S=C
      JQ(1)=I
      JQ(2)=J
      IF(B*D.GE.0.D0) GO TO 25
      D=-D
      PQ=-PQ
      M=M+1
 25   A=B
 252  CONTINUE
 251  CONTINUE
   26 FREMND=M
      RETURN
C
      END
C
C==================
C
      FUNCTION VLAT(X)
        IMPLICIT REAL*8(A-H,O-Z)
      COMMON /VLATTE/Z
C     pOTEHciAl LATTER'A
      A=X*(Z**(1.D0/3.D0))/0.8853D0
      V1=1.D0/(1.D0+1.243D0*A+0.2302D0*A*A+
     *0.00694D0*A*A*A+(0.02747D0-0.1486D0*A+
     *0.007298D0*A*A)*DSQRT(A))
      V2=Z* V1+(3.D0/(4.D0*3.14159D0))*
     *DSQRT(2.D0*Z*V1*X)
      V3=1.D0
      VLAT=DMAX1(V3,V2)
      RETURN
      END
      FUNCTION VLATA(X)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      COMMON /VLATTE/Z1
      COMMON /VLATEA/Z,A
C
      DATA RN/2.344045D-5/
      Z1=Z
      R=RN*A**0.3333333333D0
      IF(R.LE.X) GO TO 1
      VLATA=Z*0.5D0*(3.D0-(X/R)**2)*X/R
      RETURN
    1 VLATA=VLAT(X)
      RETURN
      END
C
C==================
C
      SUBROUTINE SPLEQ(Y,YD1,N,H,VSP)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      DIMENSION Y(N),YD1(N),VSP(N)
C     pPOgPAMMA By~iClEHiq pPOizBOdHyX
C     YD1(I) (ECli N.GT.4)
C     Y(I)=Y(X0+H*I)
C     PABHOMEPHAq CETKA
C
C     VSP(N,3)-PAbO~ij MACCiB
C
      IF(N-4)1,1,3
    1 CONTINUE
      RETURN
    3 VSP(1)=0.D0
      I1=N+1
      I2=N+I1
      VSP(I1)=H
      VSP(I2)=H+H
      YD1(1)=Y(3)*0.5D0-Y(1)*2.5D0+Y(2)*2.D0
      NM1=N-1
      NM2=N-2
      VSP(N)=-H-H
      N2=N+N
      N3=N2+N
      VSP(N2)=-H
      H4=H*4.D0
      VSP(N3)=0.D0
      YD1(N)=Y(NM2)*0.5D0-Y(N)*2.5D0+Y(NM1)*2.D0
      DO 6 I=2,NM1
      IM1=I-1
      IP1=I+1
      VSP(I)=H
      VSP(I+N)=H4
      VSP(I+N2)=H
      YD1(I)=3.D0*(Y(IP1)-Y(IM1))
    6 CONTINUE
C
      CALL TRMTPR(VSP,VSP(I1),VSP(I2),YD1,N)
      RETURN
      END
C
C==================
C
      SUBROUTINE TRMTPR(A,B,C,F,N)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      DIMENSION A(N),B(N),C(N),F(N)
C
C     PE{EHiE uPABHEHiq C TPEXdiAgOHAlxHOj
C     MATPicEj METOdOM pPOgOHKi
C     A(I)*X(I-1)+B(I)*X(I)+C(I)*X(I+1)=F(I), (X=F)
C
C     A i B pOPTqTCq
C     OTBET: X B F
C
      S=1.D0/B(N)
      A(N)=-S*A(N)
C***  A=Y B=Z
      B(N)=F(N)*S
      N2=N-2
      DO 1 K=1,N2
      I=N-K
      I1=I+1
      S=1.D0/(B(I)+C(I)*A(I1))
      A(I)=-S*A(I)
    1 B(I)=S*(F(I)-C(I)*B(I1))
      S=B(1)+C(1)*A(2)
      F(1)=(F(1)-C(1)*B(2))/S
      DO 2 K=2,N
      I=K-1
      F(K)=A(K)*F(I)+B(K)
    2 CONTINUE
      RETURN
      END
      
