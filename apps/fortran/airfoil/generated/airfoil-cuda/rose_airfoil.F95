PROGRAM AIRFOIL
USE GENERATED_MODULE
USE OP2_FORTRAN_DECLARATIONS
USE OP2_CONSTANTS
!USE AIRFOIL_SEQ
USE ISO_C_BINDING
IMPLICIT NONE
intrinsic sqrt, real
INTEGER(kind=4) :: iter,k,i
INTEGER(kind=4), PARAMETER :: maxnode = 9900
INTEGER(kind=4), PARAMETER :: maxcell = 9702 + 1
INTEGER(kind=4), PARAMETER :: maxedge = 19502
INTEGER(kind=4), PARAMETER :: iterationNumber = 1000
INTEGER(kind=4) :: nnode,ncell,nbedge,nedge,niter,qdim
REAL(kind=8) :: ncellr
  ! integer references (valid inside the OP2 library) for op_set
TYPE ( op_set )  :: nodes,edges,bedges,cells
  ! integer references (valid inside the OP2 library) for pointers between data sets
TYPE ( op_map )  :: pedge,pecell,pcell,pbedge,pbecell
  ! integer reference (valid inside the OP2 library) for op_data
TYPE ( op_dat )  :: p_bound,p_x,p_q,p_qold,p_adt,p_res
  ! arrays used in data
INTEGER(kind=4), DIMENSION(:), ALLOCATABLE, TARGET :: ecell,bound,edge,bedge,becell,cell
REAL(kind=8), DIMENSION(:), ALLOCATABLE, TARGET :: x,q,qold,adt,res
REAL(kind=8) :: rms
character(len=10,kind=c_char) :: savesolnName = 'save_soln' // C_NULL_CHAR
character(len=9,kind=c_char) :: adtcalcName = 'adt_calc' // C_NULL_CHAR
character(len=9,kind=c_char) :: rescalcName = 'res_calc' // C_NULL_CHAR
character(len=10,kind=c_char) :: brescalcName = 'bres_calc' // C_NULL_CHAR
character(len=7,kind=c_char) :: updateName = 'update' // C_NULL_CHAR
character(len=6,kind=c_char) :: nodesName = 'nodes' // C_NULL_CHAR
character(len=6,kind=c_char) :: edgesName = 'edges' // C_NULL_CHAR
character(len=7,kind=c_char) :: bedgesName = 'bedges' // C_NULL_CHAR
character(len=6,kind=c_char) :: cellsName = 'cells' // C_NULL_CHAR
character(len=6,kind=c_char) :: pedgeName = 'pedge' // C_NULL_CHAR
character(len=7,kind=c_char) :: pecellName = 'pecell' // C_NULL_CHAR
character(len=6,kind=c_char) :: pcellName = 'pcell' // C_NULL_CHAR
character(len=7,kind=c_char) :: pbedgeName = 'pbedge' // C_NULL_CHAR
character(len=8,kind=c_char) :: pbecellName = 'pbecell' // C_NULL_CHAR
character(len=6,kind=c_char) :: boundName = 'bound' // C_NULL_CHAR
character(len=2,kind=c_char) :: xName = 'x' // C_NULL_CHAR
character(len=2,kind=c_char) :: qName = 'q' // C_NULL_CHAR
character(len=5,kind=c_char) :: qoldName = 'qold' // C_NULL_CHAR
character(len=4,kind=c_char) :: adtName = 'adt' // C_NULL_CHAR
character(len=4,kind=c_char) :: resName = 'res' // C_NULL_CHAR
character(len=4,kind=c_char) :: gamName = 'gam' // C_NULL_CHAR
character(len=4,kind=c_char) :: gm1Name = 'gm1' // C_NULL_CHAR
character(len=4,kind=c_char) :: cflName = 'cfl' // C_NULL_CHAR
character(len=4,kind=c_char) :: epsName = 'eps' // C_NULL_CHAR
character(len=5,kind=c_char) :: machName = 'mach' // C_NULL_CHAR
character(len=6,kind=c_char) :: alphaName = 'alpha' // C_NULL_CHAR
character(len=5,kind=c_char) :: qinfName = 'qinf' // C_NULL_CHAR
INTEGER(kind=4) :: debugiter,retDebug
REAL(kind=8) :: datad
  ! read set sizes from input file (input is subdivided in two routines as we cannot allocate arrays in subroutines in
  ! fortran 90)
PRINT *, "Getting set sizes"
CALL getSetSizes(nnode,ncell,nedge,nbedge)
PRINT *, ncell
  ! allocate sets (cannot allocate in subroutine in F90)
allocate( cell(4 * ncell) )
allocate( edge(2 * nedge) )
allocate( ecell(2 * nedge) )
allocate( bedge(2 * nbedge) )
allocate( becell(nbedge) )
allocate( bound(nbedge) )
allocate( x(2 * nnode) )
allocate( q(4 * ncell) )
allocate( qold(4 * ncell) )
allocate( res(4 * ncell) )
allocate( adt(ncell) )
PRINT *, "Getting data"
CALL getSetInfo(nnode,ncell,nedge,nbedge,cell,edge,ecell,bedge,becell,bound,x,q,qold,res,adt)
PRINT *, "Initialising constants"
CALL initialise_flow_field(ncell,q,res)

DO iter = 1, 4 * ncell
res(iter) = 0.0
END DO

  ! OP initialisation
PRINT *, "Initialising OP2"
CALL op_init(0)
  ! declare sets, pointers, datasets and global constants (for now, no new partition info)
PRINT *, "Declaring OP2 sets"
CALL op_decl_set(nnode,nodes,nodesName)
CALL op_decl_set(nedge,edges,edgesName)
CALL op_decl_set(nbedge,bedges,bedgesName)
CALL op_decl_set(ncell,cells,cellsName)
PRINT *, "Declaring OP2 maps"
CALL op_decl_map(edges,nodes,2,edge,pedge,pedgeName)
CALL op_decl_map(edges,cells,2,ecell,pecell,pecellName)
CALL op_decl_map(bedges,nodes,2,bedge,pbedge,pbedgeName)
CALL op_decl_map(bedges,cells,1,becell,pbecell,pecellName)
CALL op_decl_map(cells,nodes,4,cell,pcell,pcellName)
PRINT *, "Declaring OP2 data"
CALL op_decl_dat(bedges,1,bound,p_bound,boundName)
CALL op_decl_dat(nodes,2,x,p_x,xName)
CALL op_decl_dat(cells,4,q,p_q,qName)
CALL op_decl_dat(cells,4,qold,p_qold,qoldName)
CALL op_decl_dat(cells,1,adt,p_adt,adtName)
CALL op_decl_dat(cells,4,res,p_res,resName)
PRINT *, "Declaring OP2 constants"
CALL op_decl_const(gam,1,gamName)
CALL op_decl_const(gm1,1,gm1Name)
CALL op_decl_const(cfl,1,cflName)
CALL op_decl_const(eps,1,epsName)
CALL op_decl_const(mach,1,machName)
CALL op_decl_const(alpha,1,alphaName)
CALL op_decl_const(qinf,4,qinfName)
qdim = 4
    ! main time-marching loop

call initOP2Constants(alpha,cfl,eps,gam,gm1,mach,qinf)

DO niter = 1, iterationNumber
CALL save_soln_subroutine(cells,qdim,p_q,p_qold)
!     call op_par_loop_2 ( save_soln, cells, &
!                        & op_arg_dat (p_q,    -1, OP_ID, OP_READ), &
!                        & op_arg_dat (p_qold, -1, OP_ID, OP_WRITE))
    ! predictor/corrector update loop

DO k = 1, 2
      ! calculate area/timstep
CALL adt_calc_host("adt_calc" // CHAR(0),cells,op_arg_dat(p_x,1,pcell,OP_READ),op_arg_dat(p_x,2,pcell,OP_READ),op_arg_d&
&at(p_x,3,pcell,OP_READ),op_arg_dat(p_x,4,pcell,OP_READ),op_arg_dat(p_q,-1,OP_ID,OP_READ),op_arg_dat(p_adt,-1,OP_ID,OP_&
&WRITE))
      ! calculate flux residual
CALL res_calc_host("res_calc" // CHAR(0),edges,op_arg_dat(p_x,1,pedge,OP_READ),op_arg_dat(p_x,2,pedge,OP_READ),op_arg_d&
&at(p_q,1,pecell,OP_READ),op_arg_dat(p_q,2,pecell,OP_READ),op_arg_dat(p_adt,1,pecell,OP_READ),op_arg_dat(p_adt,2,pecell&
&,OP_READ),op_arg_dat(p_res,1,pecell,OP_INC),op_arg_dat(p_res,2,pecell,OP_INC))
CALL bres_calc_host("bres_calc" // CHAR(0),bedges,op_arg_dat(p_x,1,pbedge,OP_READ),op_arg_dat(p_x,2,pbedge,OP_READ),op_&
&arg_dat(p_q,1,pbecell,OP_READ),op_arg_dat(p_adt,1,pbecell,OP_READ),op_arg_dat(p_res,1,pbecell,OP_INC),op_arg_dat(p_bou&
&nd,-1,OP_ID,OP_READ))
      ! update flow field
rms = 0.0
CALL update_host("update" // CHAR(0),cells,op_arg_dat(p_qold,-1,OP_ID,OP_READ),op_arg_dat(p_q,-1,OP_ID,OP_WRITE),op_arg&
&_dat(p_res,-1,OP_ID,OP_RW),op_arg_dat(p_adt,-1,OP_ID,OP_READ),op_arg_gbl(rms,OP_INC))
END DO

ncellr = real(ncell)
rms = sqrt(rms / ncellr)

IF (mod(niter,100) .EQ. 0) PRINT *, "=====> Iteration result ",rms
END DO

! uncomment the following statements to get the result of the airfoil written in a file
! modify the path as convenient
!       call op_get_dat ( p_q )
!        retDebug = openfile ( C_CHAR_"/work/cbertoll/AirfoilFortran/CUDA/FuseTests/q.txt"//C_NULL_CHAR )
!        do debugiter = 1, 4*ncell
!                datad = q(debugiter)
!                retDebug = writeRealToFile ( datad )
!        end do
!        retDebug = closefile ()
END PROGRAM AIRFOIL

SUBROUTINE save_soln_subroutine(set,qdim,src,dest)
USE GENERATED_MODULE
USE OP2_FORTRAN_DECLARATIONS
!USE AIRFOIL_SEQ
IMPLICIT NONE
TYPE ( op_set )  :: set
INTEGER(kind=4) :: qdim
TYPE ( op_dat )  :: src,dest
CALL save_soln_qdim_host("save_soln_qdim" // CHAR(0),set,op_arg_gbl(qdim,OP_READ),op_arg_dat_generic(src,-1,OP_ID,4,"re&
&al8",OP_READ),op_arg_dat_generic(dest,-1,OP_ID,4,"real8",OP_WRITE))
END SUBROUTINE
