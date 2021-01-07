!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
module M_testsuite
use M_sort
private
public test_suite_m_sort
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_m_sort()
   call test_sort_shell()
   call test_sort_quick_rx()
   call test_unique()
   call test_swap()

   call test_tree_insert()
   call test_tree_print()

end subroutine test_suite_m_sort
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tree_insert()

use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
   call unit_check_start('insert',msg='')
   !!call unit_check('insert', 0.eq.0, 'checking',100)
   call unit_check_done('insert',msg='')
end subroutine test_tree_insert
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tree_print()

use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
   call unit_check_start('tree_print',msg='')
   !!call unit_check('tree_print', 0.eq.0, 'checking',100)
   call unit_check_done('tree_print',msg='')
end subroutine test_tree_print
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sort_shell()
use M_verify, only : unit_check, unit_check_start, unit_check_good, unit_check_bad, unit_check_done
implicit none
integer,parameter            :: cd=kind(0.0d0)
integer,parameter            :: isz=10000
complex(kind=cd)             :: ccdd(isz)
complex                      :: cc(isz)
doubleprecision              :: dd(isz)
real                         :: rr(isz), rr2(isz)
integer                      :: ii(isz)
character(len=:),allocatable :: array(:)
integer                      :: csz
!-----------------------------------------------------------------------------------------------------------------------------------
call unit_check_start('sort_shell','-library libGPF') ! start tests
!-----------------------------------------------------------------------------------------------------------------------------------
array= [ character(len=20) :: &
   'red',   'green', 'blue', 'yellow', 'orange',   'black', 'white', 'brown', 'gray', 'cyan',   'magenta',  'purple']
csz=size(array)
call sort_shell(array,order='a')
call unit_check('sort_shell',all(array(1:csz-1) .le. array(2:csz)),msg='sort string array, ascending')  ! verify in ascending order
!-----------------------------------------------------------------------------------------------------------------------------------
array= [ character(len=20) :: &
   'RED',   'GREEN', 'BLUE', 'YELLOW', 'ORANGE',   'BLACK', 'WHITE', 'BROWN', 'GRAY', 'CYAN',   'MAGENTA',  'PURPLE']
csz=size(array)
call sort_shell(array,order='d')
call unit_check('sort_shell',all(array(1:csz-1) .ge. array(2:csz)),msg='sort string array, descending') ! verify in descending order
!-----------------------------------------------------------------------------------------------------------------------------------
CALL RANDOM_NUMBER(RR)                                           ! RR contains uniformly distributed random numbers from 0.0 to <1.0
II=RR*HUGE(1)                                                    ! spread values out along range of INTEGER
call sort_shell(ii,order='a')
call unit_check('sort_shell',all(ii(1:isz-1) .le. ii(2:isz)),msg='sort integer, ascending array')  ! verify in ascending order
!-----------------------------------------------------------------------------------------------------------------------------------
CALL RANDOM_NUMBER(RR)
II=RR*HUGE(1)
call sort_shell(ii,order='d')
call unit_check('sort_shell',all(ii(1:isz-1) .ge. ii(2:isz)),msg='sort integer, descending array')
!-----------------------------------------------------------------------------------------------------------------------------------
CALL RANDOM_NUMBER(RR)
call sort_shell(rr,order='a')
call unit_check('sort_shell',all(rr(1:isz-1) .le. rr(2:isz)),msg='sort real, ascending')
!-----------------------------------------------------------------------------------------------------------------------------------
CALL RANDOM_NUMBER(RR)
call sort_shell(rr,order='d')
call unit_check('sort_shell',all(rr(1:isz-1) .ge. rr(2:isz)),msg='sort real, descending')
!-----------------------------------------------------------------------------------------------------------------------------------
CALL RANDOM_NUMBER(RR)
dd=RR*2000.0d0
call sort_shell(dd,order='a')
call unit_check('sort_shell',all(dd(1:isz-1) .le. dd(2:isz)),msg='sort doubleprecision, ascending')
!-----------------------------------------------------------------------------------------------------------------------------------
CALL RANDOM_NUMBER(RR)
dd=RR*2000.0d0
call sort_shell(dd,order='d')
call unit_check('sort_shell',all(dd(1:isz-1) .ge. dd(2:isz)),msg='sort doubleprecision, descending')
!-----------------------------------------------------------------------------------------------------------------------------------
CALL RANDOM_NUMBER(RR)
CALL RANDOM_NUMBER(RR2)

cc=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(cc,order='a',type='real')
call unit_check('sort_shell',all(real(cc(1:isz-1)) .le. real(cc(2:isz))),msg='sort complex by real component, ascending')

cc=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(cc,order='d',type='real')
call unit_check('sort_shell',all(real(cc(1:isz-1)) .ge. real(cc(2:isz))),msg='sort complex by real component, descending')

cc=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(cc,order='a',type='imaginary')
call unit_check('sort_shell',all(aimag(cc(1:isz-1)).le.aimag(cc(2:isz))),msg='sort complex by imaginary component, ascending')

cc=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(cc,order='d',type='imaginary')
call unit_check('sort_shell',all(aimag(cc(1:isz-1)) .ge. aimag(cc(2:isz))),msg='sort complex by imaginary component, descending')

cc=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(cc,order='a',type='size')
call unit_check('sort_shell', &
   all(sqrt( dble(cc(1:isz-1))**2 +aimag(cc(1:isz-1))**2) .le. sqrt(dble(cc(2:isz))**2+aimag(cc(2:isz))**2)), &
   msg='sort complex array by magnitude, ascending')

cc=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(cc,order='d',type='size')
call unit_check('sort_shell', &
   all(sqrt( dble(cc(1:isz-1))**2 +aimag(cc(1:isz-1))**2) .ge. sqrt(dble(cc(2:isz))**2+aimag(cc(2:isz))**2)), &
   msg='sort complex array by magnitude, descending')
!-----------------------------------------------------------------------------------------------------------------------------------
CALL RANDOM_NUMBER(RR)
CALL RANDOM_NUMBER(RR2)

ccdd=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(ccdd,order='a',type='real')
call unit_check('sort_shell',all(real(ccdd(1:isz-1)).le.real(ccdd(2:isz))), msg='sort double complex by real component, ascending')

ccdd=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(ccdd,order='d',type='real')
call unit_check('sort_shell',all(real(ccdd(1:isz-1)).ge.real(ccdd(2:isz))), msg='sort double complex by real component, descending')

ccdd=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(ccdd,order='a',type='imaginary')
call unit_check('sort_shell', &
   all(aimag(ccdd(1:isz-1)).le.aimag(ccdd(2:isz))), msg='sort double complex by imaginary component, ascending')

ccdd=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(ccdd,order='d',type='imaginary')
call unit_check('sort_shell', &
   all(aimag(ccdd(1:isz-1)).ge.aimag(ccdd(2:isz))), msg='sort double complex by imaginary component, descending')

ccdd=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(ccdd,order='a',type='size')
call unit_check('sort_shell', &
   all(sqrt(real(ccdd(1:isz-1))**2+aimag(ccdd(1:isz-1))**2) .le. sqrt(real(ccdd(2:isz))**2+aimag(ccdd(2:isz))**2)),  &
   msg='sort double complex by magnitude, ascending')

ccdd=cmplx(RR*20000.0,RR2*20000.0)
call sort_shell(ccdd,order='d',type='size')
call unit_check('sort_shell', &
   all(sqrt(real(ccdd(1:isz-1))**2+aimag(ccdd(1:isz-1))**2) .ge. sqrt(real(ccdd(2:isz))**2+aimag(ccdd(2:isz))**2)),  &
   msg='sort double complex by magnitude, descending')
!-----------------------------------------------------------------------------------------------------------------------------------
call unit_check_done('sort_shell') ! assume if got here passed checks
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine test_sort_shell
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sort_quick_rx
use M_verify,   only : unit_check, unit_check_start, unit_check_good, unit_check_bad, unit_check_done
implicit none
integer,parameter            :: cd=kind(0.0d0)
integer,parameter            :: isz=10000000
real                         :: rr(isz)
integer                      :: ii(isz)
integer                      :: i
logical                      :: gb
call unit_check_start('sort_quick_rx', '-library libGPF') ! start tests

CALL RANDOM_NUMBER(RR)
rr=rr*45000
gb=.true.
call sort_quick_rx(rr,ii)
do i=1,isz-1
   if(rr(ii(i)).gt.rr(ii(i+1)))then
      call unit_check_bad('sort_quit_rx',msg='Error in sorting reals from small to large')
      gb=.false.
   endif
enddo
if(gb)call unit_check_good('sort_quick_rx',msg='sort real array')

end subroutine test_sort_quick_rx
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unique
use M_verify, only : unit_check_start, unit_check, unit_check_bad, unit_check_good, unit_check_done
use M_msg,   only : str
implicit none
integer,allocatable :: ints(:)
integer             :: ic
call unit_check_start('unique', '-library libGPF') ! start tests

ints=[1,1,2,3,4,4,10,20,20,30]
call unique(ints,ic)
call unit_check('unique',ic.eq.7.and.all(ints(:ic).eq.[1,2,3,4,10,20,30]),'expect 7 ic=',ic, 'ints=',str(ints(:ic)))

ints=[integer ::]
call unique(ints,ic)
call unit_check('unique',ic.eq.0 .and. all(ints.eq.[integer::]),msg='check empty array ')

ints=[10]
call unique(ints,ic)
call unit_check('unique',ic.eq.1 .and. all(ints(:ic).eq.[10]),msg='check array of one element')

ints=[10,10,10,10]
call unique(ints,ic)
call unit_check('unique',ic.eq.1 .and. all(ints(:ic).eq.[10,10,10,10]),msg='all duplicates')

ints=[10,20,30,40]
call unique(ints,ic)
call unit_check('unique',ic.eq.4 .and. all(ints(:ic).eq.[10, 20, 30, 40]),msg='no change required')

call unit_check_done('unique',msg='test of unique(3f) completed') ! assume if got here passed checks
end subroutine test_unique
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_swap
use M_verify,   only : unit_check, unit_check_start, unit_check_good, unit_check_bad, unit_check_done
implicit none
integer             :: iarray2(2)=[20,10],iarray(2)=[10,20]
real                :: rarray2(2)=[22.22,11.11],rarray(2)=[11.11,22.22]
doubleprecision     :: darray2(2)=[9876.54321d0,1234.56789d0],darray(2)=[1234.56789d0,9876.54321d0]
complex             :: carray2(2)=[(9876,54321),(1234,56789)],carray(2)=[(1234,56789),(9876,54321)]
logical             :: larray2(2)=[.false.,.true.],larray(2)=[.true.,.false.]
character(len=16)   :: string2(2)=["The other string","First string    "],string(2)=["First string    ", "The other string"]

   call unit_check_start('swap',' -library libGPF') ! start tests
   call swap (iarray(1), iarray(2)); call unit_check('swap',all(iarray.eq.iarray2),'integer test')
   call swap (rarray(1), rarray(2)); call unit_check('swap',all(rarray.eq.rarray2),'real test')
   call swap (darray(1), darray(2)); call unit_check('swap',all(darray.eq.darray2),'double test')
   call swap (carray(1), carray(2)); call unit_check('swap',all(carray.eq.carray2),'complex test')
   call swap (larray(1), larray(2)); call unit_check('swap',all(larray.eqv.larray2),'logical test')
   call swap (string(1), string(2)); call unit_check('swap',all(string.eq.string2),'string test')
   call unit_check_done('swap')

end subroutine test_swap
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
end module M_testsuite
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
program runtest
use M_msg
use M_verify
use M_verify, only : unit_check, unit_check_start, unit_check_good, unit_check_bad, unit_check_done
use M_verify, only : unit_check_level
use M_testsuite
   unit_check_command=''
   unit_check_keep_going=.true.
   unit_check_level=0
   call test_suite_M_sort()
end program runtest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
