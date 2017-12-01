type :: payload_t
  integer :: id
contains
endtype

type, extends(fckit_refcounted) :: RefObj
  type(payload_t), pointer :: payload => null()
contains
  procedure :: delete => RefObj__delete
  procedure :: copy => RefObj__copy
  procedure :: id

#ifdef  EC_HAVE_Fortran_FINALIZATION
  final :: RefObj__final
#endif

endtype

interface RefObj
  module procedure RefObj__constructor
end interface

contains

function id(this)
  integer :: id
  class(RefObj), intent(in) :: this
  if( associated( this%payload ) ) then
    id = this%payload%id
  else
    id = -1
  endif
end function

function create_obj(id) result(anobj)
  type(RefObj) :: anobj
  integer :: id
  write(0,'(A,I0,A)') ">>>  anobj = RefObj(",id,")"
  anobj = RefObj(id)
  write(0,'(A,I0,A)') "<<<  anobj = RefObj(",id,")"
  call anobj%return()
  write(0,*) "create_obj --> owners = ",anobj%owners()
end function


function RefObj__constructor(id) result(this)
  type(RefObj) :: this
  integer, intent(in) :: id
  write(0,*)''
  call this%reset_c_ptr( fckit__new_Owned() )
  write(0,*) "constructing obj ", id, " ptr = ",c_ptr_to_loc(this%c_ptr())
  allocate( this%payload )
  this%payload%id = id
  call this%return()
  write(0,*) "RefObj__constructor --> owners = ",this%owners()
end function

subroutine RefObj__final(this)
  type(RefObj) :: this
  write(0,*) "RefObj__final"
  if( this%id() >= 0 ) then
    write(0,*) "final obj",this%id(), " starting with owners = ",this%owners()
  else
    write(0,*) "final obj",this%id(), " (uninitialized) "
  endif
  write(0,*) "calling final()..."
  call this%final()
  write(0,*) "calling final()...done"
end subroutine

#if 1
subroutine RefObj__delete(this)
  class(RefObj), intent(inout) :: this
write(0,*) "deleting obj",this%id(), " ptr = ", c_ptr_to_loc(this%c_ptr())
  deallocate(this%payload)
  call fckit__delete_Owned(this%c_ptr())
  deleted = deleted + 1
end subroutine
#endif


subroutine RefObj__copy(this,obj_in)
  class(RefObj), intent(inout) :: this
  class(fckit_refcounted), target, intent(in) :: obj_in
  class(refObj), pointer :: obj_in_cast

  select type (obj_in)
    type is (RefObj)
      obj_in_cast => obj_in
  end select
    write(0,*) "copy obj ",obj_in_cast%id()," to ",this%id()
  this%payload => obj_in_cast%payload
    write(0,*) "   check: obj is now ", this%id()
end subroutine

