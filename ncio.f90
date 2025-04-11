module ncio

    use netcdf
    use caseConverter, only: to_lower, to_upper

    implicit none

    private
    public :: ncopen, ncclose, fread


    integer, parameter :: dim_max = 3

    type finfo
        private
        integer        :: unit
        character(256) :: file
        character(16)  :: action
        integer        :: record
        integer        :: recstep
        character(64)  :: varname
        integer        :: varid
        integer        :: ndim
        integer        :: shape(dim_max)
    end type finfo


    interface fread
        module procedure &
          & fread_1    , &
          & fread_2    , &
          & fread_3
    end interface fread


    contains


    subroutine ncopen(ftype, file, action, tstart, tstep, var, ndim, shape)
        type(finfo)   , intent(out) :: ftype
        character(256), intent(in)  :: file
        character(16) , intent(in)  :: action
        integer       , intent(in)  :: tstart
        integer       , intent(in)  :: tstep
        character(32) , intent(in)  :: var
        integer       , intent(in)  :: ndim
        integer       , intent(in)  :: shape(ndim)

        integer :: stat
        integer :: ndim_true
        integer :: action_int

        if (tstart <= 0) then
            write(0,'(A)')    '<ERROR STOP>'
            write(0,'(A,I0)') 'Invalid tstart : ', tstart
            write(0,'(A)')    'Argument "tstart" should be more than 0'
            ERROR STOP
        endif

        if (ndim > dim_max) then
            write(0,'(A)')    '<ERROR STOP>'
            write(0,'(A,I0)') 'Number of dimension out of limit : ', ndim
            write(0,'(A,I0)') 'Dimension limit is ', dim_max
            ERROR STOP
        endif

        if (any(shape(1:ndim)<=0)) then
            write(0,'(A)')                 '<ERROR STOP>'
            !write(0,'(A)',ADVANCE='NO') '"shape" includes 0 or negative value : shape=('
            write(0,'(A,*(I0,:,","),")")') '"shape" includes 0 or negative value : shape=(', shape(1:ndim)
            ERROR STOP
        endif

        if (trim(to_lower(action)) == 'write') then
            action_int = NF90_NOWRITE
        else
            write(0,'(A)') '<ERROR STOP>'
            write(0,'(A)') 'NCOPEN() for ' // trim(file)
            write(0,'(A)') 'Action ' // trim(action) // ' is not supported'
            ERROR STOP
        endif



        !! OPEN
        stat = NF90_OPEN(PATH=file      , &  !! IN
                       & MODE=action_int, &  !! IN
                       & NCID=ftype%unit  )  !! OUT

        if (stat /= NF90_NOERR) then
            write(0,'(A)') '<ERROR STOP>'
            write(0,'(A)') 'Failed to open ' // trim(file)
            ERROR STOP
        endif


        !! Get Variable ID
        stat = NF90_INQ_VARID(NCID =ftype%unit , &  !! IN
                            & NAME =trim(var)  , &  !! IN
                            & VARID=ftype%varid  )  !! OUT

        if (stat /= NF90_NOERR) then
            ! Close and Error Stop
            write(0,'(A)') '<ERROR STOP>'
            write(0,'(A)') trim(var) // ' : No such variable in ' // trim(file)
            call ncclose(ftype)
            ERROR STOP
        endif


        !! Check the Dimension
        stat = NF90_INQUIRE(NCID       =ftype%unit, &  !! IN
                          & NDIMENSIONS=ndim_true   )  !! OUT
        if (ndim /= ndim_true) then
            write(0,'(A)') '<ERROR STOP>'
            write(0,'(A,I0,A,I0,A)') 'Number of dimensions of ' // trim(var) // ' is ', ndim_true, ', but ', ndim, ' was specified'
            call ncclose(ftype)
            ERROR STOP
        endif

        ftype%file          = trim(file)
        ftype%action        = trim(action)
        ftype%record        = tstart
        ftype%recstep       = tstep
        ftype%varname       = trim(var)
        ftype%ndim          = ndim
        ftype%shape(1:ndim) = shape(1:ndim)
    
    end subroutine ncopen


    subroutine ncclose(ftype)
        type(finfo), intent(inout) :: ftype
        
        integer :: stat

        stat = NF90_CLOSE(ftype%unit)  !! IN

        ftype%unit    = -999999
        ftype%file    = 'CLOSED'
        ftype%action  = 'CLOSED'
        ftype%record  = -999999
        ftype%recstep = -999999
        ftype%varid   = -999999

    end subroutine ncclose


    subroutine fread_1(ftype, input_data)
        integer, parameter :: rk=4
        integer, parameter :: dim=1
        type(finfo), intent(inout) :: ftype
        real(rk)   , intent(out)   :: input_data(ftype%shape(1))

        real(rk) :: work_reader(ftype%shape(1),1)
        integer  :: start(dim+1)
        integer  :: count(dim+1)
        integer  :: stat
        integer  :: i

        start(1:dim)   = 1
        start(dim+1)   = ftype%record
        count(1:dim+1) = [(ftype%shape(i), i=1,dim), 1]

        stat = NF90_GET_VAR(NCID  =ftype%unit                        , &  !! IN
                          & VARID =ftype%varid                       , &  !! IN
                          & VALUES=work_reader(1:ftype%shape(1),1:1), &  !! OUT
                          & START =start(1:dim)                     , &  !! IN
                          & COUNT =count(1:dim)                       )  !! IN
        call read_error(ftype, &  !! IN
                      & stat   )  !! IN

        input_data(1:ftype%shape(1)) = work_reader(1:ftype%shape(1),1)

        ftype%record = ftype%record + ftype%recstep

    end subroutine fread_1


    subroutine fread_2(ftype, input_data)
        integer, parameter :: rk=4
        integer, parameter :: dim=2
        type(finfo), intent(inout) :: ftype
        real(rk)   , intent(out)   :: input_data(ftype%shape(1),ftype%shape(2))

        real(rk) :: work_reader(ftype%shape(1),ftype%shape(2),1)
        integer  :: start(dim+1)
        integer  :: count(dim+1)
        integer  :: stat
        integer  :: i

        start(1:dim)   = 1
        start(dim+1)   = ftype%record
        count(1:dim+1) = [(ftype%shape(i), i=1,dim), 1]

        stat = NF90_GET_VAR(NCID  =ftype%unit                                        , &  !! IN
                          & VARID =ftype%varid                                       , &  !! IN
                          & VALUES=work_reader(1:ftype%shape(1),1:ftype%shape(2),1:1), &  !! OUT
                          & START =start(1:dim)                                      , &  !! IN
                          & COUNT =count(1:dim)                                        )  !! IN
        call read_error(ftype, &  !! IN
                      & stat   )  !! IN

        input_data(1:ftype%shape(1),1:ftype%shape(2)) = work_reader(1:ftype%shape(1),1:ftype%shape(2),1)

        ftype%record = ftype%record + ftype%recstep

    end subroutine fread_2


    subroutine fread_3(ftype, input_data)
        integer, parameter :: rk=4
        integer, parameter :: dim=3
        type(finfo), intent(inout) :: ftype
        real(rk)   , intent(out)   :: input_data(ftype%shape(1),ftype%shape(2),ftype%shape(3))

        real(rk) :: work_reader(ftype%shape(1),ftype%shape(2),ftype%shape(3),1)
        integer  :: start(dim+1)
        integer  :: count(dim+1)
        integer  :: stat
        integer  :: i

        start(1:dim)   = 1
        start(dim+1)   = ftype%record
        count(1:dim+1) = [(ftype%shape(i), i=1,dim), 1]

        stat = NF90_GET_VAR(NCID  =ftype%unit                                                         , &  !! IN
                          & VARID =ftype%varid                                                        , &  !! IN
                          & VALUES=work_reader(1:ftype%shape(1),1:ftype%shape(2),1:ftype%shape(3),1:1), &  !! OUT
                          & START =start(1:dim)                                                       , &  !! IN
                          & COUNT =count(1:dim)                                                         )  !! IN
        call read_error(ftype, &  !! IN
                      & stat   )  !! IN

        input_data(1:ftype%shape(1),1:ftype%shape(2),1:ftype%shape(3)) = work_reader(1:ftype%shape(1),1:ftype%shape(2),1:ftype%shape(3),1)

        ftype%record = ftype%record + ftype%recstep

    end subroutine fread_3


    subroutine read_error(ftype, stat)
        type(finfo), intent(inout) :: ftype
        integer    , intent(in)    :: stat

        if (stat == NF90_NOERR) then
            return
        endif

        write(0,'(A)')                 '<ERROR STOP>'
        write(0,'(A)')                 'Failed to read '// trim(ftype%file)
        write(0,'(A,I0)')              'ERROR-CODE : ', stat
        write(0,'(A)')                 'VARIABLE : ' // trim(ftype%varname)
        write(0,'(A,I0)')              'T=', ftype%record
        write(0,'(A,I0)')              'TSTEP=', ftype%recstep
        write(0,'(A,*(I0,:,","),")")') 'SHAPE=(', ftype%shape(1:ftype%ndim)
        call ncclose(ftype)
        ERROR STOP

    end subroutine read_error


end module ncio

