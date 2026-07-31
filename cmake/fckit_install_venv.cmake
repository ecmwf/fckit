# (C) Copyright 2024 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

macro( fckit_install_venv )

    # Create a virtualenv
    set( VENV_PATH ${CMAKE_CURRENT_BINARY_DIR}/fckit_venv )
    ecbuild_info( "Create Python virtual environment ${VENV_PATH}" )
    execute_process( COMMAND ${Python3_EXECUTABLE} -m venv --copies "${VENV_PATH}" )

    # Make the virtualenv portable by automatically deducing the VIRTUAL_ENV path from
    # the 'activate' script's location in the filesystem
    file(READ ${VENV_PATH}/bin/activate VENV_ACTIVATE_CONTENT)
    string(REPLACE "VIRTUAL_ENV=${VENV_PATH}" "VIRTUAL_ENV=\$(cd \$(dirname \$(dirname \${BASH_SOURCE[0]} ) ) && pwd )"
           VENV_ACTIVATE_CONTENT "${VENV_ACTIVATE_CONTENT}")
    file(WRITE ${VENV_PATH}/bin/activate ${VENV_ACTIVATE_CONTENT} )

    set( FCKIT_VENV_PYTHON_EXECUTABLE "${VENV_PATH}/bin/python3" )

    # Make sure the venv has (sufficiently recent) pip
    execute_process( COMMAND ${FCKIT_VENV_PYTHON_EXECUTABLE} -m ensurepip -U OUTPUT_QUIET )

    if( Python3_VERSION VERSION_EQUAL 3.8 )
       execute_process( COMMAND ${FCKIT_VENV_PYTHON_EXECUTABLE} -m pip --disable-pip-version-check
                        install --upgrade pip OUTPUT_QUIET ERROR_QUIET )
    endif()


    unset( PIP_OPTIONS )
    # set pip options
    if( DEFINED ARTIFACTS_DIR )
        list( APPEND PIP_OPTIONS "--no-index;--find-links=${ARTIFACTS_DIR}" )
    else()
        list( APPEND PIP_OPTIONS "--disable-pip-version-check")
    endif()

    if( HAVE_FCKIT_VENV_EDITABLE )
        # Use checked-out source instead of installing into venv
        list( APPEND PIP_OPTIONS "-e" )
    endif()

    # install virtual environment from requirements, which includes fypp
    set( _pkg_name "fckit_yaml_reader")
    ecbuild_info( "Install fckit_yaml_reader and fypp in virtual environment ${VENV_PATH}" )

    # Stage the package into the build tree so concurrent builds sharing the same
    # source checkout don't race on setuptools' in-tree build/ and *.egg-info/ dirs.
    set( _pkg_src "${CMAKE_CURRENT_SOURCE_DIR}/src/fckit/${_pkg_name}" )
    set( _pkg_bld "${CMAKE_CURRENT_BINARY_DIR}/${_pkg_name}" )
    file( REMOVE_RECURSE "${_pkg_bld}" )
    file( COPY "${_pkg_src}/" DESTINATION "${_pkg_bld}"
          PATTERN "build"       EXCLUDE
          PATTERN "*.egg-info"  EXCLUDE
          PATTERN "dist"        EXCLUDE
          PATTERN "__pycache__" EXCLUDE )

    execute_process( COMMAND ${FCKIT_VENV_PYTHON_EXECUTABLE} -m pip
                     install ${PIP_OPTIONS} "${_pkg_bld}"
                     RESULT_VARIABLE _pip_result
                     OUTPUT_QUIET )
    if( NOT _pip_result EQUAL 0 )
        ecbuild_error( "pip install of ${_pkg_name} failed (exit ${_pip_result})" )
    endif()

    if( HAVE_FCKIT_VENV_INSTALL )
       install( DIRECTORY ${VENV_PATH} DESTINATION . PATTERN "bin/*" PERMISSIONS ${install_permissions} )
    elseif( HAVE_FCKIT_VENV )
        # Create a symlink in the install directory pointing to the build-directory, could possibly dangle!
        install(CODE "
        set( link_source \"${CMAKE_CURRENT_BINARY_DIR}/fckit_venv\" )
        set( link_target \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/fckit_venv\" )
        if(EXISTS \${link_target} )
            message(STATUS \"Up-to-date: \${link_target} (WARNING: symlink to \${link_source})\")
        else()
            message(STATUS \"Installing: \${link_target} (WARNING: symlink to \${link_source})\")
            execute_process(COMMAND \${CMAKE_COMMAND} -E create_symlink \${link_source} \${link_target})
        endif()
        ")
    endif()

    # add python interpreter of venv as executable target
    set( FCKIT_VENV_EXE ${FCKIT_VENV_PYTHON_EXECUTABLE} )

    # compute relative path to venv to aid with installation
    string(REPLACE "${CMAKE_CURRENT_BINARY_DIR}/" "" rel_venv_exe_path ${FCKIT_VENV_EXE})

    set( FYPP ${CMAKE_CURRENT_SOURCE_DIR}/tools/fckit-eval.sh ${FCKIT_VENV_EXE} -m fypp )

endmacro()
