#!/bin/bash

# NOTE we dont use auditwheel because we dont want eckit and its deps to end up
# here. We could use --exclude, but thats more work than what we have, and we'd
# additionally need to not have names mangled

set -euo pipefail
source_target=python/fckitlib/src/copying
mkdir -p $source_target
mkdir -p /tmp/fckit/target/fckit/lib64/

# NOTE we hardcode lib list here, but note it may change with compiler version
# in that case rerun:
# `auditwheel lddtree /tmp/fckit/build/lib/libfckit.so 2>&1 | grep realpath | grep intel`
# or refactor to do this in post-compile/post-build step (but thats more work)

if [ "$(uname)" != "Darwin" ] ; then
    libs="libifport.so.5 libimf.so libintlc.so.5 libifcoremt.so.5 libsvml.so libirc.so"
    echo "bundling in libs $libs"
    root="/opt/intel/oneapi/compiler/latest/lib/"
    for lib in $libs ; do
        cp $root/$lib /tmp/fckit/target/fckit/lib64/
        patchelf --add-rpath '$ORIGIN' /tmp/fckit/target/fckit/lib64/$lib
    done
    # a bug in the intel libraries themselves -- dependency not declared but actually there
    patchelf --add-needed libifcoremt.so.5 /tmp/fckit/target/fckit/lib64/libifport.so.5

    cp /opt/intel/oneapi/compiler/latest/share/doc/compiler/licensing/fortran/LICENSE $source_target/intel.LICENSE
    cp /opt/intel/oneapi/compiler/latest/share/doc/compiler/licensing/fortran/third-party-programs.txt $source_target/intel.third-party-programs.txt
    echo "{\"$(echo $libs | tr ' ' ',')\": {\"home\": \"https://www.intel.com/content/www/us/en/developer/articles/license/end-user-license-agreement.html\", \"path\": \"copying/intel*\"}}" > $source_target/list.json

    intel_version=$(ls /opt/intel/oneapi/compiler/ | grep -v latest | sort -r | head -n 1)
    echo "intel: $intel_version" >> $source_target/../versions.txt
else
    # TODO macos support ???
    echo "no external bundle"
fi
