#!/bin/sh

DIR=$1

case `uname -s` in
    Darwin)
        LIB="$DIR/intercept.stub.dylib"
        echo DYLD_FORCE_FLAT_NAMESPACE=1 DYLD_SHARED_REGION=avoid DYLD_INSERT_LIBRARIES=${LIB}:/System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/ATS.framework/Versions/A/Resources/libFontRegistry.dylib
    ;;
    Linux)
        LIB="$DIR/intercept.stub.so"
        echo LD_PRELOAD=${LIB}
    ;;
    *)
        echo Unknown platform: `uname -s`
        exit 1
esac

exit 0
