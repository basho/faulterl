#!/bin/sh

BASE=$1

case `uname -s` in
    Darwin)
        LIB="$BASE.dylib"
        echo DYLD_FORCE_FLAT_NAMESPACE=1 DYLD_SHARED_REGION=avoid DYLD_INSERT_LIBRARIES=${LIB}:/System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/ATS.framework/Versions/A/Resources/libFontRegistry.dylib
    ;;
    *)
        echo Unknown platform: `uname -s`
        exit 1
esac

exit 0
