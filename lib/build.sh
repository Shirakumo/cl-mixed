#!/bin/bash
# Detect platform & arch
if [[ "$OSTYPE" == linux-gnu* ]]; then
    platform=lin
elif [[ "$OSTYPE" == darwin* ]]; then
    platform=mac
elif [[ "$OSTYPE" == msys ]]; then
    platform=win
else
    echo "Unknown platform $OSTYPE"
    exit 1
fi

uname=$(uname --machine)
if [[ "$uname" == x86_64* ]]; then
    arch=amd64
elif [[ "$uname" == i*86 ]]; then
    arch=i686
elif [[ "$uname" == aarch64* ]]; then
    arch=arm64
elif [[ "$uname" == armv* ]]; then
    arch=arm7a
else
    echo "Unknown architecture $uname"
    exit 1
fi

## Build
if [[ "$platform" == "lin" ]]; then
    TARGET=mixed_shared make all
else
    TARGET=mixed_shared make native
fi

## Copy to standardised order
function maybe_copy(){
    local src="$1"
    local dst="$2"
    for file in "$src/libmixed."{dylib,dll,so}; do
        if [[ -e "$file" ]]; then
            ext="${file##*.}"
            echo "Copying $file to ../static/$dst.$ext"
            cp "$file" "../static/$dst.$ext"
        fi
    done
}

maybe_copy build "libmixed-$platform-$arch"
for dir in build-*; do
    platform="${dir#build-}"
    maybe_copy "$dir" "libmixed-$platform"
done
