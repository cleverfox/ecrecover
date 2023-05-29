#! /bin/sh

[ -f c_src/secp256k1/.libs/libsecp256k1.a ] && exit 0

cd c_src
git clone https://github.com/bitcoin/secp256k1
cd secp256k1
git -c advice.detachedHead=false checkout cbe41ac138bc0773d60ab1942b7ad6fc5eccfc19
./autogen.sh
CFLAGS="-fPIC" ./configure --enable-module-recovery
make clean
make

