
# copy the headers

cp ../../src/wxcommon/libpng/png.h src/include
cp ../../src/wxcommon/libpng/pngconf.h src/include
cp ../../src/wxcommon/zlib/zlib.h src/include
cp ../../src/wxcommon/zlib/zconf.h src/include


cp ../../src/wxcommon/zlib/* src/zlib
cd src/zlib
rm minigzip.c
rm ztest3848.c
rm maketree.c
rm example.c

cl /c *.c

cd ../..

cp ../../src/wxcommon/libpng/* src/png

cd src/png

rm example.c

cl /c /I../include *.c

cd ../..


cd src/gd

sh make_objects.sh

cd ../plplot

sh make_objects.sh 

cd ../..




mzc ++ldf "src/plplot/*.obj" ++ldf "src/gd/*.obj"  ++ldf "src/zlib/*.obj" ++ldf "src/png/*.obj"  ++ccf "/I./src/include" ++ccf "/I./src/plplot" plplot-low-level.ss

mv plplot-low-level.dll compiled/native/win32/i386/

cd src/png
rm *
cd ../zlib
rm *
