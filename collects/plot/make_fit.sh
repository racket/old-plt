cd src/fit
sh make_fit_objects.sh
cd ../..
mzc ++ccf "/I./src/fit" ++ldf "src/fit/*.obj"  fit-low-level.ss
mv fit-low-level.dll compiled/native/win32/i386/