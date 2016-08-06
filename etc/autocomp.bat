copy /y ..\src\*.* ..\tmp\
cd ..\tmp
call aspcomp commod.f
call aspcomp glcavar.f
call aspcomp glca.f
call asplink glca
copy /y glca.dll ..\bin\
cd ..\etc