echo off

pushd "%~dp0"

::-------服务器---------------------

set SceneSvrSrc=..\..\server\SceneServer\Source\
set GatewaySvrSrc=..\..\server\GatewayServer\GatewayServer\

if exist zombiebrother_inc.pb.h (
  del zombiebrother_inc.pb.h /F /Q > nul
)

if exist zombiebrother_inc.pb.cc (
  del zombiebrother_inc.pb.cc /F /Q > nul
)

if exist zombiebrother.pb.h (
  del zombiebrother.pb.h /F /Q > nul
)

if exist zombiebrother.pb.cc (
  del zombiebrother.pb.cc /F /Q > nul
)

echo create server zombiebrother.proto
set "errorlevel="
protoc --cpp_out=./ zombiebrother_inc.proto zombiebrother.proto > nul
IF %ERRORLEVEL% NEQ 0 goto ErrorLabel


if exist %SceneSvrSrc%zombiebrother.pb.h (
  del %SceneSvrSrc%zombiebrother.pb.h /F /Q > nul
)

if exist %SceneSvrSrc%zombiebrother.pb.cc (
  del %SceneSvrSrc%zombiebrother.pb.cc /F /Q > nul
)

if exist %SceneSvrSrc%zombiebrother_inc.pb.h (
  del %SceneSvrSrc%zombiebrother_inc.pb.h /F /Q > nul
)

if exist %SceneSvrSrc%zombiebrother_inc.pb.cc (
  del %SceneSvrSrc%zombiebrother_inc.pb.cc /F /Q > nul
)


echo copy zombiebrother.pb to %SceneSvrSrc%zombiebrother.pb
copy zombiebrother.pb.h %SceneSvrSrc%zombiebrother.pb.h > nul
copy zombiebrother.pb.cc %SceneSvrSrc%zombiebrother.pb.cc > nul

copy zombiebrother_inc.pb.h %SceneSvrSrc%zombiebrother_inc.pb.h > nul
copy zombiebrother_inc.pb.cc %SceneSvrSrc%zombiebrother_inc.pb.cc > nul

if exist %GatewaySvrSrc%zombiebrother.pb.h (
  del %GatewaySvrSrc%zombiebrother.pb.h /F /Q > nul
)
if exist %GatewaySvrSrc%zombiebrother.pb.cc (
  del %GatewaySvrSrc%zombiebrother.pb.cc /F /Q > nul
)

if exist %GatewaySvrSrc%zombiebrother_inc.pb.h (
  del %GatewaySvrSrc%zombiebrother_inc.pb.h /F /Q > nul
)
if exist %GatewaySvrSrc%zombiebrother_inc.pb.cc (
  del %GatewaySvrSrc%zombiebrother_inc.pb.cc /F /Q > nul
)


echo copy zombiebrother.pb to %GatewaySvrSrc%zombiebrother.pb

copy zombiebrother.pb.h %GatewaySvrSrc%zombiebrother.pb.h > nul
copy zombiebrother.pb.cc %GatewaySvrSrc%zombiebrother.pb.cc > nul

copy zombiebrother_inc.pb.h %GatewaySvrSrc%zombiebrother_inc.pb.h > nul
copy zombiebrother_inc.pb.cc %GatewaySvrSrc%zombiebrother_inc.pb.cc > nul

if exist zombiebrother.pb.h (
  del zombiebrother.pb.h /F /Q > nul
)
if exist zombiebrother.pb.cc (
  del zombiebrother.pb.cc /F /Q > nul
)

if exist zombiebrother_inc.pb.h (
  del zombiebrother_inc.pb.h /F /Q > nul
)
if exist zombiebrother_inc.pb.cc (
  del zombiebrother_inc.pb.cc /F /Q > nul
)

::-------客户端---------------------

echo del zombiebrother.cs
if exist zombiebrother.cs (
  del zombiebrother.cs /F /Q
)

echo create client zombiebrother.proto
set "errorlevel="
protogen\protogen.exe -i:zombiebrother_inc.proto -i:zombiebrother.proto -o:zombiebrother.cs -p:detectMissing -p:lightFramework > nul
IF %ERRORLEVEL% NEQ 0 goto ErrorLabel

echo del ..\..\client\zb\Assets\Script\Network\Proto\zombiebrother.cs
if exist ..\..\client\zb\Assets\Script\Network\Proto\zombiebrother.cs (
  del ..\..\client\zb\Assets\Script\Network\Proto\zombiebrother.cs /F /Q
)

echo copy zombiebrother.cs ..\..\client\zb\Assets\Script\Network\Proto\zombiebrother.cs
copy zombiebrother.cs ..\..\client\zb\Assets\Script\Network\Proto\zombiebrother.cs > nul

echo del zombiebrother.cs
del zombiebrother.cs /F /Q


::----------------------------------------------
if exist scene_report.hrl (
	del world_server.hrl /F /Q
)


ErlEnumTool.exe scene_report.proto scene_report.hrl
if exist ..\..\server\erlang\world\include\scene_report.hrl (
	del ..\..\server\erlang\world\include\scene_report.hrl /F /Q
)
copy scene_report.hrl ..\..\server\erlang\world\include\scene_report.hrl > nul
del scene_report.hrl /F /Q
::----------------------------------------------

goto SuccessLabel

:ErrorLabel
pause


:SuccessLabel
popd