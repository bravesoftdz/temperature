program pTemperature;

{$mode objfpc}{$H+}

uses
  GlobalConst,
  GlobalTypes,
  Devices,
  Platform,
  Threads,
  Console,
  Framebuffer,
  BCM2837,
  BCM2710,
  SysUtils,
  GlobalConfig,
  Logging,
  Classes,
  FileSystem,
  FATFS,
  MMC,
  Services;

var
 WindowHandle:TWindowHandle;
 SdInserted:Boolean;
 Temperature:Double;
 TemperatureDirection:Double;

function BoolToStr(X:Boolean):String;
begin
 if X then
  BoolToStr:='True'
 else
  BoolToStr:='False';
end;

procedure CheckSd;
var
 Previous:Boolean;
begin
 Previous:=SdInserted;
 SdInserted:=DirectoryExists('c:');
 if not Previous and SdInserted then
  SystemRestart(100);
end;

function Signum(X:Double):Double;
begin
 if X < 0 then
  Signum:=-1
 else if X > 0 then
  Signum:=1
 else
  Signum:=0;
end;

procedure CheckTemperature;
var
 Next:Double;
 Delta:Double;
 Line:String;
const
 Hysteresis=2.000;
begin
 Next:=TemperatureGetCurrent(0) / 1000.0;
 Delta:=Next - Temperature;
 if ((Delta <> 0) and (Signum(Delta) = Signum(TemperatureDirection))) or (Abs(Delta) > Hysteresis) then
  begin
   Temperature:=Next;
   TemperatureDirection:=Delta;
   Line:=Format('Temperature %6.3f',[Temperature]);
   LoggingOutput(Line);
   WriteLn(Line);
  end;
end;

procedure WriteLn(Line:String);
begin
 ConsoleWindowWriteLn(WindowHandle,Line);
end;

begin
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULLSCREEN,True);
 WriteLn('Started');

 FILESYS_REGISTER_LOGGING:=True;
 FILESYS_LOGGING_DEFAULT:=True;
 FILESYS_LOGGING_FILE:='c:\ultibo.log';
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_FILE));

 Temperature:=0;
 TemperatureDirection:=0;
 LoggingOutput('Updating sd card');
 while not DirectoryExists('c:') do
  Sleep(100);
 SdInserted:=True;
 FSDeleteFile('c:\cmdline.txt');
 FSDeleteFile('c:\kernel7.img');
 FSRenameFile('c:\kernel7.img.raspbian','c:\kernel7.img');
 LoggingOutput('done updating kernel7.img');

 while True do
  begin
   CheckSd;
   CheckTemperature;
   Sleep(100);
  end;
end.
