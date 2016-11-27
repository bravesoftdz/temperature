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
  MMC;

var
 WindowHandle:TWindowHandle;
 SdIsInserted:Boolean;
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
 SdWasNotInserted:Boolean;
begin
 SdWasNotInserted:=not SdIsInserted;
 SdIsInserted:=DirectoryExists('c:');
 if SdWasNotInserted and SdIsInserted then
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
 if ((Delta <> 0) and (Signum(Delta) = TemperatureDirection)) or (Abs(Delta) > Hysteresis) then
  begin
   Temperature:=Next;
   TemperatureDirection:=Signum(Delta);
   Line:=Format('Temperature %6.3f',[Temperature]);
   LoggingOutput(Line);
   WriteLn(Line);
  end;
end;

procedure WriteLn(Line:String);
begin
 ConsoleWindowWriteLn(WindowHandle,Line);
end;

procedure RestoreMissionControl(Name:String);
var
 Path:String;
begin
 Path:='c:\' + Name;
 FSDeleteFile(Path);
 FSRenameFile(Path + '.missioncontrol',Path);
end;

begin
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULLSCREEN,True);
 WriteLn('Started');

// FILESYS_REGISTER_LOGGING:=True;
// FILESYS_LOGGING_DEFAULT:=True;
// FILESYS_LOGGING_FILE:='c:\ultibo.log';
// LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_FILE));

 Temperature:=0;
 TemperatureDirection:=0;
 LoggingOutput('Updating sd card');
 while not DirectoryExists('c:') do
  Sleep(100);
 SdIsInserted:=True;
 RestoreMissionControl('cmdline.txt');
 RestoreMissionControl('kernel7.img');
 LoggingOutput('done updating kernel7.img');

 while True do
  begin
   CheckSd;
   CheckTemperature;
   Sleep(100);
  end;
end.
