program pTemperature;
{$mode objfpc}{$H+}

uses
  GlobalConst,
  GlobalTypes,
  GlobalConfig,
  Platform,
  {$ifdef TARGET_QEMUARM7A} QEMUVPB,          {$endif}
  {$ifdef TARGET_RPI3}      BCM2837, BCM2710, {$endif}
  SysUtils,
  Logging,
  Classes,
  FileSystem,
  FATFS,
  MMC,
  Crt;

var
 SdIsInserted:Boolean;
 Temperature:Double;
 TemperatureDirection:Double;
 StartTime:LongWord;

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

procedure RestoreMissionControl(Name:String);
var
 Path:String;
begin
 Path:='c:\' + Name;
 FSDeleteFile(Path);
 FSRenameFile(Path + '.missioncontrol',Path);
end;

begin
 LoggingDeviceSetTarget(LoggingDeviceFindByType(LOGGING_TYPE_FILE),'c:\ultibo.log');
 //The next line normally isn't required but FileSysLoggingStart currently has
 // a bug that causes it to fail if no target is specified on the command line
 LoggingDeviceStart(LoggingDeviceFindByType(LOGGING_TYPE_FILE)); 
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_FILE));

 StartTime:=ClockGetCount;
 WriteLn('Started');

 Temperature:=0;
 TemperatureDirection:=0;
 LoggingOutput('Updating sd card');
 while not DirectoryExists('c:') do
  Sleep(100);
 SdIsInserted:=True;
 RestoreMissionControl('cmdline.txt');
 RestoreMissionControl('kernel7.img');
 LoggingOutput('done updating kernel7.img');

 while ClockGetCount < StartTime + 30 * 1000*1000 do
  begin
   CheckSd;
   CheckTemperature;
   Sleep(100);
  end;
 SystemRestart(100);
end.
