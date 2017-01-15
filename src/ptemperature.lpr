program pTemperature;
{$mode objfpc}{$H+}

uses
  GlobalConst,
  GlobalTypes,
  GlobalConfig,
  Platform,
  {$ifdef TARGET_QEMUARM7A} QEMUVersatilePB,  {$endif}
  {$ifdef TARGET_RPI3}      BCM2837, BCM2710, {$endif}
  SysUtils,
  Logging,
  Classes,
  Crt;

var
 Temperature:Double;
 TemperatureDirection:Double;
 StartTime:LongWord;

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

begin
 LoggingDeviceSetTarget(LoggingDeviceFindByType(LOGGING_TYPE_FILE),'c:\ultibo-temperature.log');
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_FILE));

 StartTime:=ClockGetCount;
 WriteLn('Started');

 Temperature:=0;
 TemperatureDirection:=0;

 while ClockGetCount < StartTime + 30 * 1000*1000 do
  begin
   CheckTemperature;
   Sleep(100);
  end;
 SystemRestart(100);
end.
