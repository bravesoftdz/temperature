program pQemuFrameBuffer;
uses QEMUVersatilePB,Console,GlobalConfig,GlobalConst,FrameBuffer,Logging,Platform,Serial,SysUtils,Crt;
var
 Fb:pFrameBufferDevice;
 FramebufferProperties:TFramebufferProperties;

procedure StartLogging;
begin
 SERIAL_REGISTER_LOGGING:=True;
 SerialLoggingDeviceAdd(SerialDeviceGetDefault);
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_SERIAL));
end;

procedure Test;
begin
 Fb:=FrameBufferDeviceGetDefault;
 FrameBufferDeviceRelease(Fb);
 Sleep(100);
 FrameBufferProperties.Depth:=32;
 FrameBufferProperties.PhysicalWidth:=1024;
 FrameBufferProperties.PhysicalHeight:=768;
 FrameBufferProperties.VirtualWidth:=FrameBufferProperties.PhysicalWidth;
 FrameBufferProperties.VirtualHeight:=FrameBufferProperties.PhysicalHeight * 2;
 FrameBufferDeviceAllocate(Fb,@FrameBufferProperties);
 Sleep(100);
 FrameBufferDeviceSetDefault(Fb);
 FrameBufferDeviceGetProperties(Fb,@FrameBufferProperties);
 ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULLSCREEN,True);
 LoggingOutput(Format('FrameBuffer address %8.8x',[FrameBufferProperties.Address]));
 WriteLn('Test');
end;

begin
StartLogging;
Test;
while True do
 begin
  LoggingOutput('Tick');
  Sleep(1*1000);
 end;
end.
