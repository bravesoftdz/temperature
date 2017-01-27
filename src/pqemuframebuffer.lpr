program pQemuFrameBuffer;
uses QEMUVersatilePB,Console,GlobalConfig,GlobalConst,FrameBuffer,Logging,Platform,Serial,SysUtils,Crt;
const
 ScreenWidth=1920;
 ScreenHeight=1080;
var
 Fb:pFrameBufferDevice;
 FramebufferProperties:TFramebufferProperties;
 ScreenBuffer:Pointer;

procedure StartLogging;
begin
 SERIAL_REGISTER_LOGGING:=True;
 SerialLoggingDeviceAdd(SerialDeviceGetDefault);
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_SERIAL));
end;

procedure CaptureScreen;
const
 ColorFormat=COLOR_FORMAT_RGB15;
 BytesPerPixel=2;
begin
 if not Assigned(ScreenBuffer) then
  ScreenBuffer:=GetMem(ScreenWidth * ScreenHeight * BytesPerPixel);
 ConsoleDeviceGetImage(ConsoleDeviceGetDefault,0,0,ScreenBuffer,
                       ScreenWidth,ScreenHeight,ColorFormat,0);
 LoggingOutput(Format('frame buffer at 0x%x -size %dx%d -depth %d',
                      [LongWord(ScreenBuffer),ScreenWidth,ScreenHeight,BytesPerPixel * 8]));
end;

procedure Test;
begin
 Fb:=FrameBufferDeviceGetDefault;
 FrameBufferDeviceRelease(Fb);
 Sleep(100);
 FrameBufferProperties.Depth:=32;
 FrameBufferProperties.PhysicalWidth:=ScreenWidth;
 FrameBufferProperties.PhysicalHeight:=ScreenHeight;
 FrameBufferProperties.VirtualWidth:=FrameBufferProperties.PhysicalWidth;
 FrameBufferProperties.VirtualHeight:=FrameBufferProperties.PhysicalHeight * 2;
 FrameBufferDeviceAllocate(Fb,@FrameBufferProperties);
 Sleep(100);
 FrameBufferDeviceSetDefault(Fb);
 FrameBufferDeviceGetProperties(Fb,@FrameBufferProperties);
 ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULLSCREEN,True);
 WriteLn('Test');
end;

begin
StartLogging;
ScreenBuffer:=nil;
Test;
while True do
 begin
  WriteLn('Frame');
  CaptureScreen;
  Sleep(5*1000);
 end;
end.
