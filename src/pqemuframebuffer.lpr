program pQemuFrameBuffer;

uses QEMUVersatilePB,Console,GlobalConfig,GlobalConst,FrameBuffer,Logging,Platform,Threads,Serial,SysUtils,Crt;

const
 ScreenWidth=1920;
 ScreenHeight=1080;

var
 Fb:pFrameBufferDevice;
 FramebufferProperties:TFramebufferProperties;
 SavedFrameBuffer:Pointer;

procedure StartLogging;
begin
 SERIAL_REGISTER_LOGGING:=True;
 SerialLoggingDeviceAdd(SerialDeviceGetDefault);
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_SERIAL));
end;

// Save frame buffer in memory and write signal to log
procedure SaveFrameBuffer;
const
 ColorFormat=COLOR_FORMAT_RGB24;
 BytesPerPixel=3;
begin
 if not Assigned(SavedFrameBuffer) then
  SavedFrameBuffer:=GetMem(ScreenWidth * ScreenHeight * BytesPerPixel);
 ConsoleDeviceGetImage(ConsoleDeviceGetDefault,0,0,SavedFrameBuffer,
                       ScreenWidth,ScreenHeight,ColorFormat,0);
 LoggingOutput(Format('frame buffer at 0x%x -size %dx%dx%d',
                      [LongWord(SavedFrameBuffer),ScreenWidth,ScreenHeight,BytesPerPixel]));
end;

// Initialize frame buffer to larger, typical size
procedure InitializeFrameBuffer;
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
end;

var
 FrameCounter:Integer;

begin
StartLogging;
SavedFrameBuffer:=nil;
InitializeFrameBuffer;
for FrameCounter:=1 to 3 do
 begin
  if FrameCounter <> 1 then
   Sleep(Round(0.5*1000));
  WriteLn(Format('Frame %d',[FrameCounter]));
  SaveFrameBuffer;
 end;
LoggingOutput('program halt');
ThreadHalt (0);
end.
