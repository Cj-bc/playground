using System;
using System.Runtime.InteropServices;

var e = Activator.CreateInstance(typeof(MMDeviceEnumeratorClass))
    ?? throw new InvalidOperationException("MMDeviceEnumerator の生成に失敗しました。");
var enumerator = (IMMDeviceEnumerator)e;
Console.WriteLine(enumerator);

enumerator.GetDefaultAudioEndpoint(EDataFlow.Render, ERole.Multimedia, out var device); // need to release this

Console.WriteLine($"{device}");
var iid = new Guid("5CDF2C82-841E-4546-9722-0CF74078229A");

device.Activate(ref iid, ClsCtx.All /*CLSCTX_ALL*/, IntPtr.Zero, out var epVol);

var eventCtx = Guid.Empty;
var epVol_ = (IAudioEndpointVolume)epVol;

var hrMute = epVol_.SetMute(true, ref eventCtx);
var hrVol = epVol_.SetMasterVolumeLevelScalar(0, ref eventCtx);

epVol_.GetMute(out var isMuted);
epVol_.GetMasterVolumeLevelScalar(out var level);
Console.WriteLine($"SetMute: 0x{hrMute:X8}, SetVolume: 0x{hrVol:X8}, now muted={isMuted}, volume={level}");


[ComImport, Guid("BCDE0395-E52F-467C-8E3D-C4579291692E")]
public class MMDeviceEnumeratorClass { }

public enum EDataFlow
{
    Render = 0,   // 再生
    Capture = 1,  // 録音
    All = 2,
}

public enum ERole
{
    Console = 0,
    Multimedia = 1,
    Communications = 2,
}

[Flags]
public enum ClsCtx : uint
{
    All = 0x17, // INPROC_SERVER | INPROC_HANDLER | LOCAL_SERVER | REMOTE_SERVER
}

[ComImport]
[Guid("A95664D2-9614-4F35-A746-DE8DB63617E6")]
[InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
public interface IMMDeviceEnumerator
{
    int EnumAudioEndpoints(EDataFlow dataFlow, uint stateMask, out IntPtr devices); // 未使用
    int GetDefaultAudioEndpoint(EDataFlow dataFlow, ERole role, out IMMDevice device);
    // 以降のメソッドは未使用のため省略(vtable 順序維持のためここまでで打ち切り可)
}

[ComImport]
[Guid("D666063F-1587-4E43-81F1-B948E807363F")]
[InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
public interface IMMDevice
{
    int Activate(ref Guid iid, ClsCtx clsCtx, IntPtr activationParams,
                 [MarshalAs(UnmanagedType.IUnknown)] out object @interface);
                 // 以降のメソッドは未使用のため省略
}

[ComImport]
[Guid("5CDF2C82-841E-4546-9722-0CF74078229A"), InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
public interface IAudioEndpointVolume
{
    int NotImpl1();  // RegisterControlChangeNotify
    int NotImpl2();  // UnregisterControlChangeNotify
    int NotImpl3();  // GetChannelCount
    int NotImpl4();  // SetMasterVolumeLevel
    [PreserveSig] public int SetMasterVolumeLevelScalar(float fLevel, ref Guid pguidEventContext);
    int NotImpl5();  // GetMasterVolumeLevel
    [PreserveSig] public int GetMasterVolumeLevelScalar(out float pfLevel);
    int NotImpl6();  // SetChannelVolumeLevel
    int NotImpl7();  // SetChannelVolumeLevelScalar
    int NotImpl8();  // GetChannelVolumeLevel
    int NotImpl9();  // GetChannelVolumeLevelScalar
    [PreserveSig] public int SetMute([MarshalAs(UnmanagedType.Bool)] bool bMute, ref Guid pguidEventContext);
    [PreserveSig] public int GetMute([MarshalAs(UnmanagedType.Bool)] out bool pbMute);
}

