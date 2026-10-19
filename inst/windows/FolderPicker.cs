// The modern Windows folder picker (the Explorer-style dialog with normal
// fonts and the navigation pane), used by IsoFishR's "Browse" buttons via
// PowerShell. Only the COM methods needed are declared; the order matches the
// Windows SDK (shobjidl_core.h).
using System;
using System.Runtime.InteropServices;

public static class IsoFishRFolderPicker
{
    [ComImport, Guid("DC1C5A9C-E88A-4dde-A5A1-60F82A20AEF7")]
    private class FileOpenDialog { }

    [ComImport, Guid("42f85136-db7e-439c-85f1-e4075d135fc8"),
     InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    private interface IFileDialog
    {
        [PreserveSig] int Show(IntPtr parent);
        void SetFileTypes(uint count, IntPtr filters);
        void SetFileTypeIndex(uint index);
        void GetFileTypeIndex(out uint index);
        void Advise(IntPtr events, out uint cookie);
        void Unadvise(uint cookie);
        void SetOptions(uint options);
        void GetOptions(out uint options);
        void SetDefaultFolder(IShellItem item);
        void SetFolder(IShellItem item);
        void GetFolder(out IShellItem item);
        void GetCurrentSelection(out IShellItem item);
        void SetFileName([MarshalAs(UnmanagedType.LPWStr)] string name);
        void GetFileName([MarshalAs(UnmanagedType.LPWStr)] out string name);
        void SetTitle([MarshalAs(UnmanagedType.LPWStr)] string title);
        void SetOkButtonLabel([MarshalAs(UnmanagedType.LPWStr)] string label);
        void SetFileNameLabel([MarshalAs(UnmanagedType.LPWStr)] string label);
        void GetResult(out IShellItem item);
    }

    [ComImport, Guid("43826D1E-E718-42EE-BC55-A1E261C37BFE"),
     InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    private interface IShellItem
    {
        void BindToHandler(IntPtr bindContext, ref Guid handler, ref Guid iid, out IntPtr result);
        void GetParent(out IShellItem parent);
        void GetDisplayName(uint form, [MarshalAs(UnmanagedType.LPWStr)] out string name);
    }

    [DllImport("shell32.dll", CharSet = CharSet.Unicode, PreserveSig = false)]
    private static extern void SHCreateItemFromParsingName(
        string path, IntPtr bindContext, ref Guid iid, out IShellItem item);

    [DllImport("user32.dll")]
    private static extern bool SetProcessDPIAware();

    private const uint FOS_PICKFOLDERS = 0x20;
    private const uint FOS_FORCEFILESYSTEM = 0x40;
    private const uint FOS_PATHMUSTEXIST = 0x800;
    private const uint SIGDN_FILESYSPATH = 0x80058000;
    private const int ERROR_CANCELLED = unchecked((int)0x800704C7);

    // Sharp text on high-resolution screens; call before any window is made.
    public static void UseScreenResolution()
    {
        try { SetProcessDPIAware(); } catch { }
    }

    // The chosen folder, or "" when cancelled.
    public static string Show(IntPtr owner, string title, string folder)
    {
        IFileDialog dialog = (IFileDialog)new FileOpenDialog();
        try
        {
            uint options;
            dialog.GetOptions(out options);
            dialog.SetOptions(options | FOS_PICKFOLDERS | FOS_FORCEFILESYSTEM | FOS_PATHMUSTEXIST);
            if (!string.IsNullOrEmpty(title)) dialog.SetTitle(title);
            if (!string.IsNullOrEmpty(folder))
            {
                try
                {
                    Guid iid = typeof(IShellItem).GUID;
                    IShellItem start;
                    SHCreateItemFromParsingName(folder, IntPtr.Zero, ref iid, out start);
                    dialog.SetFolder(start);
                }
                catch { }
            }
            int hr = dialog.Show(owner);
            if (hr == ERROR_CANCELLED) return "";
            Marshal.ThrowExceptionForHR(hr);
            IShellItem item;
            dialog.GetResult(out item);
            string path;
            item.GetDisplayName(SIGDN_FILESYSPATH, out path);
            return path;
        }
        finally
        {
            Marshal.ReleaseComObject(dialog);
        }
    }
}
