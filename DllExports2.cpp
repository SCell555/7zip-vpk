// DLLExports2.cpp

#include "MyWindows.h"

#if defined(_7ZIP_LARGE_PAGES)
#include "Alloc.h"
#endif

#include "ComTry.h"

#include "PropVariant.h"

#include "ICoder.h"

#include "IArchive.h"

HINSTANCE g_hInstance;

#define NT_CHECK_FAIL_ACTION return FALSE;

#ifdef _WIN32
extern "C"
BOOL WINAPI DllMain(
#ifdef UNDER_CE
    HANDLE
#else
    HINSTANCE
#endif
    hInstance, DWORD dwReason, LPVOID /*lpReserved*/ )
{
    if ( dwReason == DLL_PROCESS_ATTACH )
    {
        // OutputDebugStringA("7z.dll DLL_PROCESS_ATTACH");
        g_hInstance = (HINSTANCE)hInstance;
    }
    /*
    if (dwReason == DLL_PROCESS_DETACH)
    {
      OutputDebugStringA("7z.dll DLL_PROCESS_DETACH");
    }
    */
    return TRUE;
}
#endif

DEFINE_GUID(CLSID_CArchiveHandler,
    k_7zip_GUID_Data1,
    k_7zip_GUID_Data2,
    k_7zip_GUID_Data3_Common,
    0x10, 0x00, 0x00, 0x01, 0x10, 0x00, 0x00, 0x00);

STDAPI CreateArchiver( const GUID* clsid, const GUID* iid, void** outObject );

STDAPI CreateObject( const GUID* clsid, const GUID* iid, void** outObject )
{
    // COM_TRY_BEGIN
    *outObject = 0;
    return CreateArchiver( clsid, iid, outObject );
    // COM_TRY_END
}

STDAPI SetLargePageMode()
{
    return S_OK;
}

STDAPI SetCaseSensitive( Int32 caseSensitive )
{
    return S_OK;
}

STDAPI SetCodecs( ICompressCodecsInfo* )
{
    return S_OK;
}
