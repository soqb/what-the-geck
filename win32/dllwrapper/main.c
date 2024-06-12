#include <windows.h>

#define LIB_TO_LOAD ""
#define FN_TO_RUN ""

BOOL WINAPI DllMain(
    HINSTANCE hinstDLL,
    DWORD fdwReason,
    LPVOID lpvReserved)
{
    if (fdwReason == DLL_PROCESS_ATTACH)
    {
        HMODULE mod = LoadLibrary(LIB_TO_LOAD);

        FARPROC proc = GetProcAddress(mod, FN_TO_RUN);
        proc();
    }
    return TRUE;
}
