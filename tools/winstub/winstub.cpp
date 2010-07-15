#include <windows.h> 
#include <tchar.h>

int WinMain(HINSTANCE,HINSTANCE,LPSTR,int) 
{ 
    LPTSTR szCmd = _tcsdup(TEXT("C:\\windows\\system32\\java.exe"));
    //LPTSTR szCmdline = _tcsdup(TEXT(" -jar C:\\kroc\\distribution\\windows\\output\\Transterpreter\\jEdit\\jedit.jar"));
    LPTSTR szCmdline = _tcsdup(TEXT(" -jar jEdit\\jedit.jar"));

    //LPTSTR szCmd = _tcsdup(TEXT("java.exe"));
    //LPTSTR szCmdline = _tcsdup(TEXT("java.exe -jar C:\\kroc\\distribution\\windows\\output\\Transterpreter\\jEdit\\jedit.jar"));

    STARTUPINFO si;
    PROCESS_INFORMATION pi;
  
    ZeroMemory( &si, sizeof(si) );
    si.cb = sizeof(si);
    ZeroMemory( &pi, sizeof(pi) );

  if(!CreateProcess(
    szCmd,
    szCmdline,
    NULL,
    NULL,
    FALSE,
    CREATE_NO_WINDOW,
    NULL,
    NULL,
    &si,
    &pi))
  {
    MessageBox(0,
               "Could not start The Transterpreter",
               "CreateProcess returned FALSE",
               MB_OK); 
  }

  //WaitForSingleObject( pi.hProcess, INFINITE );

    // Close process and thread handles. 
    CloseHandle( pi.hProcess );
    CloseHandle( pi.hThread );

  return 0; 
}
