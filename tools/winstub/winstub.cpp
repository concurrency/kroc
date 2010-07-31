#include <windows.h> 
#include <tchar.h>
#include <Shlobj.h>
#include <string.h>

#define MAX_CMDLINE MAX_PATH + 100

int WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow) 
{
	TCHAR szAppDataPath[MAX_PATH];
	TCHAR szCmdlineTmp[MAX_CMDLINE];
	if(FAILED(SHGetFolderPath(NULL, 
		CSIDL_APPDATA, 
		NULL, 
		0, 
		szAppDataPath))) 
	{
		MessageBox(0,
			"SHGetFolderPath returned FALSE",
			"Could not start The Transterpreter",
			MB_OK);
		return 1;
	}

	strncat(szAppDataPath, "\\Transterpreter\\jEdit\"", MAX_PATH);
	strncpy(szCmdlineTmp, " -jar jEdit\\jedit.jar -settings=\"", MAX_CMDLINE);
	strncat(szCmdlineTmp, szAppDataPath, MAX_CMDLINE);

	/*
	 * At some point we should check the registry for which java is the current one,
	 * check this documentation for the registry API:
	 * http://msdn.microsoft.com/en-us/library/ms724875(VS.85).aspx
	 * and bootstrap.py for how to find the current java using registry keys.
	 */
	LPTSTR szCmd = _tcsdup(TEXT("C:\\windows\\system32\\java.exe"));
	LPTSTR szCmdline = _tcsdup(szCmdlineTmp);

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
			"CreateProcess returned FALSE",
			"Could not start The Transterpreter",
			MB_OK);
		return 1;
	}

	// I don't think there is a good reason for this process to be hanging around
	//WaitForSingleObject( pi.hProcess, INFINITE );

	// Close process and thread handles. 
	CloseHandle( pi.hProcess );
	CloseHandle( pi.hThread );

	return 0; 
}
