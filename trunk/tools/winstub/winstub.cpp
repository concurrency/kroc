#include <windows.h> 
#include <tchar.h>
#include <Shlobj.h>
#include <string.h>

#define MAX_CMDLINE MAX_PATH + 100
#define BUFFERLEN 2000
#define JAVA_RE_KEY TEXT("SOFTWARE\\JavaSoft\\Java Runtime Environment")
#define JAVA_EXE TEXT("\\java.exe")
#define BIN_JAVA_EXE TEXT("\\bin") JAVA_EXE

UINT system_java(LPTSTR buffer, UINT size)
{
	UINT len;
	
	len = GetSystemDirectory(buffer, size);
	strncat(buffer, JAVA_EXE , size - len);

	return len + sizeof(JAVA_EXE);
}

UINT find_java(LPTSTR out_buffer, UINT out_size)
{
	HKEY java_key;
	if(RegOpenKeyEx(HKEY_LOCAL_MACHINE,
        JAVA_RE_KEY,
        0,
        KEY_READ,
        &java_key) != ERROR_SUCCESS)
	{
		/* FIXME: Possibly use FormatMessage here */
		/*
		MessageBox(0,
			"Could not open Java Runtime Environment registry key",
			"Could not start The Transterpreter",
			MB_OK);
			*/
		return system_java(out_buffer, out_size);
	}

	DWORD buffer_len = BUFFERLEN;
	TCHAR buffer[buffer_len];
	if(RegQueryValueEx(java_key,
		TEXT("currentVersion"),
		NULL,
		NULL,
		(LPBYTE) buffer,
		&buffer_len) != ERROR_SUCCESS)
	{
		RegCloseKey(java_key);

		/*
		MessageBox(0,
			"Could not get the Java Runtime Environment's current version",
			"Could not start The Transterpreter",
			MB_OK);
			*/
		return system_java(out_buffer, out_size);
	}

	HKEY current_java_key;
	if(RegOpenKeyEx(
		java_key,
		buffer,
		0,
		KEY_READ,
		&current_java_key) != ERROR_SUCCESS)
	{
		RegCloseKey(java_key);

		/*
		MessageBox(0,
			"Could not open the current Java Runtime Environment's key",
			"Could not start The Transterpreter",
			MB_OK);
			*/
		return system_java(out_buffer, out_size);
	}

	RegCloseKey(java_key);

	buffer_len = BUFFERLEN;
	if(FAILED(RegQueryValueEx(current_java_key,
		TEXT("JavaHome"),
		NULL,
		NULL,
		(LPBYTE) buffer,
		&buffer_len)))
	{
		RegCloseKey(current_java_key);

		MessageBox(0,
			"Could not get the current Java Runtime Environment's JavaHome",
			"Could not start The Transterpreter",
			MB_OK);
		return system_java(out_buffer, out_size);	
	}

	RegCloseKey(current_java_key);

	strncpy(out_buffer, buffer, buffer_len);
	strncat(out_buffer, BIN_JAVA_EXE, out_size - buffer_len);

	return  sizeof(out_buffer);
}

int WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow) 
{
	TCHAR szAppDataPath[MAX_PATH];
	TCHAR szCmd[MAX_PATH];
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

	strncat(szAppDataPath, TEXT("\\Transterpreter\\jEdit\""), MAX_PATH);
	strncpy(szCmdlineTmp, TEXT(" -jar jEdit\\jedit.jar -settings=\""), MAX_CMDLINE);
	strncat(szCmdlineTmp, szAppDataPath, MAX_CMDLINE);

	find_java(szCmd, MAX_PATH);
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
