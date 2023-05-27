program WtConfig;

{$mode objfpc}{$H+}

{$R *.res}

uses
  SysUtils,
  Classes,
  LazFileUtils,
  FSTools,
  Version,
  WtTools;

const
  ERR_SUCCESS = 0;
  ERR_INVALID_SWITCH = 1;
  ERR_WINDOWS_TERMINAL_NOT_INSTALLED = 2;

var
  ProgramName: TFileName;
  Operation: TWindowsTerminalSettingsOperation;
  OperationStr: string;

procedure WriteHelp;
begin
  WriteLn(
    GetFileDescription, ', Ver. ', GetFileVersion, sLineBreak, sLineBreak,
    'Install or uninstall the DreamSDK profile for Windows Terminal.', sLineBreak,
    'This installation/uninstallation applies only for the current user.', sLineBreak,
    sLineBreak,
    'Usage: ', ProgramName, ' <command>', sLineBreak,
    sLineBreak,
    'Command may be one of the following:', sLineBreak,
    '  install   : Install the Windows Terminal DreamSDK profile.', sLineBreak,
    '  uninstall : Uninstall the Windows Terminal DreamSDK profile.', sLineBreak,
    sLineBreak,
    'Exit codes:', sLineBreak,
    '  ', ERR_SUCCESS, ': Operation was successfully completed', sLineBreak,
    '  ', ERR_INVALID_SWITCH, ': Invalid switch supplied', sLineBreak,
    '  ', ERR_WINDOWS_TERMINAL_NOT_INSTALLED, ': Windows Terminal not installed/detected'
  );
end;

procedure SetExitError(ErrorCode: LongWord; const Message: string);
begin
  WriteLn('Error: ', Message);
  ExitCode := ErrorCode;
end;

function GetApplicationOperation: TWindowsTerminalSettingsOperation;
var
  Command: string;

begin
  Result := wtsoUndefined;
  if ParamCount > 0 then
  begin
    Command := LowerCase(ParamStr(1));
    if Command = 'install' then
      Result := wtsoInstall
    else if Command = 'uninstall' then
      Result := wtsoUninstall;
  end;
end;

begin
  ProgramName := GetProgramName;
  ExitCode := ERR_SUCCESS;
  Operation := GetApplicationOperation;

  if Operation = wtsoUndefined then
  begin
    ExitCode := ERR_INVALID_SWITCH;
    WriteHelp;
    Exit;
  end;

  if not IsWindowsTerminalInstalled then
  begin
    SetExitError(ERR_WINDOWS_TERMINAL_NOT_INSTALLED,
      'Windows Terminal has not been detected/is not installed.');
    Exit;
  end;

  OperationStr := EmptyStr;

  case Operation of
    wtsoInstall:
      OperationStr := 'Installation';
    wtsoUninstall:
      OperationStr := 'Uninstallation';
  end;

  OperationStr := OperationStr + ' of Windows Terminal DreamSDK profile';

  if UpdateWindowsTerminalSettingsFile(Operation) then
    WriteLn(Format('%s done successfully.', [OperationStr]))
  else
    SetExitError(ERR_WINDOWS_TERMINAL_NOT_INSTALLED,
      Format('%s failed.', [OperationStr]));
end.

