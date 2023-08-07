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

type
  TApplicationOperation = (aoUndefined, aoInstall, aoUninstall);

var
  ProgramName: TFileName;
  Operation: TApplicationOperation;
  OperationStr: string;
  Success: Boolean;

procedure WriteHelp;
begin
  WriteLn(
    GetFileDescription, ', Ver. ', GetFileVersion, sLineBreak, sLineBreak,
    'Install or uninstall the DreamSDK profile for Windows Terminal.', sLineBreak,
    'This process applies to all users that have Windows Terminal installed.', sLineBreak,
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

function GetApplicationOperation: TApplicationOperation;
var
  Command: string;

begin
  Result := aoUndefined;
  if ParamCount > 0 then
  begin
    Command := LowerCase(ParamStr(1));
    if Command = 'install' then
      Result := aoInstall
    else if Command = 'uninstall' then
      Result := aoUninstall;
  end;
end;

begin
  ProgramName := GetProgramName;
  ExitCode := ERR_SUCCESS;
  Operation := GetApplicationOperation;

  if Operation = aoUndefined then
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

  Success := False;
  OperationStr := EmptyStr;

  case Operation of
    aoInstall:
      begin
        OperationStr := 'Installation';
        Success := InstallWindowsTerminalIntegration;
      end;
    aoUninstall:
      begin
        OperationStr := 'Uninstallation';
        Success := UninstallWindowsTerminalIntegration;
      end;
  end;

  if Success then
    WriteLn(Format('%s of Windows Terminal DreamSDK profile done successfully.', [OperationStr]))
  else
    SetExitError(ERR_WINDOWS_TERMINAL_NOT_INSTALLED,
      Format('%s of Windows Terminal DreamSDK profile failed.', [OperationStr]));
end.

