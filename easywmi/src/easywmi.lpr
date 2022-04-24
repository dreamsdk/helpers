program EasyWMI;

{$mode objfpc}{$H+}

{$R *.res}

uses
  SysUtils,
  Classes,
  LazFileUtils,
  Contnrs,
  FSTools,
  Version,
  UtilWMI,
  InetUtil;

const
  ERR_SUCCESS = 0;
  ERR_INVALID_SWITCH = 1;

type
  TApplicationOperation = (aoUndefined, aoStatusAvailable, aoStatusInstalled, aoList);

var
  ProgramName: TFileName;

procedure WriteHelp;
begin
  WriteLn(
    GetFileDescription, ', Ver. ', GetFileVersion, sLineBreak, sLineBreak,
    'Manage KallistiOS Ports (KOS-Ports) libraries directly on command-line.', sLineBreak,
    sLineBreak,
    'Usage: ', ProgramName, ' <command> [option]', sLineBreak,
    sLineBreak,
    'Command may be one of the following:', sLineBreak,
    '  list            : List all KOS-Ports libraries installed.', sLineBreak,
    '  status [option] : Print KOS-Ports libraries count, where [option] is:', sLineBreak,
    '                      installed: Print installed libraries count', sLineBreak,
    '                      available: Print all libraries count', sLineBreak,
    sLineBreak,
    'Exit codes:', sLineBreak,
    '  ', ERR_SUCCESS, ': Operation was successfully completed', sLineBreak,
    '  ', ERR_INVALID_SWITCH, ': Invalid switch supplied'
  );
end;

function GetApplicationOperation: TApplicationOperation;
var
  Command,
  Option: string;

begin
  Result := aoUndefined;
  if ParamCount > 0 then
  begin
    Command := LowerCase(ParamStr(1));
    if Command = 'status' then
    begin
      if ParamCount > 1 then
      begin
        Option := LowerCase(ParamStr(2));
        if Option = 'installed' then
          Result := aoStatusInstalled
        else if Option = 'available' then
          Result := aoStatusAvailable;
      end
    end
    else if Command = 'list' then
      Result := aoList;
  end;
end;

procedure WindowsManagementInstrumentation_NICCONFIG;
var
   WMIResult: TFPObjectList;
   f, j: Integer;
   Buffer: TStringList;

begin
  WMIResult := GetWMIInfo('Win32_NetworkAdapterConfiguration', ['IPAddress', 'IPSubnet', 'MACAddress', 'SettingID']);
  try
    for f := 0 to Pred(WMIResult.Count) do
    begin
      Buffer := TStringList(WMIResult[f]);
      if Assigned(Buffer) then
      begin
        WriteLn(Buffer.Text);
      end;
    end;
  finally
    WMIResult.Free;
  end;
end;

begin
  ProgramName := GetProgramName;
  ExitCode := ERR_SUCCESS;
  WindowsManagementInstrumentation_NICCONFIG;
end.

