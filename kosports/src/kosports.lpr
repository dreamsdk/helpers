program KosPorts;

{$mode objfpc}{$H+}

{$R *.res}

uses
  SysUtils,
  Classes,
  LazFileUtils,
  FSTools,
  Version,
  DCSDKMgr,
  PortMgr;

type
  TApplicationOperation = (
    aoUndefined,
    aoStatusAvailable,
    aoStatusInstalled,
    aoList,
    aoRefresh
  );

  TApplicationCommandMap = record
    Name: string;
    Operation: TApplicationOperation;
  end;

  TApplicationCommandMapArray = array of TApplicationCommandMap;

const
  // Exit codes
  ERR_SUCCESS = 0;
  ERR_INVALID_SWITCH = 1;

  // Command mappings
  CommandMap: array[0..2] of TApplicationCommandMap = (
    (Name: 'list';                  Operation: aoList),
    (Name: 'status';                Operation: aoUndefined), // Not usable alone
    (Name: 'refresh';               Operation: aoRefresh)
  );

  // Status option mappings
  StatusOptionMap: array[0..1] of TApplicationCommandMap = (
    (Name: 'installed';             Operation: aoStatusInstalled),
    (Name: 'available';             Operation: aoStatusAvailable)
  );

var
  ProgramName: TFileName;
  DreamcastSoftwareDevelopmentKitManager: TDreamcastSoftwareDevelopmentKitManager;
  i: Integer;
  KallistiPortsInstalledFlag,
  Separator: string;
  KallistiPort: TKallistiPortItem;

procedure WriteHelp;
begin
  WriteLn(
    GetFileDescription, ', Ver. ', GetFileVersion, sLineBreak, sLineBreak,
    'Manage KallistiOS Ports (KOS-Ports) libraries directly on command-line.', sLineBreak,
    sLineBreak,
    'Usage: ', ProgramName, ' <command> [option]', sLineBreak,
    sLineBreak,
    'Command may be one of the following:', sLineBreak,
    '  list              : List all KOS-Ports libraries installed.', sLineBreak,
    '  status [option]   : Print various counts of KOS-Ports libraries, where [option] is:', sLineBreak,
    '    installed       : Print count of installed libraries.', sLineBreak,
    '    available       : Print count of all available libraries.', sLineBreak,
    '  refresh           : Refresh cache used for the used Integrated Development Environment (IDE).', sLineBreak,
    sLineBreak,
    'Exit codes:', sLineBreak,
    '  ', ERR_SUCCESS, ': Operation was successfully completed', sLineBreak,
    '  ', ERR_INVALID_SWITCH, ': Invalid switch supplied'
  );
end;

function GetApplicationOperation: TApplicationOperation;
var
  Command: string;
  i: Integer;

  function _GetSubOption(SuboptionMap: TApplicationCommandMapArray): TApplicationOperation;
  var
    j: Integer;
    Option: string;

  begin
    Result := aoUndefined;
    if ParamCount < 2 then
      Exit;
    Option := LowerCase(ParamStr(2));
    for j := Low(SuboptionMap) to High(SuboptionMap) do
    begin
      if SuboptionMap[j].Name = Option then
      begin
        Result := SuboptionMap[j].Operation;
        Exit;
      end;
    end;
  end;

begin
  Result := aoUndefined;
  if ParamCount = 0 then
    Exit;

  Command := LowerCase(ParamStr(1));

  // Search for command
  for i := Low(CommandMap) to High(CommandMap) do
  begin
    if CommandMap[i].Name = Command then
    begin
      // Regular case
      Result := CommandMap[i].Operation;

      // For 'status' command, we need an option
      if Command = 'status' then
        Result := _GetSubOption(StatusOptionMap);

      Break;
    end;
  end;
end;

begin
  ProgramName := GetProgramName;
  ExitCode := ERR_SUCCESS;

  DreamcastSoftwareDevelopmentKitManager :=
    TDreamcastSoftwareDevelopmentKitManager.Create(False);

  try
    with DreamcastSoftwareDevelopmentKitManager do
    begin
      KallistiPorts.RetrieveAvailablePorts;

      case GetApplicationOperation of
        aoUndefined:
          begin
            ExitCode := ERR_INVALID_SWITCH;
            WriteHelp;
          end;

        aoStatusAvailable:
          WriteLn(KallistiPorts.CountVisible);

        aoStatusInstalled:
          WriteLn(KallistiPorts.CountVisibleInstalled);

        aoList:
          begin
            Separator := EmptyStr;
            for i := 0 to KallistiPorts.Count - 1 do
            begin
              KallistiPort := KallistiPorts[i];
              if not KallistiPort.Hidden then
              begin
                KallistiPortsInstalledFlag := BoolToStr(KallistiPort.Installed, '*', ' ');
                WriteLn(Separator, '  [', KallistiPortsInstalledFlag, '] ', KallistiPort.Name, ' (', KallistiPort.Version, ')');
                WriteLn('      ', KallistiPort.ShortDescription);
                Separator := sLineBreak;
              end;
            end;
          end;

        // Handle IDE files
        aoRefresh:
          begin
            WriteLn('KallistiOS Ports IDE Library Information cache has been refreshed.');
            KallistiPorts.GenerateIntegratedDevelopmentEnvironmentLibraryInformation;
          end;
      end;
    end;
  finally
    DreamcastSoftwareDevelopmentKitManager.Free;
  end;
end.

