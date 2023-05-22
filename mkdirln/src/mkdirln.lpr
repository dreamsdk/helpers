program MkDirLn;

{$mode objfpc}{$H+}

uses
  Windows,
  SysUtils,
  Classes,
  SysTools,
  FSTools,
  PEUtils,
  Junction;

const
  ERR_SUCCESS = 0;
  ERR_FAILED = 1;

  PARAM_POSITION_OPERATION = 1;
  PARAM_POSITION_TARGET_DIRECTORY = 2;
  PARAM_POSITION_SOURCE_DIRECTORY = 3;

var
  ProgramName,
  OperationStr: string;
  SourceFileName,
  TargetFileName: TFileName;
  IsRemoveJunctionOperation: Boolean;

{$R *.res}

function HandleExit(OperationResult: Boolean): Integer;
var
  OperationStr: string;

begin
  OperationStr := 'created';
  if IsRemoveJunctionOperation then
    OperationStr := 'removed';

  Result := ERR_SUCCESS;
  if OperationResult then
    WriteLn('Junction ', OperationStr, '.')
  else
  begin
    WriteLn('Junction was NOT ', OperationStr, '.');
    Result := ERR_FAILED;
  end;
end;

procedure WriteHelp;
var
  HomeDirectory: TFileName;

begin
  HomeDirectory := ExpandEnvironmentStrings('%DREAMSDK_HOME%');

  WriteLn(
    'Usage: ', ProgramName, ' <Operation> <TargetDirectory> [SourceDirectory]', sLineBreak,
    sLineBreak,
    'Operation:', sLineBreak,
    '  create, c: Create junction from <SourceDirectory> to <TargetDirectory>', sLineBreak,
    '  remove, r: Remove <TargetDirectory> junction', sLineBreak,
    sLineBreak,
    'Example:', sLineBreak,
    '  ', ProgramName, ' c "%DREAMSDK_HOME%\usr" "%DREAMSDK_HOME%\msys\1.0"', sLineBreak,
    sLineBreak,
    '    This will create the "usr" directory in "', HomeDirectory, '", which will be a', sLineBreak,
    '    junction to "', HomeDirectory, '\msys\1.0".'
  );
end;

begin
  ProgramName := GetProgramName;

  // Initial check
  if ParamCount < 2 then
  begin
    WriteHelp;
    Exit;
  end;

  // Retrieve the operation
  IsRemoveJunctionOperation := Default(Boolean);
  OperationStr := LowerCase(ParamStr(PARAM_POSITION_OPERATION));
  if (OperationStr = 'create') or (OperationStr = 'c') then
    IsRemoveJunctionOperation := False
  else if (OperationStr = 'remove') or (OperationStr = 'r') then
    IsRemoveJunctionOperation := True
  else
  begin
    WriteLn('Unknown operation: "', OperationStr, '".');
    ExitCode := ERR_FAILED;
    Exit; // Note: Never use Halt() as this terminates the program in an improper way!
  end;

  // Check if we have all parameters for creating junctions
  if (not IsRemoveJunctionOperation) and (ParamCount < 3) then
  begin
    WriteHelp;
    Exit;
  end;

  // Retrieve the source/target
  TargetFileName := ParseInputFileSystemObject(
    ParamStr(PARAM_POSITION_TARGET_DIRECTORY),
    pifsobExcludeTrailingPathDelimiter
  );

  if not IsRemoveJunctionOperation then
  begin
    // Create
    SourceFileName := ParseInputFileSystemObject(
      ParamStr(PARAM_POSITION_SOURCE_DIRECTORY),
      pifsobExcludeTrailingPathDelimiter
    );
    ExitCode := HandleExit(
      CreateJunction(SourceFileName, TargetFileName)
    );
  end
  else
    // Remove
    ExitCode := HandleExit(
      RemoveJunction(TargetFileName)
    );
end.

