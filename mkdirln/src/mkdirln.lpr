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

var
  ProgramName: string;
  SourceFileName,
  TargetFileName: TFileName;

{$R *.res}

begin
  ProgramName := GetProgramName;
  if ParamCount < 2 then
  begin
    WriteLn('Usage: ', ProgramName, ' <SourceDirectory> <TargetDirectory>');
    Exit;
  end;

  SourceFileName := ParamStr(1);
  TargetFileName := ParamStr(2);

  if CreateJunction(SourceFileName, TargetFileName) then
  begin
    WriteLn('Junction created.');
    ExitCode := ERR_SUCCESS;
  end
  else
  begin
    WriteLn('Junction was NOT created.');
    ExitCode := ERR_FAILED;
  end;
end.

