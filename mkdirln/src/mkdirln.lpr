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
    WriteLn('Junction created.')
  else
    WriteLn('Junction was NOT created.');
end.

