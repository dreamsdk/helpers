program KosPorts;

{$mode objfpc}{$H+}

{$R *.res}

uses
  SysUtils,
  Classes,
  LazFileUtils,
  FSTools,
  Version;

const
  ERR_SUCCESS = 0;
  ERR_FIELD_UNKNOWN = 1;
  ERR_BOOTSTRAP_NOT_FOUND = 2;
  ERR_MULTIPLE_FIELDS_NOT_ALLOWED = 3;

var
  BootstrapMetadata: TInitialProgramMetadata;
  ProgramName, BootstrapFileName: TFileName;
  FieldName: string;

procedure WriteHelp;
begin
  WriteLn(
    GetFileDescription, ', Ver. ', GetFileVersion, sLineBreak, sLineBreak,
    'Extract a field from a Sega Dreamcast bootstrap file (usally called IP.BIN) and', sLineBreak,
    'print it on-screen.', sLineBreak, sLineBreak,
    'Usage: ', ProgramName, ' <IP.BIN> <field>', sLineBreak,
    sLineBreak,
    'Fields may be one of the following:', sLineBreak,
    '   --hardware-id', sLineBreak,
    '   --maker-id', sLineBreak,
    '   --checksum', sLineBreak,
    '   --device-info', sLineBreak,
    '   --area-symbols', sLineBreak,
    '   --peripherals', sLineBreak,
    '   --product-id', sLineBreak,
    '   --product-version', sLineBreak,
    '   --release-date', sLineBreak,
    '   --boot-file-name', sLineBreak,
    '   --maker-name', sLineBreak,
    '   --title', sLineBreak,
    sLineBreak,
    'Exit codes:', sLineBreak,
    '  ', ERR_SUCCESS, ': Field value extracted with success', sLineBreak,
    '  ', ERR_FIELD_UNKNOWN, ': Field is unknown', sLineBreak,
    '  ', ERR_BOOTSTRAP_NOT_FOUND, ': IP.BIN file was not found', sLineBreak,
    '  ', ERR_MULTIPLE_FIELDS_NOT_ALLOWED, ': Multiple fields not allowed'
  );
end;

begin
  ProgramName := GetProgramName;
  ExitCode := ERR_SUCCESS;
  if ParamCount < 2 then
    WriteHelp
  else
  begin
    if ParamCount > 2 then
    begin
      WriteLn(ProgramName, ': fatal: multiple fields not allowed');
      ExitCode := ERR_MULTIPLE_FIELDS_NOT_ALLOWED;
    end
    else
    begin
      BootstrapFileName := ParamStr(1);
      FieldName := ParamStr(2);
      if not FileExists(BootstrapFileName) then
      begin
        WriteLn(ProgramName, ': fatal: unable to read "', BootstrapFileName, '"');
        ExitCode := ERR_BOOTSTRAP_NOT_FOUND;
      end
      else
      begin
        BootstrapMetadata := ReadBootstrap(BootstrapFileName);
        if not WriteField then
        begin
          WriteLn(ProgramName, ': fatal: field "', FieldName, '" unknown');
          ExitCode := ERR_FIELD_UNKNOWN;
        end;
      end;
    end;
  end;
end.

