program IPReader;

{$mode objfpc}{$H+}

{$R *.res}

uses
  SysUtils,
  Classes,
  LazFileUtils,
  Version;

const
  ERR_SUCCESS = 0;
  ERR_FIELD_UNKNOWN = 1;
  ERR_BOOTSTRAP_NOT_FOUND = 2;
  ERR_MULTIPLE_FIELDS_NOT_ALLOWED = 3;

type
  TInitialProgramField = (ipfUnknown, ipfHardwareID, ipfMakerID, ipfCRC,
    ipfDeviceInfo, ipfAreaSymbols, ipfPeripherals, ipfProductNo, ipfVersion,
    ipfReleaseDate, ipfBootFilename, ipfSoftwareMakerName, ipfGameTitle);

  TInitialProgramMetadata = packed record
    HardwareID: array[0..15] of Char;
    MakerID: array[0..15] of Char;
    CRC: array[0..4] of Char;
    DeviceInfo: array[0..10] of Char;
    AreaSymbols: array[0..7] of Char;
    Peripherals: array[0..7] of Char;
    ProductNo: array[0..9] of Char;
    Version: array[0..5] of Char;
    ReleaseDate: array[0..15] of Char;
    BootFilename: array[0..15] of Char;
    SoftwareMakerName: array[0..15] of Char;
    GameTitle: array[0..127] of Char;
  end;

var
  BootstrapMetadata: TInitialProgramMetadata;
  ProgramName, BootstrapFileName: TFileName;
  FieldName: string;

procedure WriteHelp;
begin
  WriteLn(
    GetFileDescription, ', Ver. ', GetFileVersion, sLineBreak, sLineBreak,
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

function ReadBootstrap(const FileName: TFileName): TInitialProgramMetadata;
var
  FileStream: TFileStream;

begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    FileStream.ReadBuffer(Result, SizeOf(TInitialProgramMetadata));
  finally
    FileStream.Free;
  end;
end;

function StringToInitialProgramField(const S: string): TInitialProgramField;
var
  FieldName: string;

begin
  Result := ipfUnknown;
  FieldName := Trim(StringReplace(LowerCase(S), '-', EmptyStr, [rfReplaceAll]));
  if FieldName = 'hardwareid' then
    Result := ipfHardwareID
  else if FieldName = 'makerid' then
    Result := ipfMakerID
  else if (FieldName = 'crc') or (FieldName = 'checksum') then
    Result := ipfCRC
  else if FieldName = 'deviceinfo' then
    Result := ipfDeviceInfo
  else if (FieldName = 'areasymbols') or (FieldName = 'area') then
    Result := ipfAreaSymbols
  else if FieldName = 'peripherals' then
    Result := ipfPeripherals
  else if (FieldName = 'productno') or (FieldName = 'productnum') or (FieldName = 'productid') then
    Result := ipfProductNo
  else if (FieldName = 'productversion') then
    Result := ipfVersion
  else if (FieldName = 'releasedate') or (FieldName = 'date') then
    Result := ipfReleaseDate
  else if (FieldName = 'bootfilename') or (FieldName = 'bootfname') or (FieldName = 'bootfn') or (FieldName = 'bootname') or (FieldName = 'boot') then
    Result := ipfBootFilename
  else if (FieldName = 'softwaremakername') or (FieldName = 'swmakername') or (FieldName = 'makername') then
    Result := ipfSoftwareMakerName
  else if (FieldName = 'gametitle') or (FieldName = 'apptitle') or (FieldName = 'appid') or (FieldName = 'title') then
    Result := ipfGameTitle;
end;

function WriteField: Boolean;
var
  Field: TInitialProgramField;
  FieldData: string;

begin
  Result := True;
  FieldData := EmptyStr;
  Field := StringToInitialProgramField(FieldName);
  case Field of
    ipfUnknown:
      Result := False;
    ipfHardwareID:
      FieldData := BootstrapMetadata.HardwareID;
    ipfMakerID:
      FieldData := BootstrapMetadata.MakerID;
    ipfCRC:
      FieldData := BootstrapMetadata.CRC;
    ipfDeviceInfo:
      FieldData := BootstrapMetadata.DeviceInfo;
    ipfAreaSymbols:
      FieldData := BootstrapMetadata.AreaSymbols;
    ipfPeripherals:
      FieldData := BootstrapMetadata.Peripherals;
    ipfProductNo:
      FieldData := BootstrapMetadata.ProductNo;
    ipfVersion:
      FieldData := BootstrapMetadata.Version;
    ipfReleaseDate:
      FieldData := BootstrapMetadata.ReleaseDate;
    ipfBootFilename:
      FieldData := BootstrapMetadata.BootFilename;
    ipfSoftwareMakerName:
      FieldData := BootstrapMetadata.SoftwareMakerName;
    ipfGameTitle:
      FieldData := BootstrapMetadata.GameTitle;
  end;

  if Result then
  begin
    FieldData := Trim(FieldData);
    WriteLn(FieldData);
  end;
end;

begin
  ProgramName := ExtractFileNameOnly(ParamStr(0));
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

