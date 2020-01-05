program FastPing;

{$mode objfpc}{$H+}

{$R *.res}

uses
  SysUtils,
  Classes,
  PingSend,
  Laz_Synapse,
  LazFileUtils,
  FSTools,
  Version,
  InetUtil;

const
  ERR_SUCCESS = 0;
  ERR_IP_NOT_REACHABLE = 1;
  ERR_NO_IP_SUPPLIED = 2;

var
  ProgramName,
  IP: string;

function DoPing: Boolean;
var
  APingSend: TPingSend;
  i: Integer;

begin
  Result := False;
  APingSend := TPingSend.Create;
  try
    APingSend.Timeout := 1000;
    i := 0;
    while (i < 3) and (not Result) do
    begin
      Result := Result or (APingSend.Ping(IP) and (APingSend.ReplyError = IE_NoError));
{$IFDEF DEBUG}
      WriteLn(' #', i, ': ', Result);
{$ENDIF}
      Inc(i);
    end;
  finally
    APingSend.Free;
  end;
end;

procedure WriteHelp;
begin
  WriteLn(
    GetFileDescription, ', Ver. ', GetFileVersion, sLineBreak, sLineBreak,
    'Quickly check if the supplied IPv4 address is reachable or not.', sLineBreak, sLineBreak,
    'Usage: ', ProgramName, ' <IPv4_Address>', sLineBreak, sLineBreak,
    'Exit codes:', sLineBreak,
    '  ', ERR_SUCCESS, ': IPv4 Address is reachable', sLineBreak,
    '  ', ERR_IP_NOT_REACHABLE, ': IPv4 Address is NOT reachable'
  );
end;

begin
  ProgramName := GetProgramName;
  if ParamCount < 1 then
  begin
    WriteHelp;
    ExitCode := ERR_NO_IP_SUPPLIED;
  end
  else
  begin
    IP := ParseInternetProtocolAddress(ParamStr(1));
    if IP = EmptyStr then
    begin
      WriteLn(ProgramName, ': fatal: "', ParamStr(1), '" is not a valid IPv4 Address');
      ExitCode := ERR_NO_IP_SUPPLIED;
    end
    else if (not DoPing) then
      ExitCode := ERR_IP_NOT_REACHABLE;
  end;
end.
