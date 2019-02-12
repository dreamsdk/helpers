program FastPing;

{$mode objfpc}{$H+}

{$R *.res}

uses
  SysUtils,
  Classes,
  PingSend,
  Laz_Synapse,
  LazFileUtils,
  SysTools,
  Version;

const
  ERR_SUCCESS = 0;
  ERR_IP_NOT_REACHABLE = 1;
  ERR_NO_IP_SUPPLIED = 2;

var
  IP: string;

function ParseInternetProtocolAddress(const InputValue: string): string;
var
  Buffer: TStringList;
  i: Integer;

begin
  Result := EmptyStr;
  if IsValidInternetProtocolAddress(InputValue) then
  begin
    Buffer := TStringList.Create;
    try
      StringToStringList(InputValue, '.', Buffer);
      for i := 0 to Buffer.Count - 1 do
        Buffer[i] := IntToStr(StrToInt(Buffer[i]));
      Result := StringListToString(Buffer, '.');
    finally
      Buffer.Free;
    end;
  end;
end;

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
    'Usage: ', ExtractFileNameOnly(ParamStr(0)), ' <IPv4_Address>', sLineBreak, sLineBreak,
    'Exit codes:', sLineBreak,
    '  ', ERR_SUCCESS, ': IPv4 Address is reachable', sLineBreak,
    '  ', ERR_IP_NOT_REACHABLE, ': IPv4 Address is NOT reachable'
  );
end;

begin
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
      WriteLn('Error: "', ParamStr(1), '" is not a valid IPv4 Address');
      ExitCode := ERR_NO_IP_SUPPLIED;
    end
    else if (not DoPing) then
      ExitCode := ERR_IP_NOT_REACHABLE;
  end;
end.
