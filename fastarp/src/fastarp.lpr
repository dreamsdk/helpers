program FastARP;

{$mode objfpc}{$H+}

{$R *.res}

// Define this to debug the GetNetworkCardAdapterList function
{$DEFINE PRINT_NETWORK_CARD_LIST}

uses
  SysUtils,
  Classes,
  LazFileUtils,
  SynaIP,
  RunTools,
  InetUtil,
  Version;

type
  EFastArp = class(Exception);
  ENoInternetProtocolEntriesForAdapter = class(EFastArp);
  ESubnetMismatch = class(EFastArp);

const
  ERR_SUCCESS = 0;
  ERR_INVALID_PARAMS = 1;
  ERR_INVALID_IP = 2;
  ERR_INVALID_MAC = 3;
  ERR_INVALID_HOST_MAC = 4;
  ERR_ARP_FAILED = 5;

var
  NetworkCardAdapters: TNetworkCardAdapterList;
  HostMediaAccessControlAddress,
  MediaAccessControlAddress,
  InternetProtocolAddress,
  HostInternetProtocolAddress: string;
  HostInterfaceIndex: Integer;
  NetworkCardAdapterIndex: Integer;
  InputHostMediaAccessControlAddress,
  InputInternetProtocolAddress,
  InputMediaAccessControlAddress: string;

function DoARP: Boolean;
var
  Executable,
  Parameters: string;

begin
  // ARP -s inet_addr eth_addr [if_addr]
  Executable := 'arp';
  Parameters := Format('-s %s %s %s', [InternetProtocolAddress,
    MediaAccessControlAddress, HostInternetProtocolAddress]);

  if IsWindowsVistaOrGreater then
  begin
    // netsh interface ip add neighbors "Ethernet" 192.168.10.1 00-D0-F1-02-8D-DF
    Executable := 'netsh';
    Parameters := Format('interface ip add neighbors %d %s %s', [
      HostInterfaceIndex, InternetProtocolAddress, MediaAccessControlAddress]);
  end;

  // Execute the command
{$IFDEF DEBUG}
  WriteLn('Executable: ', Executable, ', Parameters: ', Parameters);
{$ENDIF}
  Result := RunAndWait(Executable, Parameters);
end;

{$IFDEF DEBUG}
{$IFDEF PRINT_NETWORK_CARD_LIST}
procedure PrintNetworkCardList;
var
  i, j: Integer;

begin
  WriteLn('GetNetworkCardAdapterList');
  for i := Low(NetworkCardAdapters) to High(NetworkCardAdapters) do
  begin
    with NetworkCardAdapters[i] do
    begin
      WriteLn(NetworkCardName, ' [', MacAddress, ']');
      WriteLn('IPv4:');
      for j := Low(IPv4Addresses) to High(IPv4Addresses) do
        WriteLn(' ', IPv4Addresses[j].Address, '/', IPv4Addresses[j].Subnet);
      WriteLn('IPv6:');
      for j := Low(IPv6Addresses) to High(IPv6Addresses) do
        WriteLn(' ', IPv6Addresses[j].Address, '/', IPv6Addresses[j].Subnet);
    end;
  end;
end;
{$ENDIF}
{$ENDIF}

procedure SetExitError(ErrorCode: LongWord; const Message: string);
begin
  WriteLn('Error: ', Message);
  ExitCode := ErrorCode;
end;

procedure WriteHelp;
begin
  WriteLn(
    GetFileDescription, ', Ver. ', GetFileVersion, sLineBreak, sLineBreak,
    'Quickly add an ARP entry for the specified couple of <IPv4> and <MAC> addresses', sLineBreak,
    'passing through the specified <Host_MAC>.', sLineBreak, sLineBreak,
    'Usage: ', ExtractFileNameOnly(ParamStr(0)), ' <Host_MAC> <IPv4> <MAC>', sLineBreak, sLineBreak,
    'Exit codes:', sLineBreak,
    '  ', ERR_SUCCESS, ': ARP entry successfully added for <MAC> to <IPv4> through <Host_MAC>', sLineBreak,
    '  ', ERR_INVALID_PARAMS, ': Parameters are missing/bad format', sLineBreak,
    '  ', ERR_INVALID_IP, ': Supplied IPv4 Address is invalid', sLineBreak,
    '  ', ERR_INVALID_MAC, ': Supplied MAC Address is invalid', sLineBreak,
    '  ', ERR_INVALID_HOST_MAC, ': Host MAC is invalid, there is no IPv4 address assigned to it', sLineBreak,
    '  ', ERR_ARP_FAILED, ': ARP entry addition failed'
  );
  ExitCode := ERR_INVALID_PARAMS;
end;

function CheckMediaAccessControlAddress(
  const AMediaAccessControlAddress: string;
  const AInputMediaAccessControlAddress: string): Boolean;
begin
  Result := IsValidMediaAccessControlAddress(AMediaAccessControlAddress);
  if not Result then
    SetExitError(ERR_INVALID_MAC,
      Format('"%s" is not a valid MAC Address', [AInputMediaAccessControlAddress]));
end;

procedure RetrieveHostInternetProtocolAddress;
var
  Addresses: TIpAddresses;
  i: Integer;

begin
  HostInternetProtocolAddress := EmptyStr;

  HostInterfaceIndex := NetworkCardAdapters[NetworkCardAdapterIndex].InterfaceIndex;
  Addresses := NetworkCardAdapters[NetworkCardAdapterIndex].IPv4Addresses;

  if Length(Addresses) = 0 then // no IPv4 address for this interface, can't do nothing
    raise ENoInternetProtocolEntriesForAdapter.Create('no IPv4 available');

  // find the first IPv4 in the same subnet
  for i := Low(Addresses) to High(Addresses) do
  begin
    if IsSameSubnet(Addresses[i].Subnet, Addresses[i].Address, InternetProtocolAddress) then
    begin
      HostInternetProtocolAddress := Addresses[i].Address;
      Break;
    end;
  end;

  // no IPv4 in the same subnet of InternetProtocolAddress
  if HostInternetProtocolAddress = EmptyStr then
    raise ESubnetMismatch.Create('subnet mismatch');
end;

begin
  NetworkCardAdapters := Default(TNetworkCardAdapterList);
  GetNetworkCardAdapterList(NetworkCardAdapters);

{$IFDEF DEBUG}
{$IFDEF PRINT_NETWORK_CARD_LIST}
  PrintNetworkCardList;
{$ENDIF}
{$ENDIF}

  ExitCode := ERR_SUCCESS;

  // Check parameters
  if ParamCount < 3 then
  begin
    WriteHelp;
    Exit;
  end;

  // Get parameters
  InputHostMediaAccessControlAddress := ParamStr(1);
  InputInternetProtocolAddress := ParamStr(2);
  InputMediaAccessControlAddress := ParamStr(3);

  // Parse parameters
  HostMediaAccessControlAddress :=
    SanitizeMediaAccessControlAddress(InputHostMediaAccessControlAddress);
  InternetProtocolAddress :=
    ParseInternetProtocolAddress(InputInternetProtocolAddress);
  MediaAccessControlAddress :=
    SanitizeMediaAccessControlAddress(InputMediaAccessControlAddress);

  // Check Host MAC
  if not CheckMediaAccessControlAddress(HostMediaAccessControlAddress,
    InputHostMediaAccessControlAddress) then
      Exit; // ERR_INVALID_MAC

  // Check if the Host MAC exists
  NetworkCardAdapterIndex := FindMediaAccessControlAddress(NetworkCardAdapters, HostMediaAccessControlAddress);
  if NetworkCardAdapterIndex = -1 then
  begin
    SetExitError(ERR_INVALID_HOST_MAC,
      Format('"%s" is an unknown MAC Address on this system', [InputHostMediaAccessControlAddress]));
    Exit;
  end;

  // Check IP
  if InternetProtocolAddress = EmptyStr then
  begin
    SetExitError(ERR_INVALID_IP,
      Format('"%s" is not a valid IPv4 Address', [InputInternetProtocolAddress]));
    Exit;
  end;

  // Check Target MAC
  if not CheckMediaAccessControlAddress(MediaAccessControlAddress, InputMediaAccessControlAddress) then
    Exit; // ERR_INVALID_MAC

  // Check the range IP from the Host MAC address
  try
    RetrieveHostInternetProtocolAddress;
  except
    on E:EFastArp do
    begin
      SetExitError(ERR_INVALID_HOST_MAC,
        Format('Host adapter "%s" can''t be used: %s', [InputHostMediaAccessControlAddress, E.Message]));
      Exit;
    end;
  end;

  // Run the thing
  if not DoARP then
  begin
    SetExitError(ERR_ARP_FAILED,
      Format('Unable to assign %s to %s', [InternetProtocolAddress, MediaAccessControlAddress]));
    Exit;
  end;
end.

