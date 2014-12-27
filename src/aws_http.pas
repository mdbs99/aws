{
    AWS
    Copyright (C) 2013-2014 by mdbs99

    See the file LICENSE.txt, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit aws_http;

{$i aws.inc}

interface

uses
  //rtl
  sysutils,
  classes,
  //synapse
  httpsend,
  synautil,
  ssl_openssl;

type
  IHTTPResponse = interface(IInterface)
    function ResultCode: Integer;
    function ResultHeader: string;
    function ResultText: string;
  end;

  IHTTPSender = interface(IInterface)
    function Send: IHTTPResponse;
  end;

  THTTPResponse = class(TInterfacedObject, IHTTPResponse)
  private
    FResultCode: Integer;
    FResultHeader: string;
    FResultText: string;
  public
    constructor Create(ResultCode: Integer; const ResultHeader, ResultText: string);
    constructor Create(Origin: IHTTPResponse);
    function ResultCode: Integer;
    function ResultHeader: string;
    function ResultText: string;
  end;

  THTTPSender = class(TInterfacedObject, IHTTPSender)
  private
    FSender: THTTPSend;
    FMethod: string;
    FHeader: string;
    FContentType: string;
    FURI: string;
    FStream: TStream;
  public
    constructor Create(const Method, Header, ContentType, URI: string; Stream: TStream);
    destructor Destroy; override;
    function Send: IHTTPResponse;
  end;

implementation

{ THTTPResponse }

constructor THTTPResponse.Create(ResultCode: Integer; const ResultHeader,
  ResultText: string);
begin
  FResultCode := ResultCode;
  FResultHeader := ResultHeader;
  FResultText := ResultText;
end;

constructor THTTPResponse.Create(Origin: IHTTPResponse);
begin
  Create(Origin.ResultCode, Origin.ResultHeader, Origin.ResultText);
end;

function THTTPResponse.ResultCode: Integer;
begin
  Result := FResultCode;
end;

function THTTPResponse.ResultHeader: string;
begin
  Result := FResultHeader;
end;

function THTTPResponse.ResultText: string;
begin
  Result := FResultText;
end;

{ THTTPSender }

constructor THTTPSender.Create(const Method, Header, ContentType, URI: string;
  Stream: TStream);
begin
  inherited Create;
  FSender := THTTPSend.Create;
  FSender.Protocol := '1.0';
  FMethod := Method;
  FHeader := Header;
  FContentType := ContentType;
  FURI := URI;
  FStream := Stream;
end;

destructor THTTPSender.Destroy;
begin
  FSender.Free;
  inherited Destroy;
end;

function THTTPSender.Send: IHTTPResponse;
begin
  FSender.Clear;
  FSender.Headers.Add(FHeader);
  FSender.MimeType := FContentType;
  if Assigned(FStream) then
    FSender.Document.LoadFromStream(FStream);
  FSender.HTTPMethod(FMethod, FURI);
  Result := THTTPResponse.Create(
    FSender.ResultCode,
    FSender.Headers.Text,
    FSender.ResultString);
end;

end.
