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
    function ResultStream: TMemoryStream;
  end;

  IHTTPSender = interface(IInterface)
    function Send: IHTTPResponse;
  end;

  THTTPResponse = class(TInterfacedObject, IHTTPResponse)
  private
    FResultCode: Integer;
    FResultHeader: string;
    FResultText: string;
    FStream: TMemoryStream;
  public
    constructor Create(ResultCode: Integer; const ResultHeader, ResultText: string; Stream: TStream);
    constructor Create(ResultCode: Integer; const ResultHeader, ResultText: string);
    constructor Create(Origin: IHTTPResponse);
    destructor Destroy; override;
    function ResultCode: Integer;
    function ResultHeader: string;
    function ResultText: string;
    function ResultStream: TMemoryStream;
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
  ResultText: string; Stream: TStream);
begin
  FResultCode := ResultCode;
  FResultHeader := ResultHeader;
  FResultText := ResultText;
  FStream := TMemoryStream.Create;
  FStream.LoadFromStream(Stream);
end;

constructor THTTPResponse.Create(ResultCode: Integer; const ResultHeader,
  ResultText: string);
begin
  Create(ResultCode, ResultHeader, ResultText, nil);
end;

constructor THTTPResponse.Create(Origin: IHTTPResponse);
begin
  Create(Origin.ResultCode, Origin.ResultHeader, Origin.ResultText, Origin.ResultStream);
end;

destructor THTTPResponse.Destroy;
begin
  FStream.Free;
  inherited Destroy;
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

function THTTPResponse.ResultStream: TMemoryStream;
begin
  Result := FStream;
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
    FSender.ResultString,
    FSender.Document);
end;

end.
