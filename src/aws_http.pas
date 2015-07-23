{
    AWS
    Copyright (C) 2013-2015 Marcos Douglas - mdbs99

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
  ssl_openssl,
  //aws
  aws_base;

type
  IHTTPResponse = interface(IInterface)
  ['{6E7E8524-88B5-48B1-95FF-30D0DF40D8F7}']
    function ResultCode: Integer;
    function ResultHeader: string;
    function ResultText: string;
    function ResultStream: IAWSStream;
  end;

  IHTTPSender = interface(IInterface)
  ['{DF9B2674-D60C-4F40-AD6A-AE158091212D}']
    function Send: IHTTPResponse;
  end;

  THTTPResponse = class(TInterfacedObject, IHTTPResponse)
  private
    FResultCode: Integer;
    FResultHeader: string;
    FResultText: string;
    FStream: IAWSStream;
  public
    constructor Create(ResultCode: Integer; const ResultHeader, ResultText: string; Stream: IAWSStream);
    constructor Create(ResultCode: Integer; const ResultHeader, ResultText: string);
    constructor Create(Origin: IHTTPResponse);
    destructor Destroy; override;
    function ResultCode: Integer;
    function ResultHeader: string;
    function ResultText: string;
    function ResultStream: IAWSStream;
  end;

  THTTPSender = class(TInterfacedObject, IHTTPSender)
  private
    FSender: THTTPSend;
    FMethod: string;
    FHeader: string;
    FContentType: string;
    FURI: string;
    FStream: IAWSStream;
  public
    constructor Create(const Method, Header, ContentType, URI: string; Stream: IAWSStream);
    destructor Destroy; override;
    function Send: IHTTPResponse;
  end;

implementation

{ THTTPResponse }

constructor THTTPResponse.Create(ResultCode: Integer; const ResultHeader,
  ResultText: string; Stream: IAWSStream);
begin
  FResultCode := ResultCode;
  FResultHeader := ResultHeader;
  FResultText := ResultText;
  FStream := Stream;
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

function THTTPResponse.ResultStream: IAWSStream;
begin
  Result := FStream;
end;

{ THTTPSender }

constructor THTTPSender.Create(const Method, Header, ContentType, URI: string;
  Stream: IAWSStream);
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
  FStream.SaveToStream(FSender.Document);
  FSender.HTTPMethod(FMethod, FURI);
  Result := THTTPResponse.Create(
    FSender.ResultCode,
    FSender.Headers.Text,
    FSender.ResultString,
    TAWSStream.Create(FSender.Document)
  );
end;

end.
