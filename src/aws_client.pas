{
    AWS
    Copyright (C) 2013-2014 by mdbs99

    See the file LICENSE.txt, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit aws_client;

{$i aws.inc}

interface

uses
  //rtl
  sysutils,
  //synapse
  httpsend,
  synacode,
  synautil,
  ssl_openssl,
  //aws
  aws_auth;

type
  TResultCode = type Integer;

  IHTTPResult = interface(IInterface)
    function GetCode: TResultCode;
    function GetBody: string;
  end;

  IHTTPSender = interface(IInterface)
    function Send: IHTTPResult;
  end;

  IAWSClient = interface(IInterface)
    function Send(const Method, Resource, SubResource, ContentType, ContentMD5,
      CanonicalizedAmzHeaders, CanonicalizedResource: string): IHTTPResult;
  end;

  THTTPResult = class sealed(TInterfacedObject, IHTTPResult)
  private
    FCode: Integer;
    FBody: string;
  public
    constructor Create(Code: Integer; const Body: string);
    function GetCode: TResultCode;
    function GetBody: string;
  end;

  THTTPSender = class sealed(TInterfacedObject, IHTTPSender)
  private
    FSender: THTTPSend;
    FMethod: string;
    FHeader: string;
    FContentType: string;
    FURI: string;
  public
    constructor Create(const Method, Header, ContentType, URI: string); reintroduce;
    destructor Destroy; override;
    function Send: IHTTPResult;
  end;

  TAWSClient = class sealed(TInterfacedObject, IAWSClient)
  private const
    AWS_URI = 's3.amazonaws.com';
  private
    FCredentials: IAWSCredentials;
  protected
    function MakeURI(const Resource, SubResource: string): string;
    function MakeAuthHeader(const Method, ContentType, ContentMD5,
      CanonicalizedAmzHeaders, CanonicalizedResource: string): string;
  public
    constructor Create(const Credentials: IAWSCredentials);
    function Send(const Method, Resource, SubResource, ContentType, ContentMD5,
      CanonicalizedAmzHeaders, CanonicalizedResource: string): IHTTPResult;
  end;

implementation

{ THTTPResult }

constructor THTTPResult.Create(Code: Integer; const Body: string);
begin
  FCode := Code;
  FBody := Body;
end;

function THTTPResult.GetCode: TResultCode;
begin
  Result := FCode;
end;

function THTTPResult.GetBody: string;
begin
  Result := FBody;
end;

{ THTTPSender }

constructor THTTPSender.Create(const Method, Header, ContentType, URI: string);
begin
  inherited Create;
  FMethod := Method;
  FHeader := Header;
  FContentType := ContentType;
  FURI := URI;
  FSender := THTTPSend.Create;
  FSender.Protocol := '1.0';
end;

destructor THTTPSender.Destroy;
begin
  FSender.Free;
  inherited Destroy;
end;

function THTTPSender.Send: IHTTPResult;
begin
  FSender.Clear;
  FSender.Headers.Add(FHeader);
  if FContentType <> '' then
    FSender.MimeType := FContentType;
  FSender.HTTPMethod(FMethod, FURI);
  Result := THTTPResult.Create(FSender.ResultCode, FSender.ResultString);
end;

{ TAWSClient }

function TAWSClient.MakeURI(const Resource, SubResource: string): string;
begin
  Result := '';
  if FCredentials.IsSSL then
    Result += 'https://'
  else
    Result += 'http://';
  if Resource <> '' then
    Result += Resource + '.';
  Result += AWS_URI + SubResource;
end;

function TAWSClient.MakeAuthHeader(const Method, ContentType, ContentMD5,
  CanonicalizedAmzHeaders, CanonicalizedResource: string): string;
var
  H: string;
  DateFmt: string;
begin
  DateFmt := RFC822DateTime(Now);
  H := Method + #10
     + ContentMD5 + #10
     + ContentType + #10
     + DateFmt + #10
     + CanonicalizedAmzHeaders
     + CanonicalizedResource;
  Result := 'Date: ' + DateFmt + #10
          + 'Authorization: AWS '
          + FCredentials.GetAccessKeyId + ':' + EncodeBase64(HMAC_SHA1(H, FCredentials.GetSecretKey));
end;

constructor TAWSClient.Create(const Credentials: IAWSCredentials);
begin
  inherited Create;
  FCredentials := Credentials;
end;

function TAWSClient.Send(const Method, Resource, SubResource, ContentType,
  ContentMD5, CanonicalizedAmzHeaders, CanonicalizedResource: string): IHTTPResult;
var
  H: string;
  Snd: IHTTPSender;
begin
  H := MakeAuthHeader(
    Method, ContentType, ContentMD5,
    CanonicalizedAmzHeaders, CanonicalizedResource);
  Snd := THTTPSender.Create(
    Method, H, ContentType, MakeURI(Resource, SubResource));
  Result := Snd.Send;
end;

end.
