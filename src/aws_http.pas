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
  //synapse
  httpsend,
  synacode,
  synautil,
  ssl_openssl,
  //aws
  aws_sys,
  aws_auth;

type
  TResultCode = type Integer;

  IHttpResult = interface(IDisposable)
    function GetCode: TResultCode;
    function GetBody: string;
  end;

  IHttpSender = interface(IDisposable)
    procedure Send(out Res: IHttpResult);
  end;

  IHttpClient = interface(IDisposable)
    procedure Send(const Method, Resource, SubResource, ContentType, ContentMD5,
      CanonicalizedAmzHeaders, CanonicalizedResource: string; out Res: IHttpResult);
  end;

  THttpResult = class sealed(TInterfacedObject, IHttpResult)
  private
    FCode: Integer;
    FBody: string;
  public
    constructor Create(Code: Integer; const Body: string);
    function GetCode: TResultCode;
    function GetBody: string;
  end;

  THttpSender = class sealed(IHttpSender)
  private
    FSender: THTTPSend;
    FMethod: string;
    FHeader: string;
    FContentType: string;
    FURI: string;
  public
    constructor Create(const Method, Header, ContentType, URI: string); reintroduce;
    destructor Destroy; override;
    procedure Send(out Res: IHttpResult);
  end;

  THttpClient = class sealed(IHttpClient)
  private const
    AWS_URI = 's3.amazonaws.com';
  private
    FCredentials: ICredentials;
  protected
    function MakeURI(const Resource, SubResource: string): string;
    function MakeAuthHeader(const Method, ContentType, ContentMD5,
      CanonicalizedAmzHeaders, CanonicalizedResource: string): string;
  public
    constructor Create(Credentials: ICredentials); reintroduce;
    procedure Send(const Method, Resource, SubResource, ContentType, ContentMD5,
      CanonicalizedAmzHeaders, CanonicalizedResource: string; out Res: IHttpResult);
  end;

implementation

{ THttpResult }

constructor THttpResult.Create(Code: Integer; const Body: string);
begin
  FCode := Code;
  FBody := Body;
end;

function THttpResult.GetCode: TResultCode;
begin
  Result := FCode;
end;

function THttpResult.GetBody: string;
begin
  Result := FBody;
end;

{ THttpSender }

constructor THttpSender.Create(const Method, Header, ContentType, URI: string);
begin
  inherited Create;
  FMethod := Method;
  FHeader := Header;
  FContentType := ContentType;
  FURI := URI;
  FSender := THTTPSend.Create;
  FSender.Protocol := '1.0';
end;

destructor THttpSender.Destroy;
begin
  FSender.Free;
  inherited Destroy;
end;

procedure THttpSender.Send(out Res: IHttpResult);
begin
  FSender.Clear;
  FSender.Headers.Add(FHeader);
  if FContentType <> '' then
    FSender.MimeType := FContentType;
  FSender.HTTPMethod(FMethod, FURI);
  Res := THttpResult.Create(FSender.ResultCode, FSender.ResultString);
end;

{ THttpClient }

function THttpClient.MakeURI(const Resource, SubResource: string): string;
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

function THttpClient.MakeAuthHeader(const Method, ContentType, ContentMD5,
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

constructor THttpClient.Create(Credentials: ICredentials);
begin
  inherited Create;
  FCredentials := Credentials;
end;

procedure THttpClient.Send(const Method, Resource, SubResource, ContentType,
  ContentMD5, CanonicalizedAmzHeaders, CanonicalizedResource: string; out
  Res: IHttpResult);
var
  H: string;
begin
  H := MakeAuthHeader(
    Method, ContentType, ContentMD5,
    CanonicalizedAmzHeaders, CanonicalizedResource);
  with THttpSender.Create(Method, H, ContentType, MakeURI(Resource, SubResource)) do
  try
    Send(Res);
  finally
    Free;
  end;
end;

end.
