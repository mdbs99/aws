{
    AWS
    Copyright (C) 2013-2015 Marcos Douglas - mdbs99

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
  classes,
  //synapse
  synacode,
  synautil,
  //aws
  aws_http;

const
  AWS_URI = 's3.amazonaws.com';

type
  IAWSResponse = IHTTPResponse;

  IAWSCredentials = interface(IInterface)
  ['{AC6EA523-F2FF-4BD0-8C87-C27E9846FA40}']
    function GetAccessKeyId: string;
    function GetSecretKey: string;
    function UseSSL: Boolean;
  end;

  IAWSRequest = interface(IInterface)
  ['{12744C05-22B6-45BF-B47A-49813F6B64B6}']
    function Method: string;
    function Name: string;
    function Resource: string;
    function SubResource: string;
    function ContentType: string;
    function ContentMD5: string;
    function CanonicalizedAmzHeaders: string;
    function CanonicalizedResource: string;
    function Stream: TStream;
    function ToString: string;
  end;

  IAWSClient = interface(IInterface)
  ['{9CE71A17-9ADC-4FC1-96ED-8E9C704A988C}']
    function Send(Request: IAWSRequest): IAWSResponse;
  end;

  TAWSResponse = THTTPResponse;

  TAWSCredentials = class(TInterfacedObject, IAWSCredentials)
  private
    FAccessKeyId: string;
    FSecretKey: string;
    FSSL: Boolean;
  public
    constructor Create(const AccessKeyId, SecretKey: string; UseSSL: Boolean); reintroduce;
    function GetAccessKeyId: string;
    function GetSecretKey: string;
    function UseSSL: Boolean;
  end;

  TAWSRequest = class(TInterfacedObject, IAWSRequest)
  private
    FMethod: string;
    FName: string;
    FResource: string;
    FSubResource: string;
    FContentType: string;
    FContentMD5: string;
    FCanonicalizedAmzHeaders: string;
    FCanonicalizedResource: string;
    FStream: TStream;
  public
    constructor Create(const Method, AName, Resource, SubResource, ContentType, ContentMD5,
      CanonicalizedAmzHeaders, CanonicalizedResource: string; Stream: TStream);
    constructor Create(const Method, AName, Resource, SubResource, ContentType, ContentMD5,
      CanonicalizedAmzHeaders, CanonicalizedResource: string);
    constructor Create(const Method, AName, Resource, SubResource, CanonicalizedResource: string);
    constructor Create(const Method, AName, Resource, CanonicalizedResource: string);
    constructor Create(const Method, AName, Resource, CanonicalizedResource: string; Stream: TStream);
    constructor Create(const Method, AName, CanonicalizedResource: string);
    function Method: string;
    function Name: string;
    function Resource: string;
    function SubResource: string;
    function ContentType: string;
    function ContentMD5: string;
    function CanonicalizedAmzHeaders: string;
    function CanonicalizedResource: string;
    function Stream: TStream;
    function ToString: string; override;
  end;

  TAWSClient = class sealed(TInterfacedObject, IAWSClient)
  private
    FCredentials: IAWSCredentials;
  protected
    function MakeURI(const AName, Query: string): string;
    function MakeAuthHeader(const Method, ContentType, ContentMD5,
      CanonicalizedAmzHeaders, CanonicalizedResource: string): string;
  public
    constructor Create(const Credentials: IAWSCredentials);
    function Send(Request: IAWSRequest): IAWSResponse;
  end;

implementation

{ TAWSCredentials }

constructor TAWSCredentials.Create(const AccessKeyId, SecretKey: string;
  UseSSL: Boolean);
begin
  FAccessKeyId := AccessKeyId;
  FSecretKey := SecretKey;
  FSSL := UseSSL;
end;

function TAWSCredentials.GetAccessKeyId: string;
begin
  Result := FAccessKeyId;
end;

function TAWSCredentials.GetSecretKey: string;
begin
  Result := FSecretKey;
end;

function TAWSCredentials.UseSSL: Boolean;
begin
  Result := FSSL;
end;

{ TAWSRequest }

constructor TAWSRequest.Create(const Method, AName, Resource, SubResource,
  ContentType, ContentMD5, CanonicalizedAmzHeaders,
  CanonicalizedResource: string; Stream: TStream);
begin
  FMethod := Method;
  FName := AName;
  FResource := Resource;
  FSubResource := SubResource;
  FContentType := ContentType;
  FContentMD5 := ContentMD5;
  FCanonicalizedAmzHeaders := CanonicalizedAmzHeaders;
  FCanonicalizedResource := CanonicalizedResource;
  FStream := Stream;
end;

constructor TAWSRequest.Create(const Method, AName, Resource, SubResource,
  ContentType, ContentMD5, CanonicalizedAmzHeaders,
  CanonicalizedResource: string);
begin
  Create(
    Method, AName, Resource, SubResource, ContentType,
    ContentMD5, CanonicalizedAmzHeaders, CanonicalizedResource, nil);
end;

constructor TAWSRequest.Create(const Method, AName, Resource, SubResource,
  CanonicalizedResource: string);
begin
  Create(Method, AName, Resource, SubResource, '', '', '', CanonicalizedResource, nil);
end;

constructor TAWSRequest.Create(const Method, AName, Resource,
  CanonicalizedResource: string);
begin
  Create(Method, AName, Resource, '', '', '', '', CanonicalizedResource, nil);
end;

constructor TAWSRequest.Create(const Method, AName, Resource,
  CanonicalizedResource: string; Stream: TStream);
begin
  Create(Method, AName, Resource, '', '', '', '', CanonicalizedResource, Stream);
end;

constructor TAWSRequest.Create(const Method, AName, CanonicalizedResource: string);
begin
  Create(Method, AName, '', '', '', '', '', CanonicalizedResource, nil);
end;

function TAWSRequest.Method: string;
begin
  Result := FMethod;
end;

function TAWSRequest.Name: string;
begin
  Result := FName;
end;

function TAWSRequest.Resource: string;
begin
  Result := FResource;
end;

function TAWSRequest.SubResource: string;
begin
  Result := FSubResource;
end;

function TAWSRequest.ContentType: string;
begin
  Result := FContentType;
end;

function TAWSRequest.ContentMD5: string;
begin
  Result := FContentMD5;
end;

function TAWSRequest.CanonicalizedAmzHeaders: string;
begin
  Result := FCanonicalizedAmzHeaders;
end;

function TAWSRequest.CanonicalizedResource: string;
begin
  Result := FCanonicalizedResource;
end;

function TAWSRequest.Stream: TStream;
begin
  Result := FStream;
end;

function TAWSRequest.ToString: string;
begin
  with TStringList.Create do
  try
    Add('Method=' + FMethod);
    Add('Resource=' + FResource);
    Add('SubResource=' + FSubResource);
    Add('ContentType=' + FContentType);
    Add('ContentMD5=' + FContentMD5);
    Add('CanonicalizedAmzHeaders=' + FCanonicalizedAmzHeaders);
    Add('CanonicalizedResource=' + FCanonicalizedResource);
    Result := Text;
  finally
    Free;
  end;
end;

{ TAWSClient }

function TAWSClient.MakeURI(const AName, Query: string): string;
begin
  Result := '';
  if FCredentials.UseSSL then
    Result += 'https://'
  else
    Result += 'http://';
  if AName <> '' then
    Result += AName + '.';
  Result += AWS_URI + Query;
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

function TAWSClient.Send(Request: IAWSRequest): IAWSResponse;
var
  H: string;
  Snd: IHTTPSender;
begin
  H := MakeAuthHeader(
    Request.Method, Request.ContentType, Request.ContentMD5,
    Request.CanonicalizedAmzHeaders, Request.CanonicalizedResource);
  Snd := THTTPSender.Create(
    Request.Method, H, Request.ContentType,
    MakeURI(Request.Name, Request.Resource),
    Request.Stream);
  Result := Snd.Send;
end;

end.
