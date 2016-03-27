{
    AWS
    Copyright (C) 2013-2016 Marcos Douglas - mdbs99

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

type
  IAWSRequest = IHTTPRequest;
  IAWSResponse = IHTTPResponse;
  TAWSRequest = THTTPRequest;
  TAWSResponse = THTTPResponse;

  IAWSCredentials = interface(IInterface)
  ['{AC6EA523-F2FF-4BD0-8C87-C27E9846FA40}']
    function AccessKeyId: string;
    function SecretKey: string;
    function UseSSL: Boolean;
  end;

  IAWSClient = interface(IInterface)
  ['{9CE71A17-9ADC-4FC1-96ED-8E9C704A988C}']
    function Send(Request: IAWSRequest): IAWSResponse;
  end;

  TAWSCredentials = class sealed(TInterfacedObject, IAWSCredentials)
  private
    FAccessKeyId: string;
    FSecretKey: string;
    FSSL: Boolean;
  public
    constructor Create(const AccessKeyId, SecretKey: string; UseSSL: Boolean); reintroduce;
    class function New(const AccessKeyId, SecretKey: string; UseSSL: Boolean): IAWSCredentials;
    function AccessKeyId: string;
    function SecretKey: string;
    function UseSSL: Boolean;
  end;

  TAWSClient = class sealed(TInterfacedObject, IAWSClient)
  private
    FCredentials: IAWSCredentials;
  protected
    function MakeURL(const SubDomain, Domain, Query: string): string;
    function MakeAuthHeader(Request: IAWSRequest): string;
  public
    constructor Create(Credentials: IAWSCredentials);
    class function New(Credentials: IAWSCredentials): IAWSClient;
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

class function TAWSCredentials.New(const AccessKeyId, SecretKey: string;
  UseSSL: Boolean): IAWSCredentials;
begin
  Result := Create(AccessKeyId, SecretKey, UseSSL);
end;

function TAWSCredentials.AccessKeyId: string;
begin
  Result := FAccessKeyId;
end;

function TAWSCredentials.SecretKey: string;
begin
  Result := FSecretKey;
end;

function TAWSCredentials.UseSSL: Boolean;
begin
  Result := FSSL;
end;

{ TAWSClient }

function TAWSClient.MakeURL(const SubDomain, Domain, Query: string): string;
begin
  Result := '';
  if FCredentials.UseSSL then
    Result += 'https://'
  else
    Result += 'http://';
  if SubDomain <> '' then
    Result += SubDomain + '.';
  Result += Domain + Query;
end;

function TAWSClient.MakeAuthHeader(Request: IAWSRequest): string;
var
  H: string;
  DateFmt: string;
begin
  DateFmt := RFC822DateTime(Now);
  H := Request.Method + #10
     + Request.ContentMD5 + #10
     + Request.ContentType + #10
     + DateFmt + #10
     + Request.CanonicalizedAmzHeaders
     + Request.CanonicalizedResource;
  Result := 'Date: ' + DateFmt + #10
          + 'Authorization: AWS '
          + FCredentials.AccessKeyId + ':'
          + EncodeBase64(HMAC_SHA1(H, FCredentials.SecretKey));
end;

constructor TAWSClient.Create(Credentials: IAWSCredentials);
begin
  inherited Create;
  FCredentials := Credentials;
end;

class function TAWSClient.New(Credentials: IAWSCredentials): IAWSClient;
begin
  Result := Create(Credentials);
end;

function TAWSClient.Send(Request: IAWSRequest): IAWSResponse;
begin
  Result := THTTPSender.New(
    Request.Method,
    MakeAuthHeader(Request),
    Request.ContentType,
    MakeURL(Request.SubDomain, Request.Domain, Request.Resource),
    Request.Stream
  )
  .Send;
end;

end.
