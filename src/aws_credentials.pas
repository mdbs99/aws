{
    AWS
    Copyright (C) 2013-2016 Marcos Douglas - mdbs99

    See the file LICENSE.txt, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit aws_credentials;

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
  IAWSCredentials = interface(IInterface)
  ['{AC6EA523-F2FF-4BD0-8C87-C27E9846FA40}']
    function AccessKeyId: string;
    function SecretKey: string;
    function UseSSL: Boolean;
  end;

  IAWSSignature = interface
    function Credentials: IAWSCredentials;
    function Calculate(Request: IHTTPRequest): string;
  end;

  TAWSCredentials = class sealed(TInterfacedObject, IAWSCredentials)
  private
    FAccessKeyId: string;
    FSecretKey: string;
    FSSL: Boolean;
  public
    constructor Create(
      const AccessKeyId, SecretKey: string;
      UseSSL: Boolean); reintroduce;
    class function New(
      const AccessKeyId, SecretKey: string;
      UseSSL: Boolean): IAWSCredentials;
    function AccessKeyId: string;
    function SecretKey: string;
    function UseSSL: Boolean;
  end;

  TAWSAbstractSignature = class abstract(TInterfacedObject, IAWSSignature)
  strict private
    FCredentials: IAWSCredentials;
  public
    constructor Create(Credentials: IAWSCredentials);
    class function New(Credentials: IAWSCredentials): IAWSSignature;
    function Credentials: IAWSCredentials;
    function Calculate(Request: IHTTPRequest): string; virtual; abstract;
  end;

  TAWSSignatureVersion1 = class sealed(TAWSAbstractSignature)
  public
    function Calculate(Request: IHTTPRequest): string; override;
  end;

  TAWSSignatureVersion3 = class sealed(TAWSAbstractSignature)
  public
    function Calculate(Request: IHTTPRequest): string; override;
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

{ TAWSAbstractSignature }

constructor TAWSAbstractSignature.Create(Credentials: IAWSCredentials);
begin
  inherited Create;
  FCredentials := Credentials;
end;

class function TAWSAbstractSignature.New(
  Credentials: IAWSCredentials): IAWSSignature;
begin
  Result := Create(Credentials);
end;

function TAWSAbstractSignature.Credentials: IAWSCredentials;
begin
  Result := FCredentials;
end;

{ TAWSSignatureVersion1 }

function TAWSSignatureVersion1.Calculate(Request: IHTTPRequest): string;
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
          + Credentials.AccessKeyId + ':'
          + EncodeBase64(HMAC_SHA1(H, Credentials.SecretKey));
end;

{ TAWSSignatureVersion3 }

function TAWSSignatureVersion3.Calculate(Request: IHTTPRequest): string;
var
  DateFmt: string;
begin
  DateFmt := RFC822DateTime(Now);
  Result := 'Date: ' + DateFmt + #10
          + 'Host: ' + Request.Domain + #10
          + 'X-Amzn-Authorization: '
          + 'AWS3-HTTPS AWSAccessKeyId=' + Credentials.AccessKeyId + ','
          + 'Algorithm=HMACSHA1,Signature='+EncodeBase64(HMAC_SHA1(DateFmt, Credentials.SecretKey));
end;

end.
