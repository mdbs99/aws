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
  synacode,
  synautil,
  //aws
  aws_auth,
  aws_http;

type
  IAWSResult = interface(IHTTPResult)
    function Success: Boolean;
  end;

  IAWSClient = interface(IInterface)
    function Send(const SuccessCode: Integer;
      const Method, Resource, SubResource, ContentType, ContentMD5,
      CanonicalizedAmzHeaders, CanonicalizedResource: string): IAWSResult;
  end;

  TAWSResult = class(THTTPResult, IAWSResult)
  private
    FSuccess: Boolean;
  public
    constructor Create(const SuccessCode: Integer; Origin: IHTTPResult);
    function Success: Boolean;
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
    function Send(const SuccessCode: Integer;
      const Method, Resource, SubResource, ContentType, ContentMD5,
      CanonicalizedAmzHeaders, CanonicalizedResource: string): IAWSResult;
  end;

implementation

{ IAWSResult }

constructor TAWSResult.Create(const SuccessCode: Integer; Origin: IHTTPResult);
begin
  inherited Create(Origin);
  FSuccess := SuccessCode = Origin.ResultCode;
end;

function TAWSResult.Success: Boolean;
begin
  Result := FSuccess;
end;

{ TAWSClient }

function TAWSClient.MakeURI(const Resource, SubResource: string): string;
begin
  Result := '';
  if FCredentials.UseSSL then
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

function TAWSClient.Send(const SuccessCode: Integer; const Method, Resource,
  SubResource, ContentType, ContentMD5, CanonicalizedAmzHeaders,
  CanonicalizedResource: string): IAWSResult;
var
  Hdr: string;
  Snd: IHTTPSender;
begin
  Hdr := MakeAuthHeader(
    Method, ContentType, ContentMD5,
    CanonicalizedAmzHeaders, CanonicalizedResource);
  Snd := THTTPSender.Create(
    Method, Hdr, ContentType, MakeURI(Resource, SubResource));
  Result := TAWSResult.Create(SuccessCode, Snd.Send);
end;

end.
