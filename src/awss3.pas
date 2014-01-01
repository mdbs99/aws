{
    AWS
    Copyright (C) 2013  -  Marcos Douglas B. dos Santos

    See the file LICENSE.txt, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit AWSS3;

{$i aws.inc}

interface

uses
  // rtl
  Classes, SysUtils,
  // synapse
  httpsend, synacode, synautil, ssl_openssl,
  // aws
  AWSBase;

type
  EAWSS3Error = class(EAWSError);

  { TAWSS3Client }

  TAWSS3Client = class(TAWSObject)
  private const
    S3_URL = 's3.amazonaws.com';
  private
    FHTTP: THTTPSend;
    FAccessKeyId: AnsiString;
    FSecretKey: AnsiString;
    FUseSSL: Boolean;
    procedure SetAuthHeader(AVerb, AContentMD5, AContentType,
      CanonicalizedAmzHeaders, CanonicalizedResource: string);
    function Send(const AVerb, ABucket, AQuery: string): Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    // SERVICE
    function GETService: Integer;

    // BUCKET
    function DELETEBucket(const ABucket, ASubResources: string): Integer;
    function GETBucket(const ABucket, ASubResources: string): Integer; overload;
    function HEADBucket(const ABucket: string): Boolean;
    function PUTBucket(const ABucket, ASubResources: string): Integer;

    // OBJECT
    function DELETEObject(const ABucket, AObjectName: string): Integer;
    function PUTObject(const ABucket, AContentType, AObjectName: string; AStream: TStream): Integer; overload;
    function PUTObject(const ABucket, AContentType, AObjectName, AFileName: string): Integer; overload;

    // Properties
    property HTTP: THTTPSend read FHTTP;
    property AccessKeyId: AnsiString read FAccessKeyId write FAccessKeyId;
    property SecretKey: AnsiString read FSecretKey write FSecretKey;
    property UseSSL: Boolean read FUseSSL write FUseSSL;
  end;

implementation

{ TAWSS3Client }

procedure TAWSS3Client.SetAuthHeader(AVerb, AContentMD5, AContentType,
  CanonicalizedAmzHeaders, CanonicalizedResource: string);
var
  H, Base64, DT: string;
begin
  DT := RFC822DateTime(Now);

  H := AVerb + #10
     + AContentMD5 + #10
     + AContentType + #10
     + DT + #10
     + CanonicalizedAmzHeaders
     + CanonicalizedResource;

  Base64 := EncodeBase64(HMAC_SHA1(H, FSecretKey));

  FHTTP.Headers.Add('Date: ' + DT);
  FHTTP.Headers.Add('Authorization: AWS ' + FAccessKeyId + ':' + Base64);
end;

function TAWSS3Client.Send(const AVerb, ABucket, AQuery: string): Integer;
var
  Url: string;
begin
  Url := '';
  if FUseSSL then
    Url += 'https://'
  else
    Url += 'http://';

  if ABucket <> '' then
    Url += ABucket + '.';

  Url += S3_URL + AQuery;
  FHTTP.HTTPMethod(AVerb, Url);
  Result := FHTTP.ResultCode;
end;

constructor TAWSS3Client.Create;
begin
  inherited Create;
  FHTTP := THTTPSend.Create;
  FHTTP.Protocol := '1.0';
end;

destructor TAWSS3Client.Destroy;
begin
  FHTTP.Free;
  inherited Destroy;
end;

function TAWSS3Client.GETService: Integer;
begin
  FHTTP.Clear;
  SetAuthHeader('GET', '', '', '', '/');
  Result := Send('GET', '', '');
end;

function TAWSS3Client.DELETEBucket(const ABucket, ASubResources: string): Integer;
begin
  FHTTP.Clear;
  SetAuthHeader('DELETE', '', '', '', '/' + ABucket + ASubResources);
  Result := Send('DELETE', ABucket, ASubResources);
end;

function TAWSS3Client.GETBucket(const ABucket, ASubResources: string): Integer;
begin
  FHTTP.Clear;
  SetAuthHeader('GET', '', '', '', '/' + ABucket + ASubResources);
  Result := Send('GET', ABucket, ASubResources);
end;

function TAWSS3Client.HEADBucket(const ABucket: string): Boolean;
begin
  FHTTP.Clear;
  SetAuthHeader('HEAD', '', '', '', '/' + ABucket + '/');
  Send('HEAD', ABucket, '');
  Result := FHTTP.ResultCode = 200;
end;

function TAWSS3Client.PUTBucket(const ABucket, ASubResources: string): Integer;
begin
  FHTTP.Clear;
  SetAuthHeader('PUT', '', '', '', '/' + ABucket + ASubResources);
  Result := Send('PUT', ABucket, ASubResources);
end;

function TAWSS3Client.DELETEObject(const ABucket, AObjectName: string): Integer;
begin
  FHTTP.Clear;
  SetAuthHeader('DELETE', '', '', '', '/' + ABucket + '/' + AObjectName);
  Result := Send('DELETE', ABucket, '/' + AObjectName);
end;

function TAWSS3Client.PUTObject(const ABucket, AContentType,
  AObjectName: string; AStream: TStream): Integer;
begin
  FHTTP.Clear;
  SetAuthHeader('PUT', '', AContentType, '', '/' + ABucket + '/' + AObjectName);
  FHTTP.MimeType := AContentType;
  FHTTP.Document.LoadFromStream(AStream);
  Result := Send('PUT', ABucket, '/' + AObjectName);
end;

function TAWSS3Client.PUTObject(const ABucket, AContentType, AObjectName,
  AFileName: string): Integer;
var
  Buf: TFileStream;
begin
  Buf := TFileStream.Create(AFileName, fmOpenRead);
  try
    Result := PUTObject(ABucket, AContentType, AObjectName, Buf);
  finally
    Buf.Free;
  end;
end;

end.
