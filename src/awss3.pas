{
    AWS
    Copyright (C) 2013-2014  -  Marcos Douglas B. dos Santos

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
    function HEADBucket(const ABucket: string): Boolean;
    function GETBucket(const ABucket, ASubResources: string): Integer; overload;
    function PUTBucket(const ABucket, ASubResources: string): Integer;
    function DELETEBucket(const ABucket, ASubResources: string): Integer;

    // OBJECT
    function PUTObject(const ABucket, AContentType, AObjectName: string; AStream: TStream): Integer; overload;
    function PUTObject(const ABucket, AContentType, AObjectName, AFileName: string): Integer; overload;
    function DELETEObject(const ABucket, AObjectName: string): Integer;

    // Utilities
    function PUTFolder(const ABucket, ANewFolder: string): Integer;
    function DELETEFolder(const ABucket, AFolderName: string): Integer;

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
  Header: string;
  HeaderBase64: string;
  DateFmt: string;
begin
  DateFmt := RFC822DateTime(Now);

  Header := AVerb + #10
     + AContentMD5 + #10
     + AContentType + #10
     + DateFmt + #10
     + CanonicalizedAmzHeaders
     + CanonicalizedResource;

  HeaderBase64 := EncodeBase64(HMAC_SHA1(Header, FSecretKey));

  FHTTP.Headers.Add('Date: ' + DateFmt);
  FHTTP.Headers.Add('Authorization: AWS ' + FAccessKeyId + ':' + HeaderBase64);
end;

function TAWSS3Client.Send(const AVerb, ABucket, AQuery: string): Integer;
var
  URL: string;
begin
  URL := '';
  if FUseSSL then
    URL += 'https://'
  else
    URL += 'http://';

  if ABucket <> '' then
    URL += ABucket + '.';

  URL += S3_URL + AQuery;
  FHTTP.HTTPMethod(AVerb, URL);
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

function TAWSS3Client.HEADBucket(const ABucket: string): Boolean;
begin
  FHTTP.Clear;
  SetAuthHeader('HEAD', '', '', '', '/' + ABucket + '/');
  Send('HEAD', ABucket, '');
  Result := FHTTP.ResultCode = 200;
end;

function TAWSS3Client.GETBucket(const ABucket, ASubResources: string): Integer;
begin
  FHTTP.Clear;
  SetAuthHeader('GET', '', '', '', '/' + ABucket + '/' + ASubResources);
  Result := Send('GET', ABucket, '/' + ASubResources);
end;

function TAWSS3Client.PUTBucket(const ABucket, ASubResources: string): Integer;
begin
  FHTTP.Clear;
  SetAuthHeader('PUT', '', '', '', '/' + ABucket + '/' + ASubResources);
  Result := Send('PUT', ABucket, '/' + ASubResources);
end;

function TAWSS3Client.DELETEBucket(const ABucket, ASubResources: string): Integer;
begin
  FHTTP.Clear;
  SetAuthHeader('DELETE', '', '', '', '/' + ABucket + ASubResources);
  Result := Send('DELETE', ABucket, ASubResources);
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

function TAWSS3Client.DELETEObject(const ABucket, AObjectName: string): Integer;
begin
  { TODO: not working yet }
  FHTTP.Clear;
  SetAuthHeader('DELETE', '', '', '', '/' + ABucket + '/' + AObjectName);
  Result := Send('DELETE', ABucket, '/' + AObjectName);
end;

function TAWSS3Client.PUTFolder(const ABucket, ANewFolder: string): Integer;
var
  Buf: TMemoryStream;
begin
  Buf := TMemoryStream.Create;
  try
    // hack to synapse add Content-Length
    Buf.WriteBuffer('', 1);
    Result := PUTObject(ABucket, '', ANewFolder + '/', Buf);
  finally
    Buf.Free;
  end;
end;

function TAWSS3Client.DELETEFolder(const ABucket, AFolderName: string): Integer;
begin
  Result := DELETEObject(ABucket, AFolderName + '/');
end;

end.
