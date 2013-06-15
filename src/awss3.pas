{
    AWS
    Copyright (C) 2013  -  Marcos Douglas B. dos Santos

    See the file LICENSE.txt, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit AwsS3;

{$i aws.inc}

interface

uses
  // rtl
  Classes, SysUtils, DOM,
  // synapse
  httpsend, synacode, synautil, ssl_openssl,
  // aws
  AwsBase;

type
  EAwsS3Error = class(EAwsError);
  TAwsS3Client = class(TAwsObject)
  private const
    S3_URL = 's3.amazonaws.com';
  private
    FHTTP: THTTPSend;
    FAccessKeyId: AnsiString;
    FSecretKey: AnsiString;
    FUseSSL: Boolean;
    procedure SetAuthHeader(AVerb, AContentMD5, AContentType,
      CanonicalizedAmzHeaders, CanonicalizedResource: string);
    procedure Send(const AVerb, ABucket, AQuery: string);
  public
    constructor Create; override;
    destructor Destroy; override;

    // BUCKET
    procedure DELETEBucket(const ABucket, ASubResources: string);
    procedure GETService;
    procedure GETBucket(const ABucket, ASubResources: string); overload;
    function HEADBucket(const ABucket: string): Boolean;
    procedure PUTBucket(const ABucket, ASubResources: string);

    // OBJECT
    procedure PUTObject(const ABucket, AContentType, AObjectName: string; AStream: TStream); overload;
    procedure PUTObject(const ABucket, AContentType, AObjectName, AFileName: string); overload;

    // Properties
    property HTTP: THTTPSend read FHTTP;
    property AccessKeyId: AnsiString read FAccessKeyId write FAccessKeyId;
    property SecretKey: AnsiString read FSecretKey write FSecretKey;
    property UseSSL: Boolean read FUseSSL write FUseSSL;
  end;

implementation

{ TAwsS3Client }

procedure TAwsS3Client.SetAuthHeader(AVerb, AContentMD5, AContentType,
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

procedure TAwsS3Client.Send(const AVerb, ABucket, AQuery: string);
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
end;

constructor TAwsS3Client.Create;
begin
  inherited Create;
  FHTTP := THTTPSend.Create;
  FHTTP.Protocol := '1.0';
end;

destructor TAwsS3Client.Destroy;
begin
  FHTTP.Free;
  inherited Destroy;
end;

procedure TAwsS3Client.DELETEBucket(const ABucket, ASubResources: string);
begin
  FHTTP.Clear;
  SetAuthHeader('DELETE', '', '', '', '/' + ABucket + ASubResources);
  Send('DELETE', ABucket, ASubResources);
end;

function TAwsS3Client.HEADBucket(const ABucket: string): Boolean;
begin
  FHTTP.Clear;
  SetAuthHeader('HEAD', '', '', '', '/' + ABucket + '/');
  Send('HEAD', ABucket, '');
  Result := FHTTP.ResultCode = 200;
end;

procedure TAwsS3Client.PUTBucket(const ABucket, ASubResources: string);
begin
  FHTTP.Clear;
  SetAuthHeader('GET', '', '', '', '/' + ABucket + ASubResources);
  Send('GET', ABucket, ASubResources);
end;

procedure TAwsS3Client.GETService;
begin
  FHTTP.Clear;
  SetAuthHeader('GET', '', '', '', '/');
  Send('GET', '', '');
end;

procedure TAwsS3Client.GETBucket(const ABucket, ASubResources: string);
begin
  FHTTP.Clear;
  SetAuthHeader('GET', '', '', '', '/' + ABucket + ASubResources);
  Send('GET', ABucket, ASubResources);
end;

procedure TAwsS3Client.PUTObject(const ABucket, AContentType, AObjectName: string;
  AStream: TStream);
begin
  FHTTP.Clear;
  SetAuthHeader('PUT', '', AContentType, '', '/' + ABucket + '/' + AObjectName);
  FHTTP.MimeType := AContentType;
  FHTTP.Document.LoadFromStream(AStream);
  Send('PUT', ABucket, '/' + AObjectName);
end;

procedure TAwsS3Client.PUTObject(const ABucket, AContentType, AObjectName,
  AFileName: string);
var
  Buf: TFileStream;
begin
  Buf := TFileStream.Create(AFileName, fmOpenRead);
  try
    PUTObject(ABucket, AContentType, AObjectName, Buf);
  finally
    Buf.Free;
  end;
end;

end.
