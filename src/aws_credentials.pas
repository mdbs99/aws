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
  dateutils,
  //synapse
  synacode,
  synautil,
  synacrypt,
  //terceiros
  SynCrypto,
  //aws
  aws_http;


type

  IAWSSignatureHMAC256 = interface(IInterface)
  ['{9158D9A2-7ABA-4126-9F63-264E947AC60A}']
    function AccessKey: string;
    function DataStamp: string;
    function RegionName: string;
    function ServiceName: string;
    function Signature: TSHA256Digest;
  end;

  { TAWSSignatureHMAC256 }

  TAWSSignatureHMAC256 = class sealed(TInterfacedObject, IAWSSignatureHMAC256)
  private
    FAccessKey: string;
    FDataStamp: string;
    FRegionName: string;
    FServiceName: string;
  public
    constructor Create(const sAccessKey, sDataStamp, sRegionName, sServiceName: string);
    class function New(const sAccessKey, sDataStamp, sRegionName, sServiceName: string): IAWSSignatureHMAC256;
    function AccessKey: string;
    function DataStamp: string;
    function RegionName: string;
    function ServiceName: string;
    function Signature: TSHA256Digest;
  end;

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

  { TAWSSignatureVersion4 }

  TAWSSignatureVersion4 = class sealed(TAWSAbstractSignature)
  private
    function BuildHeader(const sHeader: String): String;
    procedure SignedHeaders(const sHeader: String; var sToSing, sToCanonical: String);
  public
    function Calculate(Request: IHTTPRequest): string; override;
  end;

implementation

{ TAWSSignatureHMAC256 }

constructor TAWSSignatureHMAC256.Create(const sAccessKey, sDataStamp,
  sRegionName, sServiceName: string);
begin
  inherited Create;
  FAccessKey:= sAccessKey;
  FDataStamp:= sDataStamp;
  FRegionName:= sRegionName;
  FServiceName:= sServiceName;
end;

class function TAWSSignatureHMAC256.New(const sAccessKey, sDataStamp,
  sRegionName, sServiceName: string): IAWSSignatureHMAC256;
begin
  Result := Create(sAccessKey, sDataStamp, sRegionName, sServiceName);
end;

function TAWSSignatureHMAC256.AccessKey: string;
begin
  Result := FAccessKey;
end;

function TAWSSignatureHMAC256.DataStamp: string;
begin
  Result := FDataStamp;
end;

function TAWSSignatureHMAC256.RegionName: string;
begin
  Result := FRegionName;
end;

function TAWSSignatureHMAC256.ServiceName: string;
begin
  Result := FServiceName;
end;

function TAWSSignatureHMAC256.Signature: TSHA256Digest;
var
  oSHA256: TSHA256Digest;
begin
  HMAC_SHA256(UTF8Encode('AWS4'+FAccessKey), UTF8Encode(FDataStamp), oSHA256);
  HMAC_SHA256(oSHA256, UTF8Encode(FRegionName), oSHA256);
  HMAC_SHA256(oSHA256, UTF8Encode(FServiceName), oSHA256);
  HMAC_SHA256(oSHA256, UTF8Encode('aws4_request'), oSHA256);
  Result := oSHA256;
end;

{ TAWSSignatureVersion4 }

function TAWSSignatureVersion4.BuildHeader(const sHeader: String): String;
var
  i: Integer;
  sList: TStringList;
begin
  sList := TStringList.Create;
  sList.Text:=sHeader;
  sList.LineBreak:=#10;
  sList.NameValueSeparator:=':';
  sList.Sorted:=True;
  sList.Sort;
  Result := '';
  for i := 1 to sList.Count - 1 do
    Result := Result + sList[i]+#10;

end;

procedure TAWSSignatureVersion4.SignedHeaders(const sHeader: String; var sToSing, sToCanonical: String);
var
  i: Integer;
  sList: TStringList;
  sName, sValue: String;
begin
  sList := TStringList.Create;
  sList.Text:=sHeader;
  sList.LineBreak:=#10;
  sList.NameValueSeparator:=':';
  sList.Sorted:=True;
  sList.Sort;
  for i := 1 to sList.Count - 1 do
    begin
      sList.GetNameValue(i, sName, sValue);
      sToSing := sToSing + LowerCase(sName) + ';';
      sToCanonical := sToCanonical + LowerCase(sName)+':'+sValue+#10;
    end;
  system.Delete(sToSing, Length(sToSing), 1);
end;

function TAWSSignatureVersion4.Calculate(Request: IHTTPRequest): string;
const
  sAlgoritimo = 'AWS4-HMAC-SHA256';
  sTipoReq = 'aws4_request';
var
  sHeader: string;
  sCredencial: String;
  sEscopo: String;
  sDateFmt: String;
  sAwsDateTime: String;
  sMetodo: String;
  sCanonical: String;
  sCanonicalURI: String;
  sCanonicalQuery: String;
  sCanonicalHeaders: String;
  sSignedHeaders: String;
  sPayLoadHash: String;
  sCanonicalRequest: String;
  sStringToSign: String;
  sSignature: String;
  sAuthorizationHeader: String;
  oAssinatura: TSHA256Digest;
  oSHA256: TSHA256Digest;
begin
  sDateFmt:= FormatDateTime('yyyymmdd', IncHour(Now, 3));
  sAwsDateTime:= FormatDateTime('yyyymmdd', IncHour(Now, 3))+'T'+FormatDateTime('hhnnss', IncHour(Now, 3))+'Z';
  sMetodo:= Request.Method;
  sCanonicalURI:=EncodeTriplet(Request.Resource, '%', [':']);
  sCanonicalQuery:='';

  sHeader := 'Host:' + Request.Domain + #10 ;
  sCanonicalHeaders:= 'X-Amz-Date:' + sAwsDateTime + #10 + Request.CanonicalizedAmzHeaders + #10;
  SignedHeaders(sHeader+sCanonicalHeaders, sSignedHeaders, sCanonical);

  sPayLoadHash:= SHA256(Request.SubResource);

  sCanonicalRequest := sMetodo + #10 + sCanonicalURI + #10 + sCanonicalQuery + #10
                    + sCanonical + #10 + sSignedHeaders + #10 + sPayLoadHash;

  sCredencial:= sDateFmt + '/' + Request.ContentMD5 + '/' + Request.CanonicalizedResource + '/' + sTipoReq;
  sEscopo:= Credentials.AccessKeyId + '/' + sCredencial;
  sStringToSign := sAlgoritimo + #10 +  sAwsDateTime + #10 + sCredencial + #10 + SHA256( UTF8Encode(sCanonicalRequest));

  oAssinatura:=TAWSSignatureHMAC256.New(Credentials.SecretKey, sDateFmt, Request.ContentMD5, Request.CanonicalizedResource).Signature;

  HMAC_SHA256(oAssinatura, UTF8Encode(sStringToSign), oSHA256);
  sSignature := SHA256DigestToString(oSHA256);

  sAuthorizationHeader := 'Authorization:' + sAlgoritimo + ' ' + 'Credential=' + sEscopo + ', ' +
                         'SignedHeaders=' + sSignedHeaders + ', ' + 'Signature=' + sSignature;

  Result := BuildHeader(sCanonicalHeaders)
            + sAuthorizationHeader
            ;

end;

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
     + DateFmt + #10;

  if Request.CanonicalizedAmzHeaders <> EmptyStr then
    H := H + Request.CanonicalizedAmzHeaders + #10;

  H := H + Request.CanonicalizedResource;

  Result := 'Date: ' + DateFmt + #10;

  if Request.CanonicalizedAmzHeaders <> EmptyStr then
    Result := Result + Request.CanonicalizedAmzHeaders + #10;

  Result := Result + 'Authorization: AWS '
          + Credentials.AccessKeyId + ':'
          + EncodeBase64(HMAC_SHA1(H, Credentials.SecretKey))
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
