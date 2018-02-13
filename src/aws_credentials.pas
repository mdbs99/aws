{
    AWS
    Copyright (c) 2013-2018 Marcos Douglas B. Santos

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
    constructor Create(const AccessKey, DataStamp, RegionName, ServiceName: string);
    class function New(const AccessKey, DataStamp, RegionName, ServiceName: string): IAWSSignatureHMAC256;
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
    function BuildHeader(const Header: String): String;
    procedure SignedHeaders(const Header: String; var ToSing, ToCanonical: String);
  public
    function Calculate(Request: IHTTPRequest): string; override;
  end;

implementation

{ TAWSSignatureHMAC256 }

constructor TAWSSignatureHMAC256.Create(const AccessKey, DataStamp,
  RegionName, ServiceName: string);
begin
  inherited Create;
  FAccessKey:= AccessKey;
  FDataStamp:= DataStamp;
  FRegionName:= RegionName;
  FServiceName:= ServiceName;
end;

class function TAWSSignatureHMAC256.New(const AccessKey, DataStamp,
  RegionName, ServiceName: string): IAWSSignatureHMAC256;
begin
  Result := Create(AccessKey, DataStamp, RegionName, ServiceName);
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

function TAWSSignatureVersion4.BuildHeader(const Header: String): String;
var
  i: Integer;
  List: TStringList;
begin
  List := TStringList.Create;
  List.Text:=Header;
  List.LineBreak:=#10;
  List.NameValueSeparator:=':';
  List.Sorted:=True;
  List.Sort;
  Result := '';
  for i := 1 to List.Count - 1 do
    Result := Result + List[i]+#10;

end;

procedure TAWSSignatureVersion4.SignedHeaders(const Header: String; var ToSing, ToCanonical: String);
var
  i: Integer;
  List: TStringList;
  Name, Value: String;
begin
  List := TStringList.Create;
  List.Text:=Header;
  List.LineBreak:=#10;
  List.NameValueSeparator:=':';
  List.Sorted:=True;
  List.Sort;
  for i := 1 to List.Count - 1 do
    begin
      List.GetNameValue(i, Name, Value);
      ToSing := ToSing + LowerCase(Name) + ';';
      ToCanonical := ToCanonical + LowerCase(Name)+':'+Value+#10;
    end;
  system.Delete(ToSing, Length(ToSing), 1);
end;

function TAWSSignatureVersion4.Calculate(Request: IHTTPRequest): string;
const
  Algoritimo = 'AWS4-HMAC-SHA256';
  TipoReq = 'aws4_request';
var
  Header: string;
  Credencial: String;
  Escopo: String;
  DateFmt: String;
  AwsDateTime: String;
  Metodo: String;
  Canonical: String;
  CanonicalURI: String;
  CanonicalQuery: String;
  CanonicalHeaders: String;
  SignedHeader: String;
  PayLoadHash: String;
  CanonicalRequest: String;
  StringToSign: String;
  Signature: String;
  AuthorizationHeader: String;
  Assinatura: TSHA256Digest;
  oSHA256: TSHA256Digest;
begin
  DateFmt:= FormatDateTime('yyyymmdd', IncHour(Now, 3));
  AwsDateTime:= FormatDateTime('yyyymmdd', IncHour(Now, 3))+'T'+FormatDateTime('hhnnss', IncHour(Now, 3))+'Z';
  Metodo:= Request.Method;
  CanonicalURI:=EncodeTriplet(Request.Resource, '%', [':']);
  CanonicalQuery:='';

  Header := 'Host:' + Request.Domain + #10 ;
  CanonicalHeaders:= 'X-Amz-Date:' + AwsDateTime + #10 + Request.CanonicalizedAmzHeaders + #10;
  SignedHeaders(Header+CanonicalHeaders, SignedHeader, Canonical);

  PayLoadHash:= SHA256(Request.SubResource);

  CanonicalRequest := Metodo + #10 + CanonicalURI + #10 + CanonicalQuery + #10
                    + Canonical + #10 + SignedHeader + #10 + PayLoadHash;

  Credencial:= DateFmt + '/' + Request.ContentMD5 + '/' + Request.CanonicalizedResource + '/' + TipoReq;
  Escopo:= Credentials.AccessKeyId + '/' + Credencial;
  StringToSign := Algoritimo + #10 +  AwsDateTime + #10 + Credencial + #10 + SHA256( UTF8Encode(CanonicalRequest));

  Assinatura:=TAWSSignatureHMAC256.New(Credentials.SecretKey, DateFmt, Request.ContentMD5, Request.CanonicalizedResource).Signature;

  HMAC_SHA256(Assinatura, UTF8Encode(StringToSign), oSHA256);
  Signature := SHA256DigestToString(oSHA256);

  AuthorizationHeader := 'Authorization:' + Algoritimo + ' ' + 'Credential=' + Escopo + ', ' +
                         'SignedHeaders=' + SignedHeader + ', ' + 'Signature=' + Signature;

  Result := BuildHeader(CanonicalHeaders)
            + AuthorizationHeader
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
