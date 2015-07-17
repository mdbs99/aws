{
    AWS
    Copyright (C) 2013-2015 Marcos Douglas - mdbs99

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit test_s3;

{$i ../src/aws.inc}

interface

uses
  //rtl
  classes,
  sysutils,
  fpcunit,
  testregistry,
  //aws
  aws_client,
  aws_s3;

type
  TAWSFakeClient = class(TInterfacedObject, IAWSClient)
  strict private
    FRequest: IAWSRequest;
    FResponse: IAWSResponse;
  public
    function Send(Request: IAWSRequest): IAWSResponse;
    function Request: IAWSRequest;
    function Response: IAWSResponse;
  end;

  TS3Test = class abstract(TTestCase)
  private
    FCredentials: IAWSCredentials;
    FClient: IAWSClient;
  protected
    procedure SetUp; override;
    function Client: TAWSFakeClient;
  end;

  TS3RegionTest = class(TS3Test)
  published
    procedure TestIsOnline;
    procedure TestImmutableBuckets;
  end;

  TS3BucketsTest = class(TS3Test)
  published
    procedure TestCheck;
    procedure TestGet;
    procedure TestDelete;
    procedure TestPut;
  end;

  TS3BucketTest = class(TS3Test)
  published
    procedure TestImmutable;
  end;

  TS3ObjectsTest = class(TS3Test)
  published
    procedure TestGet;
    procedure TestDelete;
    procedure TestPut;
    procedure TestOptions;
    procedure TestImmutable;
  end;

implementation

{ TAWSFakeClient }

function TAWSFakeClient.Send(Request: IAWSRequest): IAWSResponse;
var
  Code: Integer;
  Header, Text: string;
  Stream: TStringStream;
begin
  FRequest := Request;
  Code := -1;
  Header := '';
  Text := '';
  case Request.Method of
    'GET', 'HEAD', 'PUT', 'OPTIONS':
      begin
        Code := 200;
        Header := 'HTTP/1.1 200 OK';
        Text := 'OK';
      end;
    'DELETE':
      begin
        Code := 204;
        Header := 'HTTP/1.1 204 No Content';
        Text := 'No Content';
      end;
  end;
  Stream := TStringStream.Create(Header + #13 + Text);
  try
    FResponse := TAWSResponse.Create(Code, Header, Text, Stream);
    Result := FResponse;
  finally
    Stream.Free;
  end;
end;

function TAWSFakeClient.Request: IAWSRequest;
begin
  Result := FRequest;
end;

function TAWSFakeClient.Response: IAWSResponse;
begin
  Result := FResponse;
end;

{ TS3Test }

procedure TS3Test.SetUp;
begin
  inherited SetUp;
  FCredentials := TAWSCredentials.Create('dummy_key', 'dummy_secret', False);
  FClient := TAWSFakeClient.Create;
end;

function TS3Test.Client: TAWSFakeClient;
begin
  Result := FClient as TAWSFakeClient;
end;

{ TS3RegionTest }

procedure TS3RegionTest.TestIsOnline;
var
  Rg: IS3Region;
begin
  Rg := TS3Region.Create(FClient);
  AssertTrue('Service denied', Rg.Online);
  AssertEquals('GET', Client.Request.Method);
  AssertEquals('/', Client.Request.CanonicalizedResource);
end;

procedure TS3RegionTest.TestImmutableBuckets;
var
  Rg: IS3Region;
begin
  Rg := TS3Region.Create(FClient);
  AssertNotNull('Buckets not alive', Rg.Buckets);
  AssertNotSame(Rg.Buckets, Rg.Buckets);
end;

{ TS3BucketsTest }

procedure TS3BucketsTest.TestCheck;
var
  Rg: IS3Region;
begin
  Rg := TS3Region.Create(FClient);
  AssertTrue(Rg.Buckets.Check('myawsbucket'));
  AssertEquals('HEAD', Client.Request.Method);
  AssertEquals(200, Client.Response.ResultCode);
  AssertEquals('HTTP/1.1 200 OK', Client.Response.ResultHeader);
  AssertEquals('OK', Client.Response.ResultText);
end;

procedure TS3BucketsTest.TestGet;
var
  Rg: IS3Region;
  Bk: IS3Bucket;
begin
  Rg := TS3Region.Create(FClient);
  Bk := Rg.Buckets.Get('myawsbucket', '');
  AssertEquals('myawsbucket', Bk.Name);
  AssertEquals('GET', Client.Request.Method);
  AssertEquals(200, Client.Response.ResultCode);
  AssertEquals('HTTP/1.1 200 OK', Client.Response.ResultHeader);
  AssertEquals('OK', Client.Response.ResultText);
end;

procedure TS3BucketsTest.TestDelete;
var
  Rg: IS3Region;
begin
  Rg := TS3Region.Create(FClient);
  Rg.Buckets.Delete('quotes', '/');
  AssertEquals('DELETE', Client.Request.Method);
  AssertEquals(204, Client.Response.ResultCode);
  AssertEquals('HTTP/1.1 204 No Content', Client.Response.ResultHeader);
  AssertEquals('No Content', Client.Response.ResultText);
end;

procedure TS3BucketsTest.TestPut;
var
  Rg: IS3Region;
begin
  Rg := TS3Region.Create(FClient);
  Rg.Buckets.Put('colorpictures', '/');
  AssertEquals('PUT', Client.Request.Method);
  AssertEquals(200, Client.Response.ResultCode);
  AssertEquals('HTTP/1.1 200 OK', Client.Response.ResultHeader);
  AssertEquals('OK', Client.Response.ResultText);
end;

{ TS3BucketTest }

procedure TS3BucketTest.TestImmutable;
var
  Rg: IS3Region;
  Bk: IS3Bucket;
begin
  Rg := TS3Region.Create(FClient);
  Bk := Rg.Buckets.Get('myawsbucket', '');
  AssertSame(Bk.Region, Bk.Region);
end;

{ TS3ObjectsTest }

procedure TS3ObjectsTest.TestGet;
var
  Rg: IS3Region;
  Bk: IS3Bucket;
  Stream: TMemoryStream;
begin
  Rg := TS3Region.Create(FClient);
  Bk := Rg.Buckets.Get('myawsbucket', '');
  Stream := TMemoryStream.Create;
  try
    Bk.Objects.Get('myobj', Stream, '');
    AssertEquals(200, Client.Response.ResultCode);
    AssertEquals('HTTP/1.1 200 OK', Client.Response.ResultHeader);
    AssertEquals('OK', Client.Response.ResultText);
    AssertTrue('Stream size is zero', Stream.Size > 0);
  finally
    Stream.Free;
  end;
end;

procedure TS3ObjectsTest.TestDelete;
var
  Rg: IS3Region;
  Bk: IS3Bucket;
begin
  Rg := TS3Region.Create(FClient);
  Bk := Rg.Buckets.Get('myawsbucket', '');
  Bk.Objects.Delete('myobj');
  AssertEquals(204, Client.Response.ResultCode);
  AssertEquals('HTTP/1.1 204 No Content', Client.Response.ResultHeader);
  AssertEquals('No Content', Client.Response.ResultText);
end;

procedure TS3ObjectsTest.TestPut;
var
  Rg: IS3Region;
  Bk: IS3Bucket;
begin
  Rg := TS3Region.Create(FClient);
  Bk := Rg.Buckets.Get('myawsbucket', '');
  Bk.Objects.Put('myobj', 'text/plain', nil, '');
  AssertEquals(200, Client.Response.ResultCode);
  AssertEquals('HTTP/1.1 200 OK', Client.Response.ResultHeader);
  AssertEquals('OK', Client.Response.ResultText);
end;

procedure TS3ObjectsTest.TestOptions;
var
  Rg: IS3Region;
  Bk: IS3Bucket;
begin
  Rg := TS3Region.Create(FClient);
  Bk := Rg.Buckets.Get('myawsbucket', '');
  Bk.Objects.Options('myobj');
  AssertEquals(200, Client.Response.ResultCode);
  AssertEquals('HTTP/1.1 200 OK', Client.Response.ResultHeader);
  AssertEquals('OK', Client.Response.ResultText);
end;

procedure TS3ObjectsTest.TestImmutable;
var
  Rg: IS3Region;
  Bk: IS3Bucket;
begin
  Rg := TS3Region.Create(FClient);
  Bk := Rg.Buckets.Get('myawsbucket', '');
  AssertNotSame(Bk.Objects, Bk.Objects);
end;

initialization
  RegisterTest('s3.region', TS3RegionTest);
  RegisterTest('s3.buckets', TS3BucketsTest);
  RegisterTest('s3.buckets', TS3BucketTest);
  RegisterTest('s3.objects', TS3ObjectsTest);

end.

