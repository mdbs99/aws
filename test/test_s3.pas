{
    AWS
    Copyright (C) 2013-2014 by mdbs99

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
  TAWSClientMocker = class(TInterfacedObject, IAWSClient)
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
    procedure TearDown; override;
    function Client: TAWSClientMocker;
  end;

  TS3RegionTest = class(TS3Test)
  published
    procedure TestIsOnline;
    procedure TestBuckets;
  end;

  TS3BucketsTest = class(TS3Test)
  published
    procedure TestCheck;
    procedure TestGet;
    procedure TestDelete;
    procedure TestPut;
  end;

  TS3ObjectsTest = class(TS3Test)
  published
    procedure TestGet;
    procedure TestDelete;
    procedure TestPut;
    procedure TestOptions;
  end;

implementation

{ TAWSClientMocker }

function TAWSClientMocker.Send(Request: IAWSRequest): IAWSResponse;
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

function TAWSClientMocker.Request: IAWSRequest;
begin
  Result := FRequest;
end;

function TAWSClientMocker.Response: IAWSResponse;
begin
  Result := FResponse;
end;

{ TS3Test }

procedure TS3Test.SetUp;
begin
  inherited SetUp;
  FCredentials := TAWSCredentials.Create('dummy_key', 'dummy_secret', False);
  FClient := TAWSClientMocker.Create;
end;

procedure TS3Test.TearDown;
begin
  inherited TearDown;
end;

function TS3Test.Client: TAWSClientMocker;
begin
  Result := FClient as TAWSClientMocker;
end;

{ TS3RegionTest }

procedure TS3RegionTest.TestIsOnline;
var
  R: IS3Region;
begin
  R := TS3Region.Create(FClient);
  AssertTrue('Service denied', R.IsOnline);
  AssertEquals('GET', Client.Request.Method);
  AssertEquals('/', Client.Request.CanonicalizedResource);
end;

procedure TS3RegionTest.TestBuckets;
var
  R: IS3Region;
begin
  R := TS3Region.Create(FClient);
  AssertNotNull('Buckets not alive', R.Buckets);
end;

{ TS3BucketsTest }

procedure TS3BucketsTest.TestCheck;
var
  R: IS3Region;
begin
  R := TS3Region.Create(FClient);
  AssertTrue(R.Buckets.Check('myawsbucket'));
  AssertEquals('HEAD', Client.Request.Method);
  AssertEquals(200, Client.Response.ResultCode);
  AssertEquals('HTTP/1.1 200 OK', Client.Response.ResultHeader);
  AssertEquals('OK', Client.Response.ResultText);
end;

procedure TS3BucketsTest.TestGet;
var
  R: IS3Region;
  Bkt: IS3Bucket;
begin
  R := TS3Region.Create(FClient);
  Bkt := R.Buckets.Get('myawsbucket', '');
  AssertEquals('myawsbucket', Bkt.Name);
  AssertEquals('GET', Client.Request.Method);
  AssertEquals(200, Client.Response.ResultCode);
  AssertEquals('HTTP/1.1 200 OK', Client.Response.ResultHeader);
  AssertEquals('OK', Client.Response.ResultText);
end;

procedure TS3BucketsTest.TestDelete;
var
  R: IS3Region;
begin
  R := TS3Region.Create(FClient);
  R.Buckets.Delete('quotes', '/');
  AssertEquals('DELETE', Client.Request.Method);
  AssertEquals(204, Client.Response.ResultCode);
  AssertEquals('HTTP/1.1 204 No Content', Client.Response.ResultHeader);
  AssertEquals('No Content', Client.Response.ResultText);
end;

procedure TS3BucketsTest.TestPut;
var
  R: IS3Region;
begin
  R := TS3Region.Create(FClient);
  R.Buckets.Put('colorpictures', '/');
  AssertEquals('PUT', Client.Request.Method);
  AssertEquals(200, Client.Response.ResultCode);
  AssertEquals('HTTP/1.1 200 OK', Client.Response.ResultHeader);
  AssertEquals('OK', Client.Response.ResultText);
end;

{ TS3ObjectsTest }

procedure TS3ObjectsTest.TestGet;
var
  R: IS3Region;
  Bkt: IS3Bucket;
  Stream: TMemoryStream;
begin
  R := TS3Region.Create(FClient);
  Bkt := R.Buckets.Get('myawsbucket', '');
  Stream := TMemoryStream.Create;
  try
    Bkt.Objects.Get('myobj', Stream, '');
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
  R: IS3Region;
  Bkt: IS3Bucket;
begin
  R := TS3Region.Create(FClient);
  Bkt := R.Buckets.Get('myawsbucket', '');
  Bkt.Objects.Delete('myobj');
  AssertEquals(204, Client.Response.ResultCode);
  AssertEquals('HTTP/1.1 204 No Content', Client.Response.ResultHeader);
  AssertEquals('No Content', Client.Response.ResultText);
end;

procedure TS3ObjectsTest.TestPut;
var
  R: IS3Region;
  Bkt: IS3Bucket;
begin
  R := TS3Region.Create(FClient);
  Bkt := R.Buckets.Get('myawsbucket', '');
  Bkt.Objects.Put('myobj', 'text/plain', nil, '');
  AssertEquals(200, Client.Response.ResultCode);
  AssertEquals('HTTP/1.1 200 OK', Client.Response.ResultHeader);
  AssertEquals('OK', Client.Response.ResultText);
end;

procedure TS3ObjectsTest.TestOptions;
var
  R: IS3Region;
  Bkt: IS3Bucket;
begin
  R := TS3Region.Create(FClient);
  Bkt := R.Buckets.Get('myawsbucket', '');
  Bkt.Objects.Options('myobj');
  AssertEquals(200, Client.Response.ResultCode);
  AssertEquals('HTTP/1.1 200 OK', Client.Response.ResultHeader);
  AssertEquals('OK', Client.Response.ResultText);
end;

initialization
  RegisterTest('s3.region', TS3RegionTest);
  RegisterTest('s3.buckets', TS3BucketsTest);
  RegisterTest('s3.objects', TS3ObjectsTest);

end.

