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
  aws_auth,
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
    function ClientMock: TAWSClientMocker;
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
    //procedure TestGet;
    procedure TestDelete;
    procedure TestPut;
  end;

  TAWSS3Test = class(TTestCase)
  protected
    FBucketName: string;
    FS3: TAWSS3Client;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHEADBucket;
    procedure TestGETService;
    procedure TestGETBucket_ListObjects;
    procedure TestGETBucket_acl;
    procedure TestGETBucket_location;
    procedure TestGETBucket_logging;
    procedure TestGETBucket_notification;
    procedure TestGETBucket_versions;
    procedure TestGETBucket_requestPayment;
    procedure TestGETBucket_versioning;
    procedure TestPUTObject;
    procedure TestFolderOperations;
  end;

implementation

uses IniFiles;

{ TAWSClientMocker }

function TAWSClientMocker.Send(Request: IAWSRequest): IAWSResponse;
var
  Code: Integer;
  Header, Text: string;
begin
  FRequest := Request;
  Code := -1;
  Header := '';
  Text := '';
  case Request.Method of
    'GET', 'HEAD', 'PUT':
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
  FResponse := TAWSResponse.Create(Code, Header, Text);
  Result := FResponse;
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

function TS3Test.ClientMock: TAWSClientMocker;
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
  AssertEquals('GET', ClientMock.Request.Method);
  AssertEquals('/', ClientMock.Request.CanonicalizedResource);
end;

procedure TS3RegionTest.TestBuckets;
begin

end;

{ TS3BucketsTest }

procedure TS3BucketsTest.TestCheck;
var
  R: IS3Region;
begin
  R := TS3Region.Create(FClient);
  AssertTrue(R.Buckets.Check('myawsbucket'));
  AssertEquals('HEAD', ClientMock.Request.Method);
  AssertEquals(200, ClientMock.Response.ResultCode);
  AssertEquals('HTTP/1.1 200 OK', ClientMock.Response.ResultHeader);
  AssertEquals('OK', ClientMock.Response.ResultText);
end;

procedure TS3BucketsTest.TestGet;
var
  R: IS3Region;
  Bkt: IS3Bucket;
begin
  R := TS3Region.Create(FClient);
  Bkt := R.Buckets.Get('myawsbucket', '');
  AssertEquals('myawsbucket', Bkt.Name);
  AssertEquals('GET', ClientMock.Request.Method);
  AssertEquals(200, ClientMock.Response.ResultCode);
  AssertEquals('HTTP/1.1 200 OK', ClientMock.Response.ResultHeader);
  AssertEquals('OK', ClientMock.Response.ResultText);
end;

procedure TS3BucketsTest.TestDelete;
var
  R: IS3Region;
begin
  R := TS3Region.Create(FClient);
  R.Buckets.Delete('quotes', '/');
  AssertEquals('DELETE', ClientMock.Request.Method);
  AssertEquals(204, ClientMock.Response.ResultCode);
  AssertEquals('HTTP/1.1 204 No Content', ClientMock.Response.ResultHeader);
  AssertEquals('No Content', ClientMock.Response.ResultText);
end;

procedure TS3BucketsTest.TestPut;
var
  R: IS3Region;
begin
  R := TS3Region.Create(FClient);
  R.Buckets.Put('colorpictures', '/');
  AssertEquals('PUT', ClientMock.Request.Method);
  AssertEquals(200, ClientMock.Response.ResultCode);
  AssertEquals('HTTP/1.1 200 OK', ClientMock.Response.ResultHeader);
  AssertEquals('OK', ClientMock.Response.ResultText);
end;

{ TS3ObjectsTest }

procedure TS3ObjectsTest.TestDelete;
var
  R: IS3Region;
  Bkt: IS3Bucket;
begin
  R := TS3Region.Create(FClient);
  Bkt := R.Buckets.Get('myawsbucket', '');
  Bkt.Objects.Delete('myawsbucket');
  AssertEquals(204, ClientMock.Response.ResultCode);
  AssertEquals('HTTP/1.1 204 No Content', ClientMock.Response.ResultHeader);
  AssertEquals('No Content', ClientMock.Response.ResultText);
end;

procedure TS3ObjectsTest.TestPut;
var
  R: IS3Region;
  Bkt: IS3Bucket;
begin
  R := TS3Region.Create(FClient);
  Bkt := R.Buckets.Get('myawsbucket', '');
  Bkt.Objects.Put('myawsbucket', 'text/plain', nil, '');
  AssertEquals(200, ClientMock.Response.ResultCode);
  AssertEquals('HTTP/1.1 200 OK', ClientMock.Response.ResultHeader);
  AssertEquals('OK', ClientMock.Response.ResultText);
end;

{ TAWSS3Test }

procedure TAWSS3Test.SetUp;
const
  KEY_FILE = 'key.ini';
var
  Ini: TIniFile;
begin
  if not FileExists(KEY_FILE) then
    raise Exception.CreateFmt('File %s not exists', [KEY_FILE]);

  FS3 := TAWSS3Client.Create;
  Ini := TIniFile.Create(KEY_FILE);
  try
    FS3.UseSSL := True;
    FS3.AccessKeyId := Ini.ReadString('AWS', 'AccessKeyId', '');
    FS3.SecretKey := Ini.ReadString('AWS', 'SecretKey', '');
    FBucketName :=  Ini.ReadString('AWS', 'BucketName', '')
  finally
    Ini.Free;
  end;
end;

procedure TAWSS3Test.TearDown;
begin
  FS3.Free;
end;

procedure TAWSS3Test.TestHEADBucket;
begin
  CheckTrue(FS3.HEADBucket(FBucketName));
end;

procedure TAWSS3Test.TestGETService;
begin
  CheckEquals(200, FS3.GETService);
end;

procedure TAWSS3Test.TestGETBucket_ListObjects;
begin
  CheckEquals(200, FS3.GETBucket(FBucketName, ''));
end;

procedure TAWSS3Test.TestGETBucket_acl;
begin
  CheckEquals(200, FS3.GETBucket(FBucketName, '?acl'));
end;

procedure TAWSS3Test.TestGETBucket_location;
begin
  CheckEquals(200, FS3.GETBucket(FBucketName, '?location'));
end;

procedure TAWSS3Test.TestGETBucket_logging;
begin
  CheckEquals(200, FS3.GETBucket(FBucketName, '?logging'));
end;

procedure TAWSS3Test.TestGETBucket_notification;
begin
  CheckEquals(200, FS3.GETBucket(FBucketName, '?notification'));
end;

procedure TAWSS3Test.TestGETBucket_versions;
begin
  CheckEquals(200, FS3.GETBucket(FBucketName, '?versions'));
end;

procedure TAWSS3Test.TestGETBucket_requestPayment;
begin
  CheckEquals(200, FS3.GETBucket(FBucketName, '?requestPayment'));
end;

procedure TAWSS3Test.TestGETBucket_versioning;
begin
  CheckEquals(200, FS3.GETBucket(FBucketName, '?versioning'));
end;

procedure TAWSS3Test.TestPUTObject;
begin
  CheckEquals(200, FS3.PUTObject(FBucketName, 'text/plan', 'test.txt', 'test.txt'));
  //CheckEquals(200, FS3.DELETEObject(FBucketName, 'test.txt'));
end;

procedure TAWSS3Test.TestFolderOperations;
const
  NEW_FOLDER = 'my_new_folder';
begin
  CheckEquals(200, FS3.PUTFolder(FBucketName, NEW_FOLDER));
  CheckEquals(200, FS3.PUTObject(FBucketName, 'text/plan', NEW_FOLDER+'/test.txt', 'test.txt'));
end;

initialization
  RegisterTest('aws.s3.region', TS3RegionTest);
  RegisterTest('aws.s3.bucket', TS3BucketsTest);

  RegisterTest(TAWSS3Test);

end.

