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
  aws_http,
  aws_client,
  aws_s3;

type
  TAWSClientMocker = class(TInterfacedObject, IAWSClient)
  private
    FLog: TStrings;
  public
    constructor Create(Log: TStrings);
    function Send(const SuccessCode: Integer; const Method, Resource, SubResource,
      ContentType, ContentMD5, CanonicalizedAmzHeaders,
      CanonicalizedResource: string): IAWSResult;
  end;

  TS3Test = class abstract(TTestCase)
  private
    FLog: TStrings;
    FCredentials: IAWSCredentials;
    FClient: IAWSClient;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TS3RegionTest = class(TS3Test)
  published
    procedure TestIsOnline;
    procedure TestBuckets;
  end;

  TS3BucketsTest = class(TS3Test)
  published
    procedure TestCheck;
    //procedure TestGet;
    procedure TestDelete;
    procedure TestPut;
  end;

  TS3ObjectsTest = class(TS3Test)
  published
    procedure TestGet;
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

constructor TAWSClientMocker.Create(Log: TStrings);
begin
  FLog := Log;
end;

function TAWSClientMocker.Send(const SuccessCode: Integer; const Method,
  Resource, SubResource, ContentType, ContentMD5, CanonicalizedAmzHeaders,
  CanonicalizedResource: string): IAWSResult;
var
  ResultCode: Integer;
  Body: TStrings;
begin
  FLog.Add('Method=' + Method);
  FLog.Add('Resource=' + Resource);
  FLog.Add('SubResource=' + SubResource);
  FLog.Add('ContentType=' + ContentType);
  FLog.Add('ContentMD5=' + ContentMD5);
  FLog.Add('CanonicalizedAmzHeaders=' + CanonicalizedAmzHeaders);
  FLog.Add('CanonicalizedResource=' + CanonicalizedResource);
  ResultCode := -1;
  Body := TStringList.Create;
  try
    // GET service
    if (Method = 'GET') and (CanonicalizedResource = '/') then
    begin
      ResultCode := 200;
      Body.LoadFromFile('get-service.txt');
    end;
    // HEAD bucket
    if (Method = 'HEAD') and (CanonicalizedResource = '/myawsbucket/') then
    begin
      ResultCode := 200;
      Body.LoadFromFile('head-bucket.txt');
    end;
    // DELETE bucket
    if (Method = 'DELETE') and (CanonicalizedResource = '/quotes/') then
    begin
      ResultCode := 204;
      Body.LoadFromFile('delete-bucket.txt');
    end;
    // PUT bucket
    if (Method = 'PUT') and (CanonicalizedResource = '/colorpictures/') then
    begin
      ResultCode := 200;
      Body.LoadFromFile('put-bucket.txt');
    end;
    Result := TAWSResult.Create(SuccessCode, THTTPResult.Create(ResultCode, Body.Text));
  finally
    Body.Free;
  end;
end;

{ TS3Test }

procedure TS3Test.SetUp;
begin
  inherited SetUp;
  FLog := TStringList.Create;
  FCredentials := TAWSCredentials.Create('dummy_key', 'dummy_secret', False);
  FClient := TAWSClientMocker.Create(FLog);
end;

procedure TS3Test.TearDown;
begin
  FLog.Free;
  inherited TearDown;
end;

{ TS3RegionTest }

procedure TS3RegionTest.TestIsOnline;
var
  R: IS3Region;
begin
  R := TS3Region.Create(FClient);
  AssertTrue('Service denied', R.IsOnline);
  AssertTrue('Method <> GET', FLog.Values['Method'] = 'GET');
  AssertTrue('CanonicalizedResource <> /', FLog.Values['CanonicalizedResource'] = '/');
end;

procedure TS3RegionTest.TestBuckets;
var
  R: IS3Region;
  Res: IS3Result;
  Body: TStrings;
begin
  R := TS3Region.Create(FClient);
  Body := TStringList.Create;
  try
    Res := R.Buckets.All;
    Body.Text := Res.ResultText;
    AssertEquals(200, Res.ResultCode);
    AssertEquals(Body[1], '<ListAllMyBucketsResult xmlns="http://s3.amazonaws.com/doc/2006-03-01"');
  finally
    Body.Free;
  end;
end;

{ TS3BucketsTest }

procedure TS3BucketsTest.TestCheck;
var
  R: IS3Region;
  Res: IS3Result;
begin
  R := TS3Region.Create(FClient);
  Res := R.Buckets.Check('myawsbucket');
  AssertEquals('Bucket invalid', 200, Res.ResultCode);
  AssertEquals('Invalid body', 1, Pos('HTTP/1.1 200 OK', Res.ResultText));
end;

procedure TS3BucketsTest.TestDelete;
var
  R: IS3Region;
  Res: IS3Result;
begin
  R := TS3Region.Create(FClient);
  Res := R.Buckets.Delete('quotes', '/');
  AssertEquals('Bucket not found', 204, Res.ResultCode);
  AssertEquals('Invalid body', 1, Pos('HTTP/1.1 204 No Content', Res.ResultText));
end;

procedure TS3BucketsTest.TestPut;
var
  R: IS3Region;
  Res: IS3Result;
begin
  R := TS3Region.Create(FClient);
  Res := R.Buckets.Put('colorpictures', '/');
  AssertEquals('ResultCode invalid', 200, Res.ResultCode);
  AssertEquals('Invalid body', 1, Pos('HTTP/1.1 200 OK', Res.ResultText));
end;

{ TS3ObjectsTest }

procedure TS3ObjectsTest.TestGet;
var
  R: IS3Region;
  Res: IS3Result;
begin
  R := TS3Region.Create(FClient);
  //Res := R.Buckets.;
end;

procedure TS3ObjectsTest.TestDelete;
begin

end;

procedure TS3ObjectsTest.TestPut;
begin

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

