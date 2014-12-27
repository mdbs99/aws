{
    AWS
    Copyright (C) 2013-2014 by mdbs99

    See the file LICENSE.txt, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit aws_s3;

{$i aws.inc}

interface

uses
  //rtl
  classes,
  sysutils,
  //synapse
  synautil,
  //aws
  aws_sys,
  aws_client;

type
  ES3Error = class(Exception);

  IS3Response = IAWSResponse;

  IS3Region = interface;
  IS3Bucket = interface;

  IS3Object = interface(IInterface)
    function Bucket: IS3Bucket;
    function Name: string;
  end;

  IS3Objects = interface(IInterface)
    function Get(const AName: string; Stream: TStream; const SubResources: string): IS3Object;
    function Get(const AName, AFileName: string; const SubResources: string): IS3Object;
    procedure Delete(const AName: string);
    function Put(const AName, ContentType: string; Stream: TStream; const SubResources: string): IS3Object;
    function Put(const AName, ContentType, AFileName, SubResources: string): IS3Object;
    function Put(const AName, SubResources: string): IS3Object;
  end;

  IS3Bucket = interface(IInterface)
    function Region: IS3Region;
    function Name: string;
    function Objects: IS3Objects;
  end;

  IS3Buckets = interface(IInterface)
    function Check(const AName: string): Boolean;
    function Get(const AName, SubResources: string): IS3Bucket;
    procedure Delete(const AName, SubResources: string);
    function Put(const AName, SubResources: string): IS3Bucket;
    { TODO : Return a Bucket list }
    function All: IS3Response;
  end;

  IS3Region = interface(IInterface)
    function Client: IAWSClient;
    function IsOnline: Boolean;
    function Buckets: IS3Buckets;
  end;

  TS3Object = class sealed(TInterfacedObject, IS3Object)
  private
    FBucket: IS3Bucket;
    FName: string;
  public
    constructor Create(Bucket: IS3Bucket; const AName: string);
    function Bucket: IS3Bucket;
    function Name: string;
  end;

  TS3Objects = class sealed(TInterfacedObject, IS3Objects)
  private
    FBucket: IS3Bucket;
  public
    constructor Create(Bucket: IS3Bucket);
    function Get(const AName: string; Stream: TStream; const SubResources: string): IS3Object;
    function Get(const AName, AFileName: string; const SubResources: string): IS3Object;
    procedure Delete(const AName: string);
    function Put(const AName, ContentType: string; Stream: TStream; const SubResources: string): IS3Object;
    function Put(const AName, ContentType, AFileName, SubResources: string): IS3Object;
    function Put(const AName, SubResources: string): IS3Object;
  end;

  TS3Region = class;

  TS3Bucket = class sealed(TInterfacedObject, IS3Bucket)
  private
    FRegion: IS3Region;
    FName: string;
    FObjects: IS3Objects;
  public
    constructor Create(Region: IS3Region; const AName: string);
    function Region: IS3Region;
    function Name: string;
    function Objects: IS3Objects;
  end;

  TS3Buckets = class sealed(TInterfacedObject, IS3Buckets)
  private
    FRegion: IS3Region;
  public
    constructor Create(Region: IS3Region);
    function Check(const AName: string): Boolean;
    function Get(const AName, SubResources: string): IS3Bucket;
    procedure Delete(const AName, SubResources: string);
    function Put(const AName, SubResources: string): IS3Bucket;
    function All: IS3Response;
  end;

  TS3Region = class sealed(TInterfacedObject, IS3Region)
  private
    FClient: IAWSClient;
    FBuckets: IS3Buckets;
  public
    constructor Create(AClient: IAWSClient);
    function Client: IAWSClient;
    function IsOnline: Boolean;
    function Buckets: IS3Buckets;
  end;

implementation

{ TS3Object }

constructor TS3Object.Create(Bucket: IS3Bucket; const AName: string);
begin
  inherited Create;
  FBucket := Bucket;
  FName := AName;
end;

function TS3Object.Bucket: IS3Bucket;
begin
  Result := FBucket;
end;

function TS3Object.Name: string;
begin
  Result := FName;
end;

{ TS3Objects }

constructor TS3Objects.Create(Bucket: IS3Bucket);
begin
  inherited Create;
  SetWeak(@FBucket, Bucket);
end;

function TS3Objects.Get(const AName: string; Stream: TStream;
  const SubResources: string): IS3Object;
begin
  { TODO :  }
  Result := nil;
end;

function TS3Objects.Get(const AName, AFileName: string;
  const SubResources: string): IS3Object;
var
  Buf: TFileStream;
begin
  Buf := TFileStream.Create(AFileName, fmCreate);
  try
    Result := Get(AName, Buf, SubResources);
  finally
    Buf.Free;
  end;
end;

procedure TS3Objects.Delete(const AName: string);
var
  Res: IAWSResponse;
begin
  Res := FBucket.Region.Client.Send(
    TAWSRequest.Create(
      'DELETE', FBucket.Name, '/' + AName, '/' + FBucket.Name + '/' + AName
    )
  );
  if 204 <> Res.ResultCode then
    raise ES3Error.CreateFmt('Delete error: %d', [Res.ResultCode]);
end;

function TS3Objects.Put(const AName, ContentType: string; Stream: TStream;
  const SubResources: string): IS3Object;
var
  Res: IAWSResponse;
begin
  Res := FBucket.Region.Client.Send(
    TAWSRequest.Create(
      'PUT', FBucket.Name, '/' + AName, SubResources, ContentType, '', '',
      '/' + FBucket.Name + '/' + AName, Stream
    )
  );
  if 200 <> Res.ResultCode then
    raise ES3Error.CreateFmt('Put error: %d', [Res.ResultCode]);
  Result := TS3Object.Create(FBucket, AName);
end;

function TS3Objects.Put(const AName, ContentType, AFileName,
  SubResources: string): IS3Object;
var
  Buf: TFileStream;
begin
  Buf := TFileStream.Create(AFileName, fmOpenRead);
  try
    Result := Put(AName, ContentType, Buf, SubResources);
  finally
    Buf.Free;
  end;
end;

function TS3Objects.Put(const AName, SubResources: string): IS3Object;
var
  Buf: TMemoryStream;
begin
  Buf := TMemoryStream.Create;
  try
    // hack to synapse add Content-Length
    Buf.WriteBuffer('', 1);
    Result := Put(AName, '', Buf, SubResources);
  finally
    Buf.Free;
  end;
end;

{ TS3Bucket }

constructor TS3Bucket.Create(Region: IS3Region; const AName: string);
begin
  inherited Create;
  FRegion := Region;
  FName := AName;
  FObjects := TS3Objects.Create(Self);
end;

function TS3Bucket.Region: IS3Region;
begin
  Result := FRegion;
end;

function TS3Bucket.Name: string;
begin
  Result := FName;
end;

function TS3Bucket.Objects: IS3Objects;
begin
  Result := FObjects;
end;

{ TS3Buckets }

constructor TS3Buckets.Create(Region: IS3Region);
begin
  inherited Create;
  SetWeak(@FRegion, Region);
end;

function TS3Buckets.Check(const AName: string): Boolean;
begin
  Result := FRegion.Client.Send(
    TAWSRequest.Create(
      'HEAD', AName, '', '', '', '', '', '/' + AName + '/'
    )
  ).ResultCode = 200;
end;

function TS3Buckets.Get(const AName, SubResources: string): IS3Bucket;
var
  Res: IAWSResponse;
begin
  Res := FRegion.Client.Send(
    TAWSRequest.Create(
      'GET', AName, '', SubResources, '', '', '', '/' + AName + '/' + SubResources
    )
  );
  if 200 <> Res.ResultCode then
    raise ES3Error.CreateFmt('Get error: %d', [Res.ResultCode]);
  Result := TS3Bucket.Create(FRegion, AName);
end;

procedure TS3Buckets.Delete(const AName, SubResources: string);
var
  Res: IAWSResponse;
begin
  Res := FRegion.Client.Send(
    TAWSRequest.Create(
      'DELETE', AName, '', SubResources, '', '', '', '/' + AName + SubResources
    )
  );
  if 204 <> Res.ResultCode then
    raise ES3Error.CreateFmt('Delete error: %d', [Res.ResultCode]);
end;

function TS3Buckets.Put(const AName, SubResources: string): IS3Bucket;
var
  Res: IAWSResponse;
begin
  Res := FRegion.Client.Send(
    TAWSRequest.Create(
      'PUT', AName, '', SubResources, '', '', '', '/' + AName + SubResources
    )
  );
  if 200 <> Res.ResultCode then
    raise ES3Error.CreateFmt('Put error: %d', [Res.ResultCode]);
  Result := TS3Bucket.Create(FRegion, AName);
end;

function TS3Buckets.All: IS3Response;
begin
  Result := FRegion.Client.Send(TAWSRequest.Create('GET', '','', '', '', '', '', '/'));
end;

{ TS3Region }

constructor TS3Region.Create(AClient: IAWSClient);
begin
  inherited Create;
  FClient := AClient;
  FBuckets := TS3Buckets.Create(Self);
end;

function TS3Region.Client: IAWSClient;
begin
  Result := FClient;
end;

function TS3Region.IsOnline: Boolean;
begin
  Result := Client.Send(
    TAWSRequest.Create(
      'GET', '', '', '', '', '', '', '/'
    )
  ).ResultCode = 200;
end;

function TS3Region.Buckets: IS3Buckets;
begin
  Result := FBuckets;
end;

end.
