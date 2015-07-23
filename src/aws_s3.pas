{
    AWS
    Copyright (C) 2013-2015 Marcos Douglas - mdbs99

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
  aws_base,
  aws_client;

type
  ES3Error = class(Exception);

  IS3Response = IAWSResponse;

  IS3Region = interface;
  IS3Bucket = interface;

  IS3Object = interface(IInterface)
  ['{FF865D65-97EE-46BC-A1A6-9D9FFE6310A4}']
    function Bucket: IS3Bucket;
    function Name: string;
    function Stream: IAWSStream;
  end;

  IS3Objects = interface(IInterface)
  ['{0CDE7D8E-BA30-4FD4-8FC0-F8291131652E}']
    function Get(const AName: string; const SubResources: string): IS3Object;
    procedure Delete(const AName: string);
    function Put(const AName, ContentType: string; Stream: IAWSStream; const SubResources: string): IS3Object;
    function Put(const AName, ContentType, AFileName, SubResources: string): IS3Object;
    function Put(const AName, SubResources: string): IS3Object;
    function Options(const AName: string): IS3Object;
  end;

  IS3Bucket = interface(IInterface)
  ['{7E7FA31D-7F54-4BE0-8587-3A72E7D24164}']
    function Name: string;
    function Objects: IS3Objects;
  end;

  IS3Buckets = interface(IInterface)
  ['{8F994521-57A1-4FA6-9F9F-3931E834EFE2}']
    function Check(const AName: string): Boolean;
    function Get(const AName, SubResources: string): IS3Bucket;
    procedure Delete(const AName, SubResources: string);
    function Put(const AName, SubResources: string): IS3Bucket;
    { TODO : Return a Bucket list }
    function All: IS3Response;
  end;

  IS3Region = interface(IInterface)
  ['{B192DB11-4080-477A-80D4-41698832F492}']
    function Online: Boolean;
    function Buckets: IS3Buckets;
  end;

  TS3Object = class sealed(TInterfacedObject, IS3Object)
  private
    FBucket: IS3Bucket;
    FName: string;
    FStream: IAWSStream;
  public
    constructor Create(Bucket: IS3Bucket; const AName: string; Stream: IAWSStream);
    function Bucket: IS3Bucket;
    function Name: string;
    function Stream: IAWSStream;
  end;

  TS3Objects = class sealed(TInterfacedObject, IS3Objects)
  private
    FClient: IAWSClient;
    FBucket: IS3Bucket;
  public
    constructor Create(Client: IAWSClient; Bucket: IS3Bucket);
    function Get(const AName: string; const SubResources: string): IS3Object;
    procedure Delete(const AName: string);
    function Put(const AName, ContentType: string; Stream: IAWSStream; const SubResources: string): IS3Object;
    function Put(const AName, ContentType, AFileName, SubResources: string): IS3Object;
    function Put(const AName, SubResources: string): IS3Object;
    function Options(const AName: string): IS3Object;
  end;

  TS3Region = class;

  TS3Bucket = class sealed(TInterfacedObject, IS3Bucket)
  private
    FClient: IAWSClient;
    FName: string;
  public
    constructor Create(Client: IAWSClient; const AName: string);
    function Name: string;
    function Objects: IS3Objects;
  end;

  TS3Buckets = class sealed(TInterfacedObject, IS3Buckets)
  private
    FClient: IAWSClient;
  public
    constructor Create(Client: IAWSClient);
    function Check(const AName: string): Boolean;
    function Get(const AName, SubResources: string): IS3Bucket;
    procedure Delete(const AName, SubResources: string);
    function Put(const AName, SubResources: string): IS3Bucket;
    function All: IS3Response;
  end;

  TS3Region = class sealed(TInterfacedObject, IS3Region)
  private
    FClient: IAWSClient;
  public
    constructor Create(Client: IAWSClient);
    function Online: Boolean;
    function Buckets: IS3Buckets;
  end;

implementation

{ TS3Object }

constructor TS3Object.Create(Bucket: IS3Bucket; const AName: string;
  Stream: IAWSStream);
begin
  inherited Create;
  FBucket := Bucket;
  FName := AName;
  FStream := Stream;
end;

function TS3Object.Bucket: IS3Bucket;
begin
  Result := FBucket;
end;

function TS3Object.Name: string;
begin
  Result := FName;
end;

function TS3Object.Stream: IAWSStream;
begin
  Result := FStream;
end;

{ TS3Objects }

constructor TS3Objects.Create(Client: IAWSClient; Bucket: IS3Bucket);
begin
  inherited Create;
  FClient := Client;
  FBucket := Bucket;
end;

function TS3Objects.Get(const AName: string; const SubResources: string): IS3Object;
var
  Res: IAWSResponse;
begin
  Res := FClient.Send(
    TAWSRequest.Create(
      'GET', FBucket.Name, '/' + AName, '/' + FBucket.Name + '/' + AName + SubResources
    )
  );
  if 200 <> Res.ResultCode then
    raise ES3Error.CreateFmt('Get error: %d', [Res.ResultCode]);
  Result := TS3Object.Create(FBucket, AName, Res.ResultStream);
end;

procedure TS3Objects.Delete(const AName: string);
var
  Res: IAWSResponse;
begin
  Res := FClient.Send(
    TAWSRequest.Create(
      'DELETE', FBucket.Name, '/' + AName, '/' + FBucket.Name + '/' + AName
    )
  );
  if 204 <> Res.ResultCode then
    raise ES3Error.CreateFmt('Delete error: %d', [Res.ResultCode]);
end;

function TS3Objects.Put(const AName, ContentType: string; Stream: IAWSStream;
  const SubResources: string): IS3Object;
var
  Res: IAWSResponse;
begin
  Res := FClient.Send(
    TAWSRequest.Create(
      'PUT', FBucket.Name, '/' + AName, SubResources, ContentType, '', '',
      '/' + FBucket.Name + '/' + AName, Stream
    )
  );
  if 200 <> Res.ResultCode then
    raise ES3Error.CreateFmt('Put error: %d', [Res.ResultCode]);
  Result := TS3Object.Create(FBucket, AName, Res.ResultStream);
end;

function TS3Objects.Put(const AName, ContentType, AFileName,
  SubResources: string): IS3Object;
var
  Buf: TFileStream;
begin
  Buf := TFileStream.Create(AFileName, fmOpenRead);
  try
    Result := Put(AName, ContentType, TAWSStream.Create(Buf), SubResources);
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
    // hack Synapse to add Content-Length
    Buf.WriteBuffer('', 1);
    Result := Put(AName, '', TAWSStream.Create(Buf), SubResources);
  finally
    Buf.Free;
  end;
end;

function TS3Objects.Options(const AName: string): IS3Object;
var
  Res: IAWSResponse;
begin
  { TODO : Not working properly yet. }
  Res := FClient.Send(
    TAWSRequest.Create(
      'OPTIONS', FBucket.Name, '/' + AName, '/' + FBucket.Name + '/' + AName
    )
  );
  if 200 <> Res.ResultCode then
    raise ES3Error.CreateFmt('Get error: %d', [Res.ResultCode]);
  Result := TS3Object.Create(FBucket, AName, Res.ResultStream);
end;

{ TS3Bucket }

constructor TS3Bucket.Create(Client: IAWSClient; const AName: string);
begin
  inherited Create;
  FClient := Client;
  FName := AName;
end;

function TS3Bucket.Name: string;
begin
  Result := FName;
end;

function TS3Bucket.Objects: IS3Objects;
begin
  Result := TS3Objects.Create(FClient, Self);
end;

{ TS3Buckets }

constructor TS3Buckets.Create(Client: IAWSClient);
begin
  inherited Create;
  FClient := Client;
end;

function TS3Buckets.Check(const AName: string): Boolean;
begin
  Result := FClient.Send(
    TAWSRequest.Create(
      'HEAD', AName, '', '', '', '', '', '/' + AName + '/'
    )
  ).ResultCode = 200;
end;

function TS3Buckets.Get(const AName, SubResources: string): IS3Bucket;
var
  Res: IAWSResponse;
begin
  Res := FClient.Send(
    TAWSRequest.Create(
      'GET', AName, '', SubResources, '', '', '', '/' + AName + '/' + SubResources
    )
  );
  if 200 <> Res.ResultCode then
    raise ES3Error.CreateFmt('Get error: %d', [Res.ResultCode]);
  Result := TS3Bucket.Create(FClient, AName);
end;

procedure TS3Buckets.Delete(const AName, SubResources: string);
var
  Res: IAWSResponse;
begin
  Res := FClient.Send(
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
  Res := FClient.Send(
    TAWSRequest.Create(
      'PUT', AName, '', SubResources, '', '', '', '/' + AName + SubResources
    )
  );
  if 200 <> Res.ResultCode then
    raise ES3Error.CreateFmt('Put error: %d', [Res.ResultCode]);
  Result := TS3Bucket.Create(FClient, AName);
end;

function TS3Buckets.All: IS3Response;
begin
  Result := FClient.Send(
    TAWSRequest.Create('GET', '','', '', '', '', '', '/')
  );
end;

{ TS3Region }

constructor TS3Region.Create(Client: IAWSClient);
begin
  inherited Create;
  FClient := Client;
end;

function TS3Region.Online: Boolean;
begin
  Result := FClient.Send(
    TAWSRequest.Create(
      'GET', '', '', '', '', '', '', '/'
    )
  ).ResultCode = 200;
end;

function TS3Region.Buckets: IS3Buckets;
begin
  Result := TS3Buckets.Create(FClient);
end;

end.
