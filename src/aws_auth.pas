{
    AWS
    Copyright (C) 2013-2014 by mdbs99

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit aws_auth;

{$i aws.inc}

interface

uses
  // aws
  aws_sys;

type
  ICredentials = interface(IDisposable)
    function GetAccessKeyId: string;
    function GetSecretKey: string;
    function IsSSL: Boolean;
  end;

  TCredentials = class(TInterfacedObject, ICredentials)
  private
    FAccessKeyId: string;
    FSecretKey: string;
    FSSL: Boolean;
  public
    constructor Create(const AccessKeyId, SecretKey: string; SSL: Boolean); reintroduce;
    function GetAccessKeyId: string;
    function GetSecretKey: string;
    function IsSSL: Boolean;
  end;

implementation

{ TCredentials }

constructor TCredentials.Create(const AccessKeyId, SecretKey: string;
  SSL: Boolean);
begin
  FAccessKeyId := AccessKeyId;
  FSecretKey := SecretKey;
  FSSL := SSL;
end;

function TCredentials.GetAccessKeyId: string;
begin
  Result := FAccessKeyId;
end;

function TCredentials.GetSecretKey: string;
begin
  Result := FSecretKey;
end;

function TCredentials.IsSSL: Boolean;
begin
  Result := FSSL;
end;

end.
