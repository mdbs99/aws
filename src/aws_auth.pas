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
  IAWSCredentials = interface(IInterface)
    function GetAccessKeyId: string;
    function GetSecretKey: string;
    function UseSSL: Boolean;
  end;

  TAWSCredentials = class(TInterfacedObject, IAWSCredentials)
  private
    FAccessKeyId: string;
    FSecretKey: string;
    FSSL: Boolean;
  public
    constructor Create(const AccessKeyId, SecretKey: string; UseSSL: Boolean); reintroduce;
    function GetAccessKeyId: string;
    function GetSecretKey: string;
    function UseSSL: Boolean;
  end;

implementation

{ TAWSCredentials }

constructor TAWSCredentials.Create(const AccessKeyId, SecretKey: string;
  UseSSL: Boolean);
begin
  FAccessKeyId := AccessKeyId;
  FSecretKey := SecretKey;
  FSSL := UseSSL;
end;

function TAWSCredentials.GetAccessKeyId: string;
begin
  Result := FAccessKeyId;
end;

function TAWSCredentials.GetSecretKey: string;
begin
  Result := FSecretKey;
end;

function TAWSCredentials.UseSSL: Boolean;
begin
  Result := FSSL;
end;

end.
