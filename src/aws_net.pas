{
    AWS
    Copyright (C) 2013-2016 Marcos Douglas - mdbs99

    See the file LICENSE.txt, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit aws_net;

{$i aws.inc}

interface

uses
  //rtl
  sysutils,
  classes;

type
  IURL = interface
    function WithSubDomain(const SubDomain: string): IURL;
    function WithResource(const Resource: string): IURL;
    function AsString: string;
  end;

  TURL = class(TInterfacedObject, IURL)
  private
    FProtocol: string;
    FDomain: string;
  public
    constructor Create(const Protocol, Domain: string); reintroduce;
    class function New(const Protocol, Domain: string): IURL;
    class function New(const Domain: string): IURL;
    function WithSubDomain(const SubDomain: string): IURL;
    function WithResource(const Resource: string): IURL;
    function AsString: string;
  end;

implementation

{ TURL }

constructor TURL.Create(const Protocol, Domain: string);
begin
  inherited Create;
  FProtocol := Protocol;
  FDomain := Domain;
end;

class function TURL.New(const Protocol, Domain: string): IURL;
begin
  Result := Create(Protocol, Domain);
end;

class function TURL.New(const Domain: string): IURL;
begin
  Result := New('http', Domain);
end;

function TURL.WithSubDomain(const SubDomain: string): IURL;
begin
  Result := New(FProtocol, SubDomain + '.' + FDomain);
end;

function TURL.WithResource(const Resource: string): IURL;
begin
  Result := New(FProtocol, FDomain + Resource);
end;

function TURL.AsString: string;
begin
  Result := FProtocol + '://' + FDomain;
end;

end.
