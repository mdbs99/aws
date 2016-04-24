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
  IAWSURL = interface
    function WithSubDomain(const SubDomain: string): IAWSURL;
    function WithResource(const Resource: string): IAWSURL;
    function AsString: string;
  end;

  TAWSURL = class(TInterfacedObject, IAWSURL)
  private
    FProtocol: string;
    FDomain: string;
  public
    constructor Create(const Protocol, Domain: string); reintroduce;
    class function New(const Protocol, Domain: string): IAWSURL;
    class function New(const Domain: string): IAWSURL;
    function WithSubDomain(const SubDomain: string): IAWSURL;
    function WithResource(const Resource: string): IAWSURL;
    function AsString: string;
  end;

implementation

{ TAWSURL }

constructor TAWSURL.Create(const Protocol, Domain: string);
begin
  inherited Create;
  FProtocol := Protocol;
  FDomain := Domain;
end;

class function TAWSURL.New(const Protocol, Domain: string): IAWSURL;
begin
  Result := Create(Protocol, Domain);
end;

class function TAWSURL.New(const Domain: string): IAWSURL;
begin
  Result := New('http', Domain);
end;

function TAWSURL.WithSubDomain(const SubDomain: string): IAWSURL;
begin
  Result := New(FProtocol, SubDomain + '.' + FDomain);
end;

function TAWSURL.WithResource(const Resource: string): IAWSURL;
begin
  Result := New(FProtocol, FDomain + Resource);
end;

function TAWSURL.AsString: string;
begin
  Result := FProtocol + '://' + FDomain;
end;

end.
