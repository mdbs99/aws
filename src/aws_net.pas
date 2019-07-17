{
  MIT License

  Copyright (c) 2013-2019 Marcos Douglas B. Santos

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
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
