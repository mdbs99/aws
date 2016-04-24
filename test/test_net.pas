{
    AWS
    Copyright (C) 2013-2016 Marcos Douglas - mdbs99

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit test_net;

{$i ../src/aws.inc}

interface

uses
  //rtl
  classes,
  sysutils,
  fpcunit,
  testregistry,
  //aws
  aws_net;

type
  TAWSURLTest = class abstract(TTestCase)
  published
    procedure TestWithSubDomain;
    procedure TestWithResource;
  end;

implementation

{ TAWSURLTest }

procedure TAWSURLTest.TestWithSubDomain;
begin
  AssertEquals(
    TAWSURL.New('localhost')
      .WithSubDomain('subdomain')
      .AsString,
    'http://subdomain.localhost'
  );
end;

procedure TAWSURLTest.TestWithResource;
begin
  AssertEquals(
    TAWSURL.New('localhost')
      .WithResource('/resource')
      .AsString,
    'http://localhost/resource'
  );
end;

initialization
  RegisterTest('net', TAWSURLTest);

end.

