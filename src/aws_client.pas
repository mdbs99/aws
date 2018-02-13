{
    AWS
    Copyright (c) 2013-2018 Marcos Douglas B. Santos

    See the file LICENSE.txt, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit aws_client;

{$i aws.inc}

interface

uses
  //rtl
  sysutils,
  classes,
  //synapse
  synautil,
  //aws
  aws_credentials,
  aws_http;

type
  IAWSRequest = IHTTPRequest;

  IAWSResponse = IHTTPResponse;

  IAWSClient = interface(IInterface)
  ['{9CE71A17-9ADC-4FC1-96ED-8E9C704A988C}']
    function Send(Request: IAWSRequest): IAWSResponse;
  end;

  TAWSRequest = THTTPRequest;

  TAWSResponse = THTTPResponse;

  TAWSClient = class sealed(TInterfacedObject, IAWSClient)
  private
    FSignature: IAWSSignature;
    function MakeURL(const SubDomain, Domain, Query: string): string;
  public
    constructor Create(Signature: IAWSSignature);
    class function New(Signature: IAWSSignature): IAWSClient;
    function Send(Request: IAWSRequest): IAWSResponse;
  end;

implementation

{ TAWSClient }

function TAWSClient.MakeURL(const SubDomain, Domain, Query: string): string;
begin
  Result := '';
  if FSignature.Credentials.UseSSL then
    Result += 'https://'
  else
    Result += 'http://';
  if SubDomain <> '' then
    Result += SubDomain + '.';
  Result += Domain + Query;
end;

constructor TAWSClient.Create(Signature: IAWSSignature);
begin
  inherited Create;
  FSignature := Signature;
end;

class function TAWSClient.New(Signature: IAWSSignature): IAWSClient;
begin
  Result := Create(Signature);
end;

function TAWSClient.Send(Request: IAWSRequest): IAWSResponse;
begin
  Result := THTTPSender.New(
    Request.Method,
    FSignature.Calculate(Request),
    Request.ContentType,
    MakeURL(Request.SubDomain, Request.Domain, Request.Resource),
    Request.Stream
  )
  .Send;
end;

end.

