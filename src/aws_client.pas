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

