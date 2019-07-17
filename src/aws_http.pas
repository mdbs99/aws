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
unit aws_http;

{$i aws.inc}

interface

uses
  //rtl
  sysutils,
  classes,
  //synapse
  httpsend,
  synautil,
  ssl_openssl,
  //aws
  aws_base;

type
  IHTTPRequest = interface(IInterface)
  ['{12744C05-22B6-45BF-B47A-49813F6B64B6}']
    function Method: string;
    function SubDomain: string;
    function Domain: string;
    function Resource: string;
    function SubResource: string;
    function ContentType: string;
    function ContentMD5: string;
    function CanonicalizedAmzHeaders: string;
    function CanonicalizedResource: string;
    function Stream: IAWSStream;
    function AsString: string;
  end;

  IHTTPResponse = interface(IInterface)
  ['{6E7E8524-88B5-48B1-95FF-30D0DF40D8F7}']
    function Code: Integer;
    function Header: string;
    function Text: string;
    function Stream: IAWSStream;
  end;

  IHTTPSender = interface(IInterface)
  ['{DF9B2674-D60C-4F40-AD6A-AE158091212D}']
    function Send: IHTTPResponse;
  end;

  THTTPRequest = class sealed(TInterfacedObject, IHTTPRequest)
  private
    FMethod: string;
    FSubDomain: string;
    FDomain: string;
    FResource: string;
    FSubResource: string;
    FContentType: string;
    FContentMD5: string;
    FCanonicalizedAmzHeaders: string;
    FCanonicalizedResource: string;
    FStream: IAWSStream;
  public
    constructor Create(
      const Method, SubDomain, Domain, Resource,
      SubResource, ContentType, ContentMD5, CanonicalizedAmzHeaders,
      CanonicalizedResource: string; Stream: IAWSStream
    );
    class function New(
      const Method, SubDomain, Domain, Resource,
      SubResource, ContentType, ContentMD5, CanonicalizedAmzHeaders,
      CanonicalizedResource: string; Stream: IAWSStream
    ): IHTTPRequest;
    class function New(
      const Method, SubDomain, Domain, Resource,
      SubResource, ContentType, ContentMD5, CanonicalizedAmzHeaders,
      CanonicalizedResource: string
    ): IHTTPRequest;
    class function New(
      const Method, SubDomain, Domain, Resource,
      SubResource, CanonicalizedResource: string
    ): IHTTPRequest;
    class function New(
      const Method, SubDomain, Domain, Resource,
      CanonicalizedResource: string
    ): IHTTPRequest;
    class function New(
      const Method, SubDomain, Domain, Resource,
      CanonicalizedResource: string; Stream: IAWSStream
    ): IHTTPRequest;
    class function New(
      const Method, SubDomain, Domain,
      CanonicalizedResource: string
    ): IHTTPRequest;
    function Method: string;
    function SubDomain: string;
    function Domain: string;
    function Resource: string;
    function SubResource: string;
    function ContentType: string;
    function ContentMD5: string;
    function CanonicalizedAmzHeaders: string;
    function CanonicalizedResource: string;
    function Stream: IAWSStream;
    function AsString: string;
  end;

  THTTPResponse = class sealed(TInterfacedObject, IHTTPResponse)
  private
    FCode: Integer;
    FHeader: string;
    FText: string;
    FStream: IAWSStream;
  public
    constructor Create(Code: Integer; const Header, Text: string; Stream: IAWSStream);
    class function New(Code: Integer; const Header, Text: string; Stream: IAWSStream): IHTTPResponse;
    class function New(Code: Integer; const Header, Text: string): IHTTPResponse;
    class function New(Origin: IHTTPResponse): IHTTPResponse;
    destructor Destroy; override;
    function Code: Integer;
    function Header: string;
    function Text: string;
    function Stream: IAWSStream;
  end;

  THTTPSender = class sealed(TInterfacedObject, IHTTPSender)
  private
    FSender: THTTPSend;
    FMethod: string;
    FHeader: string;
    FContentType: string;
    FURL: string;
    FStream: IAWSStream;
  public
    constructor Create(const Method, Header, ContentType, URL: string; Stream: IAWSStream);
    class function New(const Method, Header, ContentType, URL: string; Stream: IAWSStream): IHTTPSender;
    destructor Destroy; override;
    function Send: IHTTPResponse;
  end;

implementation

{ THTTPRequest }

constructor THTTPRequest.Create(const Method, SubDomain, Domain, Resource, SubResource,
  ContentType, ContentMD5, CanonicalizedAmzHeaders,
  CanonicalizedResource: string; Stream: IAWSStream);
begin
  FMethod := Method;
  FSubDomain := SubDomain;
  FDomain := Domain;
  FResource := Resource;
  FSubResource := SubResource;
  FContentType := ContentType;
  FContentMD5 := ContentMD5;
  FCanonicalizedAmzHeaders := CanonicalizedAmzHeaders;
  FCanonicalizedResource := CanonicalizedResource;
  FStream := Stream
end;

class function THTTPRequest.New(const Method, SubDomain, Domain, Resource, SubResource,
  ContentType, ContentMD5, CanonicalizedAmzHeaders,
  CanonicalizedResource: string; Stream: IAWSStream): IHTTPRequest;
begin
  Result := Create(
    Method, SubDomain, Domain, Resource, SubResource,
    ContentType, ContentMD5, CanonicalizedAmzHeaders,
    CanonicalizedResource, Stream
  );
end;

class function THTTPRequest.New(const Method, SubDomain, Domain, Resource, SubResource,
  ContentType, ContentMD5, CanonicalizedAmzHeaders,
  CanonicalizedResource: string): IHTTPRequest;
begin
  Result := New(
    Method, SubDomain, Domain, Resource, SubResource, ContentType,
    ContentMD5, CanonicalizedAmzHeaders, CanonicalizedResource,
    TAWSStream.New
  );
end;

class function THTTPRequest.New(const Method, SubDomain, Domain, Resource, SubResource,
  CanonicalizedResource: string): IHTTPRequest;
begin
  Result := New(
    Method, SubDomain, Domain, Resource, SubResource, '',
    '', '', CanonicalizedResource,
    TAWSStream.New
  );
end;

class function THTTPRequest.New(const Method, SubDomain, Domain, Resource,
  CanonicalizedResource: string): IHTTPRequest;
begin
  Result := New(
    Method, SubDomain, Domain, Resource, '', '',
    '', '', CanonicalizedResource,
    TAWSStream.New
  );
end;

class function THTTPRequest.New(const Method, SubDomain, Domain, Resource,
  CanonicalizedResource: string; Stream: IAWSStream): IHTTPRequest;
begin
  Result := New(
    Method, SubDomain, Domain, Resource, '', '',
    '', '', CanonicalizedResource,
    Stream
  );
end;

class function THTTPRequest.New(const Method, SubDomain, Domain,
  CanonicalizedResource: string): IHTTPRequest;
begin
  Result := New(
    Method, SubDomain, Domain, '', '', '',
    '', '', CanonicalizedResource,
    TAWSStream.New
  );
end;

function THTTPRequest.Method: string;
begin
  Result := FMethod;
end;

function THTTPRequest.SubDomain: string;
begin
  Result := FSubDomain;
end;

function THTTPRequest.Domain: string;
begin
  Result := FDomain;
end;

function THTTPRequest.Resource: string;
begin
  Result := FResource;
end;

function THTTPRequest.SubResource: string;
begin
  Result := FSubResource;
end;

function THTTPRequest.ContentType: string;
begin
  Result := FContentType;
end;

function THTTPRequest.ContentMD5: string;
begin
  Result := FContentMD5;
end;

function THTTPRequest.CanonicalizedAmzHeaders: string;
begin
  Result := FCanonicalizedAmzHeaders;
end;

function THTTPRequest.CanonicalizedResource: string;
begin
  Result := FCanonicalizedResource;
end;

function THTTPRequest.Stream: IAWSStream;
begin
  Result := FStream;
end;

function THTTPRequest.AsString: string;
begin
  with TStringList.Create do
  try
    Add('Method=' + FMethod);
    Add('Resource=' + FResource);
    Add('SubResource=' + FSubResource);
    Add('ContentType=' + FContentType);
    Add('ContentMD5=' + FContentMD5);
    Add('CanonicalizedAmzHeaders=' + FCanonicalizedAmzHeaders);
    Add('CanonicalizedResource=' + FCanonicalizedResource);
    Result := Text;
  finally
    Free;
  end;
end;

{ THTTPResponse }

constructor THTTPResponse.Create(Code: Integer; const Header, Text: string;
  Stream: IAWSStream);
begin
  inherited Create;
  FCode := Code;
  FHeader := Header;
  FText := Text;
  FStream := Stream;
end;

class function THTTPResponse.New(Code: Integer; const Header, Text: string;
  Stream: IAWSStream): IHTTPResponse;
begin
  Result := Create(Code, Header, Text, Stream);
end;

class function THTTPResponse.New(Code: Integer;
  const Header, Text: string): IHTTPResponse;
begin
  Result := New(Code, Header, Text, nil);
end;

class function THTTPResponse.New(Origin: IHTTPResponse): IHTTPResponse;
begin
  Result := New(Origin.Code, Origin.Header, Origin.Text, Origin.Stream);
end;

destructor THTTPResponse.Destroy;
begin
  inherited Destroy;
end;

function THTTPResponse.Code: Integer;
begin
  Result := FCode;
end;

function THTTPResponse.Header: string;
begin
  Result := FHeader;
end;

function THTTPResponse.Text: string;
begin
  Result := FText;
end;

function THTTPResponse.Stream: IAWSStream;
begin
  Result := FStream;
end;

{ THTTPSender }

constructor THTTPSender.Create(const Method, Header, ContentType, URL: string;
  Stream: IAWSStream);
begin
  inherited Create;
  FSender := THTTPSend.Create;
  FSender.Protocol := '1.0';
  FMethod := Method;
  FHeader := Header;
  FContentType := ContentType;
  FURL := URL;
  FStream := Stream;
end;

class function THTTPSender.New(const Method, Header, ContentType, URL: string;
  Stream: IAWSStream): IHTTPSender;
begin
  Result := Create(Method, Header, ContentType, URL, Stream);
end;

destructor THTTPSender.Destroy;
begin
  FSender.Free;
  inherited Destroy;
end;

function THTTPSender.Send: IHTTPResponse;
begin
  FSender.Clear;
  FSender.Headers.Add(FHeader);
  FSender.MimeType := FContentType;
  FStream.SaveToStream(FSender.Document);
  FSender.HTTPMethod(FMethod, FURL);
  Result := THTTPResponse.New(
    FSender.ResultCode,
    FSender.Headers.Text,
    FSender.ResultString,
    TAWSStream.New(FSender.Document)
  );
end;

end.
