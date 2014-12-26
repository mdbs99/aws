{
    AWS
    Copyright (C) 2013-2014 by mdbs99

    See the file LICENSE.txt, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit aws_http;

{$mode objfpc}{$H+}

interface

uses
  //rtl
  sysutils,
  //synapse
  httpsend,
  synautil,
  ssl_openssl;

type
  IHTTPResult = interface(IInterface)
    function ResultCode: Integer;
    function ResultText: string;
  end;

  IHTTPSender = interface(IInterface)
    function Send: IHTTPResult;
  end;

  THTTPResult = class(TInterfacedObject, IHTTPResult)
  private
    FResultCode: Integer;
    FResultText: string;
  public
    constructor Create(ResultCode: Integer; const ResultText: string);
    constructor Create(const Origin: IHTTPResult);
    function ResultCode: Integer;
    function ResultText: string;
  end;

  THTTPSender = class sealed(TInterfacedObject, IHTTPSender)
  private
    FSender: THTTPSend;
    FMethod: string;
    FHeader: string;
    FContentType: string;
    FURI: string;
  public
    constructor Create(const Method, Header, ContentType, URI: string); reintroduce;
    destructor Destroy; override;
    function Send: IHTTPResult;
  end;

implementation

{ THTTPResult }

constructor THTTPResult.Create(ResultCode: Integer; const ResultText: string);
begin
  FResultCode := ResultCode;
  FResultText := ResultText;
end;

constructor THTTPResult.Create(const Origin: IHTTPResult);
begin
  Create(Origin.ResultCode, Origin.ResultText);
end;

function THTTPResult.ResultCode: Integer;
begin
  Result := FResultCode;
end;

function THTTPResult.ResultText: string;
begin
  Result := FResultText;
end;

{ THTTPSender }

constructor THTTPSender.Create(const Method, Header, ContentType, URI: string);
begin
  inherited Create;
  FMethod := Method;
  FHeader := Header;
  FContentType := ContentType;
  FURI := URI;
  FSender := THTTPSend.Create;
  FSender.Protocol := '1.0';
end;

destructor THTTPSender.Destroy;
begin
  FSender.Free;
  inherited Destroy;
end;

function THTTPSender.Send: IHTTPResult;
begin
  FSender.Clear;
  FSender.Headers.Add(FHeader);
  if FContentType <> '' then
    FSender.MimeType := FContentType;
  FSender.HTTPMethod(FMethod, FURI);
  Result := THTTPResult.Create(
    FSender.ResultCode,
    FSender.ResultString);
end;

end.
