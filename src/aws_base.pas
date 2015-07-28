{
    AWS
    Copyright (C) 2013-2015 Marcos Douglas - mdbs99

    See the file LICENSE.txt, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit aws_base;

{$i aws.inc}

interface

uses
  //rtl
  classes,
  sysutils;

type
  IAWSStream = interface(IInterface)
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    function Size: Int64;
  end;

  TAWSStream = class sealed(TInterfacedObject, IAWSStream)
  private
    FStream: TMemoryStream;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    function Size: Int64;
  end;

implementation

{ TAWSStream }

constructor TAWSStream.Create(Stream: TStream);
begin
  FStream := TMemoryStream.Create;
  if Assigned(Stream) then
    FStream.LoadFromStream(Stream);
end;

destructor TAWSStream.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

procedure TAWSStream.SaveToFile(const FileName: string);
begin
  FStream.SaveToFile(FileName);
end;

function TAWSStream.Size: Int64;
begin
  Result := FStream.Size;
end;

procedure TAWSStream.SaveToStream(Stream: TStream);
begin
  FStream.SaveToStream(Stream);
end;

end.
