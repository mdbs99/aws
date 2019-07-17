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
    class function New(Stream: TStream): IAWSStream;
    class function New: IAWSStream;
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

class function TAWSStream.New(Stream: TStream): IAWSStream;
begin
  Result := Create(Stream);
end;

class function TAWSStream.New: IAWSStream;
begin
  Result := Create(nil);
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
