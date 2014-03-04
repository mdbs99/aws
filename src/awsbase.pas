{
    AWS
    Copyright (C) 2013-2014  -  Marcos Douglas B. dos Santos

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit AWSBase;

{$i aws.inc}

interface

uses
  // rtl
  Classes, SysUtils;

type
  EAWSError = class(Exception)
  public
    constructor Create(AInstance: TObject; const AMsg: string); overload;
    constructor CreateFmt(AInstance: TObject; const AMsg: string; const AArgs: array of const); overload;
  end;

  TAWSObject = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ EAWSError }

constructor EAWSError.Create(AInstance: TObject; const AMsg: string);
begin
  inherited CreateFmt('%s: %s', [AInstance.ClassName, AMsg]);
end;

constructor EAWSError.CreateFmt(AInstance: TObject; const AMsg: string;
  const AArgs: array of const);
var
  S: string;
begin
  S := Format('%s: %s', [AInstance.ClassName, AMsg]);
  inherited CreateFmt(S, AArgs);
end;

{ TAWSObject }

constructor TAWSObject.Create;
begin
  inherited Create;
end;

destructor TAWSObject.Destroy;
begin
  inherited Destroy;
end;

end.

