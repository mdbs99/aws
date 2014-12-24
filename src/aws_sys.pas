{
    AWS
    Copyright (C) 2013-2014 by mdbs99

    See the file LICENSE.txt, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit aws_sys;

{$i aws.inc}

interface

type
  PIInterface = ^IInterface;

  IDisposable = interface
  ['{85C70F91-68D9-4967-B482-9FCD29428A0B}']
    procedure Free;
  end;

procedure SetWeak(InterfaceField: PIInterface; const Value: IInterface);

implementation

procedure SetWeak(InterfaceField: PIInterface; const Value: IInterface);
begin
  PPointer(InterfaceField)^ := Pointer(Value);
end;

end.

