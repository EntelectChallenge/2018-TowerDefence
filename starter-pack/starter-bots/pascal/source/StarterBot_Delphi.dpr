program StarterBot_Delphi;
{$APPTYPE CONSOLE}

uses
  SysUtils;

begin
  try
    WriteLn('Helo World. I am Robot.');
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.
