program StarterBot_Pascal;

uses SysUtils;

begin
  try
    Writeln('Hello world, I am robot.');
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.

