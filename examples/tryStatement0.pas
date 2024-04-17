program Try;

begin
    try
        X := 'Moin :D';
    finally
        WriteLine('Hello');
    end;
    
    try
    finally
    end;
    
    try
    finally
        WriteLine('Hello');
    end;
    
    try
        X := 'Moin :D';
    finally
    end;
end.