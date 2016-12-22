IF "%1." == "." (
   set ADDR="127.0.0.1:2812" 
) ELSE (
   set ADDR="%1"
)

ron %ADDR%
test_dbf %ADDR%
test_dbfe %ADDR%
test_file %ADDR%
test_filt %ADDR%
test_ta %ADDR%
test_tr %ADDR%
test_var %ADDR%
test_var %ADDR%
