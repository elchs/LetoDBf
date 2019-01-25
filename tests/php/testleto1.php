<?php require("letocl.php");
  // $GLOBALS['leto_debug1'] = true;
?>
<html>
<head><title> Leto test </title>
<style><!--
  .hea { text-align: center; padding: 0.5em; color: #fff; background: #800000; }
  .tt  { text-align: left; padding: 0.5em; color: #000; background: #c0c0c0; }
  .tn  { text-align: right; padding: 0.5em; color: #fff; background: #0000ff; }
--></style>
</head>
<body>
 <?php
  $testAddress = "192.168.2.47";

  if( isset( $testAddress ) ) {
    $conn = letoConnect( $testAddress, $testPort,$user, $passwd );
    if( $conn ) {
      $arr = letoMgInfo($conn);
      echo "<center><br><br><table border=\"1\"><tr><td colspan=\"4\" class=\"hea\">";
      echo $conn["ver"]."</td></tr><tr>";
      echo "<td class=\"tt\">Users current</td><td class=\"tn\">".$arr[0]."</td><td class=\"tt\">Users max</td><td class=\"tn\">".$arr[1]."</td></tr>";
      echo "<tr><td class=\"tt\">Tables current</td><td class=\"tn\">".$arr[2]."</td><td class=\"tt\">Tables max</td><td class=\"tn\">".$arr[3]."</td></tr>";
      echo "<tr><td class=\"tt\">Operations</td><td class=\"tn\">".$arr[5]."</td><td class=\"tt\">&nbsp</td><td class=\"tn\">&nbsp</td></tr>";
      echo "<tr><td class=\"tt\">Kbytes sent</td><td class=\"tn\">".strval(round(intval($arr[6])/1024))."</td><td class=\"tt\">Kbytes read</td><td class=\"tn\">".strval(round(intval($arr[7])/1024))."</td></tr>";
      echo "</table><br>";

      letoVarSet($conn,"g_1","var0","2",987654321.123456789,1);
      $n1 = letoVarGet($conn,"g_1","var0");
      if( $n1 ) {
        echo "var0 = ".$n1."<br />";
        letoVarSum($conn,"g_1","var0",0,-321.123);
        $n1 = letoVarGet($conn,"g_1","var0");
        if( $n1 )
          echo "var0 + -321.123 = ".$n1."<br />";
      }
      else
        echo "var0 = Error<br />";

      letoVarSet($conn,"g_1","var1","2",101,1);
      $n1 = letoVarGet($conn,"g_1","var1");
      if( $n1 )
        echo "var1 = ".$n1."<br />";
      else
        echo "var1 = Error<br />";

      letoVarIncr($conn,"g_1","var1",0);
      $n1 = letoVarGet($conn,"g_1","var1");
      echo "After increment ";
      if( $n1 )
        echo "var1 = ".$n1."<br />";
      else
        echo "var1 = Error<br />";

      letoVarDecr($conn,"g_1","var1",0);
      $n1 = letoVarGet($conn,"g_1","var1");
      echo "After decrement ";
      if( $n1 )
        echo "var1 = ".$n1."<br />";
      else
        echo "var1 = Error<br />";

      letoVarSum($conn,"g_1","var1",0,-10);
      letoVarSum($conn,"g_1","var1",0,+20.111);
      $n1 = letoVarGet($conn,"g_1","var1");
      echo "After sum -10/ sum +20 ";
      if( $n1 )
        echo "var1 = ".$n1."<br />";
      else
        echo "var1 = Error<br />";

      letoVarSet($conn,"g_1","var2","3","Leto_DBf in PHP",1);
      $n2 = letoVarGet($conn,"g_1","var2");
      if( $n2 )
        echo "var2 = '".$n2."'<br />";
      else
        echo "var2 = Error<br />";

      letoVarSet($conn,"g_1","var3","5","T",1);
      $n3 = letoVarGet($conn,"g_1","var3");
      if( $n3 )
        echo "var3 = #true#<br />";
      else
        echo "var3 = Error<br />";

      letoVarSet($conn,"g_1","var4","5","2019/01/25",1);
      $n2 = letoVarGet($conn,"g_1","var4");
      if( $n2 )
        echo "var4 = '".$n2."'<br />";
      else
        echo "var4 = Error<br />";

      letoVarDel($conn,"g_1","var0");
      letoVarDel($conn,"g_1","var1");
      letoVarDel($conn,"g_1","var2");
      letoVarDel($conn,"g_1","var3");
      letoVarDel($conn,"g_1","var4");


      echo "<br />Leto_UDF action ...<br />";
      $udf = "TRANSFORM";
      $udfexist = letoUdfExist($conn,$udf);
      if( $udfexist )
        echo "function '".$udf."' available at LetoDBf<br />";
      else
        echo "Error with UDF test<br />";

      $udf = "DATE";
      $udfexec = letoUdf($conn,$udf,true,false);
      if( $udfexec )
        echo "function '".$udf."' executed at LetoDBf<br />";
      else
        echo "Error with UDF exec<br />";

    }

    if(isset($conn))
      socket_close($conn["sock"]);
  }
 ?>

</body>
</html>