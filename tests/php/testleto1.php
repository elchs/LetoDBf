<?php require("letocl.php");
  //$GLOBALS['leto_debug1'] = true;
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
  $testAddress = $_POST['addr'];
  if( empty( $testAddress ) ) {
    echo "<hr><center><form action=\"testleto2.php\" method=\"post\">";
    echo "Server ip address: <input type=\"text\" name=\"addr\" value=\"127.0.0.1\" size=\"18\">";
    echo " port: <input type=\"text\" name=\"port\" value=\"2812\" size=\"5\">";
    echo "<br><input type=\"submit\" value=\"  Ok  \"></form></center><hr>";
  }
  else {
    $testPort = $_POST['port'];
    $conn = letoConnect( $testAddress, intval($testPort) );
    if( $conn ) {
      $arr = letoMgInfo($conn);
      echo "<center><br><br><table border=\"1\"><tr><td colspan=\"4\" class=\"hea\">";
      echo $conn["ver"]."</td></tr><tr>";
      echo "<td class=\"tt\">Users current</td><td class=\"tn\">".$arr[0]."</td><td class=\"tt\">Users max</td><td class=\"tn\">".$arr[1]."</td></tr>";
      echo "<tr><td class=\"tt\">Tables current</td><td class=\"tn\">".$arr[2]."</td><td class=\"tt\">Tables max</td><td class=\"tn\">".$arr[3]."</td></tr>";
      echo "<tr><td class=\"tt\">Operations</td><td class=\"tn\">".$arr[5]."</td><td class=\"tt\">&nbsp</td><td class=\"tn\">&nbsp</td></tr>";
      echo "<tr><td class=\"tt\">Kbytes sent</td><td class=\"tn\">".strval(round(intval($arr[6])/1024))."</td><td class=\"tt\">Kbytes read</td><td class=\"tn\">".strval(round(intval($arr[7])/1024))."</td></tr>";
      echo "</table><br>";

      letoVarSet($conn,"g_1","var1","2",101,false,true);

      $n1 = letoVarGet($conn,"g_1","var1");
      if( $n1 )
        echo "var1 = ".strval($n1)."<br>";
      else
        echo "var1 = Error<br>";

      letoVarIncr($conn,"g_1","var1",false);
      $n1 = letoVarGet($conn,"g_1","var1");
      echo "After increment ";
      if( $n1 )
        echo "var1 = ".strval($n1)."<br>";
      else
        echo "var1 = Error<br>";

      letoVarDel($conn,"g_1","var1");
    }

    if(isset($conn))
      socket_close($conn["sock"]);
  }
 ?>

</body>
</html>