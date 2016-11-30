<?php
/*
 * Php client module for LetoDB
 * Copyright 2014 Alexander S. Kresin <alex / at / kresin.ru>
 * www - http://www.kresin.ru
 */
$leto_debug1 = false;

function letoRead( $conn ) {
  if( $conn["proto"] == 1 ) {
    $awr = socket_read( $conn["sock"], 1024 );
  }
  else {
    $awr = socket_read( $conn["sock"], 4 );
    $a1 = unpack( "V", $awr );
    $awr = socket_read( $conn["sock"], $a1[1] );
  }
  if( $GLOBALS['leto_debug1'] )
    echo $awr."<br />";
  return $awr;
}

function letoDataSend( $conn, $data, $datalen ) {
  if( $GLOBALS['leto_debug1'] )
    echo "Send to server: ".$data."<br />";
  if( $conn["proto"] == 1 )
    socket_write( $conn["sock"], $data, $datalen );
  else
    socket_write( $conn["sock"], pack( "V", $datalen ).$data, $datalen + 4 );
  return letoRead( $conn );
}

function letoConnect( $addr, $port ) {
  $socket = socket_create(AF_INET, SOCK_STREAM, SOL_TCP);
  if( $socket >= 0 )
    if( !socket_connect( $socket, $addr, $port ) ) {
      socket_close( $socket );
      $socket = -1;
    }
  if( $socket < 0 ) {
    echo "Error : ".socket_strerror(socket_last_error())."<br />";
    return NULL;
  }

  if( $GLOBALS['leto_debug1'] )
    echo "OK: ";
  $conn = array( 'sock' => $socket, 'proto' => 1, 'ver' => '' );
  $awr = socket_read( $socket, 4 );
  if( strcmp( $awr, "Leto" ) == 0 )
    $awr = $awr.socket_read( $socket, 1024 );
  else {
    $conn["proto"] = 2;
    $a1 = unpack( "V", $awr );
    $awr = socket_read( $socket, $a1[1] );
  }
  $a1 = explode( ";",$awr );
  $conn["ver"] = $a1[0];
  if( $GLOBALS['leto_debug1'] )
    echo $awr."<br />";

  $msg = "I;0.99;WEB;WebClient;Alex;;EN;dd/mm/yy;T;;";
  letoDataSend( $conn, $msg, strlen($msg) );

  return $conn;
}

function letoMgInfo( $conn ) {
  $msg = "M;00;";
  $awr = letoDataSend( $conn, $msg, strlen( $msg ) );
      
  return explode( ";",$awr );
}

function letoVarGet( $conn, $group, $var ) {
  $msg = "V;g;".$group.";".$var.";";
  $awr = letoDataSend( $conn, $msg, strlen( $msg ) );
  if( substr( $awr, 0, 1 ) == "+" ) {
    $pos = strpos( $awr, ';' );
    if( $pos === false )
      return NULL;
    else {
      $a1 = explode( ";",$awr );
      return $a1[1];
    }
  }
  else
    return NULL;
}

function letoVarSet( $conn, $group, $var, $type, $value, $lReturn, $lCreate ) {
  $fl1 = $lCreate ? '!' : ' ';
  $fl2 = $lReturn ? '!' : ' ';
  $msg = "V;s;".$group.";".$var.";".strval( $value ).";".$type.$fl1.$fl2.";";
  $awr = letoDataSend( $conn, $msg, strlen( $msg ) );
  if( substr( $awr, 0, 1 ) == "+" ) {
    $pos = strpos( $awr, ';' );
    if( $pos === false )
      return NULL;
    else {
      if( $lReturn )
        return substr( $awr, 1, $pos - 1 );
      else
        return true;
    }
  }
  else
    return NULL;
}

function letoVarIncr( $conn, $group, $var, $lCreate ) {
  $fl1 = $lCreate ? '!' : ' ';
  $msg = "V;i;".$group.";".$var.";2".$fl1."!;";
  $awr = letoDataSend( $conn, $msg, strlen( $msg ) );
  if( substr( $awr, 0, 1 ) == "+" ) {
    $pos = strpos( $awr, ';' );
    if( $pos === false )
      return NULL;
    else
      return intval( substr( $awr, 1, $pos - 1 ) );
  }
  else
    return NULL;
}

function letoVarDecr( $conn, $group, $var, $lCreate ) {
  $fl1 = $lCreate ? '!' : ' ';
  $msg = "V;d;".$group.";".$var.";2".$fl1."!;";
  $awr = letoDataSend( $conn, $msg, strlen( $msg ) );
  if( substr( $awr, 0, 1 ) == "+" ) {
    $pos = strpos( $awr, ';' );
    if( $pos === false )
      return NULL;
    else
      return intval( substr( $awr, 1, $pos-1 ) );
  }
  else
    return NULL;
}

function letoVarDel( $conn, $group, $var ) {
  $msg = "V;r;".$group.";".$var.";";
  $awr = letoDataSend( $conn, $msg, strlen( $msg ) );
  if( substr( $awr, 0, 1 ) == "+" )
      return true;
  else
    return NULL;
}

?>
