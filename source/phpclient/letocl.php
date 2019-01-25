<?php
/*
 * Php client module for LetoDB
 * Copyright 2014 Alexander S. Kresin <alex / at / kresin.ru>
 * adaption for LetoDBf 2019 Rolf 'elch' Beckmann
 * www - http://www.kresin.ru
 */

$leto_debug1 = false;
$leto_dateform = "dd/mm/yy";
$leto_language = "EN";

/* variable type */
const LETOVAR_LOG    = "1";
const LETOVAR_NUM    = "2";
const LETOVAR_STR    = "3";
const LETOVAR_DAT    = "5";

/* variable flags */
const LETO_VCREAT    =  1;
const LETO_VOWN      =  2;
const LETO_VDENYWR   =  4;
const LETO_VDENYRD   =  8;
const LETO_VPREVIOUS = 16;


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

function letoConnect( $addr, $port, $user, $passwd ) {
  $socket = socket_create(AF_INET, SOCK_STREAM, SOL_TCP);
  if( $socket >= 0 ) {
    if( ! isset( $addr ) )
      $addr = "127.0.0.1";
    if( ! isset( $port ) )
      $port = 2812;
    if( ! isset( $user ) )
      $user = "";
    if( ! isset( $passwd ) )
      $passwd = "";
    if( ! socket_connect( $socket, $addr, $port ) ) {
      socket_close( $socket );
      $socket = -1;
    }
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
    $conn["proto"] = 3;
    $a1 = unpack( "V", $awr );
    $awr = socket_read( $socket, $a1[1] );
  }
  $a1 = explode( ";",$awr );
  $conn["ver"] = $a1[0];
  if( $GLOBALS['leto_debug1'] )
    echo $awr."<br />";

  $msg = "J;3.00;WWW;PHPClient;".$user.";".$passwd.";".$leto_language.";TTT0;".$leto_dateform.";1900;;";
  /* LETOCMD_intro */
  $awr = letoDataSend( $conn, $msg, strlen($msg) );
  if( substr( $awr, 0, 1 ) == "+" ) {
    return $conn;
  }
  else
    return NULL;
}

function letoMgInfo( $conn ) {
  $msg = "M;00;";
  $awr = letoDataSend( $conn, $msg, strlen( $msg ) );
  if( substr( $awr, 0, 1 ) == "+" ) {
    return explode( ";", substr( $awr, 1 ) );
  }
  else
    return NULL;
}

function letoVarGet( $conn, $group, $var ) {
  /* LETOCMD_var */
  $msg = "V;g;".$group.";".$var.";";
  $awr = letoDataSend( $conn, $msg, strlen( $msg ) );
  if( substr( $awr, 0, 1 ) == "+" ) {
    $type = substr( $awr, 1, 1 );  /* TYPE character -- followed by ';' */
    if( $type == LETOVAR_STR or $type == LETOVAR_DAT )
      return substr( $awr, 3 );
    elseif( $type == LETOVAR_LOG ) {
       if( substr( $awr, 3, 1 ) == "1" )
          return true;
       else
          return false;
    }
    elseif( $type == LETOVAR_NUM ) {
       return strval( substr( $awr, 3 ) );
    }
    else {
      return NULL;
    }
  }
  else
    return NULL;
}

function addLen( $len ) {
  if( $len < 256 ) {
    $msg = chr( 1 );
    $msg .= chr( $len );
  }
  else {
    $msg = chr( 2 );
    $msg .= chr( $len & 255 );
    $msg .= chr( ( $$len >> 8 ) & 255 );
  }
  return $msg;
}

function letoVarFlag( $flags, $nflag ) {
 if( $nflag == 1 )
   $flag = 32 | ( $flags & ( LETO_VCREAT | LETO_VOWN | LETO_VDENYWR | LETO_VDENYRD ) );
 else
   $flag = 32 | ( $flags & LETO_VPREVIOUS );
 return chr( $flag );
}

function letoVarSet( $conn, $group, $var, $type, $value, $flags ) {
  $fl1 = letoVarFlag( $flags, 1 );
  $fl2 = letoVarFlag( $flags, 2 );
  $addlen = addLen( strlen( $value ) );
  $msg = "V;s;".$group.";".$var.";".$type.$fl1.$fl2.";".$addlen.$value;
  $awr = letoDataSend( $conn, $msg, strlen( $msg ) );
  if( substr( $awr, 0, 1 ) == "+" ) {
    return true;
  }
  else
    return false;
}

function letoVarSum( $conn, $group, $var, $flags, $incr ) {
  $fl1 = letoVarFlag( $flags, 1 );
  $fl2 = letoVarFlag( $flags, 2 );
  $msg = "V;i;".$group.";".$var.";2".$fl1.$fl2.";".$incr.";";
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

function letoVarIncr( $conn, $group, $var, $flags ) {
  $fl1 = letoVarFlag( $flags, 1 );
  $fl2 = letoVarFlag( $flags, 2 );
  $msg = "V;i;".$group.";".$var.";2".$fl1.$fl2.";1;";
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

function letoVarDecr( $conn, $group, $var, $flags ) {
  $fl1 = letoVarFlag( $flags, 1 );
  $fl2 = letoVarFlag( $flags, 2 );
  $msg = "V;d;".$group.";".$var.";2".$fl1.$fl2.";1;";
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

function letoUdfExist( $conn, $func ) {
  $msg = "U;3;;;".$func.";";
  /* LETOCMD_udf_fun */
  $awr = letoDataSend( $conn, $msg, strlen( $msg ) );
  if( substr( $awr, 0, 1 ) == "+" )
    return true;
  else
    return false;
}

function letoUdf( $conn, $func, $ldeleted, $lexclusive ) {
  $fl1 = $ldeleted ? 'A' : '@';
  $fl2 = $lexclusive ? 'T' : 'F';
  $msg = "U;2;".$fl1.";0;".$func.";".$fl2.";0;";
  /* LETOCMD_udf_fun */
  $awr = letoDataSend( $conn, $msg, strlen( $msg ) );
  if( substr( $awr, 0, 1 ) == "+" )
    return true;
  else
    return false;
}

?>
