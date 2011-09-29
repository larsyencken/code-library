<?php

$xs = array(
	"cats" => 2,
	"suburb" => "Collingwood",
	"company" => "99designs"
);

function mod_array1($a)
{
	$a["dogs"] = 0;
}

function mod_array2(&$a)
{
	$a["dogs"] = 0;
}

var_dump($xs);
echo "----\n";
mod_array1($xs);
var_dump($xs);
echo "----\n";
mod_array2($xs);
var_dump($xs);
