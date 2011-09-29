<?php

$good = '{"a": 10, "b": 5}';
$bad = '{"a": 10, "b": )';

var_dump(json_decode($good));
var_dump(json_last_error());

var_dump(json_decode(null));
var_dump(json_last_error());

var_dump(json_decode($bad));
var_dump(json_last_error());

var_dump(json_decode(null));
var_dump(json_last_error());

