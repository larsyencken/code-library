<?php

function encp($s)
{
	echo 'Raw:  '.$s."\n";
	echo 'Utf8: '.auto_encode_utf8($s)."\n";
}

function auto_encode_utf8($s)
{
	$detect_order = array(
		"UTF-8",
		"ISO-8859-1",
	);

	$enc = mb_detect_encoding($s, $detect_order, True);
	if ($enc != null)
	{
		if ($enc == "UTF-8")
		{
			return $s." [UTF-8]";
		}
		else
		{
			return mb_convert_encoding($s, "UTF-8", $enc)." [$enc]";
		}
	}
	else 
	{
		return mb_convert_encoding($s, "UTF-8")." [?]";
	}
}

$xs = array(
	"Nordeste Telecomunica\xe7\xf5es Ltda.",
	"Nordeste Telecomunicações Ltda.",
	"トンカツをもう食べられなかったんです。",
	"\x83g\x83\x93\x83J\x83c\x82\xf0\x82\xe0\x82\xa4\x90H\x82\xd7\x82\xe7\x82\xea\x82\xc8\x82\xa9\x82\xc1\x82\xbd\x82\xf1\x82\xc5\x82\xb7\x81B",
	"Someone's dog"
);

function sanitize_encoding($arr)
{
	// only consider utf8-1 or latin-1 as options
	$expected_encodings = array(
		"UTF-8",
		"ISO-8859-1"
	);

	foreach ($arr as &$v)
	{
		if (is_string($v))
		{
			$enc = mb_detect_encoding($v, $expected_encodings, True);

			if ($enc != "UTF-8")
			{
				// not UTF-8, so v needs fixing
				if ($enc != null)
				{
					// known other encoding -- lossless
					$v = mb_convert_encoding($v, "UTF-8", $enc);
				}
				else
				{
					// unknown other encoding -- lossy
					$v = mb_convert_encoding($v, "UTF-8");
				}
			}
		}
	}
	return $arr;
}

echo json_encode($xs)."\n";
echo json_last_error() == JSON_ERROR_UTF8 ."\n";
//var_dump($xs);
//echo "\n------\n";
//var_dump(sanitize_encoding($xs));
