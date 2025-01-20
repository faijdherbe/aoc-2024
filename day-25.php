<?php

fputs($fp = fopen('php://memory', 'r+'), preg_replace(
    ['/[^\\.#]/', '/\\./', '/#/'],
    ['','0','1'],
    file_get_contents(__DIR__ . '/input/25.txt'))
); rewind($fp);

$objects = []; $count = 0;

while($object = fgets($fp, 7*5+1)) {
    $a = $objects[] = bindec(substr($object, 4, -4));
    foreach ($objects as $b) $count += ($a+$b) == ($a ^ $b);
}

echo $count . PHP_EOL;
