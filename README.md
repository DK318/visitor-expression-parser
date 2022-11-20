# Парсер арифметических выражений

Пример исполнения:
```bash
$ stack run

(23 + 10) * 5 - 3 * (32 + 5) * (10 - 4 * 5) + 8 / 2

NUMBER(23) NUMBER(10) PLUS NUMBER(5) MULT NUMBER(3) NUMBER(32) NUMBER(5) PLUS MULT NUMBER(10) NUMBER(4) NUMBER(5) MULT MINUS MULT MINUS NUMBER(8) NUMBER(2) DIV PLUS

1279
```
Запуск программы: `stack build`.
