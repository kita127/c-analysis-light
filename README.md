# c-analysis-light
[![Build Status](https://travis-ci.org/kita127/c-analysis-light.svg?branch=master)](https://travis-ci.org/kita127/c-analysis-light)

`c-analysis-light` is a library and tool which parse C language source.

It provides json that is written in AST info, too.

It can parse C source that is not done pre-process.

## Description

```
Usage: cal-exe FILE
  It is a tool which parse C language source.

Available options:
  -h,--help                Show this help text
  FILE                     input files
```

## Requirement
[Haskell stack](https://docs.haskellstack.org/en/stable/README/)

## Usage
```
$ cat ./examples/main_00.c 
#include <stdio.h>
#include    "../../external_inc.h"

#define HOGE    (1)
#define FUGA    (2)
#define VARI    HOGE


#if VARI == HOGE
char hoge_globvar = 100;
  #if VARI_2 == HOGE
static int hoge_globvar_2 = xxx;
  #endif
#endif    /* VARI */

int arr_var[10];

int main( void )
{
    int local_var;
    local_var = 0;
    local_var++;

    printf("local_var...%d\n", local_var);

    return (0);
}
```
```
$ stack exec cal-exe -- ./examples/main_00.c
 
[
    {
        "tag": "Preprocess",
        "contents": {
            "tag": "Include",
            "prepro": [],
            "file": "<stdio.h>"
        }
    },
    {
        "tag": "Preprocess",
        "contents": {
            "tag": "Include",
            "prepro": [],
            "file": "../../external_inc.h"
        }
    },
    {
        "tag": "Preprocess",
        "contents": {
            "tag": "Define",
            "value": "(1)",
            "name": "HOGE",
            "prepro": []
        }
    },
    {
        "tag": "Preprocess",
        "contents": {
            "tag": "Define",
            "value": "(2)",
            "name": "FUGA",
            "prepro": []
        }
    },
    {
        "tag": "Preprocess",
        "contents": {
            "tag": "Define",
            "value": "HOGE",
            "name": "VARI",
            "prepro": []
        }
    },
    {
        "tag": "Var",
        "name": "hoge_globvar",
        "prepro": [
            {
                "expr": {
                    "op": "==",
                    "tag": "Binary",
                    "left": {
                        "tag": "Identifire",
                        "name": "VARI"
                    },
                    "right": {
                        "tag": "Identifire",
                        "name": "HOGE"
                    }
                },
                "command": "#if"
            }
        ],
        "typ": [
            "char"
        ],
        "initVal": {
            "tag": "Literal",
            "value": "100"
        }
    },
    {
        "tag": "Var",
        "name": "hoge_globvar_2",
        "prepro": [
            {
                "expr": {
                    "op": "==",
                    "tag": "Binary",
                    "left": {
                        "tag": "Identifire",
                        "name": "VARI"
                    },
                    "right": {
                        "tag": "Identifire",
                        "name": "HOGE"
                    }
                },
                "command": "#if"
            },
            {
                "expr": {
                    "op": "==",
                    "tag": "Binary",
                    "left": {
                        "tag": "Identifire",
                        "name": "VARI_2"
                    },
                    "right": {
                        "tag": "Identifire",
                        "name": "HOGE"
                    }
                },
                "command": "#if"
            }
        ],
        "typ": [
            "static",
            "int"
        ],
        "initVal": {
            "tag": "Identifire",
            "name": "xxx"
        }
    },
    {
        "tag": "Var",
        "name": "arr_var",
        "prepro": [],
        "typ": [
            "int",
            "[10]"
        ],
        "initVal": null
    },
    {
        "args": [
            {
                "tag": "Var",
                "name": "",
                "prepro": [],
                "typ": [
                    "void"
                ],
                "initVal": null
            }
        ],
        "return": [
            "int"
        ],
        "tag": "Func",
        "name": "main",
        "prepro": [],
        "procs": [
            {
                "tag": "LVar",
                "var": {
                    "tag": "Var",
                    "name": "local_var",
                    "prepro": [],
                    "typ": [
                        "int"
                    ],
                    "initVal": null
                },
                "prepro": []
            },
            {
                "tag": "ExpState",
                "contents": {
                    "op": "=",
                    "tag": "Binary",
                    "left": {
                        "tag": "Identifire",
                        "name": "local_var"
                    },
                    "right": {
                        "tag": "Literal",
                        "value": "0"
                    }
                },
                "prepro": []
            },
            {
                "tag": "ExpState",
                "contents": {
                    "op": "++",
                    "tag": "PostUnary",
                    "operand": {
                        "tag": "Identifire",
                        "name": "local_var"
                    }
                },
                "prepro": []
            },
            {
                "tag": "ExpState",
                "contents": {
                    "args": [
                        {
                            "tag": "StrLiteral",
                            "value": "local_var...%d\\n"
                        },
                        {
                            "tag": "Identifire",
                            "name": "local_var"
                        }
                    ],
                    "tag": "Call",
                    "func": "printf"
                },
                "prepro": []
            },
            {
                "tag": "Return",
                "prepro": [],
                "operand": {
                    "tag": "Literal",
                    "value": "0"
                }
            }
        ]
    }
]
```






## Install
1. Clone or download this project
2. Move to the downloaded directory
3. execute `stack build`
4. execute `stack install`

## Uninstall
Delete `cal-exe` in the directory displayed by `stack path --local-bin`


## Author

[kita127](https://github.com/kita127)
