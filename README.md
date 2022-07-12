# iching
I Ching generator using brick

## code

### Hexagram representation
this is how hexagrams are represented internally

1. **Yao**:  
This is the binary representation of a hexagram read bottom to top, in accordance with *The Everyday I Ching, by Sarah Dening*. A broken line `- -` is represented with a `0` and an unbroken one `---` with a `1`. Eg. ䷕ would be `101001`

2. **Values**:  
Similar to yao, values describe the hexagram, but with *moving lines*. Values usualy return 2 hexagrams (Current situaltion and future possiblilities), and also tell *Further aspects of the situation*.

| Coins   | Calculation | Value | Line  | Hexagram(s)       |
| :---:   | :---------: | :---: | :---: | :---------------: |
| 3T      | 3 x 2       | `6`   | `-x-` | `- -` &rarr; `---`|
| 3H      | 3 x 3       | `9`   | `-o-` | `---` &rarr; `- -`|
| 2T + 1H | 2 + 2 + 3   | `7`   | `---` | `---`             |
| 2H + 1T | 3 + 3 + 2   | `8`   | `- -` | `- -`             |

### Sequences
A hexagram sequence is the order of hexagrams in binary read top to bottom. It determines which hexagram "description" matches the symbol. There are a few to choose from:
1. [King Wen Sequence](https://en.wikipedia.org/wiki/King_Wen_sequence) (`wenSeq`)
2. [Mawangdui Sequence](https://www.biroco.com/yijing/sequence.htm#mwd) (`mawSeq`)
3. [Binary / Shao Yong / Fu Xi Sequence](https://www.biroco.com/yijing/sequence.htm#shaoyong) (`binSeq`)
4. [Eight Places Sequence](https://www.biroco.com/yijing/sequence.htm#jingfang) (`plaSeq`)

But `wenSeq` is the default

### JSON Format
The *Hexagrams.json* file contains all the hexagrams and their descriptions from the book. Most of the format is obvious, exept for `furtherAspects`: Each aspect is a *moving line*, starting from bottom to top. In the book, it is in the format of "`VALUE` AT THE `POSITION`", here it just `POSITION`

```json
[
    {
        "number": 1, // the number of the hexagram
        "pattern": "111111", // yao pattern
        "name": { // self explanitory
            "chinese": "乾",
            "pinyin": "qián",
            "english": "The Creative Force"
        },
        "symbol": "䷀", // UTF-8 symbol
        "quote": "I must Create a System...", // quote from the book
        "quoteAuthor": "William Blake", // author of quote
        "desc": "Success is assured when...", //description
        "descPersonal": "Dynamic energy, unless it...", // personal description
        "furtherAspects": [ // described above
            [1,"Be patient. The time..."],
            [2,"Aim high. Your abilities..."],
            [3,"New opportunities are offered..."],
            [4,"You are at a crossroads..."],
            [5,"This is one of..."],
            [6,"You have become too..."]
        ]
    },
    ...
]
```

