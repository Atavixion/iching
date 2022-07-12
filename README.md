# iching
I Ching generator using brick

## code

### Hexagram representation
this is how hexagrams are represented internally

1. **Yao**
This is the binary representation of a hexagram read bottom to top, in accordance with *The Everyday I Ching, by Sarah Dening*. A broken line `- -` is represented with a `0` and an unbroken one `---` with a `1`. Eg. ䷕ would be `101001`

2. **Values**
Similar to yao, values describe the hexagram, but with *moving lines*. Values usualy return 2 hexagrams (Current situaltion and future possiblilities), and also tell *Further aspects of the situation*.

| Coins | Calculation | Value | Line  |
| :---: | :---------: | :---: | :---: |
| 3T    | 3 x 2       | `6`   | `-x-` |
| 3H    | 3 x 3       | `9`   | `-o-` |
| 2T + 1H | 2 + 2 + 3 | `7`   | `---` |
| 2H + 1T | 3 + 3 + 2 | `8`   | `- -` |

### Sequences
The hexagram sequence is the order of hexagrams in binary read top to bottom. It determines which hexagram "description" matches the symbol. There are afew to choose from:
1. [King Wen Sequence](https://en.wikipedia.org/wiki/King_Wen_sequence) (`wenSeq`)
2. [Mawangdui Sequence](https://www.biroco.com/yijing/sequence.htm#mwd) (`mawSeq`)
3. [Binary / Shao Yong / Fu Xi Sequence](https://www.biroco.com/yijing/sequence.htm#shaoyong) (`binSeq`)
4. [Eight Places Sequence](https://www.biroco.com/yijing/sequence.htm#jingfang) (`plaSeq`)

But the King Wen Sequence (`wenSeq`) is the default