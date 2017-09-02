package main

import (
	"bufio"
	"flag"
	"fmt"
	"sort"
	"strings"
)

func uniqStrings(a []string) []string {
	if len(a) > 0 {
		origin := a[0]
		// a = a[1:]
		i := 1
		for _, s := range a[1:] {
			if s != origin {
				a[i] = s
				origin = s
				i++
			}
		}
		return a[:i]
	} else {
		return a
	}
}

func squeezeSpaces(bs []byte) []byte {
	i := 0
	fstSpace := true
	for _, b := range bs {
		if b == ' ' {
			if fstSpace {
				bs[i] = ' '
				fstSpace = false
				i++
			}
		} else {
			bs[i] = b
			fstSpace = true
			i++
		}
	}
	return bs[:i]
}

// implement couneter using closures with three function: to increase count
// of given word, to get count and to list most used words
func makeCounter() (adder func(string), getter func(string) int, top func(int) []string) {
	h := make(map[string]int)
	adder = func(s string) { h[s]++ }
	getter = func(s string) int { return h[s] }
	top = func(n int) []string {
		keys := make([]string, 0)
		for key := range h {
			keys = append(keys, key)
		}
		sort.Slice(keys, func(i, j int) bool { return h[keys[i]] > h[keys[j]] })
		return keys[:n]
	}
	return
}

func main() {
	// var arrSP = flag.String("arrS", "", "")
	var sP = flag.String("s", "", "")

	flag.Parse()

	// arrS := strings.Split(*arrSP, ",")
	s := *sP

	// fmt.Printf("%s\n", squeezeSpaces([]byte(s)))

	adder, _, top := makeCounter()

	scanner := bufio.NewScanner(strings.NewReader(s))
	scanner.Split(bufio.ScanWords)
	for scanner.Scan() {
		adder(scanner.Text())
	}

	fmt.Println(top(2))
}
